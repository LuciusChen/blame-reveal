;;; blame-reveal-state.el --- State machine for async operations -*- lexical-binding: t; -*-

;;; Commentary:
;; State machine for managing async loading operations.
;; Coordinates between initial load, expansion, and recursive blame operations.

;;; Code:

(require 'blame-reveal-core)

;;; State Machine Core Functions

(defun blame-reveal--state-can-start-p (operation)
  "Check if OPERATION can start in current state.
Returns (CAN-START-P . REASON)."
  (let ((status blame-reveal--state-status)
        (current-op blame-reveal--state-operation))
    (cond
     ((eq status 'idle)
      '(t . "idle"))
     ((and (eq status 'loading)
           (eq current-op operation))
      `(nil . ,(format "already loading %s" operation)))
     ((and (eq status 'loading)
           (eq current-op 'initial)
           (eq operation 'expansion))
      '(t . "upgrade initial->expansion"))
     ((eq operation 'recursive)
      '(t . "recursive cancels current"))
     (t
      `(nil . ,(format "busy: %s %s" status current-op))))))

(defun blame-reveal--state-start (operation mode &optional metadata)
  "Start OPERATION in MODE with optional METADATA.
Returns t if started successfully."
  (pcase-let ((`(,can-start . ,reason)
               (blame-reveal--state-can-start-p operation)))
    (if can-start
        (progn
          (when (not (eq blame-reveal--state-status 'idle))
            (blame-reveal--state-cancel reason))
          ;; Important: Ensure that the old process has been cleaned up before generating a new ID.
          (when blame-reveal--state-process
            (when (process-live-p blame-reveal--state-process)
              (delete-process blame-reveal--state-process))
            (setq blame-reveal--state-process nil))
          ;; Generate a new process ID
          (setq blame-reveal--process-id (blame-reveal--generate-process-id))
          (setq blame-reveal--state-status 'loading
                blame-reveal--state-operation operation
                blame-reveal--state-mode mode
                blame-reveal--state-metadata metadata
                blame-reveal--state-process nil
                blame-reveal--state-buffer nil)
          (blame-reveal--start-loading-animation)
          (message "[State] Start: %s (%s) - %s [ID: %s]"
                   operation mode reason blame-reveal--process-id)
          t)
      (message "[State] Cannot start %s: %s" operation reason)
      nil)))

(defun blame-reveal--state-set-async-resources (process buffer)
  "Set async PROCESS and BUFFER for current operation."
  ;; Ensure there is a process ID
  (unless blame-reveal--process-id
    (setq blame-reveal--process-id (blame-reveal--generate-process-id)))
  ;; Store process ID to the process object.
  (when (and process (processp process))
    (process-put process 'blame-reveal-process-id blame-reveal--process-id)
    (process-put process 'source-file (buffer-file-name)))
  (setq blame-reveal--state-process process
        blame-reveal--state-buffer buffer))

(defun blame-reveal--state-verify-process (process)
  "Verify PROCESS belongs to current operation."
  (and (processp process)
       ;; If the process is no longer in state-process, it means it has been replaced.
       (eq process blame-reveal--state-process)
       (let ((proc-id (process-get process 'blame-reveal-process-id))
             (proc-file (process-get process 'source-file)))
         (and proc-id
              blame-reveal--process-id
              (equal proc-id blame-reveal--process-id)
              (equal proc-file (buffer-file-name))))))

(defun blame-reveal--state-transition (new-status)
  "Transition to NEW-STATUS."
  (let ((old-status blame-reveal--state-status))
    (setq blame-reveal--state-status new-status)
    (message "[State] Transition: %s -> %s (op:%s)"
             old-status new-status blame-reveal--state-operation)))

(defun blame-reveal--state-cleanup-async ()
  "Cleanup async resources (process and buffer)."
  (when blame-reveal--state-process
    (when (process-live-p blame-reveal--state-process)
      (delete-process blame-reveal--state-process))
    (setq blame-reveal--state-process nil))
  (when blame-reveal--state-buffer
    (when (buffer-live-p blame-reveal--state-buffer)
      (kill-buffer blame-reveal--state-buffer))
    (setq blame-reveal--state-buffer nil)))

(defun blame-reveal--state-complete ()
  "Complete current operation successfully.
Ensures cleanup happens even if rendering fails."
  (unwind-protect
      (progn
        (blame-reveal--stop-loading-animation)
        (blame-reveal--state-cleanup-async))
    ;; Always reset state
    (setq blame-reveal--state-status 'idle
          blame-reveal--state-operation nil
          blame-reveal--state-mode nil
          blame-reveal--state-metadata nil
          blame-reveal--process-id nil))
  (message "[State] Complete"))

(defun blame-reveal--state-reset-internal ()
  "Reset all internal state variables and async resources (process/buffer).
DOES NOT handle UI elements or application timers."
  ;; Keep the loading animation stopped
  (ignore-errors (blame-reveal--stop-loading-animation))

  (blame-reveal--state-cleanup-async)

  (setq blame-reveal--state-status 'idle
        blame-reveal--state-operation nil
        blame-reveal--state-mode nil
        blame-reveal--state-metadata nil
        blame-reveal--process-id nil))

(defun blame-reveal--state-error (error-msg)
  "Handle error with ERROR-MSG. Performs emergency UI cleanup defensively."
  (message "[State] Error: %s" error-msg)
  (ignore-errors (blame-reveal--clear-header))

  (blame-reveal--state-reset-internal)
  (setq blame-reveal--state-status 'error)
  (run-with-timer 0.1 nil
                  (lambda ()
                    (when (eq blame-reveal--state-status 'error)
                      (setq blame-reveal--state-status 'idle)))))

(defun blame-reveal--state-cancel (reason)
  "Cancel current operation for REASON. Caller must clean up UI artifacts."
  (message "[State] Cancel: %s" reason)
  (blame-reveal--state-reset-internal))

(defun blame-reveal--state-is-busy-p ()
  "Check if state machine is busy."
  (not (eq blame-reveal--state-status 'idle)))

(provide 'blame-reveal-state)
;;; blame-reveal-state.el ends here
