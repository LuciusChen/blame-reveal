# 2026-03-24 Input Model And Lifecycle Cleanup

## Context

The package had grown beyond a simple display mode. It included recursive
navigation, focus mode, transient configuration, async loading, and several
cross-module advice paths. The result was feature-rich, but some interaction
choices drifted away from normal Emacs expectations.

## Problems Observed

- `blame-reveal-mode` commands were reachable through `C-c l`, but focus mode
  also installed bare keys like `F`, `n`, and `N`.
- The mode keymap was added through `emulation-mode-map-alists`, which gave a
  passive display mode unusually high precedence.
- Theme hooks and transient advice were installed and removed per buffer, so one
  buffer disabling the mode could break another buffer still using it.
- `blame-reveal-global-mode` was implemented as an ad hoc global mode instead of
  a standard globalized minor mode.
- Header update suppression during transient setup had effectively regressed due
  to a duplicate function definition.

## Decisions

- Keep command entry points explicit: `M-x` and `C-c l`.
- Treat `blame-reveal-mode` as a normal minor mode, not an emulation layer.
- Install shared integrations only while at least one blame buffer is active.
- Use `define-globalized-minor-mode` for global enablement.
- Add regression tests for the fixed interaction and lifecycle paths before
  expanding feature coverage.

## Why This Direction

The package is useful precisely because it augments ordinary editing. Once it
starts competing with ordinary editing keys or tears down shared state from the
wrong buffer, the feature set stops feeling additive. The cleanup favored
predictable Emacs behavior over convenience shortcuts.

## Follow-up Work

- Expand tests around recursive blame and focus mode.
- Add manual verification notes for optional integrations.
- Continue tightening architectural boundaries between buffer-local rendering
  state and global integration state.
