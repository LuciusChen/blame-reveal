# Product Requirements

## Summary

`blame-reveal` is an Emacs package for contextual Git blame. It should show
recent authorship information inline with editing, without forcing the user to
leave the current buffer or switch into a separate blame view.

## Primary Users

- Emacs users reading active code and wanting recent blame context at point.
- Users navigating commit history for a line or block without opening Magit
  first.
- Users working in large files who need blame information without paying the
  cost of rendering the entire buffer at once.

## Core Jobs To Be Done

- Show recent commit context for the visible portion of a file.
- Keep current-line and current-block blame information obvious while moving.
- Let the user drill into commit details, diffs, line history, and recursive
  blame from the current location.
- Preserve acceptable responsiveness on large files and during scrolling.

## Product Goals

- Emacs-native interaction model: commands live behind explicit entry points,
  primarily `M-x` and the `C-c l` prefix.
- Context over noise: prioritize recent and visible blame information instead of
  rendering every commit equally.
- Predictable performance: prefer viewport-local rendering, caching, and async
  work when needed.
- Optional power features: recursive blame, focus mode, and transient config
  should extend the base package without becoming required for normal use.

## Non-Goals

- Replacing Magit as a full history browser.
- Acting like a modal interface that takes over ordinary editing keys.
- Rendering all blame metadata at all times regardless of file size.
- Hiding Git failures behind silent fallback behavior.

## UX Requirements

- Enabling `blame-reveal-mode` must not steal ordinary movement or insertion
  keys.
- Minor-mode commands must remain reachable through stable, documented entry
  points.
- Error states should explain what is unavailable and what the user can do next.
- Global mode should behave like a normal globalized minor mode: opt in
  eligible buffers and stop auto-enabling when disabled.

## Technical Constraints

- Support Emacs 27.1+.
- Depend on Git CLI.
- Keep optional integrations optional: Magit, transient, diff-hl, nerd-icons.
- Byte-compilation must stay clean.
- High-risk behavior changes should have ERT coverage.
