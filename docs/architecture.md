# Architecture

## Module Boundaries

- `blame-reveal.el`
  Main entry point. Defines user options, commands, the mode, and package-level
  integration points.
- `blame-reveal-core.el`
  Shared structs, constants, caches, and buffer-local state.
- `blame-reveal-git.el`
  Git command construction, blame parsing, sync/async loading, and lazy range
  expansion.
- `blame-reveal-color.el`
  Commit ranking, time-window calculation, and color strategy logic.
- `blame-reveal-header.el`
  Header formatting and style-specific display construction.
- `blame-reveal-ui.el`
  Overlay registry, rendering lifecycle, scrolling, loading animation, and
  header updates.
- `blame-reveal-recursive.el`
  Revision-stack navigation and recursive blame behavior.
- `blame-reveal-focus.el`
  Commit focus mode and focused-block navigation.
- `blame-reveal-transient.el`
  Transient-based configuration UI.

## State Model

- Buffer-local state lives in `blame-reveal-core.el`.
- Rendering state and overlays are buffer-local.
- Theme-change and transient integrations are global and should be installed
  once while any blame buffer is active.

## Rendering Model

- The package is overlay-driven.
- The visible region plus margin is the primary rendering unit.
- Header, sticky header, fringe markers, loading indicators, and temp overlays
  are separate visual layers.
- Display logic should consume cached blame data, not recover state from
  rendered text.

## Interaction Model

- The package is a normal minor mode, not a modal editor layer.
- Default command entry points are `M-x` and `C-c l`.
- Transient is a convenience UI, not the only way to access functionality.

## Risk Areas

- async and lazy loading interactions
- recursive blame stack restoration
- focus mode advice around header and rendering functions
- cross-buffer global integration teardown
- header update timing during transient/minibuffer activity
