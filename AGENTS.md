# AGENTS.md

Guidance for agents working in `blame-reveal`.

## Project shape

- `blame-reveal.el`: entry point, mode definitions, user options, interactive commands.
- `blame-reveal-core.el`: shared structs, constants, buffer-local state, cache/state helpers.
- `blame-reveal-git.el`: git command building, blame parsing, async/sync loading.
- `blame-reveal-color.el`: commit age and gradient logic.
- `blame-reveal-header.el`: commit header formatting and rendering.
- `blame-reveal-ui.el`: overlays, viewport updates, rendering lifecycle.
- `blame-reveal-recursive.el`, `blame-reveal-focus.el`, `blame-reveal-transient.el`: optional feature modules.

Keep responsibilities in those files unless a new boundary is clearly justified. Do not split files for cosmetic reasons.

## Change principles

- Fix the owning layer. Do not compensate in UI code for git/parsing bugs, or in commands for state bugs.
- Prefer direct code over new abstractions. Repeated logic is acceptable if it keeps control flow obvious.
- Delete dead code instead of leaving compatibility shims or commented "removed" paths.
- Keep experiments narrow. Add the smallest slice that proves the feature or fix.

## Error handling and testing

- Errors should surface instead of being hidden by silent fallback behavior.
- Catch recoverable failures only at the outer command or process boundary. Do not wrap internal logic just to keep execution going.
- Use `user-error` for user-facing misuse and `error` for programmer faults.
- When fixing a bug, write or update a failing regression test first. Confirm it fails, then fix the code.
- Tests must fail when the implementation is wrong. Avoid vague assertions that still pass with hard-coded or broken behavior.
- If a behavior change cannot be covered automatically yet, document the manual verification path explicitly in the same change.

## Elisp conventions

- Loading a file must not change editor behavior. Activation must be explicit through commands or modes.
- Public APIs use the `blame-reveal-` prefix. Internal helpers stay file-private with `blame-reveal--`.
- Use `defvar-local` for per-buffer state and `setq-local` in mode setup when needed.
- Use `defcustom` only for real user-facing configuration. Give it a precise `:type` and `:group`.
- Interactive commands should stay thin: gather input, call internal logic, report results.
- Favor flat control flow. Use `if-let*`, `when-let*`, and `pcase` when they make the code clearer.

## Rendering and state

- This package is overlay-heavy by design. Use overlays for ephemeral blame visuals and sticky headers.
- Use text properties only for persistent data annotations that should travel with text.
- Build display from cached blame data and commit metadata. Do not reparse rendered text to recover state.
- Preserve buffer-local caches and state-machine invariants when changing async, lazy-load, or recursive blame behavior.
- When adding state, decide explicitly whether it is per-buffer, global, or transient process state.

## Module-specific expectations

- Git parsing changes must be validated against porcelain output semantics, especially rename and move/copy metadata.
- Color changes must preserve readable gradients in both light and dark themes.
- UI changes must consider large files, viewport-only rendering, and scroll/update cost.
- Optional modules should remain optional. Do not make `focus`, `recursive`, or `transient` behavior required at load time.

## Verification

Before finishing a change:

- Read the full diff.
- Byte-compile the touched `.el` files with zero warnings.
- Smoke-test the affected workflow in Emacs when behavior changes are user-visible.
- If you fix a bug, add or update a regression test if the repository has a test harness for that area. If it does not, document the manual verification path clearly.

## Avoid

- Silent fallback behavior that hides real failures.
- Speculative caching, timing tweaks, or async changes without a named root cause.
- New files or abstraction layers without a concrete ownership boundary.
- Cross-file calls to private `--` helpers unless the boundary is being intentionally redesigned.
