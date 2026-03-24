# Known Limitations

- The package is heavily overlay-based. Very unusual combinations of other
  overlay-heavy modes may still expose display conflicts.
- Recursive blame depends on Git porcelain semantics and move/copy metadata.
  Cross-file history remains best-effort unless move/copy detection is enabled.
- Focus mode is implemented by advising core rendering and header helpers, so
  changes in those helpers need focused regression testing.
- The current automated test suite is intentionally small. It covers recent
  regressions, not the full feature matrix yet.
- Optional integrations such as Magit and transient are not exercised by the
  batch test suite.
