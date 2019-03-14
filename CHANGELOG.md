# Changes

All significant changes to this project may be documented in the notes below.
This project does not adhere to any specific versioning scheme. Specifically,
please see [Semantic Versioning](http://semver.org/) for the fascinating details
of how the versioning scheme that this project does not use is clearly superior
to random natural numbers and how incrementing the minor version number
indicates backward compatible changes, unless it does not.

- 2019-03-14 — Add `trivial-gray-streams:stream-line-column` method for CCL in
  order to fix `ql:quickload` failure.
- 2019-03-14 — Add options to specify implementation during install.
- 2019-03-12 — Add image option for kernel installation.
- 2019-03-12 — Add install of icons and other resources.
- 2019-03-10 — Add support for `history_request`.
- 2019-03-09 — Use `describe` for code inspection versus trivial-documentation.
- 2019-03-09 — Fix switch statement in handle-message which prevented kernel
  shutdown.
- 2019-03-08 — Preserve `*package*` after `execute_request` thus making
  `in-package` work correctly.
- 2019-03-03 — Add Quicklisp based install.
- 2019-03-01 — Fix Quicklisp quickload.
- 2019-02-24 — Add support for core IPython widgets.
- 2019-02-18 — Add REPL variables.
- 2019-02-18 — Add multiple value support.
- 2019-02-18 — Add code completion support.
- 2019-02-17 — Add code inspection support.
- 2019-02-16 — First working fork from Maxima-Jupyter.
