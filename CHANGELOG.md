# Changelog

## v0.4.0-dev

### Enhancements

  * Improve loading time for documention by means of removing the temporary
    file for evaluation.
  * Simplify the search function for searching for the current `expression` under
    cursor position. `alchemist-help-search-at-point`
  * Add a project logo

### Deprecated

  * Mark `alchemist-help-sexp-at-point` and
    `alchemist-help-module-sexp-at-point` as deprecated. The new function
    `alchemist-help-search-at-point` will adapt the functionality of both.

## v0.3.0

### Enhancements

  * [ALCHEMIST-HELP] Inline Documentation
  * [ALCHEMIST-HELP] Alchemist Help Minor Mode (Keymap)
  * [ALCHEMIST-HOOKS] `after-save-hook` call `alchemist-hooks--test-on-save`
  * [ALCHEMIST-BUFFER] By default the status of `alchemist-mix-test`, `alchemist-compile` etc will be shown in the mode-line
