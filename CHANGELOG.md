# Changelog

## v0.5.0

### Enhancements

   * Make `alchemist-help-search-marked-region` also useful via short key inside
     the `*elixir help*` buffer.
   * Build status from `mix`, `compile` or `execute` will now shown they're
     status inside the `mode-name` of the `elixir-mode` instead in the
     `global-mode-line`. (thanks @pragdave)
   * Add a alchemist keymap refcard
   * Add a [default-keymap](https://github.com/tonini/alchemist.el/pull/2/files?diff=unified#diff-0)

## v0.4.1

### Enhancements

  * Add `alchemist-project-name` to get the name of the current mix project.

### Bug fixes

  * Fix bad function call in `alchemist-mix-execute` (thx @pragdave)

## v0.4.0

### Enhancements

  * Documentation for dependencies under the `deps` directory is now be
    included in the documentation search.
  * Inform about no documentation for the current search in minibuffer.
    It's not handy if workflow is cut apart when there is no
    documentation for a given search term.
  * Improve loading time for documention by means of removing the temporary
    file for evaluation.
  * Simplify the search function for searching for the current `expression` under
    cursor position. `alchemist-help-search-at-point`
  * Add a project logo

### Deprecated

  * Mark `alchemist-help-sexp-at-point` and
    `alchemist-help-module-sexp-at-point` as deprecated. The new function
    `alchemist-help-search-at-point` will adapt the functionality of both.

### Bug fixes

  * "No documentation found.." will be displayed in the minibuffer
    when `TokenMissingError`, `SyntaxError` or `FunctionClauseError` apears in
    the search output.
  * Search term used to fail when dots and commas where at the end or beginning
    of it. `__CALLER__.` / `List.to_integer/1,`

## v0.3.0

### Enhancements

  * [ALCHEMIST-HELP] Inline Documentation
  * [ALCHEMIST-HELP] Alchemist Help Minor Mode (Keymap)
  * [ALCHEMIST-HOOKS] `after-save-hook` call `alchemist-hooks--test-on-save`
  * [ALCHEMIST-BUFFER] By default the status of `alchemist-mix-test`, `alchemist-compile` etc will be shown in the mode-line
