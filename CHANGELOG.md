# Changelog

## v0.14.0-dev (unreleased)

## v0.13.0 (2015-01-15)

### Enhancements

  * [Goto] Functionality to jump to Module and Function definition. Works for Elixir and Erlang source.
  * [Refcard] Update refcard with new keybindings.
  * [Project] Add functionality to bootstrap a new test file when try to jump to it and
    it's not existsing.
  * [Help] Inform with message when no documentation is found for given search term.

## v0.12.0 (2015-01-08)

### Enhancements

  * [Complete] Add arity annotation to completion candidates.
  * [IEx] Add `alchemist-iex-recompile-this-buffer` which recompiles and reloads the current buffer in the IEx process.
  * [Help] Add `alchemist-help` module prompt. It's loads all available Modules in the current environemnt which have a proper moduledoc.
  * [Help] Add `alchemist-help-history` to `alchemist-help-minor-mode`. `alchemist-help-history` lookup straight for the stored search text, without completion.
  * [Mix] Add mix command prompt with completion
  * [Tests] Add more tests for eval functionality.
  * [Tests] switch to ert-runner instead of custom test runner.

### Changes

  * [Messages] Don't switch to Message buffer after displaying it. Use `display-buffer` instead of `pop-to-buffer`.
  * [Help] Remove `alchemist-help-next-search` and `alchemist-help-previous-search`. They work clumsy and it's not fast enought to work with.
  * [Help] Remove obsolete aliases for `alchemist-help-sexp-at-point` and `alchemist-help-module-sexp-at-point`

### Bugfixes

  * [Refcard] Display correct function for the `C-c a t` keybinding.

## v0.11.1

### Bugfixes

  * [Eval] Project root needs to be established before evaluate code in context
    of the mix projects.

## v0.11.0

### Enhancements

  * [Eval] Implementation of Eval functionality.
  * [Eval] Evaluate the Elixir code on the current line with `alchemist-eval-current-line`.
  * [Eval] Evaluate the Elixir code on the current line and
    insert the result with `alchemist-eval-print-current-line`.
  * [Eval] Get the Elixir code representation of the expression on the current
    line  with `alchemist-eval-quoted-current-line`.
  * [Eval] Get the Elixir code representation of the expression on the current line
    and insert the result with `alchemist-eval-print-quoted-current-line`.
  * [Eval] Evaluate the Elixir code on marked region with
    `alchemist-eval-region`.
  * [Eval] Evaluate the Elixir code on marked region and
    insert the result with `alchemist-eval-print-region`.
  * [Eval] Get the Elixir code representation of the expression on marked region
    with `alchemist-eval-quoted-region`.
  * [Eval] Get the Elixir code representation of the expression on marked region
    and insert the result with `alchemist-eval-print-quoted-region`.
  * [Eval] Evaluate the Elixir code in the current buffer with
    `alchemist-eval-buffer`.
  * [Eval] Evaluate the Elixir code in the current buffer and insert the result
    with `alchemist-eval-print-buffer`.
  * [Eval] Get the Elixir code representation of the expression in the current
    buffer with `alchemist-eval-quoted-buffer`.
  * [Eval] Get the Elixir code representation of the expression in the current buffer
    and insert result with `alchemist-eval-print-quoted-buffer`.
  * [IEx] compile the current buffer file within an IEx process. `alchemist-iex-compile-this-buffer`

### Changes

  * [Complete] `alchemist-complete-debug-mode` is `t` by default and failed
    completions output will be displayed.

## v0.10.1

### Changes

  * [IEx] Remove default keymap, TAB is not needed because users have theyre own
    setup for `company-complete`.

### Bugfixes

  * [Utils] Clean buffer content from ansi escape sequences. (issue: #25)

## v0.10.0

### Enhancements

   * [IEx] Run IEx with mix. `alchemist-iex-project-run`
   * [Alchemist] Add keybindings for Alchemist-IEx.
   * [IEx] Implemention of an Elixir IEx process buffer.
   * [Message] Implement Alchemist internal message API for things like debug
     message etc.
   * [Complete] Add the function `alchemist-complete-debug-mode` for controlling
     the display of the error output from completion.

## v0.9.0

### Enhancements

   * [Project] `alchemist-project-find-test` Open project test directory and
     list all test files.
   * [Project] `alchemist-project-open-tests-for-current-file` Opens the appropriate test file for the current buffer file in a new window.
   * [Project] Add keybindings for `alchemist-project-` specific interactive
     functions.
   * [Project] Refine the controlling of documentation lookup and completion
     inside Elixir project codebase. The variable
     `alchemist-project-compile-when-needed` brings a much better handeling.
   * [Company] Documentation lookup for current company candidate selection with
     `C-d`.
   * [Project] Configuration variable
     `alchemist-project-compile-when-needed` is set to value
     `nil` default. Many users didn't realize how completion for theyr own
     Elixir project codebase works, when they start the first time using Alchemist.
   * Update Refcard with project keybindings.

## v0.8.0

### Enhancements

   * Toggle between completion and documention lookup for current codebase with
     `alchemist-project-toggle-complete-and-docs`.
   * Introduce `.alchemist` file for project specific configuration
     (`docs-ansi-color-enabled`, `complete-and-docs-enabled`).
   * Big refining of `alchemist-help` and `alchemist-complete`.
   * Better handling of non-complete output in the subprocesses.
   * Improve README file.

## v0.7.2

### Bugfix

   * Fix the loading of the alchemist-company functionality via `eval-after-load`

## v0.7.1

### Bugfix

   * Codebase need to be compiled to deliver proper completion.

## v0.7.0

### Enhancements

   * Implement alchemist-company backend.
   * Separate alchemist-complete functionality.
   * Optimize the building of the complete list.
   * Replace `shell-command-to-string` with background processes to improve.
     performance and remove emacs workflow distruption.
   * Add project directory check to save-hook (thx @pragdave)
   * Remove dispensable informations in compilation buffers

## v0.6.0

### Enhancements

   * Introduce autocomplete feature for
     searching. `alchemist-help-search-at-point` and
     `alchemist-help-search-marked-region` use the autocomplete functionality.
   * Introduce `alchemist-help-history` for toggle through search
     history. `alchemist-help` has no history completing anymore.
   * Remove the `Code.eval_sting/1` call inside the
     `alchemist-help--eval-string-command` function
   * Improve the readability of the alchemist buffer through removing
     dispensable output

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
