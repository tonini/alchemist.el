# Changelog

## v1.8.0 (unreleased)

### Enhancements
  * [Hex] Add Hex package interface for fetching specific package informations.
  * [Refcard] Update dynamic generated refcard with phoenix-mode

### Bug fixes
  * [Completion] The char 'colon' is a none valid expression for completion.
  * [System] The `mode-name` should not be updated if it's not a string.
  * [Server] Just start one Alchemist-Server process when inside the Elixir codebase.

## v1.7.0 (2016-01-06)

### Enhancements

  * [Completion] New shortcut alias handling for Elixir v1.2 is now supported.
  * [Info] Introduce Elixir v1.2 `IEx.Helpers.t` and `IEx.Helpers.i` functionality.
  * [Eval] Display ansi escape sequences inside evaluation buffers correctly.
  * [Test] Add a new key (`t`) to toggle the truncation of lines in the test report buffer.
  * [IEx] Functionality to recompiles and reloads the current context module in the IEx process. `alchemist-iex-reload-module`

### Bug fixes

  * [IEx] Proper handling of sending multiline input to the IEx process.

## v1.6.0 (2015-10-20)

### Enhancements

  * [Macroexpand] Implement functionality to expand Elixir macros.
  * [Phoenix] Implement Alchemist-Phoenix minor mode to work with phoenix mix based Elixir applications.

## v1.5.2 (2015-09-19)

### Bug fixes

  * [Goto] Fix issue with definition lookup when namespace with multiple modules where involved.

## v1.5.1 (2015-09-17)

### Bug fixes

  * [Base] The missing `alchemist-compile` library is now required properly.

## v1.5.0 (2015-08-23)

### Enhancements

  * [Completion] Fix dabbrev-code fallback lookup and case - insensitivity lookup.
  * [Help] Better documentation lookup with search term handling.
  * [Goto] Definition lookup for functions which are imported. Alchemist-Server now works with context informations.
  * [Goto] Big performance boost for fetching symbols.
  * [Goto] Add more def symbols for code navigation.
  * [Goto] Jump to definition just when position line differ.

## v1.4.0 (2015-08-11)

### Enhancements

  * [Test] Control the output of compilation information inside the test report. (variable `alchemist-test-display-compilation-output`)
  * [Test] The hook `alchemist-hooks-test-on-save` should run in the background.
  * [Test] The function `alchemist-test-toggle-test-report-display` to toggle dispaying or hidden of the test report buffer.
  * [Help] Better documentation lookup for currently selected completion candidates.
  * [Refcard] Display all defined keys for a function not just the first defined.
  * [Codebase] Fixed all byte-compile warnings.
  * [Codebase] Add missing docstrings for all Alchemist-Server library functions/variables.

### Bug fixes

  * [Help] Continue with help search although no candidates was found.
  * [Test] Correct rendering of stacktrace and failing tests files.
    Just expand file with project root if the path file doesnt't resolve.

## v1.3.1 (2015-07-26)

### Enhancements

  * [Test] Before running tests ask for saving changed file buffers. (control variable: `alchemist-test-ask-about-save`)
  * [Eval] Introduce an evaluation result popup buffer which holds the evaluation results.
  * [Help] The function `alchemist-help-search-marked-region` is deprecated. The functionality is also handlet by the `alchemist-help-search-at-point`.
  * [Server] Display status of the current Alchemist server. `alchemist-server-status`

### Changes

  * [Mix] Removed all the not keybinding interactive functionality in favore of `alchemist-mix` (instantly call).

## v1.3.0 (2015-07-20)

### Enhancements

  * [Refcard] Introduce a dynamic generated REFCARD which can be called from inside Emacs. `alchemist-refcard`
  * [Mix] The Alchemist Elixir server now handles available Mix tasks. `alchemist-mix` is now instantly fast and serves all available tasks.
  * [Mix] The establishing of the project root path before running Mix tasks, is now working properly.
  * [Test] The test report mode brings now several useful functions, toggle through failed test results or stacktrace files.
  * [Goto] Jump to to a function definition in a source file which is defined multiple times is now much more accessible.
    A prompt with all the possible available functions gets now opened after jumping to source file.

### Changes

  * [Buffer] Replace the whole buffer package which used the compile.el library to handle command runs with
     an own more lightweight solution. `alchemist-report`
  * [Mix] Declare most of the alchemist-mix interactive functions as obsolete. (because of `alchemist-mix` server feature)

## v1.2.0 (2015-07-06)

### Enhancements

  * [Goto] Handle multiple symbol definitions in local file while using goto definition functionality.
  * [Test] Rerun the last test run with `alchemist-mix-rerun-last-test`. (default binding: `C-c a r`)
  * [Project] Run tests for current Elixir file.
  * [Project] Create new Elixir file for the current mix project.
  * [Goto] Handle trailing slash for elixir/erlang source directories.
  * [Alchemist] Replace interactive functions in codebase with faster none-interactve functions.
  * [Alchemist] Refine codebase that warnings/error which occures when package.el bytes-compile elisp code won't happen.
  * [Alchemist] Refine configuration section of the README.
  * [Refcard] Update refcard with new functionalities.
  * [Completion] Better completion candidates part of module prefix
  * [Server] Run the completion for context modules first to be sure modules are loaded.
  * [Server] Alchemist Elixir server basic tests.

### Changes

  * [Server] Start an alchemist server when alchemist-mode is initialized.
  * [Server] Use Application module instead of ets table for aliases storage
  * [Mix] `*-with-prompt` functions are deprecated.

## v1.1.1 (2015-06-26)

### Enhancements

  * [Test] List tests inside current test file and prompt for jumping to it.
  * [Completion] Use company-dabbrev-code as fallback backend
  * [Server] Use a custom server environments variable (`alchemist-server--envs`)

### Bugfixes

  * [Server] Remove legacy IEx.Autocomplete call, it's not working anymore because
    IEx.Server is now added through application environemnt.
  * [Complete] Fix doc and definition lookup through company candidate selection
  * [Goto] Fix issue that mark will be moved even without founded definition

## v.1.1.0 (2015-06-22)

### Enhancements

  * [Completion] Implement completion for alias modules.
  * [Goto] Jump to definition of aliased modules implemented.
  * [Goto] Refine regex extraction of symbol defintion for one line def (remove `,`).
  * [Help] Jump to documentation of aliased modules implemented.
  * [Server] Refine the server to be more structured and maintainable.
  * [Server] Implement a custom version of the IEx.Autocompleter to handle context aliases.
  * [IEx] Add keybindings for documentation lookup and jump to definition.
  * [IEx] Add keybinding for open a buffer with the history of the current session to choose from.
  * [Test] The test keybindings are now also available inside the test report.
  * [Test] Toggle through tests inside the `*alchemist-test-report*`
  * [Mix] Separate the buffer in which task will run. (example: `*alchemist-test-report*` or `*mix* `)
  * [Test-Mode] Make the `test`, `asssert_*` and `refute_*` syntax highlighting more significant within the test mode.

## v1.0.1 (2015-06-16)

### Bugfix

  * [Utils] Remove `s-trim` function from the library `s.el` and replace it with native emacs lisp.

## v1.0 (2015-06-16)

### Enhancements

  * [Server] Implement a server which runs as background process for each mix project or one for project independent work.
  * [Completion] Use the server background process for all the completion related functionality.
  * [Goto] Use the server background process for all the go to code definition related functionality.
  * [Eval] Use the server background process for all the inline code evaluation related functionality.
  * [Help] Use the server background process for all the documentation lookup related functionality.
  * [Completion] Functions of modules which are used inside a context with `use` or `import` will be completed.
  * [Completion] Functions in the context of the current module will be completed.
  * [Goto] Handle Erlang syntax specific aliases
  * [Completion] Use the company-mode specific functions for doc and location lookup
    The functionality to open the doc and jumpt to location for the current selected completion candidate is now called through the proper company-mode functions.
  * [Goto] Improvement of symbols for one line definitions.

### Changes

  * [Project] remove `.alchemist` setup file. There is no need anymore for controlling the compiling in special cases.
  * [Project] remove legacy `alchemist-project-open-tests-for-current-file` function

### Bugfix

  * [Help] Fix wrong message about not existing doc
  * [Goto] Fix regex for jumping to functions inside file
  * [Goto] Fix issue that jumping to functions inside file without function parentheses
  * [Goto] `save-excursion` have to be used to restore the last position informations after `parse-partial-sexp` call.

## v0.16.1 (2015-06-01)

### Bugfix

  * [Completion] Completion process buffer will not be deleted after usage because there are know issues with `evil-mode` users.

## v0.16.0 (2015-06-01)

### Enhancements

  * [Mix] Mix commands can be executed in a specific environment with the usage of `C-u` (universal-argument)
  * [Goto] Open a prompt with all the modules/function/macros definitions inside the current file. `alchemist-goto-list-symbol-definitions`
  * [Goto] Jump to definition inside the current file.
  * [Test-Mode] Introduce a ExUnit minor mode for `*_test.exs` files.
  * [Test-Mode] Default keybindings with prefix `C-c ,` for running specific test functions.
  * [Test-Mode] Functionality to go to next/previous test block.
  * [Completion] Completion process buffer will be deleted after usage, there is no need to have it around after it.

## v0.15.0 (2015-05-28)

### Enhancements

  * [Goto] Support jumping to `Kernel` and `Kernel.Specialforms` functions
  * [Project] `alchemist-project-open-tests-for-current-file` needs to be an interactive function
    and `alchemist--project-open-tests-for-current-file` not (it's a private function)
  * [Keybindings] Add customizable keybinding prefix (default: `C-c a`)
  * [Keybindings] Add keybinding for `alchemist-mix` (`C-c a x`)
  * [IEx] IEx prompt should not be deletable, set it read-only.
  * [Mix] Add keybinding for `alchemist-mix-compile`. (`C-c a m c`)
  * [Completion] Make documentation lookup and jump to defintion for selected completion
    candidate more accessable with using the same keybindings as company-mode uses.
  * [Refcard] Update refcard with keybinding for `alchemist-mix` (`C-c a x`)
  * [Refcard] Update refcard with new keybindings for project test files functionality.

### Bugfixes

  * [Goto] To avoid issues with void `find-tag-marker-ring` variable require `etags` package.

## v0.14.0 (2015-05-16)

### Enhancements

  * [Goto] Handle alias of module name, so jump to defintion and modules also working.
  * [Project] Add functions to toggle between test and implementation.
  * [Mix] Exclude pending tests by default.
  * [Mix] Use a variable for default mix test options.
  * [IEx] Use `company-complete` as default `completion-at-point` function if company is available.
  * [Compile] Don't compile `*.exs` files.

### Bugfixes

  * [Buffer] `alchemist-buffer--error-link-options` should match compilation errors

## v0.13.1 (2015-01-16)

### Bugfixes

  * [Goto] Correct jump to definition inside the same file.
  * [Goto] Simplify and make message about none found more understandable.
  * [Help] Improve Regex for fetch expression under cursor.
  * [Help] Handle long module constalations.

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
