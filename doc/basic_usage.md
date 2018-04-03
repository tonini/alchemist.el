## Mix

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a x</kbd>|Prompt for a mix command including a list of all available mix commands. `alchemist-mix`|
|<kbd>C-c a m c</kbd>|Compile the whole elixir project. `alchemist-mix-compile`|
|<kbd>C-c a m r</kbd>|Runs the given file or expression in the context of the application. `alchemist-mix-run`|
|<kbd>C-c a m l</kbd>|Rerun the last mix task which was run by alchemist. `alchemist-mix-rerun-last-task`|

Mix tasks could also be executed in a specific environment with the usage of `C-u` (universal-argument).
Default environments are `prod`, `dev` and `test`. [Mix environments](http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#environments)

### Mix Task Mode

The Mix tasks running in a separate `alchemist-mix-mode`, in which the following keybindings are available:

| Keybinding   | Description                                          |
|--------------|------------------------------------------------------|
|<kbd>q</kbd>  |Quit `*mix*` buffer window                            |
|<kbd>i</kbd>  |Send an input to the current running mix task process.|
|<kbd>r</kbd>  |Rerun the last mix task which was run by alchemist.   |

## Mix Hex

| Keybinding   | Description                                          |
|--------------|------------------------------------------------------|
|<kbd>C-c a X i</kbd>  | Display Hex package information for the package at point. |
|<kbd>C-c a X r</kbd>  | Display Hex package releases for the package at point. |
|<kbd>C-c a X I</kbd>  | Display Hex package info for a certain package. |
|<kbd>C-c a X R</kbd>  | Display Hex package releases for a certain package.|
|<kbd>C-c a X s</kbd>  | Search for Hex packages. |
|<kbd>C-c a X d</kbd>  | Display Hex package dependencies for the current Mix project. |


## Testing

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a t</kbd>|Run the whole elixir test suite. `alchemist-mix-test`|
|<kbd>C-c a r</kbd>|Rerun the last test that was run by alchemist. `alchemist-mix-rerun-last-test`|
|<kbd>C-c a m t f</kbd>|Run `alchemist-mix--test-file` with the FILENAME. `alchemist-mix-test-file`|
|<kbd>C-c a m t b</kbd>|Run the current buffer through mix test. `alchemist-mix-test-this-buffer`|
|<kbd>C-c a m t .</kbd>|Run the test at point. `alchemist-mix-test-at-point`|
|<kbd>C-c a m t s</kbd>|Run only stale tests (Elixir 1.3+). `alchemist-mix-test-stale` |
|<kbd>C-c a m t r</kbd>|Rerun the last test that was run by alchemist. `alchemist-mix-rerun-last-test` |
|<kbd>C-c M-r</kbd>|Toggle between displaying or hidding the test report buffer. `alchemist-test-toggle-test-report-display`|

## Compile And Execute

### Compile functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a c b</kbd>|Compile the current buffer with the `elixirc` command. `alchemist-compile-this-buffer`|
|<kbd>C-c a c f</kbd>|Compile the given `FILENAME` with the `elixirc` command. `alchemist-compile-file`|
|<kbd>C-c a c c</kbd>|Run a custom compile command with `elixirc`. `alchemist-compile`|

### Execute functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a e b</kbd>|Run the current buffer through `elixir` command. `alchemist-execute-this-buffer`|
|<kbd>C-c a e f</kbd>|Run `elixir` command with the given `FILENAME`. `alchemist-execute-file` |
|<kbd>C-c a e e</kbd>|Run a custom execute command with `elixir`. `alchemist-execute` |

## Project

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a p s</kbd>|Toggle between a file and its tests in the current window. `alchemist-project-toggle-file-and-tests`|
|<kbd>C-c a p o</kbd>|Toggle between a file and its tests in other window. `alchemist-project-toggle-file-and-tests-other-window`|
|<kbd>C-c a p t</kbd>|Run the tests related to the current file. `alchemist-project-run-tests-for-current-file`|
|<kbd>C-c a p f</kbd>|List all files available in the `test` directory. `alchemist-project-find-test`|
|<kbd>C-c a p l</kbd>|List all files available in the `lib` directory. `alchemist-project-find-lib` |

## alchemist-phoenix-mode

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a n w</kbd>|List all files available in the `web` directory. `alchemist-phoenix-find-web`|
|<kbd>C-c a n c</kbd>|List all controllers in `web/controllers` directory. `alchemist-phoenix-find-controllers`|
|<kbd>C-c a n l</kbd>|List all channels in `web/channels` directory. `alchemist-phoenix-find-channels`|
|<kbd>C-c a n t</kbd>|List all templates in `web/templates` directory. `alchemist-phoenix-find-templates`|
|<kbd>C-c a n m</kbd>|List all models in `web/models` directory. `alchemist-phoenix-find-models`|
|<kbd>C-c a n v</kbd>|List all views in `web/views` directory. `alchemist-phoenix-find-views`|
|<kbd>C-c a n s</kbd>|List all files in `web/static` directory. `alchemist-phoenix-find-static`|
|<kbd>C-c a n r</kbd>|Open the `router.ex` file in `web` directory. `alchemist-phoenix-router`|
|<kbd>C-c a n R</kbd>|Run the Mix task `phoenix.routes`. `alchemist-phoenix-routes`|

## Documentation lookup

There is the `alchemist-help-minor-mode` for a complete fully functional
interface to the Elixir documentation. The `alchemist-help-minor-mode` uses the
same functions like Elixir's [IEx](http://elixir-lang.org/docs/stable/iex/).

What does that mean? It means no matter which Elixir version is currently
installed on the system, the documentation you get by `alchemist` is the same
`IEx` would deliver.

| Keybinding | Description                                     |
|------------|-------------------------------------------------|
|<kbd>C-c a h h</kbd>| Run a custom search. `alchemist-help`              |
|<kbd>C-c a h i</kbd>| Look through search history. `alchemist-help-history` |
|<kbd>C-c a h e</kbd>| Run `alchemist-help` with the expression under the cursor. (example: `is_binary`  or `Code.eval_string`). If there is a currently marked region this will be used as the search term. `alchemist-help-search-at-point` |
|<kbd>C-c a h r</kbd>| Open a buffer with a refcard of alchemist bindings. `alchemist-refcard`|

### Alchemist Help Minor Mode Keymap

Inside of the `alchemist-help-minor-mode` (`*elixir help*` buffer) the key `?` will
open a keymap summary in the `minibuffer` with the following functionality:

You're always be able to continue to search inside the `*elixir help*` buffer.

Hit <kbd>?</kbd> to get the keybinding summary for the `alchemist-help-minor-mode`.

```
[q]-quit [e]-search-at-point [m]-search-module [s]-search [h]-history [?]-keys
```

| Keybinding | Description                                     |
|------------|-------------------------------------------------|
|<kbd>q</kbd>| Quit `*elixir help*` buffer window              |
|<kbd>e</kbd>| `alchemist-help-search-at-point`                |
|<kbd>m</kdb>| `alchemist-help-module`                         |
|<kbd>s</kbd>| `alchemist-help`                                |
|<kbd>h</kbd>| `alchemist-help-history`                        |
|<kbd>?</kbd>| `alchemist-help-minor-mode-key-binding-summary` |

## Definition lookup

With the function `alchemist-goto-definition-at-point`, which is bound to <kbd>M-.</kbd>, you
can jump to module and function definitions. If you want to jump back, just use <kbd>M-,</kbd> which calls `alchemist-goto-jump-back`.

You also can jump to the current selected completion candidate with just hit the same key as normally, <kbd>M-.</kbd>.

By default you're able to jump to definitions of your own mix project codebase and dependencies.
But if you would like to also jump to Elixir and Erlang source code you need to tell Alchemist where
it can find the source code of Elixir and Erlang.

For that purpose there're two variables you can set:

```el
(setq alchemist-goto-erlang-source-dir "/path/to/erlang/source/")
```

```el
(setq alchemist-goto-elixir-source-dir "/path/to/elixir/source/")
```

If you inside an Erlang file and the `erlang-mode` is enabled you can't use `alchemist-goto-jump-back` anymore.
But if you would like to use it also inside the `erlang-mode` just setup the following custom hook:

```el
(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
```

![Definition Lookup](http://i.imgur.com/KGIHEOh.gif)

### Symbol definitions

There is the function `alchemist-goto-list-symbol-definitions` which lets you jump to a specific module, function or macro definitions in the current file.

## Auto-completion

Alchemist users are advised to use
[company-mode](http://company-mode.github.io/) to enable auto-completion inside
of Elixir source code.

Alchemist enables a [company-mode](http://company-mode.github.io/) elixir backend by default if company-mode is
installed.

![Alchemist Company](images/alchemist-company.gif)

There are the same keybindings for documentation lookup and definition opening for the selected
candidate available like [company-mode](http://company-mode.github.io/) [provides](https://github.com/company-mode/company-mode/blob/27c913afb9446971d1e0f1f3b272e5650a6206c5/company.el#L609).

<kbd>C-h</kbd> and <kbd>\<f1\></kbd> for documentation lookup for the current selected candidate.
<kbd>C-w</kbd> to jump to the definition of the current selected candidate.

![Alchemist Completion Candidate Functionalities](images/alchemist-company-doc-goto.gif)

>
### Important Note:
* Auto complete and jump to definition only works if your project or newly added file has been compiled.
* If you moved the project to a different path you will need to compile it again at the current location.
* If you compile the project inside a Docker container then the project will need to have the same path both inside the container and on the host, or you will need to run emacs inside the container.

## IEx

Alchemist provides a `REPL` buffer, connected to an
[Elixir IEx](http://elixir-lang.org/docs/master/iex/IEx.html) subprocess.

To start an IEx process just run <kbd>M-x alchemist-iex-run</kbd>

To start an IEx process in the context of an Elixir project (`iex -S mix`) just run <kbd>M-x alchemist-iex-project-run</kbd>

To start a custom IEx process with additional arguments (like: `iex --sname custom`) just use the
[universal-argument](http://www.gnu.org/software/emacs/manual/html_node/emacs/Arguments.html) <kbd>C-u</kbd>
before run <kbd>M-x alchemist-iex-run</kbd>

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a i i</kbd>| Start an IEx process. `alchemist-iex-run`|
|<kbd>C-c a i p</kbd>| Start an IEx process with mix (`iex -S mix`). `alchemist-iex-project-run`|
|<kbd>C-c a i l</kbd>| Sends the current line to the IEx process. `alchemist-iex-send-current-line`|
|<kbd>C-c a i c</kbd>| Sends the current line to the IEx process and jump to the buffer. `alchemist-iex-send-current-line-and-go`|
|<kbd>C-c a i r</kbd>| Sends the marked region to the IEx process. `alchemist-iex-send-region`|
|<kbd>C-c a i m</kbd>| Sends the marked region to the IEx process and jump to the buffer. `alchemist-iex-send-region-and-go`|
|<kbd>C-c a i b</kbd>| Compiles the current buffer in the IEx process. `alchemist-iex-compile-this-buffer`|
|<kbd>C-c a i R</kbd>| Recompiles and reloads the current module in the IEx process. `alchemist-iex-reload-module`|

### Complete & Documentation lookup

When Alchemist finds [company-mode](http://company-mode.github.io/) it enables
if for completion inside the IEx process buffer.

## Eval

Alchemist comes with the functionality to evaluate code inside the buffer.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a v l</kbd>| Evaluate the Elixir code on the current line. `alchemist-eval-current-line`.|
|<kbd>C-c a v k</kbd>| Evaluate the Elixir code on the current line and insert the result. `alchemist-eval-print-current-line`.|
|<kbd>C-c a v j</kbd>| Get the Elixir code representation of the expression on the current line. `alchemist-eval-quoted-current-line`. |
|<kbd>C-c a v h</kbd>| Get the Elixir code representation of the expression on the current line and insert the result. `alchemist-eval-print-quoted-current-line`. |
|<kbd>C-c a v o</kbd>| Evaluate the Elixir code on marked region. `alchemist-eval-region`.|
|<kbd>C-c a v i</kbd>| Evaluate the Elixir code on marked region and insert the result. `alchemist-eval-print-region`.|
|<kbd>C-c a v u</kbd>| Get the Elixir code representation of the expression on marked region. `alchemist-eval-quoted-region`.|
|<kbd>C-c a v y</kbd>| Get the Elixir code representation of the expression on marked region and insert the result. `alchemist-eval-print-quoted-region`.|
|<kbd>C-c a v q</kbd>| Evaluate the Elixir code in the current buffer. `alchemist-eval-buffer`.|
|<kbd>C-c a v w</kbd>| Evaluate the Elixir code in the current buffer and insert the result. `alchemist-eval-print-buffer`.|
|<kbd>C-c a v e</kbd>| Get the Elixir code representation of the expression in the current buffer. `alchemist-eval-quoted-buffer`.|
|<kbd>C-c a v r</kbd>| Get the Elixir code representation of the expression in the current buffer and insert result. `alchemist-eval-print-quoted-buffer`.|
|<kbd>C-c a v !</kbd>| Quit the Elixir evaluation popup window. `alchemist-eval-close-popup`.|

## Macroexpand

| Keybinding | Description | Command |
|--------------------|------------------------------------------|------------------------------------------------|
|<kbd>C-c a o l</kbd>| Macro expand once on the current line. | `alchemist-macroexpand-once-current-line`.|
|<kbd>C-c a o L</kbd>| Macro expand once on the current line and print the result. | `alchemist-macroexpand-once-print-current-line`.|
|<kbd>C-c a o k</kbd>| Macro expand on the current line. | `alchemist-macroexpand-current-line`.|
|<kbd>C-c a o K</kbd>| Macro expand on the current line and print the result. | `alchemist-macroexpand-print-current-line`.|
|<kbd>C-c a o i</kbd>| Macro expand once on region. | `alchemist-macroexpand-once-region`.|
|<kbd>C-c a o I</kbd>| Macro expand once on region and print the result. | `alchemist-macroexpand-once-print-region`.|
|<kbd>C-c a o r</kbd>| Macro expand on region. | `alchemist-macroexpand-region`.|
|<kbd>C-c a o R</kbd>| Macro expand on region and print the result. | `alchemist-macroexpand-print-region`.|
|<kbd>C-c a o !</kbd>| Quit the Elixir macroexpand popup window. | `alchemist-macroexpand-close-popup`.|

**Note**

Macroexpand works currently only for Elixir core macros, but why is this?

> Macros are lexical: it is impossible to inject code or macros globally. In order to use a macro, you need to explicitly require or import the module that defines the macro.

The [Alchemist-Server](https://github.com/tonini/alchemist-server) is currently under development to handle more knowledge about the current context. After that update expanding custom macros will supported too.

But if you like to expand a custom macro in the mean time and you know where it comes from, you can do something like the following with the Alchemist inline evaluation functionality.

As example, select the code and call the `alchemist-eval-print-region` and you get the macro expansion below.

```elixir
require Unless # In order to use a macro, you need to explicitly require the module
expr = quote do: Unless.macro_unless(true, IO.puts "this should never be printed")
res  = Macro.expand_once(expr, __ENV__)
IO.puts Macro.to_string(res)
# => if(!true) do
# =>   IO.puts("this should never be printed")
# => end
# => :ok
```

## Datatype Informations

With Elixir `v1.2` comes two new `IEx` helper functions `t/1` and `i/1`.

- Display type docs with `t(Module.type)` and `t(Module.type/arity)`
- Prints information about any data type with `i/1`.

These two helper functions are available now with the following keybindings/functions.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c a f i</kbd>| Prints information about any datatype under the cursor. `alchemist-info-datatype-at-point` |
|<kbd>C-c a f t</kbd>| Prints information of types under the cursor. `alchemist-info-types-at-point` |

## Testing Mode

Alchemist comes with an minor mode for testing which will be enabled by default inside `*_test.exs` files.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c , s</kbd>| Run the test at point. `alchemist-mix-test-at-point` |
|<kbd>C-c , v</kbd>| Run all tests in the current file. `alchemist-mix-test-this-buffer` |
|<kbd>C-c , a</kbd>| Run the whole elixir test suite. `alchemist-mix-test` |
|<kbd>C-c , f</kbd>| Run all tests of a specific file `alchemist-mix-test-file` |
|<kbd>C-c , n</kbd>| Jump to the next test inside the current file. `alchemist-test-mode-jump-to-next-test` |
|<kbd>C-c , p</kbd>| Jump to the previous test inside the current file `alchemist-test-mode-jump-to-previous-test` |

### Testing Report

The tests are reported in a [compilation buffer](https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html#Compilation-Mode).  Some of the standard keybindings for that mode are:

| Keybinding       | Description |
|------------------|------------------------------------------|
|<kbd>g</kbd>      | Rerun the latest test run. Equivalent to `alchemist-mix-rerun-last-test` |
|<kbd>M-n</kbd>    | Jump to the next error in the test report.|
|<kbd>M-g n</kbd   | Visit the locus of the next error in another buffer.|
|<kbd>M-p</kbd>    | Jump to the previous error in the test report. |
|<kbd>M-g n</kbd   | Visit the locus of the previous error in another buffer.|
|<kbd>C-c C-f</kbd>| Toggle Next Error Follow minor mode, which makes `M-{n|p}` behave like `M-g {n|p}`|
|<kbd>C-c C-k</kbd>| Interrupt the current running report process. |
|<kbd>q</kbd>      | Close the test report window |

## Keymap

Alchemist comes with a default keymap.

The the default prefix keybinding is <kbd>C-c a</kbd>

### Refcards

You find and overview of all the key-bindings on the [Alchemist-Refcard](https://github.com/tonini/alchemist.el/blob/master/doc/alchemist-refcard.pdf?raw=true).

There is also a refcard for usage inside Emacs, which gets dynamically generated with the current adjusted keybindings.
If you use the keybinding <kbd>i</kbd> on a specific row, it will call `describe-function` on that function.

Just `M-x alchemist-refcard RET`
