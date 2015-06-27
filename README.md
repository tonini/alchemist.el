[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://img.shields.io/travis/tonini/alchemist.el.svg)](https://travis-ci.org/tonini/alchemist.el)
[![MELPA](http://melpa.org/packages/alchemist-badge.svg)](http://melpa.org/#/alchemist)
[![MELPA Stable](http://stable.melpa.org/packages/alchemist-badge.svg)](http://stable.melpa.org/#/alchemist)
[![Paypal](https://img.shields.io/badge/paypal-donate-blue.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=tonini%2esamuel%40gmail%2ecom&lc=CH&item_name=Support%20Alchemist%20maintainer&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHostedGuest)

<br/>

<p align="left">
  <img
  src="https://raw.githubusercontent.com/tonini/alchemist.el/master/logo/alchemist_logo.png"
  alt="Alchemist Logo"/>
</p>

> Elixir Tooling Integration Into Emacs</blockquote>

Alchemist comes with a bunch of **features**, which are:

* Mix integration
* Compile & Execution of Elixir code
* Inline code evaluation
* Documentation lookup
* Definition lookup
* Powerful IEx integration
* Smart code completion
* Elixir project management
* Integration with [company-mode](http://company-mode.github.io/)

***

- [Installation](#installation)
  - [Requirements](#requirements)
  - [ELPA](#installation-via-packageel)
  - [Via el-get](#via-el-get)
  - [Manual](#manual)
- [Configuration](#configuration)
  - [Mix setup](#mix-setup)
  - [Keybindings](#keybindings)
  - [Testing-Mode](#testing-mode)
  - [Hooks](#hooks)
- [Mix](#mix)
- [Compile & Execute](#compile-and-execute)
  - [Compile](#compile-functions)
  - [Execute](#execute-functions)
- [Project](#project)
- [Documentation lookup](#documentation-lookup)
  - [Keymap](#alchemist-help-minor-mode-keymap)
- [Definition lookup](#definition-lookup)
  - [Symbol definitions](#symbol-definitions)
- [Auto-completion](#auto-completion)
- [IEx](#iex)
  - [Complete & Documentation lookup](#complete--documentation-lookup)
- [Eval](#eval)
- [Testing](#testing)
- [Modeline](#modeline)
- [Keymap](#keymap)
- [Contributing](#contributing)


## Installation

### Requirements

  * Emacs 24.4 or later
  * Elixir 1.0 or later

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

Alchemist.el is available on the three major community maintained repositories -
[MELPA STABLE](http://melpa-stable.milkbox.net), [MELPA](http://melpa.milkbox.net) and [Marmalade](https://marmalade-repo.org/).

You can install `Alchemist` with the following command:

<kbd>M-x package-install [RET] alchemist [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining Alchemist, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin Alchemist to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(alchemist . "melpa-stable") t)
```

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs. If you're an el-get
user just do <kbd>M-x el-get-install [RET] alchemist [RET]</kbd>.

### Manual

You can install Alchemist manually by placing Alchemist on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor/alchemist.el/")
(require 'alchemist)
```

## Configuration

There are some ways Alchemist can be adjusted that certain workflows operating differently.

### Mix setup

* Use a different shell command for mix.

```el
(setq alchemist-mix-command "/usr/local/bin/mix")
```

* Use a different task for running tests.

```el
(setq alchemist-mix-test-task "espec")
```

* Use custom mix test task options.

```el
(setq alchemist-mix-test-default-options "--exclude pending:true") ;; default
```

* Use a different environment variable in which mix tasks will run.

Mix tasks could always be executed in a specific environment with the usage of `C-u` (universal-argument).
But if you like to change the run of Mix task permanently to a specific environment set it
through the variable.

```el
(setq alchemist-mix-env "prod")
```

### Keybindings

* Use a different keybinding prefix than <kbd>C-c a</kbd>

```el
(setq alchemist-key-command-prefix (kbd "C-c ,")) ;; default: (kbd "C-c a")
```

### Testing Mode

* Disable the use of a more significant syntax highlighting on functions like `test`, `assert_*` and `refute_*`

```el
(setq alchemist-test-mode-highlight-tests nil) ;; default t
```

### Hooks

* Run the whole test suite with `alchemist-mix-test` after saving a buffer.

```el
(setq alchemist-hooks-test-on-save t)
```

## Mix

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a x</kbd>|Prompt for a mix command including a list of all available mix commands. `alchemist-mix`|
|<kbd>C-c a t</kbd>|Run the whole elixir test suite. `alchemist-mix-test`|
|<kbd>C-c a t f</kbd>|Run `alchemist-mix--test-file` with the FILENAME. `alchemist-mix-test-file`|
|<kbd>C-c a t b</kbd>|Run the current buffer through mix test. `alchemist-mix-test-this-buffer`|
|<kbd>C-c a t .</kbd>|Run the test at point. `alchemist-mix-test-at-point`|
|<kbd>C-c a m c</kbd>|Compile the whole elixir project. Prompt for the mix env if the prefix arg is set. `alchemist-mix-compile`|


<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>alchemist-mix</code></td>
        <td>Prompt for a mix command including a list of all available mix commands.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-new</code></td>
        <td>Create a new Elixir application.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-run</code></td>
        <td>Runs the given expression in the Elixir application context.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-deps-with-prompt</code></td>
        <td>Prompt for <code>mix deps</code> commands.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-local-with-prompt</code></td>
        <td>Prompt for <code>mix local</code> commands.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-local-install</code></td>
        <td>Prompt for <code>mix local.install</code> PATH or URL.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-local-install-with-path</code></td>
        <td>Runs <code>mix local.install</code> and prompt for a PATH as argument.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-local-install-with-url</code></td>
        <td>Runs <code>mix local.install</code> and prompt for a URL as argument.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-help</code></td>
        <td>Show help output for a specific mix command.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-execute</code></td>
        <td>Prompt for a mix command.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-hex-search</code></td>
        <td>Display packages matching the given search query.</td>
    </tr>
</table>

Mix tasks could also be executed in a specific environment with the usage of `C-u` (universal-argument).
Default environments are `prod`, `dev` and `test`. [Mix environments](http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#environments)

## Compile And Execute

### Compile functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a c c</kbd>|Compile the current buffer with the `elixirc` command. `alchemist-compile-this-buffer`|
|<kbd>C-c a c f</kbd>|Compile the given `FILENAME` with the `elixirc` command. `alchemist-compile-file`|
|<kbd>C-c a c b</kbd>|Run a custom compile command with `elixirc`. `alchemist-compile`|

### Execute functions

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a e e</kbd>|Run the current buffer through `elixir` command. `alchemist-execute-this-buffer`|
|<kbd>C-c a e f</kbd>|Run `elixir` command with the given `FILENAME`. `alchemist-execute-file` |
|<kbd>C-c a e b</kbd>|Run a custom execute command with `elixir`. `alchemist-execute` |

## Project

| Keybinding | Description |
|-------------------|-------------|
|<kbd>C-c a p f</kbd>|Open project test directory and list all test files. `alchemist-project-find-test`|
|<kbd>C-c a p s</kbd>|Toggle between a file and its tests in the current window. `alchemist-project-toggle-file-and-tests`
|<kbd>C-c a p o</kbd>|Toggle between a file and its tests in other window. `alchemist-project-toggle-file-and-tests-other-window`

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
|<kbd>C-c a h e</kbd>| Run `alchemist-help` with the expression under the cursor. (example: `is_binary`  or `Code.eval_string`). `alchemist-help-search-at-point`              |
|<kbd>C-c a h m</kbd>| Run `alchemist-help` with the current marked region. `alchemist-help-search-marked-region`|

### Alchemist Help Minor Mode Keymap

Inside of the `alchemist-help-minor-mode` (`*elixir help*` buffer) the key `?` will
open a keymap summary in the `minibuffer` with the following functionality:

You're always be able to continue to search inside the `*elixir help*` buffer.

![Alchemist Help Minor Mode Key Summary](logo/help_summary.png)

| Keybinding | Description                                     |
|------------|-------------------------------------------------|
|<kbd>q</kbd>| Quit `*elixir help*` buffer window              |
|<kbd>e</kbd>| `alchemist-help-search-at-point`                |
|<kbd>m</kbd>| alchemist-help-search-marked-region             |
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

![Alchemist Company](logo/alchemist-company.gif)

There are the same keybindings for documentation lookup and definition opening for the selected
candidate available like [company-mode](http://company-mode.github.io/) [provides](https://github.com/company-mode/company-mode/blob/27c913afb9446971d1e0f1f3b272e5650a6206c5/company.el#L609).

<kbd>C-h</kbd> and <kbd>\<f1\></kbd> for documentation lookup for the current selected candidate.
<kbd>C-w</kbd> to jump to the definition of the current selected candidate.

![Alchemist Completion Candidate Functionalities](logo/alchemist-company-doc-goto.gif)

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
|<kbd>C-c a i c</kbd>| Sends the current line to the IEx process and jump to the buffer.. `alchemist-iex-send-current-line-and-go`|
|<kbd>C-c a i r</kbd>| Sends the marked region to the IEx process. `alchemist-iex-send-region`|
|<kbd>C-c a i m</kbd>| Sends the marked region to the IEx process and jump to the buffer. `alchemist-iex-send-region-and-go`|
|<kbd>C-c a i b</kbd>| Compiles the current buffer in the IEx process. `alchemist-iex-compile-this-buffer`|

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

## Testing

Alchemist comes with an minor mode for testing which will be enabled by default inside `*_test.exs` files.

| Keybinding | Description |
|--------------------|------------------------------------------|
|<kbd>C-c , s</kbd>| Run the test at point. `alchemist-mix-test-at-point` |
|<kbd>C-c , v</kbd>| Run all tests in the current file. `alchemist-mix-test-this-buffer` |
|<kbd>C-c , a</kbd>| Run the whole elixir test suite. `alchemist-mix-test` |
|<kbd>C-c , f</kbd>| Run all tests of a specific file `alchemist-mix-test-file` |
|<kbd>C-c , n</kbd>| Jump to the next test inside the current file. `alchemist-test-mode-jump-to-next-test` |
|<kbd>C-c , p</kbd>| Jump to the previous test inside the current file `alchemist-test-mode-jump-to-previous-test` |

## Modeline

By default the status of `alchemist-mix-test`, `alchemist-compile` etc will be
represented via the colorized `mode-name`. In our case that will be the `elixir-mode`.

If you don't like that just set the `alchemist-buffer-status-modeline` variable
via `(setq alchemist-buffer-status-modeline nil)` or `M-x customize-group [RET] alchemist-buffer`

![Alchemist Build Status](logo/build_status.png)

## Keymap

Alchemist comes with a default keymap.

You find and overview of all the key-bindings on the [Alchemist-Refcard](https://github.com/tonini/alchemist.el/blob/master/doc/alchemist-refcard.pdf?raw=true).

## Contributing

Contributions are very welcome!

1. Fork alchemist.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

## License

Copyright © 2014-2015 Samuel Tonini and
[contributors](https://github.com/tonini/alchemist.el/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
