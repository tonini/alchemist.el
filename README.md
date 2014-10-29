[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/tonini/alchemist.el.png)](https://travis-ci.org/tonini/alchemist.el)
[![MELPA](http://melpa.org/packages/alchemist-badge.svg)](http://melpa.org/#/alchemist)

# Elixir Tooling Integration Into Emacs

## Installation

### ELPA

alchemist.el is available on community maintained repository - [MELPA](http://melpa.milkbox.net/)

Just run `M-x package-install [RET] alchemist [RET]` inside your emacs and you're ready to go.

If you're not already using ELPA, check the [emacswiki](http://www.emacswiki.org/emacs/ELPA) page to get
familiar with it.

### Manual

```lisp
(add-to-list 'load-path "~/path/to/alchemist.el/")
(require 'alchemist)
(alchemist-mode 1)
```

## Usage

- [Mix](#mix)
- [Compile & Execute](#compile-and-execute)
  - [Compile](#compile-functions)
  - [Execute](#execute-functions)
- [Inline Docs](#inline-documentation)
  - [Keymap](#alchemist-help-minor-mode-keymap)
- [Hooks](#hooks)
- [Modeline](#modeline)
- [Contributing](#contributing)

## Mix

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>alchemist-mix-new</code></td>
        <td>Create a new Elixir application.</td>
    </tr>
     <tr>
        <td><code>alchemist-mix-test</code></td>
        <td>Run the whole Elixir application test suite.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-test-this-buffer</code></td>
        <td>Run the current buffer through <code>mix test</code> command.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-test-file</code></td>
        <td>Run a file through <code>mix test</code> command.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-test-at-point</code></td>
        <td>Run the test at point.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-compile</code></td>
        <td>Compile the whole Elixir application.</td>
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
        <td>Run any command in the context of the application.</td>
    </tr>
    <tr>
        <td><code>alchemist-mix-hex-search</code></td>
        <td>Display packages matching the given search query.</td>
    </tr>
</table>


## Compile And Execute

### Compile functions

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>alchemist-compile-this-buffer</code></td>
        <td>Compile the current buffer with <code>elixirc</code>.</td>
    </tr>
    <tr>
        <td><code>alchemist-compile-file</code></td>
        <td>Compile the given <code>FILENAME</code>.</td>
    </tr>
    <tr>
        <td><code>alchemist-compile</code></td>
        <td>Run a custom compile command with <code>elixirc</code>.</td>
    </tr>
</table>

### Execute functions

<table>
    <tr>
      <th>Command (For the <code>M-x</code> prompt.)</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>alchemist-execute-this-buffer</code></td>
      <td>Run the current buffer through <code>elixir</code>.</th>
    </tr>
    <tr>
      <td><code>alchemist-execute-file</code></th>
      <td>Run <code>elixir</code> with the given <code>FILENAME</code>.</th>
    </tr>
    <tr>
      <td><code>alchemist-execute</code></th>
      <td>Run a custom execute command with <code>elixir</code>.</th>
    </tr>
</table>

## Inline Documentation

There is the `alchemist-help-minor-mode` for a complete fully functional
interface to the Elixir documentation. The `alchemist-help-minor-mode` uses the
same functions like Elixir's [IEx](http://elixir-lang.org/docs/stable/iex/).

What does that mean? It means no matter which Elixir version is currently
installed on the system, the documentation you get by `alchemist` is the same
`IEx` would deliver.

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>alchemist-help</code></td>
        <td>Run a custom search.</td>
    </tr>
    <tr>
        <td><code>alchemist-help-sexp-at-point</code></td>
        <td>Run <code>alchemist-help</code> with the expression under the cursor. (example: <code>is_binary</code>)</td>
    </tr>
    <tr>
        <td><code>alchemist-help-module-sexp-at-point</code></td>
        <td>Run <code>alchemist-help</code> with the module and expression under the cursor. (example: <code>String.slice</code>)</td>
    </tr>
    <tr>
        <td><code>alchemist-help-search-marked-region</code></td>
        <td>Run <code>alchemist-help</code> with the current marked region.</td>
    </tr>
</table>

### Alchemist Help Minor Mode Keymap

Inside of the `alchemist-help-minor-mode` (`*elixir help*` buffer) the key `?` will
open a keymap summary in the `minibuffer` with the following functionality:

You're always be able to continue to search inside the `*elixir help*` buffer
with keys like `e` and `E`.

![Alchemist Help Minor Mode Key Summary](http://i.imgur.com/UrDyU0K.png)

<table>
    <tr>
        <th>Key</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>q</code></td>
        <td>Quit <code>*elixir help*</code> buffer window</td>
    </tr>
    <tr>
        <td><code>e</code></td>
        <td><code>alchemist-help-sexp-at-point</code></td>
    </tr>
    <tr>
        <td><code>E</code></td>
        <td><code>alchemist-help-module-sexp-at-point</code></td>
    </tr>
    <tr>
        <td><code>s</code></td>
        <td><code>alchemist-help</code></td>
    </tr>
    <tr>
        <td><code>n</code></td>
        <td><code>alchemist-help-next-search</code></td>
    </tr>
    <tr>
        <td><code>p</code></td>
        <td><code>alchemist-help-previous-search</code></td>
    </tr>
    <tr>
        <td><code>?</code></td>
        <td><code>alchemist-help-minor-mode-key-binding-summary</code></td>
    </tr>
</table>

## Hooks

There is a `after-save-hook` called `alchemist-hooks--test-on-save` which runs
the whole elixir test suite via `alchemist-mix-test`. The `alchemist-mix-test`
will just run if the current buffer is in major `elixir-mode`.

If you would like to use it just set the `alchemist-hooks-test-on-save` variable
via `(setq alchemist-hooks-test-on-save t)` or `M-x customize-group [RET] alchemist-hooks`

## Modeline

By default the status of `alchemist-mix-test`, `alchemist-compile` etc will be
shown in the
[mode-line](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html)

If you don't like that just set the `alchemist-buffer-status-modeline` variable
via `(setq alchemist-buffer-status-modeline nil)` or `M-x customize-group [RET] alchemist-buffer`

![Alchemist modeline](http://i.imgur.com/SBfhajV.png)

## Contributing

Contributions are very welcome!

1. Fork alchemist.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

## License

Copyright © 2014 Samuel Tonini and
[contributors](https://github.com/tonini/alchemist.el/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
