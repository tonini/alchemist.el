# Elixir Tooling Integration Into Emacs

[![Build Status](https://travis-ci.org/tonini/alchemist.el.png)](https://travis-ci.org/tonini/alchemist.el)

## Installation

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
- [Eval](#eval)
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

## Eval

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>alchemist-eval-on-region</code></td>
        <td>Evaluates the Elixir code on the marked region.</td>
    </tr>
    <tr>
        <td><code>alchemist-eval-on-current-line</code></td>
        <td>Evaluates the Elixir code on the current line.</td>
    </tr>
    <tr>
        <td><code>alchemist-eval-on-current-buffer</code></td>
        <td>Evaluates the Elixir code in the current buffer.</td>
    </tr>
    <tr>
        <td><code>alchemist-eval-string-to-quoted-on-region</code></td>
        <td>Get the representation of the expression on the marked region.</td>
    </tr>
    <tr>
        <td><code>alchemist-eval-string-to-quoted-on-current-line</code></td>
        <td>Get the representation of the expression on the current line.</td>
    </tr>
</table>

## Contributing

Contributions are very welcome!

1. Fork alchemist.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!
