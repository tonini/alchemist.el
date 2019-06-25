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
(setq alchemist-mix-test-default-options '()) ;; default
```

* Use a different environment variable in which mix tasks will run.

Mix tasks could always be executed in a specific environment with the usage of `C-u` (universal-argument).
But if you like to change the run of Mix task permanently to a specific environment set it
through the variable.

```el
(setq alchemist-mix-env "prod")
```

### IEx setup

* Use a different shell command for iex.

```el
(setq alchemist-iex-program-name "/usr/local/bin/iex") ;; default: iex
```

### Execute setup

* Use a different shell command for elixir.

```el
(setq alchemist-execute-command "/usr/local/bin/elixir") ;; default: elixir
```

### Compile setup

* Use a different shell command for elixirc.

```el
(setq alchemist-compile-command "/usr/local/bin/elixirc") ;; default: elixirc
```

### Modeline setup

* Disable the change of the modeline color with the last test run status.

```el
(setq alchemist-test-status-modeline nil)
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

* Don't ask to save changed file buffers before running tests.

```el
(setq alchemist-test-ask-about-save nil)
```

* Don't change the color of the `mode-name` when test run failed or passed.

```el
(setq alchemist-test-status-modeline nil)
```

* Show compilation output in test report.

```el
(setq alchemist-test-display-compilation-output t)
```

* Toggle truncating lines in test report.

```el
(setq alchemist-test-truncate-lines nil) ;; default t
```

### Hooks

* Run the whole test suite with `alchemist-mix-test` after saving a buffer.

```el
(setq alchemist-hooks-test-on-save t)
```

* Compile your project with `alchemist-mix-compile` after saving a buffer.

```el
(setq alchemist-hooks-compile-on-save t)
```

* Start Alchemist minor mode automatically with elixir-mode when opening .ex or .exs files

```el
(add-hook 'elixir-mode-hook 'alchemist-mode)
```