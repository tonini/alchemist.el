## Installation

### Requirements

  * [Emacs](https://www.gnu.org/software/emacs/) 24.4 or later
  * [Elixir](http://elixir-lang.org/) 1.0 or later

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

Alchemist.el is available on the three major community maintained repositories -
[MELPA STABLE](https://stable.melpa.org), [MELPA](https://melpa.org) and [Marmalade](https://marmalade-repo.org/).

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
