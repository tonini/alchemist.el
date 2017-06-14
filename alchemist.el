;;; alchemist.el --- Elixir tooling integration into Emacs

;; Copyright © 2014-2017 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>
;; Maintainer: Samuel Tonini <tonini.samuel@gmail.com>
;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 1.8.2
;; Package-Requires: ((elixir-mode "2.2.5") (dash "2.11.0") (emacs "24.4") (company "0.8.0") (pkg-info "0.4") (s "1.11.0"))
;; Keywords: languages, elixir, elixirc, mix, hex, alchemist

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  What Does Alchemist Do For You?
;;
;;    Alchemist brings you all the Elixir tooling and power inside your Emacs editor.
;;
;;  Alchemist comes with a bunch of features, which are:
;;
;;    * Mix integration
;;    * Compile & Execution of Elixir code
;;    * Inline code evaluation
;;    * Inline macro expanding
;;    * Documentation lookup
;;    * Definition lookup
;;    * Powerful IEx integration
;;    * Smart code completion
;;    * Elixir project management
;;    * Phoenix support

;;; Code:

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

(defgroup alchemist nil
  "Elixir Tooling Integration Into Emacs."
  :prefix "alchemist-"
  :group 'applications
  :link '(url-link :tag "Website" "http://www.alchemist-elixir.org")
  :link '(url-link :tag "Github" "https://github.com/tonini/alchemist.el")
  :link '(emacs-commentary-link :tag "Commentary" "alchemist"))

(defvar alchemist-mode-keymap nil)

(require 'easymenu)
(require 'company)
(require 'elixir-mode)
(require 'alchemist-utils)
(require 'alchemist-key)
(require 'alchemist-eval)
(require 'alchemist-goto)
(require 'alchemist-info)
(require 'alchemist-report)
(require 'alchemist-mix)
(require 'alchemist-hex)
(require 'alchemist-hooks)
(require 'alchemist-message)
(require 'alchemist-iex)
(require 'alchemist-compile)
(require 'alchemist-refcard)
(require 'alchemist-complete)
(require 'alchemist-company)
(require 'alchemist-macroexpand)
(require 'alchemist-phoenix)

(defun alchemist-mode-hook ()
  "Hook which enables `alchemist-mode'"
  (alchemist-mode 1))

(defun alchemist-version (&optional show-version)
  "Get the Alchemist version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'alchemist)))
    (when show-version
      (message "Alchemist version: %s" version))
    version))

(defun alchemist-elixir-version ()
  "Display the current Elixir version on the system."
  (interactive)
  (message "Elixir %s" (alchemist-utils-elixir-version)))

(define-prefix-command 'alchemist-mode-keymap)

;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}"
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :group 'alchemist
  :global nil
  :keymap `((,alchemist-key-command-prefix . alchemist-mode-keymap))
  (cond (alchemist-mode
         (alchemist-server-start-if-not-running)
         (alchemist-test-initialize-modeline))
        (t
         (alchemist-test-reset-modeline))))

(let ((map alchemist-mode-keymap))
  (define-key map (kbd "x") 'alchemist-mix)
  (define-key map (kbd "t") 'alchemist-mix-test)
  (define-key map (kbd "r") 'alchemist-mix-rerun-last-test)

  (define-key map (kbd "m c") 'alchemist-mix-compile)
  (define-key map (kbd "m r") 'alchemist-mix-run)
  (define-key map (kbd "m l") 'alchemist-mix-rerun-last-task)
  (define-key map (kbd "m t f") 'alchemist-mix-test-file)
  (define-key map (kbd "m t b") 'alchemist-mix-test-this-buffer)
  (define-key map (kbd "m t .") 'alchemist-mix-test-at-point)
  (define-key map (kbd "m t s") 'alchemist-mix-test-stale)
  (define-key map (kbd "m t r") 'alchemist-mix-rerun-last-test)

  (define-key map (kbd "c c") 'alchemist-compile)
  (define-key map (kbd "c f") 'alchemist-compile-file)
  (define-key map (kbd "c b") 'alchemist-compile-this-buffer)

  (define-key map (kbd "e e") 'alchemist-execute)
  (define-key map (kbd "e f") 'alchemist-execute-file)
  (define-key map (kbd "e b") 'alchemist-execute-this-buffer)

  (define-key map (kbd "h h") 'alchemist-help)
  (define-key map (kbd "h i") 'alchemist-help-history)
  (define-key map (kbd "h e") 'alchemist-help-search-at-point)
  (define-key map (kbd "h r") 'alchemist-refcard)

  (define-key map (kbd "p s") 'alchemist-project-toggle-file-and-tests)
  (define-key map (kbd "p o") 'alchemist-project-toggle-file-and-tests-other-window)
  (define-key map (kbd "p t") 'alchemist-project-run-tests-for-current-file)
  (define-key map (kbd "p l") 'alchemist-project-find-lib)
  (define-key map (kbd "p f") 'alchemist-project-find-test)

  (define-key map (kbd "i i") 'alchemist-iex-run)
  (define-key map (kbd "i p") 'alchemist-iex-project-run)
  (define-key map (kbd "i l") 'alchemist-iex-send-current-line)
  (define-key map (kbd "i c") 'alchemist-iex-send-current-line-and-go)
  (define-key map (kbd "i r") 'alchemist-iex-send-region)
  (define-key map (kbd "i m") 'alchemist-iex-send-region-and-go)
  (define-key map (kbd "i b") 'alchemist-iex-compile-this-buffer)
  (define-key map (kbd "i R") 'alchemist-iex-reload-module)

  (define-key map (kbd "v l") 'alchemist-eval-current-line)
  (define-key map (kbd "v k") 'alchemist-eval-print-current-line)
  (define-key map (kbd "v j") 'alchemist-eval-quoted-current-line)
  (define-key map (kbd "v h") 'alchemist-eval-print-quoted-current-line)
  (define-key map (kbd "v o") 'alchemist-eval-region)
  (define-key map (kbd "v i") 'alchemist-eval-print-region)
  (define-key map (kbd "v u") 'alchemist-eval-quoted-region)
  (define-key map (kbd "v y") 'alchemist-eval-print-quoted-region)
  (define-key map (kbd "v q") 'alchemist-eval-buffer)
  (define-key map (kbd "v w") 'alchemist-eval-print-buffer)
  (define-key map (kbd "v e") 'alchemist-eval-quoted-buffer)
  (define-key map (kbd "v r") 'alchemist-eval-print-quoted-buffer)
  (define-key map (kbd "v !") 'alchemist-eval-close-popup)

  (define-key map (kbd "o l") 'alchemist-macroexpand-once-current-line)
  (define-key map (kbd "o L") 'alchemist-macroexpand-once-print-current-line)
  (define-key map (kbd "o k") 'alchemist-macroexpand-current-line)
  (define-key map (kbd "o K") 'alchemist-macroexpand-print-current-line)
  (define-key map (kbd "o i") 'alchemist-macroexpand-once-region)
  (define-key map (kbd "o I") 'alchemist-macroexpand-once-print-region)
  (define-key map (kbd "o r") 'alchemist-macroexpand-region)
  (define-key map (kbd "o R") 'alchemist-macroexpand-print-region)
  (define-key map (kbd "o !") 'alchemist-macroexpand-close-popup)

  (define-key map (kbd "f i") 'alchemist-info-datatype-at-point)
  (define-key map (kbd "f t") 'alchemist-info-types-at-point)

  (define-key map (kbd "X i") 'alchemist-hex-info-at-point)
  (define-key map (kbd "X r") 'alchemist-hex-releases-at-point)
  (define-key map (kbd "X R") 'alchemist-hex-releases)
  (define-key map (kbd "X s") 'alchemist-hex-search)
  (define-key map (kbd "X I") 'alchemist-hex-info)
  (define-key map (kbd "X d") 'alchemist-hex-all-dependencies))

(define-key alchemist-mode-map (kbd "M-.") 'alchemist-goto-definition-at-point)
(define-key alchemist-mode-map (kbd "M-,") 'alchemist-goto-jump-back)
(define-key alchemist-mode-map (kbd "C-c , .") 'alchemist-goto-list-symbol-definitions)
(define-key alchemist-mode-map (kbd "M-P") 'alchemist-goto-jump-to-previous-def-symbol)
(define-key alchemist-mode-map (kbd "M-N") 'alchemist-goto-jump-to-next-def-symbol)
(define-key alchemist-mode-map (kbd "C-c M-r") 'alchemist-test-toggle-test-report-display)

(easy-menu-define alchemist-mode-menu alchemist-mode-map
  "Alchemist mode menu."
  '("Alchemist"
    ("Goto"
     ["Jump to definition at point" alchemist-goto-definition-at-point]
     ["Jump back" alchemist-goto-jump-back])
    ("Evaluate"
     ["Evaluate current line" alchemist-eval-current-line]
     ["Evaluate current line and print" alchemist-eval-print-current-line]
     ["Evaluate quoted current line" alchemist-eval-quoted-current-line]
     ["Evaluate quoted current line and print" alchemist-eval-print-quoted-current-line]
     "---"
     ["Evaluate region" alchemist-eval-region]
     ["Evaluate region and print" alchemist-eval-print-region]
     ["Evaluate quoted region" alchemist-eval-quoted-region]
     ["Evaluate quoted region and print" alchemist-eval-print-quoted-region]
     "---"
     ["Evaluate buffer" alchemist-eval-buffer]
     ["Evaluate buffer and print" alchemist-eval-print-buffer]
     ["Evaluate quoted buffer" alchemist-eval-quoted-buffer]
     ["Evaluate quoted buffer and print" alchemist-eval-print-quoted-buffer])
    ("Macroexpand"
     ["Macro expand once current line" alchemist-macroexpand-once-current-line]
     ["Macro expand once current line and print" alchemist-macroexpand-print-current-line]
     ["Macro expand current line" alchemist-macroexpand-current-line]
     ["Macro expand current line and print" alchemist-macroexpand-print-current-line]
     "---"
     ["Macro expand once region" alchemist-macroexpand-once-region]
     ["Macro expand once region and print" alchemist-macroexpand-print-region]
     ["Macro expand region" alchemist-macroexpand-region]
     ["Macro expand region and print" alchemist-macroexpand-print-region])
    ("Compile"
     ["Compile..." alchemist-compile]
     ["Compile this buffer" alchemist-compile-this-buffer]
     ["Compile file" alchemist-compile-file])
    ("Execute"
     ["Execute..." alchemist-compile]
     ["Execute this buffer" alchemist-execute-this-buffer]
     ["Execute file" alchemist-execute-file])
    ("Mix"
     ["Mix compile..." alchemist-mix-compile]
     ["Mix run..." alchemist-mix-run]
     "---"
     ["Mix run whole test suite." alchemist-mix-test]
     ["Mix test this buffer" alchemist-mix-test-this-buffer]
     ["Mix test file..." alchemist-mix-test-file]
     ["Mix test at point" alchemist-mix-test-at-point]
     ["Mix run stale tests (Elixir 1.3+)" alchemist-mix-test-stale]
     "---"
     ["Mix..." alchemist-mix]
     "---"
     ["Display mix buffer" alchemist-mix-display-mix-buffer]
     "---"
     ["Mix help..." alchemist-mix-help])
    ("IEx"
     ["IEx send current line" alchemist-iex-send-current-line]
     ["IEx send current line and go" alchemist-iex-send-current-line-and-go]
     "---"
     ["IEx send last region" alchemist-iex-send-last-sexp]
     ["IEx send region" alchemist-iex-send-region]
     ["IEx send region and go" alchemist-iex-send-region-and-go]
     "---"
     ["IEx compile this buffer" alchemist-iex-compile-this-buffer]
     ["IEx recompile this buffer" alchemist-iex-recompile-this-buffer]
     "---"
     ["IEx run" alchemist-iex-run])
    ("Project"
     ["Project list all files inside test directory" alchemist-project-find-test]
     ["Project list all files inside lib directory" alchemist-project-find-lib]
     ["Project toggle between file and test" alchemist-project-toggle-file-and-tests]
     ["Project toggle between file and test in other window" alchemist-project-toggle-file-and-tests-other-window])
    ("Documentation"
     ["Documentation search..." alchemist-help]
     ["Documentation search history..." alchemist-help-history]
     "---"
     ["Documentation search at point..." alchemist-help-search-at-point])
    ("About"
     ["Show Alchemist version" alchemist-version t])))

(add-hook 'elixir-mode-hook 'alchemist-mode-hook)

(provide 'alchemist)

;;; alchemist.el ends here
