;;; alchemist.el --- Elixir tooling integration into Emacs

;; Copyright © 2014-2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "24"))
;; Keywords: languages, mix, elixir, elixirc, hex

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

;; Alchemist integrate Elixir's tooling into Emacs

;;; Code:

(require 'easymenu)

(defgroup alchemist nil
  "Elixir Tooling Integration Into Emacs."
  :prefix "alchemist-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/tonini/alchemist.el")
  :link '(emacs-commentary-link :tag "Commentary" "alchemist"))

(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-server)
(require 'alchemist-buffer)
(require 'alchemist-compile)
(require 'alchemist-execute)
(require 'alchemist-mix)
(require 'alchemist-hooks)
(require 'alchemist-help)
(require 'alchemist-complete)
(require 'alchemist-message)
(require 'alchemist-iex)
(require 'alchemist-eval)
(require 'alchemist-goto)
(require 'alchemist-test-mode)

(eval-after-load 'company
  '(progn
     (require 'alchemist-company)))

(defun alchemist-mode-hook ()
  "Hook which enables `alchemist-mode'"
  (alchemist-mode 1))

(defvar alchemist--version "1.0.0")

;;;###autoload
(defun alchemist-version (&optional show-version)
  "Display Alchemist's version."
  (interactive)
  (message "Alchemist %s" (replace-regexp-in-string "-cvs" "snapshot" alchemist--version)))

(defcustom alchemist-key-command-prefix
  (kbd "C-c a")
  "The prefix for alchemist related key commands."
  :type 'string
  :group 'alchemist)

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
         (alchemist-buffer-initialize-modeline))
        (t
         (alchemist-buffer-reset-modeline))))

(let ((map alchemist-mode-keymap))
  (define-key map (kbd "x") 'alchemist-mix)
  (define-key map (kbd "t") 'alchemist-mix-test)
  (define-key map (kbd "m c") 'alchemist-mix-compile)
  (define-key map (kbd "m t f") 'alchemist-mix-test-file)
  (define-key map (kbd "m t b") 'alchemist-mix-test-this-buffer)
  (define-key map (kbd "m t .") 'alchemist-mix-test-at-point)
  (define-key map (kbd "c c") 'alchemist-compile)
  (define-key map (kbd "c f") 'alchemist-compile-file)
  (define-key map (kbd "c b") 'alchemist-compile-this-buffer)
  (define-key map (kbd "e e") 'alchemist-execute)
  (define-key map (kbd "e f") 'alchemist-execute-file)
  (define-key map (kbd "e b") 'alchemist-execute-this-buffer)
  (define-key map (kbd "h h") 'alchemist-help)
  (define-key map (kbd "h i") 'alchemist-help-history)
  (define-key map (kbd "h e") 'alchemist-help-search-at-point)
  (define-key map (kbd "h m") 'alchemist-help-search-marked-region)
  (define-key map (kbd "p f") 'alchemist-project-find-test)
  (define-key map (kbd "p s") 'alchemist-project-toggle-file-and-tests)
  (define-key map (kbd "p o") 'alchemist-project-toggle-file-and-tests-other-window)
  (define-key map (kbd "i i") 'alchemist-iex-run)
  (define-key map (kbd "i p") 'alchemist-iex-project-run)
  (define-key map (kbd "i l") 'alchemist-iex-send-current-line)
  (define-key map (kbd "i c") 'alchemist-iex-send-current-line-and-go)
  (define-key map (kbd "i r") 'alchemist-iex-send-region)
  (define-key map (kbd "i m") 'alchemist-iex-send-region-and-go)
  (define-key map (kbd "i b") 'alchemist-iex-compile-this-buffer)
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
  (define-key map (kbd "v r") 'alchemist-eval-print-quoted-buffer))

(define-key alchemist-mode-map (kbd "M-.") 'alchemist-goto-definition-at-point)
(define-key alchemist-mode-map (kbd "M-,") 'alchemist-goto-jump-back)
(define-key alchemist-mode-map (kbd "C-c , .") 'alchemist-goto-list-symbol-definitions)

(easy-menu-define alchemist-mode-menu alchemist-mode-map
  "Alchemist mode menu."
  '("Alchemist"
    ("Goto"
     ["Jump to definiton at point" alchemist-goto-definition-at-point]
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
    ("Compile"
     ["Compile..." alchemist-compile]
     ["Compile this buffer" alchemist-compile-this-buffer]
     ["Compile file" alchemist-compile-file])
    ("Execute"
     ["Execute..." alchemist-compile]
     ["Execute this buffer" alchemist-execute-this-buffer]
     ["Execute file" alchemist-execute-file])
    ("Mix"
     ["Mix deps..." alchemist-mix-deps-with-prompt]
     ["Mix compile..." alchemist-mix-compile]
     ["Mix run..." alchemist-mix-run]
     "---"
     ["Mix test this buffer" alchemist-mix-test-this-buffer]
     ["Mix test file..." alchemist-mix-test-file]
     ["Mix test at point" alchemist-mix-test-at-point]
     "---"
     ["Mix..." alchemist-mix]
     ["Mix new..." alchemist-mix-new]
     ["Mix hex search..." alchemist-mix-hex-search]
     "---"
     ["Mix local..." alchemist-mix-local-with-prompt]
     ["Mix local install..." alchemist-mix-local-install]
     ["Mix local install (Path)..." alchemist-mix-local-install-with-path]
     ["Mix local install (URL)..." alchemist-mix-local-install-with-url]
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
     ["Project find all tests" alchemist-project-find-test]
     ["Project toggle between file and test" alchemist-project-toggle-file-and-tests]
     ["Project toggle between file and test in other window" alchemist-project-toggle-file-and-tests-other-window]
     "---"
     ["Project toggle compile when needed" alchemist-project-toggle-compile-when-needed
      :style toggle :selected alchemist-project-compile-when-needed])
    ("Documentation"
     ["Documentation search..." alchemist-help]
     ["Documentation search history..." alchemist-help-history]
     "---"
     ["Documentation search at point..." alchemist-help-search-at-point]
     ["Documentation search marked region..." alchemist-help-search-marked-region])
    ))

(add-hook 'elixir-mode-hook 'alchemist-mode-hook)

(provide 'alchemist)

;;; alchemist.el ends here
