;;; alchemist.el --- Elixir tooling integration into Emacs

;; Copyright © 2014-2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 0.14.0-cvs
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

(defgroup alchemist nil
  "Elixir Tooling Integration Into Emacs."
  :prefix "alchemist-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/tonini/alchemist.el")
  :link '(emacs-commentary-link :tag "Commentary" "alchemist"))

(require 'alchemist-utils)
(require 'alchemist-project)
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

(eval-after-load 'company
  '(progn
     (require 'alchemist-company)))

(defun alchemist-mode-hook ()
  "Hook which enables `alchemist-mode'"
  (alchemist-mode 1))

(defvar alchemist--version "0.14.0-cvs")

;;;###autoload
(defun alchemist-version (&optional show-version)
  "Display Alchemist's version."
  (interactive)
  (message "Alchemist %s" (replace-regexp-in-string "-cvs" "snapshot" alchemist--version)))

;;
;;; Keybindings
;;
(defcustom alchemist-key-command-prefix
  (kbd "C-c a")
  "The prefix for alchemist related key commands."
  :type 'string
  :group 'alchemist)

(define-prefix-command 'alchemist-mode-keymap)

(easy-menu-define alchemist-mode-menu alchemist-mode-map
  "Alchemist mode menu."
  '("Alchemist"
    ["Goto Defn at Point" alchemist-goto-definition-at-point]
    ("Evaluate"
     ["Current Line" alchemist-eval-current-line]
     ["Current Line and Print" alchemist-eval-print-current-line]
     ["Quoted Current Line" alchemist-eval-quoted-current-line]
     ["Quoted Current Line and Print" alchemist-eval-print-quoted-current-line]
     "---"
     ["Region" alchemist-eval-region]
     ["Region and Print" alchemist-eval-print-region]
     ["Quoted Region" alchemist-eval-quoted-region]
     ["Quoted Region and Print" alchemist-eval-print-quoted-region]
     "---"
     ["Buffer" alchemist-eval-buffer]
     ["Buffer and Print" alchemist-eval-print-buffer]
     ["Quoted Buffer" alchemist-eval-quoted-buffer]
     ["Quoted Buffer and Print" alchemist-eval-print-quoted-buffer]
     )
    ("Compile"
     ["Compile..." alchemist-compile]
     ["Buffer" alchemist-compile-this-buffer]
     ["File" alchemist-compile-file])
    ("Mix"
     ["Deps..." alchemist-mix-deps-with-prompt]
     ["Compile..." alchemist-mix-compile]
     ["Run..." alchemist-mix-run]
     "---"
     ["Test Buffer..." alchemist-mix-test-this-buffer]
     ["Test File..." alchemist-mix-test-file]
     ["Test at Point" alchemist-mix-test-at-point]
     "---"
     ["Mix..." alchemist-mix]
     ["New..." alchemist-mix-new]
     ["Hex Search..." alchemist-mix-hex-search]
     "---"
     ["Local..." alchemist-mix-local-with-prompt]
     ["Local Install..." alchemist-mix-local-install]
     ["Local Install (Path)..." alchemist-mix-local-install-with-path]
     ["Local Install (URL)..." alchemist-mix-local-install-with-url]
     "---"
     ["Display Mix Buffer" alchemist-mix-display-mix-buffer]
     "---"
     ["Help for Command..." alchemist-mix-help])
    ("IEx"
     ["Send Current Line" alchemist-iex-send-current-line]
     ["Send Current Line and Go" alchemist-iex-send-current-line-and-go]
     "---"
     ["Send Last Region" alchemist-iex-send-last-sexp]
     ["Send Region" alchemist-iex-send-region]
     ["Send Region and Go" alchemist-iex-send-region-and-go]
     "---"
     ["Compile Buffer" alchemist-iex-compile-this-buffer]
     ["Recompile Buffer" alchemist-iex-recompile-this-buffer]
     "---"
     ["Start IEx Process" alchemist-iex-start-process]
     ["Start Mix IEx Process" alchemist-mix-iex])
    ("Project"
     ["Find All Tests" alchemist-project-find-test]
     ["Find Tests for File" alchemist-project-open-tests-for-current-file]
     "---"
     ["Compile When Needed" alchemist-project-toggle-compile-when-needed
      :style toggle :selected alchemist-project-compile-when-needed])
    ("Help"
     ["Elixir Help..." alchemist-help]
     ["Help History..." alchemist-help-history]
     "---"
     ["Search at Point..." alchemist-help-search-at-point]
     ["Search Marked Region..." alchemist-help-search-marked-region]
     "---"
     ["Minor Mode Keybindings" alchemist-help-minor-mode-key-binding-summary])
    ))

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

(define-key alchemist-mode-keymap (kbd "t") 'alchemist-mix-test)
(define-key alchemist-mode-keymap (kbd "m t f") 'alchemist-mix-test-file)
(define-key alchemist-mode-keymap (kbd "m t b") 'alchemist-mix-test-this-buffer)
(define-key alchemist-mode-keymap (kbd "m t .") 'alchemist-mix-test-at-point)
(define-key alchemist-mode-keymap (kbd "c c") 'alchemist-compile)
(define-key alchemist-mode-keymap (kbd "c f") 'alchemist-compile-file)
(define-key alchemist-mode-keymap (kbd "c b") 'alchemist-compile-this-buffer)
(define-key alchemist-mode-keymap (kbd "e e") 'alchemist-execute)
(define-key alchemist-mode-keymap (kbd "e f") 'alchemist-execute-file)
(define-key alchemist-mode-keymap (kbd "e b") 'alchemist-execute-this-buffer)
(define-key alchemist-mode-keymap (kbd "h h") 'alchemist-help)
(define-key alchemist-mode-keymap (kbd "h e") 'alchemist-help-search-at-point)
(define-key alchemist-mode-keymap (kbd "h m") 'alchemist-help-search-marked-region)
(define-key alchemist-mode-keymap (kbd "p f") 'alchemist-project-find-test)
(define-key alchemist-mode-keymap (kbd "p t") 'alchemist-project-open-tests-for-current-file)
(define-key alchemist-mode-keymap (kbd "i i") 'alchemist-iex-run)
(define-key alchemist-mode-keymap (kbd "i p") 'alchemist-iex-project-run)
(define-key alchemist-mode-keymap (kbd "i l") 'alchemist-iex-send-current-line)
(define-key alchemist-mode-keymap (kbd "i c") 'alchemist-iex-send-current-line-and-go)
(define-key alchemist-mode-keymap (kbd "i r") 'alchemist-iex-send-region)
(define-key alchemist-mode-keymap (kbd "i m") 'alchemist-iex-send-region-and-go)
(define-key alchemist-mode-keymap (kbd "i b") 'alchemist-iex-compile-this-buffer)
(define-key alchemist-mode-keymap (kbd "v l") 'alchemist-eval-current-line)
(define-key alchemist-mode-keymap (kbd "v k") 'alchemist-eval-print-current-line)
(define-key alchemist-mode-keymap (kbd "v j") 'alchemist-eval-quoted-current-line)
(define-key alchemist-mode-keymap (kbd "v h") 'alchemist-eval-print-quoted-current-line)
(define-key alchemist-mode-keymap (kbd "v o") 'alchemist-eval-region)
(define-key alchemist-mode-keymap (kbd "v i") 'alchemist-eval-print-region)
(define-key alchemist-mode-keymap (kbd "v u") 'alchemist-eval-quoted-region)
(define-key alchemist-mode-keymap (kbd "v y") 'alchemist-eval-print-quoted-region)
(define-key alchemist-mode-keymap (kbd "v q") 'alchemist-eval-buffer)
(define-key alchemist-mode-keymap (kbd "v w") 'alchemist-eval-print-buffer)
(define-key alchemist-mode-keymap (kbd "v e") 'alchemist-eval-quoted-buffer)
(define-key alchemist-mode-keymap (kbd "v r") 'alchemist-eval-print-quoted-buffer)

(define-key alchemist-mode-map (kbd "M-.") 'alchemist-goto-definition-at-point)
(define-key alchemist-mode-map (kbd "M-,") 'alchemist-goto-jump-back)

(add-hook 'elixir-mode-hook 'alchemist-mode-hook)

(provide 'alchemist)

;;; alchemist.el ends here
