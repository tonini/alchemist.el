;;; alchemist-mix.el --- Emacs integration for Elixir's mix

;; Copyright © 2014 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

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

;;; Code:

(defcustom alchemist-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'alchemist-mix)

(defvar alchemist-mix-buffer-name "*mix*"
  "Name of the mix output buffer.")

(defvar alchemist-mix--deps-commands
  '("deps" "deps.clean" "deps.compile" "deps.get" "deps.unlock" "deps.unlock")
  "List of all deps.* available commands.")

(defvar alchemist-mix--local-commands
  '("local" "local.install" "local.rebar" "local.uninstall")
  "List of all local.* available commands.")

(defvar alchemist-mix--local-install-option-types '("path" "url")
  "List of local.install option types.")

(defun alchemist-mix-display-mix-buffer ()
  (interactive)
  (when (get-buffer alchemist-mix-buffer-name)
    (display-buffer alchemist-mix-buffer-name)))

(defun alchemist-mix--completing-read (prompt cmdlist)
  (completing-read prompt cmdlist nil t nil nil (car cmdlist)))

(defun alchemist-mix-new (name)
  "Create a new elixir project with mix."
  (interactive "Gmix new: ")
  (alchemist-mix-execute (list alchemist-mix-command "new" (expand-file-name name))))

(defun alchemist-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (alchemist-mix-execute (list alchemist-mix-command "test")))

(defun alchemist-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (alchemist-mix--test-file buffer-file-name))

(defun alchemist-mix-test-file (filename)
  "Run <mix test> with the given `filename`"
  (interactive "Fmix test: ")
  (alchemist-mix--test-file (expand-file-name filename)))

(defun alchemist-mix--test-file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (alchemist-mix-execute (list alchemist-mix-command "test" (expand-file-name filename))))

(defun alchemist-mix-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
         (file-and-line (format "%s:%s" buffer-file-name line)))
    (alchemist-mix-execute (list alchemist-mix-command "test" file-and-line))))

(defun alchemist-mix-compile (command)
  "Compile the whole elixir project."
  (interactive "Mmix compile: ")
  (alchemist-mix-execute (list alchemist-mix-command "compile" command)))

(defun alchemist-mix-run (command)
  "Runs the given file or expression in the context of the application."
  (interactive "Mmix run: ")
  (alchemist-mix-execute (list alchemist-mix-command "run" command)))

(defun alchemist-mix-deps-with-prompt (command)
  "Prompt for mix deps commands."
  (interactive
   (list (alchemist-mix--completing-read "mix deps: " alchemist-mix--deps-commands)))
  (alchemist-mix-execute (list alchemist-mix-command command)))

(defun alchemist-mix-local-with-prompt (command)
  "Prompt for mix local commands."
  (interactive
   (list (alchemist-mix--completing-read "mix local: " alchemist-mix--local-commands)))
  (if (string= command "local.install")
      (call-interactively 'alchemist-mix-local-install)
    (alchemist-mix-execute (list alchemist-mix-command command))))

(defun alchemist-mix-local-install (path-or-url)
  "Prompt for mix local.install <path> or <url>."
  (interactive
   (list (completing-read "mix local.install FORMAT: "
                          alchemist-mix--local-install-option-types
                          nil t nil nil (car alchemist-mix--local-install-option-types))))
  (if (string= path-or-url (car alchemist-mix--local-install-option-types))
      (call-interactively 'alchemist-mix-local-install-with-path)
    (call-interactively 'alchemist-mix-local-install-with-url)))

(defun alchemist-mix-local-install-with-path (path)
  "Runs local.install and prompt for a <path> as argument."
  (interactive "fmix local.install PATH: ")
  (alchemist-mix-execute (list alchemist-mix-command "local.install" path)))

(defun alchemist-mix-local-install-with-url (url)
  "Runs local.install and prompt for a <url> as argument."
  (interactive "Mmix local.install URL: ")
  (alchemist-mix-execute (list alchemist-mix-command "local.install" url)))

(defun alchemist-mix-hex-search (command)
  "Display packages matching the given search query."
  (interactive "Mmix hex.search: ")
  (alchemist-mix-execute (list alchemist-mix-command "hex.search" command)))

(defun alchemist-mix-help (command)
  "Show help output for a specific mix command."
  (interactive "Mmix help: ")
  (alchemist-mix-execute (list alchemist-mix-command "help" command)))

(defun alchemist-mix-execute (cmdlist)
  "Run a mix command."
  (interactive "Mmix: ")
  (let ((old-directory default-directory))
    (alchemist-utils--establish-project-root-directory)
    (alchemist-buffer-run (alchemist-utils--build-runner-cmdlist cmdlist)
                          alchemist-mix-buffer-name)
    (cd old-directory)))

(provide 'alchemist-mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alchemist-mix.el ends here
