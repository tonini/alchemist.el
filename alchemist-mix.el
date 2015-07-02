;;; alchemist-mix.el --- Emacs integration for Elixir's mix

;; Copyright © 2014-2015 Samuel Tonini

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

;; Emacs integration for Elixir's mix

;;; Code:

(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-buffer)

(defgroup alchemist-mix nil
  "Emacs integration for Elixir's mix."
  :prefix "alchemist-mix-"
  :group 'alchemist)

;; Variables

(defvar alchemist-test-mode-buffer-name "")

(defcustom alchemist-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-task "test"
  "Default task to run tests."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-default-options '("--exclude pending:true")
  "Default options for alchemist test command."
  :type '(repeat string)
  :group 'alchemist-mix)

(defcustom alchemist-mix-env nil
  "The default mix env to run mix commands with.  If nil, the mix env is
not set explicitly."
  :type '(string boolean)
  :group 'alchemist-mix)

(defvar alchemist-mix-buffer-name "*mix*"
  "Name of the mix output buffer.")

(defvar alchemist-mix--envs '("dev" "prod" "test")
  "The list of mix envs to use as defaults.")

(defvar alchemist-mix--deps-commands
  '("deps" "deps.clean" "deps.compile" "deps.get" "deps.unlock" "deps.unlock")
  "List of all deps.* available commands.")

(defvar alchemist-mix--local-commands
  '("local" "local.install" "local.rebar" "local.uninstall")
  "List of all local.* available commands.")

(defvar alchemist-mix--local-install-option-types '("path" "url")
  "List of local.install option types.")

;; Private functions

(defun alchemist-mix--completing-read (prompt cmdlist)
  (completing-read prompt cmdlist nil t nil nil (car cmdlist)))

(defun alchemist-mix--test-file (filename)
  "Run a specific FILENAME as argument for the mix command test."
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (alchemist-mix-execute `(,alchemist-mix-test-task ,(expand-file-name filename) ,@alchemist-mix-test-default-options)
                         alchemist-test-mode-buffer-name))

(defun alchemist-mix--commands ()
  (let ((mix-cmd-list (shell-command-to-string (format "%s help" alchemist-mix-command))))
    (mapcar (lambda (s)
              (cdr (split-string (car (split-string s "#")))))
            (cdr (split-string mix-cmd-list "\n")))))

;; Public functions

(defun alchemist-mix-display-mix-buffer ()
  "Display the mix buffer when exists."
  (interactive)
  (when (get-buffer alchemist-mix-buffer-name)
    (display-buffer alchemist-mix-buffer-name)))

(defun alchemist-mix-new (name)
  "Create a new elixir project named by NAME."
  (interactive "Gmix new: ")
  (alchemist-mix-execute (list "new" (expand-file-name name))
                         alchemist-mix-buffer-name))

(defun alchemist-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (alchemist-mix-execute `(,alchemist-mix-test-task ,@alchemist-mix-test-default-options)
                         alchemist-test-mode-buffer-name))

(defun alchemist-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (alchemist-mix--test-file buffer-file-name))

(defun alchemist-mix-test-file (filename)
  "Run `alchemist-mix--test-file' with the FILENAME."
  (interactive "Fmix test: ")
  (alchemist-mix--test-file (expand-file-name filename)))

(defun alchemist-mix-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
         (file-and-line (format "%s:%s" buffer-file-name line)))
    (alchemist-mix-execute (list alchemist-mix-test-task file-and-line)
                           alchemist-test-mode-buffer-name)))

(defun alchemist-mix-compile (command &optional prefix)
  "Compile the whole elixir project. Prompt for the mix env if the prefix
arg is set."
  (interactive "Mmix compile: \nP")
  (alchemist-mix-execute (list "compile" command)
                         alchemist-mix-buffer-name prefix))

(defun alchemist-mix-run (command &optional prefix)
  "Runs the given file or expression in the context of the application.
Prompt for the mix env if the prefix arg is set."
  (interactive "Mmix run: \nP")
  (alchemist-mix-execute (list "run" command)
                         alchemist-mix-buffer-name prefix))

(defun alchemist-mix-deps-with-prompt (command &optional prefix)
  "Prompt for mix deps commands."
  (interactive
   (list (alchemist-mix--completing-read "mix deps: " alchemist-mix--deps-commands)
         current-prefix-arg))
  (alchemist-mix-execute (list command)
                         alchemist-mix-buffer-name prefix))

(defun alchemist-mix (command &optional prefix)
  "Prompt for mix commands. Prompt for the mix env if the prefix arg is set."
  (interactive
   (list (alchemist-mix--completing-read "mix: " (alchemist-mix--commands))
         current-prefix-arg))
  (let ((command (read-string "mix " (concat command " "))))
    (alchemist-mix-execute (list command)
                           alchemist-mix-buffer-name prefix)))

(defun alchemist-mix-local-with-prompt (command)
  "Prompt for mix local commands."
  (interactive
   (list (alchemist-mix--completing-read "mix local: " alchemist-mix--local-commands)))
  (if (string= command "local.install")
      (call-interactively 'alchemist-mix-local-install)
    (alchemist-mix-execute (list command)
                           alchemist-mix-buffer-name)))

(defun alchemist-mix-local-install (path-or-url)
  "Prompt for mix local.install PATH-OR-URL."
  (interactive
   (list (completing-read "mix local.install FORMAT: "
                          alchemist-mix--local-install-option-types
                          nil t nil nil (car alchemist-mix--local-install-option-types))))
  (if (string= path-or-url (car alchemist-mix--local-install-option-types))
      (call-interactively 'alchemist-mix-local-install-with-path)
    (call-interactively 'alchemist-mix-local-install-with-url)))

(defun alchemist-mix-local-install-with-path (path)
  "Runs local.install and prompt for a PATH as argument."
  (interactive "fmix local.install PATH: ")
  (alchemist-mix-execute (list "local.install" path)
                         alchemist-mix-buffer-name))

(defun alchemist-mix-local-install-with-url (url)
  "Runs local.install and prompt for a URL as argument."
  (interactive "Mmix local.install URL: ")
  (alchemist-mix-execute (list "local.install" url)
                         alchemist-mix-buffer-name))

(defun alchemist-mix-hex-search (command &optional prefix)
  "Display packages matching the given search query. Prompt for the mix env
if the prefix arg is set."
  (interactive "Mmix hex.search: \nP")
  (alchemist-mix-execute (list "hex.search" command)
                         alchemist-mix-buffer-name prefix))

(defun alchemist-mix-help (command &optional prefix)
  "Show help output for a specific mix command. Prompt for the mix env if
the prefix arg is set."
  (interactive "Mmix help: \nP")
  (alchemist-mix-execute (list "help" command)
                         alchemist-mix-buffer-name prefix))

(defun alchemist-mix-execute (cmdlist buffer-name &optional prefix)
  "Run a mix command. Prompt for the mix env if the prefix arg is set."
  (interactive "Mmix: \nP")
  (let ((old-directory default-directory)
        (mix-env (if prefix
                     (completing-read "mix env: "
                                      alchemist-mix--envs nil nil alchemist-mix-env)
                   alchemist-mix-env)))
    (alchemist-project--establish-root-directory)
    (alchemist-buffer-run (alchemist-utils--build-runner-cmdlist
                           (list (if mix-env (concat "MIX_ENV=" mix-env) "")
                                 alchemist-mix-command cmdlist))
                          buffer-name)
    (cd old-directory)))

(provide 'alchemist-mix)

;;; alchemist-mix.el ends here
