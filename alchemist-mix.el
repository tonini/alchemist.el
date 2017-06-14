;;; alchemist-mix.el --- Interface to run Elixir mix tasks inside Emacs

;; Copyright © 2014-2017 Samuel Tonini

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

;; Interface to run Elixir mix tasks inside Emacs.

;;; Code:

(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-test-mode)
(require 'alchemist-server)

(defgroup alchemist-mix nil
  "Emacs integration for Elixir's mix."
  :prefix "alchemist-mix-"
  :group 'alchemist)

;; Variables

(defvar alchemist-last-run-test nil)
(defvar alchemist-mix-filter-output nil)
(defvar alchemist-mix-last-task-command nil)

(defcustom alchemist-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-task "test"
  "Default task to run tests."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-default-options '()
  "Default options for alchemist test command."
  :type '(repeat string)
  :group 'alchemist-mix)

(defcustom alchemist-mix-env nil
  "The default mix env to run mix commands with.  If nil, the mix env is
not set explicitly."
  :type '(string boolean)
  :group 'alchemist-mix)

(defvar alchemist-mix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "i" #'alchemist-mix-send-input-to-mix-process)
    (define-key map "r" #'alchemist-mix-rerun-last-task)
    map))

(defvar alchemist-mix-buffer-name "*alchemist mix*"
  "Name of the mix output buffer.")

(defvar alchemist-mix--envs '("dev" "prod" "test")
  "The list of mix envs to use as defaults.")

(defconst alchemist-mix-process-name "alchemist-mix-report"
  "Name of the mix process.")

;; Private functions

(defun alchemist-mix--completing-read (prompt cmdlist)
  (completing-read prompt cmdlist nil t nil nil (car cmdlist)))

(defun alchemist-mix--execute-test (&optional what)
  "Execute 'mix test' on the given `WHAT'.

`WHAT' could be a filename, a filename:line string or the empty string (meaning
run all tests)."
  (if what
      (setq alchemist-last-run-test what)
    (setq alchemist-last-run-test ""))
  (alchemist-test-execute (list alchemist-mix-command
                                alchemist-mix-test-task
                                what
                                alchemist-mix-test-default-options)))

(defun alchemist-mix--test-file (filename)
  "Run a specific FILENAME as argument for the mix command test."
  (when (not (file-exists-p filename))
    (error "The given file doesn't exist"))
  (alchemist-mix--execute-test (expand-file-name filename)))

;; Public functions

(defun alchemist-mix ()
  "Prompt for a specific mix task to run.

If the command `universal-argument' is called before `alchemist-mix',
a prompt for a specific mix environment in which the task will be
executed, gets called."
  (interactive)
  (alchemist-server-info "{ :type, :mixtasks }" #'alchemist-mix-filter))

(defun alchemist-mix-display-mix-buffer ()
  "Display the mix buffer when exists."
  (interactive)
  (when (get-buffer alchemist-mix-buffer-name)
    (display-buffer alchemist-mix-buffer-name)))

(defun alchemist-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (alchemist-mix--execute-test))

(defun alchemist-mix-test-stale ()
  "Run stale tests (Elixir 1.3+ only)."
  (interactive)
  (if (alchemist-utils-elixir-version-check-p 1 3 0)
      (alchemist-mix--execute-test "--stale")
    (progn (message "Elixir needs to be >= 1.3.0 for mix test --stale")
           (alchemist-mix-test))))

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
    (alchemist-mix--execute-test file-and-line)))

(defun alchemist-mix-rerun-last-test ()
  "Rerun the last test that was run by alchemist.

When no tests had been run before calling this function, do nothing."
  (interactive)
  (if alchemist-last-run-test
      (alchemist-mix--execute-test alchemist-last-run-test)
    (message "No tests have been run yet")))

(defun alchemist-mix-compile (command &optional prefix)
  "Compile the whole elixir project. Prompt for the mix env if the prefix
arg is set."
  (interactive "Mmix compile: \nP")
  (alchemist-mix-execute (list "compile" command) prefix))

(defun alchemist-mix-run (command &optional prefix)
  "Runs the given file or expression in the context of the application."
  (interactive "Mmix run: \nP")
  (alchemist-mix-execute (list "run" command) prefix))

(defun alchemist-mix-send-input-to-mix-process (input)
  "Send INPUT to the current running mix task process."
  (interactive "MSend to running mix task: ")
  (let* ((buffer (get-buffer alchemist-mix-buffer-name))
         (process (get-buffer-process buffer)))
    (if (and process (eq (process-status process) 'run))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (concat input "\n\n"))
            (set-marker (process-mark process) (point)))
          (comint-send-string process (concat input "\n")))
      (error "No %s process is running" alchemist-mix-buffer-name))))

(defun alchemist-mix-rerun-last-task ()
  "Rerun the last mix task which was run by alchemist.
When no mix task had been run before calling this function, do nothing."
  (interactive)
  (if alchemist-mix-last-task-command
      (alchemist-report-run alchemist-mix-last-task-command
			    alchemist-mix-process-name
			    alchemist-mix-buffer-name 'alchemist-mix-mode)
    (message "No mix task have been run yet")))

(defun alchemist-mix-filter (_process output)
  (with-local-quit
    (setq alchemist-mix-filter-output (cons output alchemist-mix-filter-output))
    (when (alchemist-server-contains-end-marker-p output)
      (let* ((output (alchemist-server-prepare-filter-output alchemist-mix-filter-output))
             (tasks (split-string output "\n"))
             (selected-task (alchemist-mix--completing-read "mix: " (-distinct tasks)))
             (command (read-shell-command "mix " (concat selected-task " "))))
        (setq alchemist-mix-filter-output nil)
        (alchemist-mix-execute (list command) current-prefix-arg)))))

(define-derived-mode alchemist-mix-mode fundamental-mode "Mix Mode"
  "Major mode for presenting Mix tasks.

\\{alchemist-mix-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil))

(defun alchemist-mix-execute (command-list &optional prefix)
  "Run a mix task specified by COMMAND-LIST.

If PREFIX is non-nil, prompt for a mix environment variable."
  (let* ((mix-env (if prefix
                      (completing-read "mix env: " alchemist-mix--envs nil nil alchemist-mix-env)
                    alchemist-mix-env))
         (command (alchemist-utils-build-command
                   (list (when mix-env (concat "MIX_ENV=" mix-env))
                         alchemist-mix-command command-list))))
    (setq alchemist-mix-last-task-command command)
    (alchemist-report-run command alchemist-mix-process-name alchemist-mix-buffer-name 'alchemist-mix-mode)))

(provide 'alchemist-mix)

;;; alchemist-mix.el ends here
