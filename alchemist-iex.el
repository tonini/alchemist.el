;;; alchemist-help.el --- Interaction with an Elixir IEx process

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

;; Interaction with an Elixir IEx process

;;; Code:

(require 'comint)
(require 'company)
(require 'alchemist-key)
(require 'alchemist-scope)
(require 'alchemist-project)

(defgroup alchemist-iex nil
  "Interaction with an Elixir IEx process."
  :prefix "alchemist-iex-"
  :group 'alchemist)

(defcustom alchemist-iex-program-name "iex"
  "The shell command for iex."
  :type 'string
  :group 'alchemist-iex)

(defvar alchemist-iex-prompt-regexp "^\\(iex\\|\\.\\.\\.\\)(.+)>"
  "Prompt regex pattern of IEx interpreter.

Should match prompts that looks like these:
iex(1)>
...(1)>")

(defcustom alchemist-iex-prompt-read-only t
  "If non-nil, the prompt will be read-only."
  :type 'boolean
  :group 'alchemist-iex)

(defvar alchemist-iex-buffer nil
  "The buffer in which the Elixir IEx process is running.")

(defvar alchemist-iex-mode-hook nil
  "Hook for customizing `alchemist-iex-mode'.")

(defvar alchemist-iex-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'company-complete)
    (define-key map (kbd "TAB") 'company-complete)
    (define-key map (kbd (format "%s i r" alchemist-key-command-prefix)) 'alchemist-iex-open-input-ring)
    (define-key map (kbd (format "%s i c" alchemist-key-command-prefix)) 'alchemist-iex-clear-buffer)
    (define-key map (kbd (format "%s h e" alchemist-key-command-prefix)) 'alchemist-help-search-at-point)
    (define-key map (kbd "M-.") 'alchemist-goto-definition-at-point)
    map))

(define-derived-mode alchemist-iex-mode comint-mode "Alchemist-IEx"
  "Major mode for interacting with an Elixir IEx process.

\\<alchemist-iex-mode-map>"
  nil "Alchemist-IEx"
  (set (make-local-variable 'comint-prompt-regexp) alchemist-iex-prompt-regexp)
  (set (make-local-variable 'comint-prompt-read-only) alchemist-iex-prompt-read-only)
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (set (make-local-variable 'comint-input-sender) 'alchemist-iex--send-command)
  (add-hook 'comint-output-filter-functions 'alchemist-iex-spot-prompt nil t))

(defun alchemist-iex-command (arg)
  (split-string-and-unquote
   (if (null arg) alchemist-iex-program-name
     (read-string "Command to run Elixir IEx: " (concat alchemist-iex-program-name arg)))))

(defun alchemist-iex-start-process (command)
  "Start an IEX process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `alchemist-iex-program-name'.
It runs the hook `alchemist-iex-mode-hook' after starting the process and
setting up the IEx buffer."
  (interactive (list (alchemist-iex-command current-prefix-arg)))
  (setq alchemist-iex-buffer
        (apply 'make-comint "Alchemist-IEx" (car command) nil (cdr command)))
  (with-current-buffer alchemist-iex-buffer
    (alchemist-iex-mode)
    (run-hooks 'alchemist-iex-mode-hook)))

(defun alchemist-iex-process (&optional arg)
  (or (if (buffer-live-p alchemist-iex-buffer)
          (get-buffer-process alchemist-iex-buffer))
      (progn
        (let ((current-prefix-arg arg))
          (call-interactively 'alchemist-iex-start-process))
        (alchemist-iex-process arg))))

(defun alchemist-iex--remove-newlines (string)
  (replace-regexp-in-string "\n" " " string))

(defun alchemist-iex-send-last-sexp ()
  "Send the previous sexp to the inferior IEx process."
  (interactive)
  (alchemist-iex-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun alchemist-iex-send-current-line ()
  "Sends the current line to the IEx process."
  (interactive)
  (let ((str (thing-at-point 'line)))
    (alchemist-iex--send-command (alchemist-iex-process) str)))

(defun alchemist-iex-send-current-line-and-go ()
  "Sends the current line to the inferior IEx process
and jump to the buffer."
  (interactive)
  (call-interactively 'alchemist-iex-send-current-line)
  (pop-to-buffer (process-buffer (alchemist-iex-process))))

(defun alchemist-iex-send-region-and-go ()
  "Sends the marked region to the inferior IEx process
and jump to the buffer."
  (interactive)
  (call-interactively 'alchemist-iex-send-region)
  (pop-to-buffer (process-buffer (alchemist-iex-process))))

(defun alchemist-iex-send-region (beg end)
  "Sends the marked region to the IEx process."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let* ((region (buffer-substring-no-properties beg end)))
    (alchemist-iex--send-command (alchemist-iex-process) region)))

(defun alchemist-iex-compile-this-buffer ()
  "Compiles the current buffer in the IEx process."
  (interactive)
  (let* ((path (if (alchemist-project-p)
		   (format "%s/_build/dev/" (alchemist-project-root))
		 "."))
	 (str (format "c(\"%s\", \"%s\")" (buffer-file-name) path)))
    (alchemist-iex--send-command (alchemist-iex-process) str)))

(defun alchemist-iex-compile-this-buffer-and-go ()
  "Compiles the current buffer in the IEx process and jump to the buffer."
  (interactive)
  (alchemist-iex-compile-this-buffer)
  (pop-to-buffer (process-buffer (alchemist-iex-process))))

(defun alchemist-iex-reload-module ()
  "Recompiles and reloads the current module in the IEx process."
  (interactive)
  (let ((str (format "r(%s)" (alchemist-scope-module))))
    (alchemist-iex--send-command (alchemist-iex-process) str)))

(defun alchemist-iex--send-command (proc str)
  (let ((lines (split-string str "\n" nil)))
    (with-current-buffer (process-buffer proc)
      (-map (lambda (line)
              (alchemist-iex-wait-for-prompt proc)
              (goto-char (process-mark proc))
              (insert-before-markers (concat line "\n"))
              (move-marker comint-last-input-end (point))
              (comint-send-string proc (concat line "\n"))) lines))))

(defvar alchemist-iex-seen-prompt nil)
(make-variable-buffer-local 'alchemist-iex-seen-prompt)

(defun alchemist-iex-spot-prompt (_string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq alchemist-iex-seen-prompt t))))))

(defun alchemist-iex-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or alchemist-iex-seen-prompt
                      (setq alchemist-iex-seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless alchemist-iex-seen-prompt
      (error "Can't find the IEx prompt"))
    (setq alchemist-iex-seen-prompt nil)))

(defun alchemist-iex-clear-buffer ()
  "Clear the current iex process buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun alchemist-iex-open-input-ring ()
    "Open the buffer containing the input history."
    (interactive)
    (progn
      (comint-dynamic-list-input-ring)
      (other-window 1)))

;;;###autoload
(defalias 'run-elixir 'alchemist-iex-run)
(defalias 'inferior-elixir 'alchemist-iex-run)

;;;###autoload
(defun alchemist-iex-run (&optional arg)
  "Start an IEx process.
Show the IEx buffer if an IEx process is already run."
  (interactive "P")
  (let ((proc (alchemist-iex-process arg)))
    (pop-to-buffer (process-buffer proc))))

;;;###autoload
(defun alchemist-iex-project-run ()
  "Start an IEx process with mix 'iex -S mix' in the
context of an Elixir project.
Show the IEx buffer if an IEx process is already run."
  (interactive)
  (if (alchemist-project-p)
      (let ((default-directory (alchemist-project-root)))
        (pop-to-buffer (process-buffer (alchemist-iex-process " -S mix"))))
    (message "No mix.exs file available. Please use `alchemist-iex-run' instead.")))

(provide 'alchemist-iex)

;;; alchemist-iex.el ends here
