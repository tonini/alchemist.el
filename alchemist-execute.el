;;; alchemist-execute.el --- Elixir's script execution integration

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

;; Elixir's script execution integration

;;; Code:

(require 'alchemist-utils)
(require 'alchemist-test-mode)
(require 'alchemist-report)

(defgroup alchemist-execute nil
  "Elixir's script execution integration."
  :prefix "alchemist-execute-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-execute-command "elixir"
  "The shell command for elixir."
  :type 'string
  :group 'alchemist-execute)

(defvar alchemist-execute-buffer-name "*alchemist elixir*"
  "Name of the elixir output buffer.")

(defvar alchemist-execute-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map))

;; Private functions

(defun alchemist-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exist"))
  (alchemist-execute (list alchemist-execute-command (expand-file-name filename))))

(defun alchemist-execute--read-command (command)
  (read-shell-command "elixir command: " (concat command " ")))

;; Public functions

(defun alchemist-execute-this-buffer ()
  "Run the current buffer through elixir."
  (interactive)
  (alchemist-execute--file buffer-file-name))

(defun alchemist-execute-file (filename)
  "Run elixir with the given FILENAME."
  (interactive "Felixir: ")
  (alchemist-execute--file (expand-file-name filename)))

(define-derived-mode alchemist-execute-mode fundamental-mode "Elixir Execute Mode"
  "Major mode for execute Elixir files.

\\{alchemist-execute-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil))

(defun alchemist-execute (cmdlist)
  "Run a elixir with CMDLIST."
  (interactive (list (alchemist-execute--read-command alchemist-execute-command)))
  (let ((command (alchemist-utils-build-command cmdlist)))
    (alchemist-report-run command
                          "alchemist-execute-report"
                          alchemist-execute-buffer-name
                          'alchemist-execute-mode)))

(provide 'alchemist-execute)

;;; alchemist-execute.el ends here
