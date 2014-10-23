;;; alchemist-execute.el --- Elixir's script execution integration

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

(defcustom alchemist-execute-command "elixir"
  "The shell command for elixir."
  :type 'string
  :group 'alchemist-execute)

(defvar alchemist-execute-buffer-name "*elixir*"
  "Name of the elixir output buffer.")

(defun alchemist-execute-this-buffer ()
  "Run the current buffer through elixir."
  (interactive)
  (alchemist-execute--file buffer-file-name))

(defun alchemist-execute-file (filename)
  "Run elixir with the given `FILENAME`."
  (interactive "Felixir: ")
  (alchemist-execute--file (expand-file-name filename)))

(defun alchemist-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (alchemist-execute (list alchemist-execute-command (expand-file-name filename))))

(defun alchemist-execute--read-command (command)
  (read-shell-command "elixir command: " (concat command " ")))

(defun alchemist-execute (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive (list (alchemist-execute--read-command alchemist-execute-command)))
  (alchemist-buffer-run (alchemist-utils--build-runner-cmdlist cmdlist)
                        alchemist-execute-buffer-name))

(provide 'alchemist-execute)

;;; alchemist-execute.el ends here
