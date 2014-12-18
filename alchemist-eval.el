;;; alchemist-eval.el --- Elixir code inline evaluation functionality

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

;;  Elixir code inline evaluation functionality

;;; Code:

(defun alchemist-eval-current-line ()
  "Evaluate the Elixir code on the current line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (message (alchemist-eval--evaluate-code current-line))))

(defun alchemist-eval-region (beg end)
  "Evaluate the Elixir code on marked region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (message (alchemist-eval--evaluate-code string))))

(defun alchemist-eval-buffer ()
  "Evaluate the Elixir code in the current buffer."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (message (alchemist-eval--evaluate-code string))))

(defun alchemist-eval--evaluate-code (string)
  (let ((tmp-file ".alchemist-eval.exs"))
    (with-temp-file tmp-file
      (insert string))
    (let ((output (shell-command-to-string
                   (alchemist-eval--build-code-evaluation-command tmp-file))))
      (delete-file tmp-file)
      (alchemist-utils--remove-newline-at-end output))))

(defun alchemist-eval--build-code-evaluation-command (file)
  (format "%s -e 'IO.inspect(elem(Code.eval_string(File.read!(\"%s\")), 0))'"
          elixir-mode-command
          ".alchemist-eval.exs"))

(provide 'alchemist-eval)

;;; alchemist-eval.el ends here
