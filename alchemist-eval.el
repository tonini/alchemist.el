;;; alchemist-eval.el --- Elixir code inline evaluation functionality

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

;; Elixir code inline evaluation functionality

;;; Code:

;; Tell the byte compiler to assume that functions are defined
(eval-when-compile
  (declare-function alchemist-server-eval "alchemist-server.el")
  (declare-function alchemist-server-eval-and-insert "alchemist-server.el")
  (declare-function alchemist-server-eval-quote "alchemist-server.el")
  (declare-function alchemist-server-eval-quote-and-insert "alchemist-server.el"))

(defgroup alchemist-eval nil
  "Elixir code inline evaluation functionality."
  :prefix "alchemist-eval-"
  :group 'alchemist)

;; Private functions

(defun alchemist-eval--insert (string)
  (let ((lines (split-string string "\n")))
    (if (> (length lines) 1)
        (progn
          (save-excursion
            (end-of-line)
            (mapc (lambda (s)
                    (newline)
                    (insert (format "# => %s" s))
                    (indent-according-to-mode))
                  lines)))
      (save-excursion
        (end-of-line)
        (insert (format "  # => %s" string))))))

(defun alchemist-eval--expression (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval file)))

(defun alchemist-eval--expression-and-print (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval-and-insert file)))

(defun alchemist-eval--quote-expression (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval-quote file)))

(defun alchemist-eval--quote-expression-and-print (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval-quote-and-insert file)))

;; Public functions

(defun alchemist-eval-current-line ()
  "Evaluate the Elixir code on the current line."
  (interactive)
  (alchemist-eval--expression (thing-at-point 'line)))

(defun alchemist-eval-print-current-line ()
  "Evaluate the Elixir code on the current line and insert the result."
  (interactive)
  (alchemist-eval--expression-and-print (thing-at-point 'line)))

(defun alchemist-eval-region (beg end)
  "Evaluate the Elixir code on marked region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (alchemist-eval--expression string)))

(defun alchemist-eval-print-region (beg end)
  "Evaluate the Elixir code on marked region and insert the result."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (when (> end beg)
      (exchange-point-and-mark))
    (alchemist-eval--expression-and-print string)))

(defun alchemist-eval-buffer ()
  "Evaluate the Elixir code in the current buffer."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (alchemist-eval--expression string)))

(defun alchemist-eval-print-buffer ()
  "Evaluate the Elixir code in the current buffer and insert the result."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (goto-char (point-max))
    (alchemist-eval--expression-and-print string)))

(defun alchemist-eval-quoted-current-line ()
  "Get the Elixir code representation of the expression on the current line."
  (interactive)
  (alchemist-eval--quote-expression (thing-at-point 'line)))

(defun alchemist-eval-print-quoted-current-line ()
  "Get the Elixir code representation of the expression on the current line and insert the result."
  (interactive)
  (alchemist-eval--quote-expression-and-print (thing-at-point 'line)))

(defun alchemist-eval-quoted-region (beg end)
  "Get the Elixir code representation of the expression on marked region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (alchemist-eval--quote-expression string)))

(defun alchemist-eval-print-quoted-region (beg end)
  "Get the Elixir code representation of the expression on marked region and insert the result."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (when (> end beg)
      (exchange-point-and-mark))
    (alchemist-eval--quote-expression-and-print string)))

(defun alchemist-eval-quoted-buffer ()
  "Get the Elixir code representation of the expression in the current buffer."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (alchemist-eval--quote-expression string)))

(defun alchemist-eval-print-quoted-buffer ()
  "Get the Elixir code representation of the expression in the current buffer and insert result."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min) (point-max))))
    (alchemist-eval--quote-expression-and-print string)))

(provide 'alchemist-eval)

;;; alchemist-eval.el ends here
