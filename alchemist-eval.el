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

(require 'elixir-mode)
(require 'alchemist-server)

(defgroup alchemist-eval nil
  "Elixir code inline evaluation functionality."
  :prefix "alchemist-eval-"
  :group 'alchemist)

(defvar alchemist-eval-buffer-name "*alchemist-eval-mode*"
  "Name of the Elixir evaluation buffer.")

(defvar alchemist-eval-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `alchemist-eval-minor-mode'.")

(defvar alchemist-eval-filter-output nil)

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
    (alchemist-server-eval file #'alchemist-eval-filter)))

(defun alchemist-eval--expression-and-print (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval file #'alchemist-eval-insert-filter)))

(defun alchemist-eval--quote-expression (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval-quote file #'alchemist-eval-quoted-filter)))

(defun alchemist-eval--quote-expression-and-print (expression)
  (let ((file (make-temp-file "alchemist-eval" nil ".exs")))
    (with-temp-file file
      (insert expression))
    (alchemist-server-eval-quote file #'alchemist-eval-quoted-insert-filter)))

(defun alchemist-eval-filter (_process output)
  (setq alchemist-eval-filter-output (cons output alchemist-eval-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-eval-popup-buffer
     (alchemist-server-prepare-filter-output alchemist-eval-filter-output))
    (setq alchemist-eval-filter-output nil)))

(defun alchemist-eval-insert-filter (_process output)
  (setq alchemist-eval-filter-output (cons output alchemist-eval-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-eval--insert
     (alchemist-server-prepare-filter-output alchemist-eval-filter-output))
    (setq alchemist-eval-filter-output nil)))

(defun alchemist-eval-quoted-filter (_process output)
  (setq alchemist-eval-filter-output (cons output alchemist-eval-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-eval-popup-buffer
     (alchemist-server-prepare-filter-output alchemist-eval-filter-output))
    (setq alchemist-eval-filter-output nil)))

(defun alchemist-eval-quoted-insert-filter (_process output)
  (setq alchemist-eval-filter-output (cons output alchemist-eval-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-eval--insert
     (alchemist-server-prepare-filter-output alchemist-eval-filter-output))
    (setq alchemist-eval-filter-output nil)))

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

(defun alchemist-eval-close-popup ()
  "Quit the evaluation buffer window."
  (interactive)
  (quit-windows-on alchemist-eval-buffer-name))

(defun alchemist-eval-popup-buffer (eval)
  (let ((buffer (get-buffer-create alchemist-eval-buffer-name)))
    (with-current-buffer buffer
      (with-current-buffer-window
       buffer (cons 'display-buffer-below-selected
                    '((window-height . fit-window-to-buffer)))
       (lambda (window _value))
      (let ((inhibit-read-only t))
        (insert eval)
        (goto-char (point-min))
        (elixir-mode)
        (alchemist-eval-minor-mode))))))

(define-minor-mode alchemist-eval-minor-mode
  "Minor mode for displaying Elixir evaluation results."
  :group 'alchemist-eval
  :keymap alchemist-eval-minor-mode-map)

(provide 'alchemist-eval)

;;; alchemist-eval.el ends here
