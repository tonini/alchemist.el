;;; alchemist-macroexpand.el --- Macro expansion support -*- lexical-binding: t -*-

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

;; Macro expansion support

;;; Code:

(require 'alchemist-server)
(require 'alchemist-interact)

(defgroup alchemist-macroexpand nil
  "Macro expansion support."
  :prefix "alchemist-macroexpand-"
  :group 'alchemist)

(defvar alchemist-macroexpand-filter-output nil)

(defconst alchemist-macroexpand-buffer-name "*alchemist macroexpand*"
  "Name of the Elixir Macro expand buffer.")

(defun alchemist-macroexpand-filter (_process output)
  (setq alchemist-macroexpand-filter-output (cons output alchemist-macroexpand-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-interact-create-popup alchemist-macroexpand-buffer-name
                                     (alchemist-server-prepare-filter-output alchemist-macroexpand-filter-output)
                                     #'(lambda ()
                                         (elixir-mode)
                                         (alchemist-macroexpand-mode)))
    (setq alchemist-macroexpand-filter-output nil)))

(defun alchemist-macroexpand-insert-filter (_process output)
  (setq alchemist-macroexpand-filter-output (cons output alchemist-macroexpand-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-interact-insert-as-comment
     (alchemist-server-prepare-filter-output alchemist-macroexpand-filter-output))
    (setq alchemist-macroexpand-filter-output nil)))

(defun alchemist-macroexpand-expand-request (expr)
  (let ((file (make-temp-file "alchemist-expand" nil ".exs")))
    (with-temp-file file
      (insert expr))
    (alchemist-server-eval (format "{ :expand, '%s' }" file) #'alchemist-macroexpand-filter)))

(defun alchemist-macroexpand-expand-and-print-request (expr)
  (let ((file (make-temp-file "alchemist-expand" nil ".exs")))
    (with-temp-file file
      (insert expr))
    (alchemist-server-eval (format "{ :expand, '%s' }" file) #'alchemist-macroexpand-insert-filter)))

(defun alchemist-macroexpand-expand-once-request (expr)
  (let ((file (make-temp-file "alchemist-expand-once" nil ".exs")))
    (with-temp-file file
      (insert expr))
    (alchemist-server-eval (format "{ :expand_once, '%s' }" file) #'alchemist-macroexpand-filter)))

(defun alchemist-macroexpand-expand-once-and-print-request (expr)
  (let ((file (make-temp-file "alchemist-expand-once" nil ".exs")))
    (with-temp-file file
      (insert expr))
    (alchemist-server-eval (format "{ :expand_once, '%s' }" file) #'alchemist-macroexpand-insert-filter)))

(defun alchemist-macroexpand-current-line ()
  "Macro expand the Elixir code on the current line."
  (interactive)
  (alchemist-macroexpand-expand-request (thing-at-point 'line)))

(defun alchemist-macroexpand-print-current-line ()
  "Macro expand the Elixir code on the current line and insert the result."
  (interactive)
  (alchemist-macroexpand-expand-and-print-request (thing-at-point 'line)))

(defun alchemist-macroexpand-once-current-line ()
  "Macro expand the Elixir code on the current line."
  (interactive)
  (alchemist-macroexpand-expand-once-request (thing-at-point 'line)))

(defun alchemist-macroexpand-once-print-current-line ()
  "Macro expand the Elixir code on the current line and insert the result."
  (interactive)
  (alchemist-macroexpand-expand-once-and-print-request (thing-at-point 'line)))

(defun alchemist-macroexpand-print-region (beg end)
  "Macro expand the Elixir code on marked region and insert the result."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (when (> end beg)
      (exchange-point-and-mark))
    (alchemist-macroexpand-expand-and-print-request string)))

(defun alchemist-macroexpand-region (beg end)
  "Macro expand the Elixir code on marked region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (alchemist-macroexpand-expand-request string)))

(defun alchemist-macroexpand-once-print-region (beg end)
  "Macro expand the Elixir code on marked region once and insert the result."
  (interactive "r")
  (let ((string (buffer-substring-no-properties beg end)))
    (when (> end beg)
      (exchange-point-and-mark))
    (alchemist-macroexpand-expand-once-and-print-request string)))

(defun alchemist-macroexpand-once-region (beg end)
  "Macro expand the Elixir code on marked region once."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((string (buffer-substring-no-properties beg end)))
    (alchemist-macroexpand-expand-once-request string)))

(defun alchemist-macroexpand-close-popup ()
  "Quit the macroexpand buffer window."
  (interactive)
  (quit-windows-on alchemist-macroexpand-buffer-name))

(defvar alchemist-macroexpand-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `alchemist-macroexpand-mode'.")

(define-minor-mode alchemist-macroexpand-mode
  "Minor mode for Alchemist Elixir macroexpand functionality.

\\{alchemist-macroexpand-mode-map}"
  nil
  alchemist-macroexpand-mode-map)

(provide 'alchemist-macroexpand)

;;; alchemist-macroexpand.el ends here
