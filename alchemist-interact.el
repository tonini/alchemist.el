;;; alchemist-interact.el --- Interaction interface -*- lexical-binding: t -*-

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

;; Interaction interface.

;;; Code:

(defgroup alchemist-interact nil
  "Interaction interface."
  :prefix "alchemist-interact-"
  :group 'alchemist)

(defun alchemist-interact-insert-as-comment (string)
  "Insert STRING at point as comment."
  (let ((lines (split-string string "\n")))
    (if (> (length lines) 1)
        (save-excursion
          (end-of-line)
          (mapc (lambda (s)
                  (newline)
                  (insert (format "# => %s" s))
                  (indent-according-to-mode))
                lines))
      (save-excursion
        (end-of-line)
        (insert (format "  # => %s" string))))))

(defun alchemist-interact-create-popup (name content mode)
  "Create a NAME buffer and insert CONTENT.
Call the MODE afterwards."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (with-current-buffer-window
       buffer (cons 'display-buffer-below-selected
                    '((window-height . fit-window-to-buffer)))
       (lambda (_window _buffer))
       (let ((inhibit-read-only t))
         (insert content)
         (goto-char (point-min))
         (funcall mode))))))

(provide 'alchemist-interact)

;;; alchemist-interact.el ends here
