;;; alchemist-goto.el --- Functionality to jump modules and function definitions -*- lexical-binding: t -*-

;; Copyright © 2015 Samuel Tonini

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

;; Functionality to jump modules and function definitions

;;; Code:

(require 'xref)
(require 'etags)

(defvar alchemist-goto--symbol-list '())
(defvar alchemist-goto--symbol-name-and-pos '())

(defalias 'alchemist-goto-definition-at-point '(lambda ()
                                                 (interactive)
                                                 (xref-find-definitions (alchemist-scope-expression))))
(defalias 'alchemist-goto-jump-back 'pop-tag-mark)

(defun alchemist-goto-list-symbol-definitions ()
  "List all symbol definitions in the current file like functions/macros/modules.

It will jump to the position of the symbol definition after selection."
  (interactive)
  (alchemist-goto--fetch-symbol-definitions)
  (ring-insert find-tag-marker-ring (point-marker))
  (let* ((selected-def (completing-read "Symbol definitions:" alchemist-goto--symbol-list))
         (position (cdr (assoc selected-def alchemist-goto--symbol-name-and-pos))))
    (goto-char (if (overlayp position) (overlay-start position) position))))

(defun alchemist-goto--fetch-symbol-definitions ()
  (alchemist-goto--search-for-symbols "^\\s-*\\(defp?\\|defmacrop?\\|defimpl\\|defstruct\\|defmodule\\)\s.*"))

(defun alchemist-goto--search-for-symbols (regex)
  (setq alchemist-goto--symbol-list '())
  (setq alchemist-goto--symbol-name-and-pos '())
  (setq alchemist-goto--symbol-list-bare '())
  (setq alchemist-goto--symbol-name-and-pos-bare '())
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex nil t)
          (when (not (alchemist-scope-inside-string-p))
            (when (alchemist-goto--get-symbol-from-position (car (match-data)))
              (let* ((position (car (match-data)))
                     (symbol (alchemist-goto--get-symbol-from-position position))
                     (symbol-bare (alchemist-goto--get-symbol-from-position-bare position)))
                (setq alchemist-goto--symbol-list (append alchemist-goto--symbol-list (list symbol)))
                (setq alchemist-goto--symbol-name-and-pos (append alchemist-goto--symbol-name-and-pos (list (cons symbol position))))
                (setq alchemist-goto--symbol-list-bare (append alchemist-goto--symbol-list-bare (list symbol-bare)))
                (setq alchemist-goto--symbol-name-and-pos-bare (append alchemist-goto--symbol-name-and-pos-bare (list (cons symbol-bare position))))))))))))

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
