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

(require 'cl-lib)
(require 'etags)
(require 'dash)
(require 's)
(require 'alchemist-utils)
(require 'alchemist-server)
(require 'alchemist-scope)

(defgroup alchemist-goto nil
  "Functionality to jump modules and function definitions."
  :prefix "alchemist-goto-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-goto-erlang-source-dir ""
  "Path to the erlang source code."
  :type 'string
  :group 'alchemist-goto)

(defcustom alchemist-goto-elixir-source-dir ""
  "Path to the elixir source code."
  :type 'string
  :group 'alchemist-goto)

(defvar alchemist-goto--symbol-list '())
(defvar alchemist-goto--symbol-name-and-pos '())
(defvar alchemist-goto--symbol-list-bare '())
(defvar alchemist-goto--symbol-name-and-pos-bare '())
(defvar alchemist-goto-filter-output nil)
(defvar alchemist-goto-callback nil)

(defconst alchemist-goto--symbol-def-extract-regex
  "^\\s-*\\(defp?\\|defmacrop?\\|defmodule\\|defimpl\\)[ \n\t]+\\([a-z_\?!]+\\)\\(.*\\)\\(do\\|\n\\)?$")

(defconst alchemist-goto--symbol-def-regex
  "^[[:space:]]*\\(defmodule\\|defmacrop?\\|defimpl\\|defp?\\)")

;; Faces

(defface alchemist-goto--def-face
  '((t (:inherit font-lock-constant-face)))
  "Face for def* symbols."
  :group 'alchemist-goto)

(defface alchemist-goto--name-face
  '((t (:bold t)))
  "Face for def* symbol names."
  :group 'alchemist-goto)

;; Private functions

(defun alchemist-goto--build-elixir-ex-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\)\\/.+\.ex$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (alchemist-utils-add-trailing-slash
                              (expand-file-name alchemist-goto-elixir-source-dir))))
      (concat source-directory file))))

(defun alchemist-goto--build-elixir-erl-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (alchemist-utils-add-trailing-slash
                              (expand-file-name alchemist-goto-elixir-source-dir))))
      (concat source-directory file))))

(defun alchemist-goto--build-erlang-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (alchemist-utils-add-trailing-slash
                              (expand-file-name alchemist-goto-erlang-source-dir))))
      (concat source-directory file))))

(defun alchemist-goto-elixir-file-p (file)
  "Return non-nil if FILE is an Elixir file type."
  (string-match-p "\\.ex\\(s\\)?$" file))

(defun alchemist-goto-erlang-file-p (file)
  "Return non-nil if FILE is an Erlang file type."
  (string-match-p "\\.erl$" file))

(defun alchemist-goto--symbol-definition-p (symbol)
  (alchemist-goto--fetch-symbol-definitions)
  (if (member symbol alchemist-goto--symbol-list-bare)
      t
    nil))

(defun alchemist-goto--fetch-symbols-from-propertize-list (symbol)
  (-remove 'null (-map (lambda (e)
                         (when (string-match-p (format "^\\s-*\\(defp?\\|defmacrop?\\|defimpl\\|defmodule\\)\s+%s\\((.*\\)?$" symbol) e)
                           e)) alchemist-goto--symbol-list)))

(defun alchemist-goto--goto-symbol (symbol)
  (let ((amount (length (-remove 'null (-map (lambda (e) (when (string= symbol e) e))
                                             alchemist-goto--symbol-list-bare)))))
    (if (> amount 1)
        (let* ((selected-def (completing-read "Symbol definitions:"
                                              (alchemist-goto--fetch-symbols-from-propertize-list symbol)))
               (position (cdr (assoc selected-def alchemist-goto--symbol-name-and-pos))))
          (goto-char (if (overlayp position) (overlay-start position) position)))
      (let* ((position (cdr (assoc symbol alchemist-goto--symbol-name-and-pos-bare)))
             (position (if (overlayp position) (overlay-start position) position)))
        (when (not (equal (line-number-at-pos)
			  (line-number-at-pos position)))
          (goto-char position))))))

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

(defun alchemist-goto--extract-symbol (str)
  (save-match-data
    (when (string-match alchemist-goto--symbol-def-extract-regex str)
      (let ((type (substring str (match-beginning 1) (match-end 1)))
            (name (substring str (match-beginning 2) (match-end 2)))
            (arguments (substring str (match-beginning 3) (match-end 3))))
        (concat
         (propertize type
                     'face 'alchemist-goto--def-face)
         " "
         (propertize name
                     'face 'alchemist-goto--name-face)
         (replace-regexp-in-string ",?\s+do:.*$" "" (replace-regexp-in-string "\s+do$" "" arguments)))))))

(defun alchemist-goto--file-contains-defs-p ()
  (alchemist-utils-occur-in-buffer-p (current-buffer) alchemist-goto--symbol-def-extract-regex))

(defun alchemist-goto-jump-to-next-def-symbol ()
  (interactive)
  (alchemist-utils-jump-to-next-matching-line alchemist-goto--symbol-def-regex 'back-to-indentation))

(defun alchemist-goto-jump-to-previous-def-symbol ()
  (interactive)
  (alchemist-utils-jump-to-previous-matching-line alchemist-goto--symbol-def-regex 'back-to-indentation))

(defun alchemist-goto--extract-symbol-bare (str)
  (save-match-data
    (when (string-match alchemist-goto--symbol-def-extract-regex str)
      (let ((name (substring str (match-beginning 2) (match-end 2))))
        name))))

(defun alchemist-goto--get-symbol-from-position (position)
  (goto-char position)
  (end-of-line)
  (let* ((end-position (point))
         (line (buffer-substring-no-properties position end-position)))
    (alchemist-goto--extract-symbol line)))

(defun alchemist-goto--get-symbol-from-position-bare (position)
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char position)
      (end-of-line)
      (let* ((end-position (point))
             (line (buffer-substring-no-properties position end-position)))
        (alchemist-goto--extract-symbol-bare line)))))

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

(defun alchemist-goto--open-definition (expr)
  (let* ((module (alchemist-scope-extract-module expr))
         (aliases (alchemist-utils-prepare-aliases-for-elixir
                   (alchemist-scope-aliases)))
         (function (alchemist-scope-extract-function expr))
         (modules (alchemist-utils-prepare-modules-for-elixir
                   (alchemist-scope-all-modules))))
    (ring-insert find-tag-marker-ring (point-marker))
    (cond
     ((and (null module)
           (alchemist-goto--symbol-definition-p function)
           (not (string= (buffer-name) alchemist-help-buffer-name)))
      (alchemist-goto--goto-symbol function))
     (t
      (setq alchemist-goto-callback (lambda (file)
				      (cond ((s-blank? file)
                                             (message "Don't know how to find: %s" expr))
                                            ((file-exists-p file)
					     (alchemist-goto--open-file file module function))
                                            ((alchemist-goto-elixir-file-p file)
					     (let* ((elixir-source-file (alchemist-goto--build-elixir-ex-core-file file)))
					       (if (and elixir-source-file (file-exists-p elixir-source-file))
                                                   (alchemist-goto--open-file elixir-source-file module function)
                                                 (message "Don't know how to find: %s" expr))))
                                            ((alchemist-goto-erlang-file-p file)
                                             (let* ((elixir-source-file (alchemist-goto--build-elixir-erl-core-file file))
                                                    (erlang-source-file (alchemist-goto--build-erlang-core-file file)))
                                               (cond ((file-exists-p elixir-source-file)
                                                      (alchemist-goto--open-file elixir-source-file module function))
                                                     ((file-exists-p erlang-source-file)
                                                      (alchemist-goto--open-file erlang-source-file module function))
                                                     (t
                                                      (message "Don't know how to find: %s" expr)))))
                                            (t
                                             (pop-tag-mark)
                                             (message "Don't know how to find: %s" expr)))))
      (alchemist-server-goto (format "{ \"%s,%s\", [ context: Elixir, imports: %s, aliases: %s ] }" module function modules aliases)
                             #'alchemist-goto-filter)))))

(defun alchemist-goto--open-file (file module function)
  (let ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (cond ((alchemist-goto-elixir-file-p file)
           (alchemist-goto--jump-to-elixir-source module function))
          ((alchemist-goto-erlang-file-p file)
           (alchemist-goto--jump-to-erlang-source module function)))))

(defun alchemist-goto--jump-to-elixir-source (module function)
  (cond
   (function
    (alchemist-goto--fetch-symbol-definitions)
    (alchemist-goto--goto-symbol function))
   (t
    (when (re-search-forward (format "\\(defmodule\\|defimpl\\|defprotocol\\)\s+%s\s+do" module) nil t)
      (goto-char (match-beginning 0))))))

(defun alchemist-goto--jump-to-erlang-source (module function)
  (when (re-search-forward (format "\\(^%s\(\\)" function) nil t)
    (goto-char (match-beginning 0)))
  (when (re-search-forward (format "\\(^-module\(%s\)\\)" (substring module 1)) nil t)
    (goto-char (match-beginning 0))))

(defun alchemist-goto-filter (_process output)
  (with-local-quit
    (setq alchemist-goto-filter-output (cons output alchemist-goto-filter-output))
    (when (alchemist-server-contains-end-marker-p output)
      (let* ((output (alchemist-server-prepare-filter-output alchemist-goto-filter-output))
	     (file output))
	(setq alchemist-goto-filter-output nil)
	(funcall alchemist-goto-callback file)))))

;; Public functions

(defun alchemist-goto-definition-at-point ()
  "Jump to the elixir expression definition at point."
  (interactive)
  (alchemist-goto--open-definition (alchemist-scope-expression)))

(defalias 'alchemist-goto-jump-back 'pop-tag-mark)

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
