;;; alchemist-goto.el --- Functionality to jump modules and function definitions

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

(require 'etags)

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

;; Private functions

(defun alchemist-goto--current-module-name ()
  "Searches backward in the current buffer until a module
declaration has been found."
  (save-excursion
    (let ((found-flag-p nil)
          (module-name ""))
      (save-match-data
        (while (and (not found-flag-p)
                    (re-search-backward "defmodule \\([A-Za-z\._]+\\)\s+" nil t))
          (when (not (alchemist-goto--string-at-point-p))
            (setq module-name (match-string 1))
            (setq found-flag-p t))
          (when (equal 1 (line-number-at-pos (point)))
            (setq found-flag-p t)))
        module-name))))

(defun alchemist-goto--use-modules-in-the-current-module-context ()
  (let ((modules '())
        (context (alchemist-goto--current-module-name)))
    (save-excursion
      (while (re-search-backward "^\s+use\s+\\([A-Za-z0-9\.]+\\)" nil t)
        (if (and (match-string 1)
                 (not (alchemist-goto--string-at-point-p))
                 (equal context (alchemist-goto--current-module-name)))
            (setq modules (add-to-list 'modules (substring-no-properties (match-string 1))))
            ))
      modules)))

(defun alchemist-goto--import-modules-in-the-current-module-context ()
  (let ((modules '())
        (context (alchemist-goto--current-module-name)))
    (save-excursion
      (while (re-search-backward "^\s+import\s+\\([A-Za-z0-9\.]+\\)" nil t)
        (if (and (match-string 1)
                 (not (alchemist-goto--string-at-point-p))
                 (equal context (alchemist-goto--current-module-name)))
            (setq modules (add-to-list 'modules (substring-no-properties (match-string 1))))
            ))
    modules)))

(defun alchemist-goto--extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z_\?!]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z_\?!]+" (car parts))
      (replace-regexp-in-string "\\.$" "" (mapconcat 'concat parts ".")))))

(defun alchemist-goto--extract-function (code)
  "Extract function from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z_\?!]+" function))
      function)))

(defun alchemist-goto--build-elixir-ex-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/lib\\)\\/.+\.ex$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-elixir-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--build-elixir-erl-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-elixir-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--build-erlang-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-erlang-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--elixir-file-p (file)
  (string-match-p  "\\.ex\\(s\\)?$" file))

(defun alchemist-goto--erlang-file-p (file)
  (string-match-p  "\\.erl$" file))

(defun alchemist-goto--get-full-path-of-alias (module)
  (if (not (alchemist-utils--empty-string-p module))
      (let* ((aliases (mapcar (lambda (m)
                                (when (string-match-p (format "^%s" (car (cdr m))) module)
                                  (replace-regexp-in-string (format "^%s" (car (cdr m))) (car m) module)))
                              (alchemist-goto--alises-of-current-buffer)))
             (aliases (delete nil aliases)))
        (if aliases
            (car aliases)
          module))))

(defun alchemist-goto--string-at-point-p (&optional complete)
  "Return non-nil if cursor is at a string."
  (save-excursion
  (or (and (nth 3 (save-excursion
                    (let ((pos (point)))
                      (when complete
                        (end-of-buffer))
                      (parse-partial-sexp 1 pos))))
           (nth 8 (save-excursion
                    (let ((pos (point)))
                      (when complete
                        (end-of-buffer))
                      (parse-partial-sexp 1 pos)))))
      (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")
           (match-beginning 0)))))

(defun alchemist-goto--symbol-definition-p (symbol)
  (alchemist-goto--fetch-symbol-definitions)
  (if (member symbol alchemist-goto--symbol-list-bare)
      t
    nil))

(defun alchemist-goto--goto-symbol (symbol)
  (let ((position (cdr (assoc symbol alchemist-goto--symbol-name-and-pos-bare))))
    (goto-char (if (overlayp position) (overlay-start position) position))))

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
  (alchemist-goto--search-for-symbols "^\\s-*\\(defp?\\|defmacrop?\\|defmodule\\)\s.*"))

(defface alchemist-goto--def-face
  '((t (:inherit font-lock-constant-face)))
  ""
  :group 'alchemist-goto)

(defface alchemist-goto--name-face
  '((t (:bold t)))
  ""
  :group 'alchemist-goto)

(defvar alchemist-goto--symbol-def-extract-regex
  "^\\s-*\\(defp?\\|defmacrop?\\|defmodule\\)[ \n\t]+\\([a-z_\?!]+\\)\\(.*\\)\\(do\\|\n\\)?$")

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
         (replace-regexp-in-string " do:.*$" "" (replace-regexp-in-string " do$" "" arguments)))))))

(defun alchemist-goto--extract-symbol-bare (str)
  (save-match-data
    (when (string-match alchemist-goto--symbol-def-extract-regex str)
      (let ((type (substring str (match-beginning 1) (match-end 1)))
            (name (substring str (match-beginning 2) (match-end 2)))
            (arguments (substring str (match-beginning 3) (match-end 3))))
        name))))

(defun alchemist-goto--get-symbol-from-position (position)
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char position)
      (end-of-line)
      (let* ((end-position (point))
             (line (buffer-substring-no-properties position end-position)))
        (alchemist-goto--extract-symbol line)))))

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
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char (point-max))
      (goto-char (point-min))
      (let ()
        (save-match-data
          (while (re-search-forward regex nil t)
            (when (not (alchemist-goto--string-at-point-p t))
              (when (alchemist-goto--get-symbol-from-position (car (match-data)))
                (let* ((position (car (match-data)))
                       (symbol (alchemist-goto--get-symbol-from-position position))
                       (symbol-bare (alchemist-goto--get-symbol-from-position-bare position)))
                  (setq alchemist-goto--symbol-list (append alchemist-goto--symbol-list (list symbol)))
                  (setq alchemist-goto--symbol-name-and-pos (append alchemist-goto--symbol-name-and-pos (list (cons symbol position))))
                  (setq alchemist-goto--symbol-list-bare (append alchemist-goto--symbol-list-bare (list symbol-bare)))
                  (setq alchemist-goto--symbol-name-and-pos-bare (append alchemist-goto--symbol-name-and-pos-bare (list (cons symbol-bare position)))))))))))))

(defun alchemist-goto--open-definition (expr)
  (let* ((module (alchemist-goto--extract-module expr))
         (module (alchemist-goto--get-full-path-of-alias module))
         (module (if module module "nil"))
         (function (alchemist-goto--extract-function expr))
         (function (if function function "\"\"")))
    (ring-insert find-tag-marker-ring (point-marker))
    (cond
     ((and (string-equal module "nil")
           (string-equal major-mode "elixir-mode")
           (alchemist-goto--symbol-definition-p function))
      (alchemist-goto--goto-symbol function))
     (t (alchemist-server-goto module function expr)
        ))))

(defun alchemist-goto--open-file (file module function)
  (let* ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (beginning-of-buffer)
    (cond ((alchemist-goto--elixir-file-p file)
           (alchemist-goto--jump-to-elixir-source module function))
          ((alchemist-goto--erlang-file-p file)
           (alchemist-goto--jump-to-erlang-source module function)))))

(defun alchemist-gogo--symbol-definition-regex (symbol)
  (format "^\s+\\(defp?\s+%s\(?\\|defmacrop?\s+%s\(?\\)" symbol symbol))

(defun alchemist-goto--jump-to-elixir-source (module function)
  (let ((function (replace-regexp-in-string "\?" "\\?" function)))
    (when (re-search-forward (alchemist-gogo--symbol-definition-regex function) nil t)
      (goto-char (match-beginning 0)))
    (when (re-search-forward (format "\\(defmodule\\|defimpl\\|defprotocol\\)\s+%s\s+do" module) nil t)
      (goto-char (match-beginning 0)))))

(defun alchemist-goto--jump-to-erlang-source (module function)
  (when (re-search-forward (format "\\(^%s\(\\)" function) nil t)
    (goto-char (match-beginning 0)))
  (when (re-search-forward (format "\\(^-module\(%s\)\\)" (substring module 1)) nil t)
    (goto-char (match-beginning 0))))

(defun alchemist-goto--context-exists-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "defmodule \\([A-Za-z\._]+\\)\s+" nil t)
        t
      nil)))

(defun alchemist-goto--alises-of-current-buffer ()
  (let* ((aliases '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\s+alias\s+\\([-:_A-Za-z0-9,\.\?!\]+\\)\\(\s*,\s*as:\s*\\)?\\([-_A-Za-z0-9,\.\?!\]+\\)?\n" nil t)
        (let* ((alias (match-string 1))
               (as (if (match-string 3) (match-string 3) nil))
               (as (if as as (car (last (split-string alias "\\."))))))
          (setq aliases (append aliases (list (list alias as)))))))
    aliases))

;; Public functions

(defun alchemist-goto-definition-at-point ()
  "Jump to the elixir expression definition at point."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "-_A-Za-z0-9.?!:")
    (setq p1 (point))
    (skip-chars-forward "-_A-Za-z0-9.?!:")
    (setq p2 (point))
    (alchemist-goto--open-definition (buffer-substring-no-properties p1 p2))))

(defalias 'alchemist-goto-jump-back 'pop-tag-mark)

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
