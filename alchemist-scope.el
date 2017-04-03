;;; alchemist-scope.el --- Provides information about Elixir source code context   -*- lexical-binding: t -*-

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

;; Provides information about the Elixir source code context.

;;; Code:

(require 'dash)
(require 's)

(defgroup alchemist-scope nil
  "Provides information about the Elixir source code context."
  :prefix "alchemist-scope-"
  :group 'alchemist)

(defconst alchemist-scope-defmodule-regex "defmodule \\([A-Z][A-Za-z0-9\._]+\\)\s+"
  "The regex for matching Elixir defmodule macro.")

(defconst alchemist-scope-alias-regex
  "^\s+alias\s+\\([-:_A-Za-z0-9,\.\?!\]+\\)\\(\s*,\s*as:\s*\\)?\\([-_A-Za-z0-9,\.\?!\]+\\)?\n"
  "The regex for matching Elixir alias definitions.
   Example:
   alias Phoenix.Router.Resource, as: Special")

(defconst alchemist-scope-alias-regex-two
  "^\s+alias\s+\\([-:_A-Za-z0-9,\.\?!\]+\\)\.{\\([-:_A-Za-z0-9\s,\.\?!\]+\\)}\n"
  "The regex for matching Elixir alias definitions.
   Example:
   alias List.Chars.{Atom, Float}")

(defconst alchemist-scope-use-regex
  "^\s+use\s+\\([A-Za-z0-9\.]+\\)"
  "The regex for matching Elixir use definitions.")

(defconst alchemist-scope-import-regex
  "^\s+import\s+\\([A-Za-z0-9\.]+\\)"
  "The regex for matching Elixir import definitions.")

(defun alchemist-scope-inside-string-p ()
  "Return non-nil if `point' is inside a string or heredoc."
  (let* ((pos (point))
         (parse-info (syntax-ppss pos)))
    (or (and (nth 3 parse-info)
             (nth 8 parse-info))
        (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")
             (match-beginning 0)))))

(defun alchemist-scope-inside-module-p ()
  "Return non-nil if `point' is currently inside a module."
  (save-excursion
    (end-of-line)
    (let ((found-flag-p nil)
          (found-p nil))
      (while (and (not found-flag-p)
                  (re-search-backward alchemist-scope-defmodule-regex nil t))
        (when (not (alchemist-scope-inside-string-p))
          (setq found-flag-p t)
          (setq found-p t)))
      found-p)))

(defun alchemist-scope-module ()
  "Return name from the current defmodule."
  (save-excursion
    (let ((found-flag-p nil)
          (module-name ""))
      (save-match-data
        (while (and (not found-flag-p)
                    (re-search-backward alchemist-scope-defmodule-regex nil t))
          (when (not (alchemist-scope-inside-string-p))
            (setq module-name (match-string 1))
            (setq found-flag-p t))
          (when (equal 1 (line-number-at-pos (point)))
            (setq found-flag-p t)))
        module-name))))

(defun alchemist-scope-aliases ()
  "Return aliases from the current module."
  (let* ((aliases '())
         (context (alchemist-scope-module)))
    (save-excursion
      (when (alchemist-scope-inside-module-p)
        (end-of-line)
        ;; alias definition like:
        ;;
        ;;   alias Phoenix.Router.Resource, as: Special
        (while (re-search-backward alchemist-scope-alias-regex nil t)
          (when (and
                 (not (alchemist-scope-inside-string-p))
                 (equal context (alchemist-scope-module)))
            (let* ((alias (match-string 1))
                   (as (if (match-string 3) (match-string 3) nil))
                   (as (if as as (car (last (split-string alias "\\."))))))
              (setq aliases (append aliases (list (list (alchemist-utils-remove-dot-at-the-end alias)
                                                        (alchemist-utils-remove-dot-at-the-end as))))))))))
    (save-excursion
      (when (alchemist-scope-inside-module-p)
        (end-of-line)
        ;; alias definition like:
        ;;
        ;;   alias List.Chars.{Atom, Float}
        (while (re-search-backward alchemist-scope-alias-regex-two nil t)
          (when (and
                 (not (alchemist-scope-inside-string-p))
                 (equal context (alchemist-scope-module)))
            (let* ((prefix (match-string 1))
                   (alias-collection (if (match-string 2) (split-string (match-string 2) ",") nil)))
              (-map (lambda (alias)
                      (let* ((alias (replace-regexp-in-string "\s+" "" alias))
                             (namespace (format "%s.%s" prefix alias)))
                        (setq aliases (append aliases (list (list (alchemist-utils-remove-dot-at-the-end namespace)
                                                                  (alchemist-utils-remove-dot-at-the-end alias)))))))
                    alias-collection))))))
    aliases))

(defun alchemist-scope--modules (regex)
  (let ((modules '())
        (context (alchemist-scope-module)))
    (save-excursion
      (when (not (s-blank? context))
        (while (re-search-backward regex nil t)
          (when (and (match-string 1)
                     (not (alchemist-scope-inside-string-p))
                     (equal context (alchemist-scope-module)))
            (cl-pushnew (substring-no-properties (match-string 1)) modules))))
      modules)))

(defun alchemist-scope-use-modules ()
  "Return `use' introduced module names from the current module."
  (alchemist-scope--modules alchemist-scope-use-regex))

(defun alchemist-scope-import-modules ()
  "Return `import' introduced module names from the current module."
  (alchemist-scope--modules alchemist-scope-import-regex))

(defun alchemist-scope-all-modules ()
  "Return `use' and `import' introduced modules from the current module."
  (let ((current (alchemist-scope-module))
        (use (alchemist-scope-use-modules))
        (import (alchemist-scope-import-modules))
        (modules '()))
    (push current modules)
    (push use modules)
    (push import modules)
    (-flatten modules)))

(defun alchemist-scope-extract-module (expr)
  "Extract module from EXPR."
  (let* ((parts (split-string expr "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z_\?!]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z_\?!]+" (car parts))
      (alchemist-utils-remove-dot-at-the-end (mapconcat 'concat parts ".")))))

(defun alchemist-scope-extract-function (expr)
  "Extract function from EXPR."
  (let* ((parts (split-string expr "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z_\?!]+" function))
      function)))

(defun alchemist-scope-alias-full-path (module)
  "Solve the full path for the MODULE alias."
  (if (not (s-blank? module))
      (let* ((aliases (-map (lambda (m)
                              (when (string-match-p (format "^%s" (car (cdr m))) module)
                                (replace-regexp-in-string (format "^%s" (car (cdr m))) (car m) module t)))
                            (alchemist-scope-aliases)))
             (aliases (delete nil aliases)))
        (if aliases
            (car aliases)
          module))))

(defun alchemist-scope-expression ()
  "Return the expression under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9.?!@:")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9.?!@:")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(provide 'alchemist-scope)

;;; alchemist-scope.el ends here
