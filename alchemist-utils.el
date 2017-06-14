;;; alchemist-utils.el --- Common utility functions that don't belong anywhere else -*- lexical-binding: t -*-

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

;; Common utility functions that don't belong anywhere else

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defun alchemist-utils-build-command (command-list)
  "Build the commands list for the runner."
  (let* ((command-list (-flatten (if (stringp command-list)
                                     (split-string command-list)
                                   command-list)))
         (command (-remove (lambda (e) (equal e "")) command-list)))
    (mapconcat 'concat command " ")))

(defun alchemist-utils-count-char-occurence (regexp str)
  "Count occurrence of char with REGEXP inside STR."
  (cl-loop with start = 0
           for count from 0
           while (string-match regexp str start)
           do (setq start (match-end 0))
           finally return count))

(defun alchemist-utils-test-file-p ()
  "Return non-nil `current-buffer' holds an Elixir test file."
  (string-match "_test\\.exs$" (or (buffer-file-name) "")))

(defun alchemist-utils-remove-dot-at-the-end (string)
  "Remove dot character at the end of STRING."
  (replace-regexp-in-string "\\.$" "" string))

(defun alchemist-utils-prepare-aliases-for-elixir (aliases)
  (let* ((aliases (-map (lambda (a)
                            (let ((module (alchemist-utils-remove-dot-at-the-end (car a)))
                                  (alias (alchemist-utils-remove-dot-at-the-end (car (cdr a)))))
                            (if (not (or (s-blank? alias)
                                         (string= alias module)))
                                (format "{%s, %s}"
                                        (if (s-blank? alias)
                                            module
                                          alias)
                                        module)))) aliases))
         (aliases (mapconcat #'identity aliases ",")))
    (format "[%s]" aliases)))

(defun alchemist-utils-prepare-modules-for-elixir (modules)
  (let* ((modules (mapconcat #'identity modules ",")))
    (format "[%s]" modules)))

(defun alchemist-utils--snakecase-to-camelcase (str)
  "Convert a snake_case string STR to a CamelCase string.

This function is useful for converting file names like my_module to Elixir
module names (MyModule)."
  (mapconcat 'capitalize (split-string str "_") ""))

(defun alchemist-utils-add-ext-to-path-if-not-present (path ext)
  "Add EXT to PATH if PATH doesn't already ends with EXT."
  (if (string-suffix-p ext path)
      path
    (concat path ext)))

(defun alchemist-utils-path-to-module-name (path)
  "Convert PATH to its Elixir module name equivalent.

For example, convert 'my_app/my_module.ex' to 'MyApp.MyModule'."
  (let* ((path (file-name-sans-extension path))
         (path (split-string path "/"))
         (path (-remove (lambda (str) (equal str "")) path)))
    (mapconcat #'alchemist-utils--snakecase-to-camelcase path ".")))

(defun alchemist-utils-add-trailing-slash (path)
  "Add trailing slash to PATH if not already contain."
  (if (not (string-match-p "/$" path))
      (format "%s/" path)
    path))

(defun alchemist-utils-occur-in-buffer-p (buffer regex)
  "Return non-nil if BUFFER contains at least one occurrence of REGEX."
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (re-search-forward regex nil t)))))

(defun alchemist-utils-jump-to-regex (regex before-fn after-fn search-fn reset-fn)
  "Jump to REGEX using SEARCH-FN to search for it.
A common use case would be to use `re-search-forward' as the SEARCH-FN.
Call RESET-FN if the regex isn't found at the first try. BEFORE-FN is called
before performing the search while AFTER-FN after."
  (when (alchemist-utils-occur-in-buffer-p (current-buffer) regex)
    (save-match-data
      (funcall before-fn)
      (unless (funcall search-fn regex nil t)
        (funcall reset-fn)
        (funcall search-fn regex nil t))
      (funcall after-fn))))

(defun alchemist-utils-jump-to-next-matching-line (regex after-fn)
  "Jump to the next line matching REGEX.
Call AFTER-FN after performing the search."
  (alchemist-utils-jump-to-regex regex 'end-of-line after-fn 're-search-forward 'beginning-of-buffer))

(defun alchemist-utils-jump-to-previous-matching-line (regex after-fn)
  "Jump to the previous line matching REGEX.

Call AFTER-FN after performing the search."
  (alchemist-utils-jump-to-regex regex 'beginning-of-line after-fn 're-search-backward 'end-of-buffer))

(defun alchemist-utils-elixir-version ()
  "Return the current Elixir version on the system."
  (let* ((output (shell-command-to-string (format "%s --version" alchemist-execute-command)))
         (output (split-string output "\n"))
         (output (-remove (lambda (string) (s-blank? string))
                          output))
         (version (-last-item output))
         (version (replace-regexp-in-string "Elixir " "" version)))
    version))

(defun alchemist-utils-elixir-version-check-p (major minor tiny &optional version)
  "Returns t if the current elixir version is greater than or equal to the supplied version"
  (let* ((version-string (or version (alchemist-utils-elixir-version)))
         (version-list (split-string version-string "\\."))
         (current-major (string-to-number (car version-list)))
         (current-minor (string-to-number (car (cdr version-list))))
         (current-tiny (string-to-number (car (cdr (cdr version-list))))))
    (if (> current-major major)
        t
      (if (and (= current-major major) (> current-minor minor))
          t
        (and (= current-minor minor) (>= current-tiny tiny))))))

(provide 'alchemist-utils)

;;; alchemist-utils.el ends here
