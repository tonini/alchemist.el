;;; alchemist-project.el --- API to identify Elixir mix projects.

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

;; API to identify Elixir mix projects.

;;; Code:

(require 'cl)
(require 'json)

(defgroup alchemist-project nil
  "API to identify Elixir mix projects."
  :prefix "alchemist-help-"
  :group 'alchemist)

(defcustom alchemist-project-config-filename ".alchemist"
  "Name of the file which holds the Elixir project setup."
  :type 'string
  :group 'alchemist)

(defcustom alchemist-project-compile-when-needed nil
  "When `t', it compiles the Elixir project codebase when needed.

For example:
If documentation lookup or completion for code is made, it first tries to
compile the current Elixir project codebase. This makes sure that the
documentation and completion is always up to date with the codebase.

Please be aware that when the compilation fails, no documentation or
completion will be work.
"
  :type 'string
  :group 'alchemist)

(defun alchemist-project-toggle-compile-when-needed ()
  ""
  (interactive)
  (if alchemist-project-compile-when-needed
      (setq alchemist-project-compile-when-needed nil)
    (setq alchemist-project-compile-when-needed t))
  (if alchemist-project-compile-when-needed
      (message "Compilation of project when needed is enabled")
    (message "Compilation of project when needed is disabled")))

(defun alchemist-project--load-compile-when-needed-setting ()
  (let ((config (gethash "compile-when-needed" (alchemist-project-config))))
    (if config
        (intern config)
      alchemist-project-compile-when-needed)))

(defun alchemist-project--config-filepath ()
  "Return the path to the config file."
  (format "%s/%s"
          (alchemist-project-root)
          alchemist-project-config-filename))

(defun alchemist-project--config-exists-p ()
  "Check if project config file exists."
  (file-exists-p (alchemist-project--config-filepath)))

(defun alchemist-project-config ()
  "Return the current Elixir project configs."
  (let* ((json-object-type 'hash-table)
         (config (if (alchemist-project--config-exists-p)
                     (json-read-from-string
                      (with-temp-buffer
                        (insert-file-contents (alchemist-project--config-filepath))
                        (buffer-string)))
                   (make-hash-table :test 'equal))))
    config))

(defvar alchemist-project-root-indicators
  '("mix.exs")
  "list of file-/directory-names which indicate a root of a elixir project")

(defvar alchemist-project-deps-indicators
  '(".hex")
  "list of file-/directory-names which indicate a root of a elixir project")

(defun alchemist-project-p ()
  "Returns whether alchemist has access to a elixir project root or not"
  (stringp (alchemist-project-root)))

(defun alchemist-project-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun alchemist-project-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (alchemist-project-parent-directory a-directory)))

(defun alchemist-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a project root indicator."
  (let* ((directory (file-name-as-directory (or directory (expand-file-name default-directory))))
         (present-files (directory-files directory)))
    (cond ((alchemist-project-root-directory-p directory) nil)
          ((> (length (intersection present-files alchemist-project-deps-indicators :test 'string=)) 0)
           (alchemist-project-root (file-name-directory (directory-file-name directory))))
          ((> (length (intersection present-files alchemist-project-root-indicators :test 'string=)) 0) directory)
          (t (alchemist-project-root (file-name-directory (directory-file-name directory)))))))

(defun alchemist-project--establish-root-directory ()
  "Set the default-directory to the Elixir project root."
  (let ((project-root (alchemist-project-root)))
    (when project-root
      (setq default-directory project-root))))

(defun alchemist-project-open-tests-for-current-file ()
  "Opens the appropriate test file for the current buffer file
in a new window."
  (interactive)
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (if (file-exists-p filename)
        (find-file-other-window filename)
      (if (y-or-n-p "No test file found; create one now?")
          (alchemist-project--create-test-for-current-file
           filename (current-buffer))
        (message "No test file found.")))))

(defun alchemist-project--create-test-for-current-file (filename buffer)
  "Creates and populates a test module, FILENAME, for the code in BUFFER.
The module name given to the test module is determined from the name of the
first module defined in BUFFER."
  (let* ((directory-name (file-name-directory filename))
         (module-name (alchemist-project--grok-module-name buffer))
         (test-module-name (concat module-name "Test")))
    (unless (file-exists-p directory-name)
      (make-directory (file-name-directory filename) t))
    (alchemist-project--insert-test-boilerplate
     (find-file-other-window filename) test-module-name)))

(defun alchemist-project--grok-module-name (buffer)
  "Determines the name of the first module defined in BUFFER."
  (save-excursion
    (set-buffer buffer)
    (goto-line 1)
    (re-search-forward "defmodule\\s-\\(.+?\\)\\s-?,?\\s-do")
    (match-string 1)))

(defun alchemist-project--insert-test-boilerplate (buffer module)
  "Inserts ExUnit boilerplate for MODULE in BUFFER.
Point is left in a convenient location."
  (set-buffer buffer)
  (insert (concat "defmodule " module " do\n"
                  "  use ExUnit.Case\n"
                  "\n"
                  "end\n"))
  (goto-char (point-min))
  (beginning-of-line 3))

(defun alchemist-project-find-test ()
  "Open project test directory and list all test files."
  (interactive)
  (when (alchemist-project-p)
    (find-file (alchemist-project--open-directory-files "test"))))

(defun alchemist-project--open-directory-files (directory)
  (let ((directory (concat (replace-regexp-in-string "\/?$" "" (concat (alchemist-project-root) directory) "/"))))
    (message directory)
    (concat directory "/" (completing-read (concat directory ": ")
                                           (mapcar (lambda (path)
                                                     (replace-regexp-in-string (concat "^" (regexp-quote directory) "/") "" path))
                                                   (split-string
                                                    (shell-command-to-string
                                                     (concat
                                                      "find \"" directory
                                                      "\" -type f | grep \"_test\.exs\" | grep -v \"/.git/\" | grep -v \"/.yardoc/\""))))))))

(defun alchemist-project-name ()
  "Return the name of the current Elixir project."
  (if (alchemist-project-p)
      (car (cdr (reverse (split-string (alchemist-project-root) "/"))))
    ""))

(provide 'alchemist-project)

;;; alchemist-project.el ends here
