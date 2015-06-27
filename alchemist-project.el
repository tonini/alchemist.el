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

(defgroup alchemist-project nil
  "API to identify Elixir mix projects."
  :prefix "alchemist-help-"
  :group 'alchemist)

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

(defun alchemist-project-toggle-file-and-tests-other-window ()
  "Toggle between a file and its tests in other window."
  (interactive)
  (if (alchemist-utils--is-test-file-p)
      (alchemist--project-open-file-for-current-tests 'find-file-other-window)
    (alchemist--project-open-tests-for-current-file 'find-file-other-window)))

(defun alchemist-project-toggle-file-and-tests ()
  "Toggle between a file and its tests in the current window."
  (interactive)
  (if (alchemist-utils--is-test-file-p)
      (alchemist--project-open-file-for-current-tests 'find-file)
    (alchemist--project-open-tests-for-current-file 'find-file)))

(defun alchemist--project-open-file-for-current-tests (toggler)
  "Open the appropriate implementation file for the current buffer by calling TOGGLER with filename."
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^test/" "lib/" filename))
         (filename (replace-regexp-in-string "_test\.exs$" "\.ex" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (funcall toggler filename)))

(defun alchemist--project-open-tests-for-current-file (toggler)
  "Opens the appropriate test file by calling TOGGLER with filename."
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (if (file-exists-p filename)
        (funcall toggler filename)
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

(defun alchemist-project--underscore-to-camelcase (string)
  "Convert an underscore_string to a CamelCase string."
  (mapconcat 'capitalize (split-string string "_") ""))

(defun alchemist-project--path-to-module-name (path)
  "Convert a `path' like my_lib/foo.ex to a module name like MyLib.Foo."
  (let* ((path (split-string path "/"))
         (path (mapcar (lambda (el) (replace-regexp-in-string "\.ex$" "" el)) path)))
    (mapconcat 'alchemist-project--underscore-to-camelcase path ".")))

(defun alchemist-project-create-file (relative-path)
  "Create a file under lib/ in the current project.

The newly created buffer is filled with a module definition based on the file name."
  (interactive "sFile to create in lib/: ")
  (let ((root (alchemist-project-root)))
    (when root
      (let* ((module-name (alchemist-project--path-to-module-name relative-path))
             (abs-path (concat root "lib/" relative-path))
             (abs-path (if (string-match-p "\.ex$" abs-path)
                           abs-path
                         (concat abs-path ".ex"))))
        (make-directory (file-name-directory abs-path) t)
        (find-file abs-path)
        (insert (concat "defmodule " module-name " do\n"
                        "  \n"
                        "end\n"))
        (goto-char (point-min))
        (beginning-of-line 2)
        (back-to-indentation)))))

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
                                                      "\" -type f | grep \"_test\.exs\" | grep -v \"/.git/\""))))))))

(defun alchemist-project-name ()
  "Return the name of the current Elixir project."
  (if (alchemist-project-p)
      (car (cdr (reverse (split-string (alchemist-project-root) "/"))))
    ""))

(provide 'alchemist-project)

;;; alchemist-project.el ends here
