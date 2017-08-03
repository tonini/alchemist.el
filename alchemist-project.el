;;; alchemist-project.el --- API to identify Elixir mix projects.

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

;; API to identify Elixir mix projects.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'alchemist-utils)
(require 'alchemist-file)

;; Tell the byte compiler about autoloaded functions from packages
(eval-when-compile
  (declare-function alchemist-goto-elixir-source-dir ""))

(defgroup alchemist-project nil
  "API to identify Elixir mix projects."
  :prefix "alchemist-help-"
  :group 'alchemist)

(defconst alchemist-project-mix-project-indicator "mix.exs"
  "File which indicates the root directory of an Elixir Mix project.")

(defconst alchemist-project-hex-pkg-indicator ".hex"
  "File which indicates the root directory of an Elixir Hex package.")

(defvar alchemist-project-root-path-cache nil
  "Variable which holds the cached project root path.")

(defun alchemist-project-elixir-p ()
  "Return non-nil if `default-directory' is inside the Elixir source codebase."
  (stringp (alchemist-project-elixir-root)))

(defun alchemist-project-elixir-root (&optional dir)
  "Return root directory of the Elixir source."
  (if (and (not (s-blank? alchemist-goto-elixir-source-dir))
	   (string-prefix-p (expand-file-name alchemist-goto-elixir-source-dir)
			    (expand-file-name default-directory)))
      alchemist-goto-elixir-source-dir
    nil))

(defun alchemist-project-p ()
  "Return non-nil if `default-directory' is inside an Elixir Mix project."
  (stringp (alchemist-project-root)))

(defun alchemist-project-top-level-dir-p (dir)
  "Return non-nil if DIR is the top level directory."
  (equal dir (file-name-directory (directory-file-name dir))))

(defun alchemist-project-root (&optional dir)
  "Return root directory of the current Elixir Mix project.

It starts walking the directory tree to find the Elixir Mix root directory
from `default-directory'. If DIR is non-nil it starts walking the
directory from there instead."
  (if (and alchemist-project-root-path-cache
	   (string-prefix-p alchemist-project-root-path-cache
			    (expand-file-name default-directory)))
      alchemist-project-root-path-cache
    (let* ((dir (file-name-as-directory (or dir (expand-file-name default-directory))))
	   (present-files (directory-files dir)))
      (cond ((alchemist-project-top-level-dir-p dir)
	     nil)
	    ((-contains-p present-files alchemist-project-hex-pkg-indicator)
	     (alchemist-project-root (file-name-directory (directory-file-name dir))))
	    ((-contains-p present-files alchemist-project-mix-project-indicator)
	     (setq alchemist-project-root-path-cache dir)
	     dir)
	    (t
	     (alchemist-project-root (file-name-directory (directory-file-name dir))))))))

(defun alchemist-project-root-or-default-dir ()
  "Return the current Elixir mix project root or `default-directory'."
  (let* ((project-root (alchemist-project-root))
         (dir (if project-root
                  project-root
                default-directory)))
    dir))

(defun alchemist-project-toggle-file-and-tests-other-window ()
  "Toggle between a file and its tests in other window."
  (interactive)
  (if (alchemist-utils-test-file-p)
      (alchemist-project-open-file-for-current-tests 'find-file-other-window)
    (alchemist-project-open-tests-for-current-file 'find-file-other-window)))

(defun alchemist-project-toggle-file-and-tests ()
  "Toggle between a file and its tests in the current window."
  (interactive)
  (if (alchemist-utils-test-file-p)
      (alchemist-project-open-file-for-current-tests 'find-file)
    (alchemist-project-open-tests-for-current-file 'find-file)))

(defun alchemist-project-file-under-test (file directory)
  "Return the file which are tested by FILE.
DIRECTORY is the place where the file under test is located."
  (let* ((filename (file-relative-name file (alchemist-project-root)))
         (filename (replace-regexp-in-string "^test" directory filename))
         (filename (replace-regexp-in-string "_test\.exs$" "\.ex" filename)))
    (concat (alchemist-project-root) filename)))

(defun alchemist-project-open-file-for-current-tests (opener)
  "Visit the implementation file for the current buffer with OPENER."
  (let* ((filename (alchemist-project-file-under-test (buffer-file-name) "web"))
         (filename (if (file-exists-p filename)
                       filename
                     (alchemist-project-file-under-test (buffer-file-name) "lib"))))
    (funcall opener filename)))

(defun alchemist-project-open-tests-for-current-file (opener)
  "Visit the test file for the current buffer with OPENER."
  (let* ((filename (file-relative-name (buffer-file-name) (alchemist-project-root)))
         (filename (replace-regexp-in-string "^lib/" "test/" filename))
         (filename (replace-regexp-in-string "^apps/\\(.*\\)/lib/" "apps/\\1/test/" filename))
         (filename (replace-regexp-in-string "^web/" "test/" filename))
         (filename (replace-regexp-in-string "^apps/\\(.*\\)/web/" "apps/\\1/test/" filename))
         (filename (replace-regexp-in-string "\.ex$" "_test\.exs" filename))
         (filename (format "%s/%s" (alchemist-project-root) filename)))
    (if (file-exists-p filename)
        (funcall opener filename)
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
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "defmodule\\s-\\(.+?\\)\\s-?,?\\s-do")
      (match-string 1))))

(defun alchemist-project--insert-test-boilerplate (buffer module)
  "Inserts ExUnit boilerplate for MODULE in BUFFER.
Point is left in a convenient location."
  (with-current-buffer buffer
    (insert (concat "defmodule " module " do\n"
                    "  use ExUnit.Case\n\n\n"
                    "end\n"))
    (goto-char (point-min))
    (beginning-of-line 4)
    (indent-according-to-mode)))

(defun alchemist-project-run-tests-for-current-file ()
  "Run the tests related to the current file."
  (interactive)
  (alchemist-project-open-tests-for-current-file 'alchemist-mix-test-file))

(defun alchemist-project-create-file ()
  "Create a file under lib/ in the current project.

The newly created buffer is filled with a module definition based on the file name."
  (interactive)
  (let ((root (alchemist-project-root)))
    (if (not root)
        (message "You're not in a Mix project")
      (let* ((lib-path (concat root "lib/"))
             (abs-path (read-file-name "New file in lib/: " lib-path))
             (abs-path (alchemist-utils-add-ext-to-path-if-not-present abs-path ".ex"))
             (relative-path (file-relative-name abs-path lib-path)))
        (if (file-readable-p abs-path)
            (message "%s already exists" relative-path)
          (make-directory (file-name-directory abs-path) t)
          (find-file abs-path)
          (insert (concat "defmodule "
                          (alchemist-utils-path-to-module-name relative-path)
                          " do\n"
                          "  \n"
                          "end\n"))
          (goto-char (point-min))
          (beginning-of-line 2)
          (back-to-indentation))))))

(defun alchemist-project-name ()
  "Return the name of the current Elixir Mix project."
  (if (alchemist-project-p)
      (car (cdr (reverse (split-string (alchemist-project-root) "/"))))
    ""))

(defun alchemist-project-find-dir (directory)
  (unless (alchemist-project-p)
    (error "Could not find an Elixir Mix project root."))
  (alchemist-file-find-files (alchemist-project-root) directory))

(defun alchemist-project-find-lib ()
  (interactive)
  (alchemist-project-find-dir "lib"))

(defun alchemist-project-find-test ()
  (interactive)
  (alchemist-project-find-dir "test"))

(provide 'alchemist-project)

;;; alchemist-project.el ends here
