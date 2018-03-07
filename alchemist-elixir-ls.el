(setq use-package-always-pin "melpa-stable")
(setq use-package-always-ensure t)

(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(use-package lsp-mode :pin "melpa")

(use-package elixir-mode)
(use-package dash)
(use-package s)

(lsp-define-stdio-client
  lsp-elixir-mode
  "elixir"
   (lambda () (alchemist-project-root-or-default-dir))
   '("~/src/projects/alchemist.el/elixir-ls/erl19/language_server.sh"))

(add-hook 'elixir-mode-hook 'lsp-elixir-mode-enable)

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


;;; alchemist-file.el --- Functionality to work with directory content

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

;; Functionality to work with directory content.

;;; Code:

(defgroup alchemist-file nil
  "Functionality to work with directory content."
  :prefix "alchemist-file-"
  :group 'alchemist)

(defun alchemist-file-find-files (root directory)
  "Open DIRECTORY inside ROOT and prompt for a file."
  (let* ((files (alchemist-file-read-dir root directory))
         (root-name (car (cdr (reverse (split-string root "/")))))
         (file (completing-read (format "[%s] %s: " root-name directory) files)))
    (find-file (expand-file-name file root))))

(defun alchemist-file-read-dir (root directory)
  "Return all files in DIRECTORY and use ROOT as `default-directory'."
  (let ((default-directory root))
    (-map (lambda (file) (file-relative-name file root))
          (alchemist-file--files-from directory))))

(defun alchemist-file--files-from (directory)
  (--mapcat
   (if (file-directory-p it)
       (unless (or (equal (file-relative-name it directory) "..")
                   (equal (file-relative-name it directory) "."))
         (alchemist-file--files-from it))
     (list it))
   (directory-files directory t)))


;;; alchemist-file.el ends here

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

;;; alchemist-utils.el ends here

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
