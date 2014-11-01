;;; alchemist-project.el --- API to identify Elixir mix projects.

;; Copyright © 2014 Samuel Tonini

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

;;; Code:

(require 'cl)

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

(defun alchemist-project-name ()
  "Return the name of the current Elixir project."
  (if alchemist-project-p
      (car (cdr (reverse (split-string (alchemist-project-root) "/"))))
    ""))

(provide 'alchemist-project)

;;; alchemist-project.el ends here
