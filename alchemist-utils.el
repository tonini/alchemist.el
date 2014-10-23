;;; alchemist-utils.el ---

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

(defvar alchemist-utils--elixir-project-root-indicator
  "mix.exs"
  "The file which indicate an elixir project root.")

(defun alchemist-utils--elixir-project-root ()
  "Finds the root directory of the project.
It walking the directory tree until it finds a elixir project root indicator."
  (let* ((file (file-name-as-directory (expand-file-name default-directory))))
    (locate-dominating-file file alchemist-utils--elixir-project-root-indicator)))

(defun alchemist-utils--flatten (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (alchemist-utils--flatten (car alist))
                   (alchemist-utils--flatten (cdr alist))))))

(defun alchemist-utils--build-runner-cmdlist (command)
  "Build the commands list for the runner."
  (remove "" (alchemist-utils--flatten
              (list (if (stringp command)
                        (split-string command)
                      command)))))

(defun alchemist-utils--establish-project-root-directory ()
  "Set the default-directory to the Elixir project root."
  (let ((project-root (alchemist-utils--elixir-project-root)))
    (when project-root
      (setq default-directory project-root))))

(provide 'alchemist-utils)

;;; alchemist-utils.el ends here
