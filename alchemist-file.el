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

(provide 'alchemist-file)

;;; alchemist-file.el ends here
