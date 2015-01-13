;;; alchemist-goto.el ---

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

;;

;;; Code:

(defun alchemist-goto-extract-function (code)
  "Extract function from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z]+" function))
      function)))

(defun alchemist-goto-extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
         (when (string-match-p "^[a-z]+" function)
           (delete function parts))
         (mapconcat 'concat parts ".")))

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
