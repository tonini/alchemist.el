;;; alchemist-info-test.el ---

;; Copyright © 2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

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

(defun datatype-info-of-expr (expr)
  (with-temp-buffer
    (alchemist-mode)
    (insert expr)
    (goto-char (point-min))
    (alchemist-info-expression-at-point)))

(defun info-buffer-content ()
  (with-current-buffer alchemist-info-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun write-expr-and-get-datatype (expr)
  (with-temp-buffer
    (alchemist-mode)
    (insert expr)
    (goto-char (point-min))
    (alchemist-info-datatype-at-point)
    (wait 3)))

;; Because the following functionality are Elixir v1.2 dependent
;; the tests will just run if a proper Elixir version is available.
(when (string-match "Elixir 1\.2.*" (alchemist-elixir-version))
  (ert-deftest alchemist-info/datatype-info ()
    (write-expr-and-get-datatype "List")
    (should (string-match "Term
  List
Data type
  Atom" (info-buffer-content)))
    (write-expr-and-get-datatype ":ok")
    (should (string-match "Term
  :ok
Data type
  Atom
Reference modules
  Atom" (info-buffer-content)))
    (write-expr-and-get-datatype "\"string\"")
    (should (string-match "Term
  \"string\"
Data type
  BitString
Byte size
  6" (info-buffer-content))))

  (ert-deftest alchemist-info/expression-at-point ()
    (should (equal (datatype-info-of-expr "Enum.any?")
                   "Enum.any?"))
    (should (equal (datatype-info-of-expr "String")
                   "String"))
    (should (equal (datatype-info-of-expr "String.Unicode")
                   "String.Unicode"))
    (should (equal (datatype-info-of-expr ":ok")
                   ":ok"))
    (should (equal (datatype-info-of-expr "'Bitstring'")
                   "'Bitstring'"))
    (should (equal (datatype-info-of-expr "\"And?\"")
                   "\"And?\""))))
(provide 'alchemist-info-test)

;;; alchemist-info-test.el ends here
