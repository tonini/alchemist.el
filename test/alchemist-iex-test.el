;;; alchemist-mix-test.el --- -*- lexical-binding: t; -*-

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

(require 'test-helper)

(ert-deftest match-prompt ()
  (let ((prompt alchemist-iex-prompt-regexp))
    (should (equal 0 (string-match prompt "iex(1)>")))
    (should (equal 0 (string-match prompt "iex(123)>")))
    (should (equal 0 (string-match prompt "...(1)>")))
    (should (equal nil (string-match prompt "abc(1)>")))
    (should (equal nil (string-match prompt " iex(1)>")))
    (should (equal nil (string-match prompt "iex>")))))

(provide 'alchemist-iex-test)

;;; alchemist-iex-test.el ends here
