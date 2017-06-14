;;; alchemist-help-tests.el --- Test suite for alchemist-help.el

;; Copyright © 2014-2017 Samuel Tonini
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

;;; Code:

(ert-deftest test-no-doc-available-p ()
  (should (alchemist-help-no-doc-available-p
           "Could not load module CustomModule, got: nofile"))
  (should (alchemist-help-no-doc-available-p
           "No documentation for List.Chars.Atom was found"))
  (should (alchemist-help-no-doc-available-p
           ":lists is an Erlang module and, as such, it does not have Elixir-style docs"))
  (should (alchemist-help-no-doc-available-p
           ""))
  (should-not (alchemist-help-no-doc-available-p
               "List ...")))

(provide 'alchemist-help-tests)

;;; alchemist-help-tests.el ends here
