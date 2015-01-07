;;; alchemist-complete-tests.el ---

;; Copyright © 2014-2015 Samuel Tonini
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

(require 'test-helper)

(ert-deftest test-elixir-output/format-to-list ()
  "Formats the output from elixir into a proper list"
  (should (equal (alchemist-complete--output-to-list "cmp:List.
cmp:delete/2
cmp:to_string/1")
                 '("List." "delete/2" "to_string/1"))))

(ert-deftest test-complete-candidates/build-candidates ()
  "Build a candidates list"
  (should (equal (alchemist-complete--build-candidates '("List." "delete/2" "to_string/1"))
                 '("List.delete" "List.to_string")))
  (should (equal (alchemist-complete--build-candidates '("List." "delete/2" "delete/3" "to_string/1"))
                 '("List.delete" "List.delete" "List.to_string")))
  (should (equal (alchemist-complete--build-candidates '("Enum" "Enum" "Enumerable"))
                 '("Enum" "Enumerable")))
  (should (equal (alchemist-complete--build-candidates '("def" "def/2" "defdelegate/2" "defexception/1"))
                 '("def" "defdelegate" "defexception")))
  (should (equal (alchemist-complete--build-candidates '("List.delete" "delete/2" "delete_at/2"))
                 '("List.delete" "List.delete_at")))
  (should (equal (alchemist-complete--build-candidates '(":file" "filename" "file_server" "file_io_server"))
                 '(":filename" ":file_server" ":file_io_server")))
  (should (equal (alchemist-complete--build-candidates '(":file." "pid2name/1" "set_cwd/1" "rename/2"))
                 '(":file.pid2name" ":file.set_cwd" ":file.rename"))))

(ert-deftest test-complete-candidates/build-help-candidates ()
  "Build a candidates list"
  (should (equal (alchemist-complete--build-help-candidates '("List." "delete/2" "delete/3" "to_string/1"))
                 '("List" "List.delete/2" "List.delete/3" "List.to_string/1")))
  (should (equal (alchemist-complete--build-help-candidates '("Enum" "Enum" "Enumerable"))
                 '("Enum" "Enumerable")))
  (should (equal (alchemist-complete--build-help-candidates '("List" "def/2" "defdelegate/2" "defexception/1"))
                 '("List" "def/2" "defdelegate/2" "defexception/1")))
  (should (equal (alchemist-complete--build-help-candidates '("List.delete" "delete/2" "delete_at/2"))
                 '("List.delete/2" "List.delete_at/2")))
  )




(provide 'alchemist-complete-tests)
