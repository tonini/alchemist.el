;;; alchemist-utils-tests.el ---

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

(ert-deftest test-cmdlist-runner-builder ()
  (should (equal (alchemist-utils-build-command "mix help")
                 "mix help"))
  (should (equal (alchemist-utils-build-command '("mix" "hex.search"))
                 "mix hex.search"))
  (should (equal (alchemist-utils-build-command "elixir -v")
                 "elixir -v"))
  (should (equal (alchemist-utils-build-command '("elixirc" ""))
                 "elixirc")))

(ert-deftest test-utils/is-test-file-p ()
  "Should return t if visited file is a test file"
  (with-sandbox
   (f-touch "this_is_a_test.exs")
   (find-file "this_is_a_test.exs")
   (should (alchemist-utils-test-file-p))))

(ert-deftest test-if-string-is-empty ()
  (should (equal (alchemist-utils-empty-string-p nil)
                 t))
  (should (equal (alchemist-utils-empty-string-p "")
                 t))
  (should (equal (alchemist-utils-empty-string-p " ")
                 t))
  (should (equal (alchemist-utils-empty-string-p "story")
                 nil))
  (should (equal (alchemist-utils-empty-string-p "    ")
                 t)))

(ert-deftest test-utils/prepare-aliases-for-elixir ()
  (should (equal "[{MyList, List},{AlreadySentError, Plug.Conn.AlreadySentError}]"
                 (alchemist-utils-prepare-aliases-for-elixir
                  (list (list "List" "MyList") (list "Plug.Conn.AlreadySentError" "AlreadySentError")))))
  (should (equal "[]" (alchemist-utils-prepare-aliases-for-elixir '()))))

(ert-deftest test-utils/prepare-modules-for-elixir ()
  (should (equal "[Ek,Behaviour,Plug.Conn]"
                 (alchemist-utils-prepare-modules-for-elixir
                  (list "Ek" "Behaviour" "Plug.Conn"))))
  (should (equal "[]" (alchemist-utils-prepare-modules-for-elixir '("")))))

(ert-deftest test-utils/snakecase-to-camelcase ()
  (should (equal "MyCustomModule" (alchemist-utils--snakecase-to-camelcase "my_custom_module")))
  (should (equal "Foo" (alchemist-utils--snakecase-to-camelcase "foo"))))

(ert-deftest test-utils/add-ext-to-path-if-not-present ()
  (should (equal "foo.ex" (alchemist-utils-add-ext-to-path-if-not-present "foo.ex" ".ex")))
  (should (equal "foo.ex" (alchemist-utils-add-ext-to-path-if-not-present "foo" ".ex")))
  (should (equal "foo.ex.exs" (alchemist-utils-add-ext-to-path-if-not-present "foo.ex" ".exs"))))

(ert-deftest test-utils/path-to-module-name ()
  (should (equal "MyApp.MyModule" (alchemist-utils-path-to-module-name "my_app/my_module.ex")))
  (should (equal "Foo.Bar" (alchemist-utils-path-to-module-name "/foo/bar")))
  (should (equal "Foo" (alchemist-utils-path-to-module-name "//foo//"))))

(ert-deftest test-utils/remove-dot-at-the-end-of-string ()
  (should (equal "Module" (alchemist-utils-remove-dot-at-the-end "Module.")))
  (should (equal "Module.Foo" (alchemist-utils-remove-dot-at-the-end "Module.Foo."))))

(ert-deftest test-utils/count-char-in-string ()
  (should (equal 5 (alchemist-utils-count-char-occurence "\\." "This.Is.A.Long.One.")))
  (should (equal 2 (alchemist-utils-count-char-occurence "\\." "My.Module.Namespace")))
  (should (equal 1 (alchemist-utils-count-char-occurence "\\." "Foo.Bar")))
  (should (equal 0 (alchemist-utils-count-char-occurence "\\." "List"))))

(ert-deftest test-utils/add-trailing-slash-to-path ()
  (should (equal "/path/to/some/thing/" (alchemist-utils-add-trailing-slash "/path/to/some/thing")))
  (should (equal "/path/to/some/thing/" (alchemist-utils-add-trailing-slash "/path/to/some/thing/"))))

(ert-deftest test-utils/occur-in-buffer-p ()
  (with-temp-buffer
    (insert "1 2 3 foo 4 5 6")
    (should (alchemist-utils-occur-in-buffer-p (current-buffer) "f[oO]o"))
    (should-not (alchemist-utils-occur-in-buffer-p (current-buffer) "bar"))))

(ert-deftest test-utils/jump-to-next-matching-line ()
  (with-temp-buffer
    (insert "
foo
bar
foo
")
    (alchemist-utils-jump-to-next-matching-line "foo" 'beginning-of-line)
    (should (equal (point) 2))
    (alchemist-utils-jump-to-next-matching-line "foo" 'beginning-of-line)
    (should (equal (point) 10))
    (alchemist-utils-jump-to-next-matching-line "foo" 'beginning-of-line)
    (should (equal (point) 2))))

(ert-deftest test-utils/jump-to-previous-regex ()
  (with-temp-buffer
    (insert "
foo
bar
foo
")
    (alchemist-utils-jump-to-previous-matching-line "foo" 'beginning-of-line)
    (should (equal (point) 10))
    (alchemist-utils-jump-to-previous-matching-line "foo" 'beginning-of-line)
    (should (equal (point) 2))))

(ert-deftest test-utils/get-current-elixir-version ()
  (should (string-match-p "^[0-9]+\.[0-9]+\.[0-9]+.*$"
                          (alchemist-utils-elixir-version))))

(ert-deftest test-utils/elixir-version-check-p ()
  (should (alchemist-utils-elixir-version-check-p 1 3 0 "1.3.0"))
  (should-not (alchemist-utils-elixir-version-check-p 1 3 1 "1.3.0"))
  (should-not (alchemist-utils-elixir-version-check-p 1 3 0 "1.2.9"))
  (should-not (alchemist-utils-elixir-version-check-p 2 0 0 "1.3.0"))
  (should (alchemist-utils-elixir-version-check-p 1 3 0 "2.0.0")))

(provide 'alchemist-utils-tests)

;;; alchemist-utils-tests.el ends here
