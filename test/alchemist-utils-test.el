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

(ert-deftest test-flatten-of-list ()
  (should (equal (alchemist-utils--flatten '(1 2 (3 4) 5))
                 '(1 2 3 4 5)))
  (should (equal (alchemist-utils--flatten '(1 2 ("dude" "hero" (3)) 4 5))
                 '(1 2 "dude" "hero" 3 4 5))))

(ert-deftest test-cmdlist-runner-builder ()
  (should (equal (alchemist-utils--build-runner-cmdlist "mix help")
                 '("mix" "help")))
  (should (equal (alchemist-utils--build-runner-cmdlist '("mix" "hex.search"))
                 '("mix" "hex.search")))
  (should (equal (alchemist-utils--build-runner-cmdlist "elixir -v")
                 '("elixir" "-v")))
  (should (equal (alchemist-utils--build-runner-cmdlist '("elixirc" ""))
                 '("elixirc"))))

(ert-deftest test-utils/clear-search-text ()
  (should (equal (alchemist-utils--clear-search-text "Elixir.Module.")
                 "Elixir.Module"))
  (should (equal (alchemist-utils--clear-search-text "Elixir.Module,")
                 "Elixir.Module"))
  (should (equal (alchemist-utils--clear-search-text "Elixir.Module.function.")
                 "Elixir.Module.function"))
  (should (equal (alchemist-utils--clear-search-text "__CALLER__.")
                 "__CALLER__")))

(ert-deftest test-utils/remove-newline-at-end ()
  "Remove newline at the end of string."
  (should (equal (alchemist-utils--remove-newline-at-end "This is
a text
") "This is
a text")))

(provide 'alchemist-utils-tests)

;;; alchemist-utils-tests.el ends here
