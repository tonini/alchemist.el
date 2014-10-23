;;; alchemist.el --- Elixir tooling integration into emacs

;; Copyright © 2014 Samuel Tonini
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

(require 'ert)
(require 'test-helper)

(ert-deftest test-elixir-project-root/mix-file-exists ()
  (within-sandbox "lib/elixir"
                  (f-touch "../../mix.exs")
                  (should (equal (f-expand (alchemist-utils--elixir-project-root)) alchemist-sandbox-path))))

(ert-deftest test-alchemist-project-root/mix-file-dont-exists ()
  (within-sandbox
   (should (equal (alchemist-utils--elixir-project-root) nil))))

(ert-deftest test-flatten-of-list ()
  (should (equal (alchemist-utils--flatten '(1 2 (3 4) 5))
                 '(1 2 3 4 5)))
  (should (equal (alchemist-utils--flatten '(1 2 ("dude" "hero" (3)) 4 5))
                 '(1 2 "dude" "hero" 3 4 5))))

(ert-deftest test-establish-root-directory/set-default-directory ()
  (within-sandbox "lib/elixir"
                  (f-touch "../../mix.exs")
                  (should (equal (alchemist-utils--establish-project-root-directory)
                                 default-directory))))

(ert-deftest test-establish-root-directory/no-root-exists ()
  (within-sandbox
   (should (equal (alchemist-utils--establish-project-root-directory) nil))))

(ert-deftest test-cmdlist-runner-builder ()
  (should (equal (alchemist-utils--build-runner-cmdlist "mix help")
                 '("mix" "help")))
  (should (equal (alchemist-utils--build-runner-cmdlist '("mix" "hex.search"))
                 '("mix" "hex.search")))
  (should (equal (alchemist-utils--build-runner-cmdlist "elixir -v")
                 '("elixir" "-v")))
  (should (equal (alchemist-utils--build-runner-cmdlist '("elixirc" ""))
                 '("elixirc"))))

(provide 'alchemist-tests)

;;; alchemist-tests.el ends here
