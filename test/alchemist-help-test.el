;;; alchemist-help-tests.el --- Test suite for alchemist-help.el

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

(ert-deftest test-search-output/contains-bad-text ()
  "Should return t"
  (setq alchemist-help-current-search-text "CustomModule")
  (should (equal (alchemist-help--bad-search-output-p "No documentation for CustomModule was found")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "Invalid arguments for h helper")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "** (TokenMissingError)....")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "** (SyntaxError)....")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "** (FunctionClauseError)...")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "Could not load module....")
                 t))
  (should (equal (alchemist-help--bad-search-output-p
                  "** (CompileError) nofile:5:")
                 t)))

(ert-deftest test-load-config/ansi-color ()
  "Should test different cases how ansi-color is set."
  (should (equal (alchemist-help--load-ansi-color-setting)
                 t))
  (with-current-variable alchemist-help-ansi-color-docs t
                         (should (equal (alchemist-help--load-ansi-color-setting) t)))
  (with-sandbox
   (f-touch ".alchemist")
   (f-touch "mix.exs")
   (f-write "{
  \"ansi-color-docs\": \"t\"
}" 'utf-8 ".alchemist")
   (should (equal (alchemist-help--load-ansi-color-setting) t)))
  (with-sandbox
   (f-touch ".alchemist")
   (f-touch "mix.exs")
   (f-write "{
}" 'utf-8 ".alchemist")
   (should (equal (alchemist-help--load-ansi-color-setting) t))))

(provide 'alchemist-help-tests)

;;; alchemist-help-tests.el ends here
