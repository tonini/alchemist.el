;;; alchemist-mix-test.el --- -*- lexical-binding: t; -*-

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

;;; Code:

(require 'test-helper)

(ert-deftest test-mix/run-mix-test ()
  (cd "test/dummy_elixir/test/")
  (find-file "dummy_elixir_test.exs")
  (with-current-buffer "dummy_elixir_test.exs"
    (shut-up
      (alchemist-mix-test)))
  (delay 0.8
         (lambda ()
           (should (alchemist-buffer--last-run-successful-p)))))

(provide 'alchemist-mix-test)

;;; alchemist-goto-test.el ends here
