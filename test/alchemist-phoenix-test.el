;;; alchemist-phoenix-test.el --- Summary

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

(ert-deftest alchemist-phoenix-test/a-phoenix-project ()
  (with-sandbox
   (f-write "{:phoenix, \"~> 1.3.4\"}" 'utf-8 "mix.exs")
   (should (alchemist-phoenix-project-p))))

(ert-deftest alchemist-phoenix-test/no-phoenix-project ()
  (with-sandbox
   (f-write "{:cowboy, \"~> 1.0\"}" 'utf-8 "mix.exs")
   (should-not (alchemist-phoenix-project-p))))

(provide 'alchemist-phoenix-test)
;;; alchemist-phoenix-test.el ends here
