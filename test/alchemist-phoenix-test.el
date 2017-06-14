;;; alchemist-phoenix-test.el ---

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
   (f-touch "mix.exs")
   (f-mkdir "web")
   (should (alchemist-phoenix-project-p))))

(ert-deftest alchemist-phoenix-test/no-phoenix-project ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib")
   (should-not (alchemist-phoenix-project-p))))

(provide 'alchemist-phoenix-test)
