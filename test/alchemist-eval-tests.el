;;; alchemist-eval-tests.el --- Test suite for Alchemist eval functionality.

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

;;  Test suite for Alchemist eval functionality.

;;; Code:

(require 'test-helper)

(ert-deftest show-result-in-echo-area ()
  "Evalute code on current line"
  (should (equal "2" (with-temp-buffer
		       (alchemist-mode)
		       (insert "1 + 1")
		       (goto-char (point-min))
		       (alchemist-eval-current-line)))))

(ert-deftest show-result-in-echo-area ()
  "Evalute code on current line"
  (should (equal "12" (with-temp-buffer
			(alchemist-mode)
			(insert "a = 10
                                 b = 2
                                 a + b")
			(alchemist-eval-region (point-min) (point-max))))))

(provide 'alchemist-eval-tests)

;;; alchemist-eval-tests.el ends here
