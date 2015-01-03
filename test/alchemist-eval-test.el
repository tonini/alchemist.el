;;; alchemist-eval-tests.el --- Test suite for Alchemist eval functionality.

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

;;  Test suite for Alchemist eval functionality.

;;; Code:

(require 'test-helper)

(ert-deftest evaluate-code-of-current-line ()
  "Evalute code on current line and output result."
  (should (equal "2" (with-temp-buffer
                       (alchemist-mode)
                       (insert "1 + 1")
                       (goto-char (point-min))
                       (alchemist-eval-current-line)))))

(ert-deftest evaluate-code-of-current-line-and-print-inline ()
  "Evalute code on current line and print result inline."
  (should (equal "1 + 1  # => 2" (with-temp-buffer
                                   (alchemist-mode)
                                   (insert "1 + 1")
                                   (goto-char (point-min))
                                   (alchemist-eval-print-current-line)
                                   (buffer-substring-no-properties (point-min) (point-max))))))


(ert-deftest evaluate-code-of-marked-region ()
  "Evalute code on region and output result."
  (should (equal "12" (with-temp-buffer
                        (alchemist-mode)
                        (insert "a = 10
                                 b = 2
                                 a + b")
                        (alchemist-eval-region (point-min) (point-max))))))

(ert-deftest evaluate-code-of-marked-region-and-print-inline ()
  "Evalute code on region and print result inline."
  (should (equal
           "
a = 10
b = 2
a + b  # => 12"
           (with-temp-buffer
             (alchemist-mode)
             (insert "
a = 10
b = 2
a + b")
             (alchemist-eval-print-region (point-max) (point-min))
             (goto-char (point-min))
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest evaluate-code-in-current-buffer ()
  "Evalute code in current buffer."
  (should (equal "54" (with-temp-buffer
                        (alchemist-mode)
                        (insert "sum = fn (a, b) ->
                                   a + b
                                 end
                                 sum.(21, 33)")
                        (alchemist-eval-buffer)))))

(ert-deftest evaluate-code-in-current-buffer-and-print-inline ()
  "Evalute code in current buffer and print result inline."
  (should (equal "
sum = fn (a, b) ->
  a + b
end
sum.(21, 33)  # => 54"
                 (with-temp-buffer
                   (alchemist-mode)
                   (insert "
sum = fn (a, b) ->
  a + b
end
sum.(21, 33)")
                   (alchemist-eval-print-buffer)
                   (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'alchemist-eval-tests)

;;; alchemist-eval-tests.el ends here
