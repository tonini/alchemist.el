;;; alchemist-test-mode-test.el --- Test suite for Alchemist testing mode

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

;; Test suite for Alchemist testing mode

;;; Code:

(require 'test-helper)

(defun alchemist-test-current-position ()
  (interactive)
  (message "current position > %s"))

(defmacro alchemist-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENTS."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (alchemist-mode)
     (alchemist-test-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun alchemist-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (alchemist-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(ert-deftest fontify-specific-functions-inside-testing-mode ()
  (alchemist-test-with-temp-buffer
   "
defmodule MyTest do
  use ExUnit.Case

  test \"foo\" do
    assert true
    assert_in_delta 1.1, 1.5, 0.2
    assert_raise(ArgumentError, fn -> :foo end)

    refute false
    refute_in_delta 1.1, 1.5, 0.2
    refute_receive :foo
    refute_receive(:foo)
  end

  test \"...\" do
  end
end
"
   (should (eq (alchemist-test-face-at 43) 'font-lock-variable-name-face))
   (should (eq (alchemist-test-face-at 63) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 114) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 162) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 179) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 213) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 237) 'font-lock-type-face))
   (should (eq (alchemist-test-face-at 267) 'font-lock-variable-name-face))))

(ert-deftest get-list-of-all-tests-in-buffer ()
  (should (equal '("\"create a pkg file/dir skeleton\"" ":symbol")
                 (with-temp-buffer
                   (alchemist-test-mode)
                   (insert "
defmodule MyTest do
  test \"create a pkg file/dir skeleton\" do
  end

  test :symbol do
  end
end
")
                   (mapcar 'car (alchemist-test-mode--tests-in-buffer))))))

(provide 'alchemist-test-mode-test)

;;; alchemist-test-mode-test.el ends here
