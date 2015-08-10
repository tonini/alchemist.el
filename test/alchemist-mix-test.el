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

(defun prepare-test-report-buffer ()
  (when (process-live-p (get-buffer-process (get-buffer alchemist-test-report-buffer-name)))
    (set-process-query-on-exit-flag (get-buffer-process (get-buffer alchemist-test-report-buffer-name)) nil)))

(ert-deftest test-mix/run-mix-test ()
  (prepare-test-report-buffer)
  (cd "test/dummy_elixir/test/")
  (shut-up
    (alchemist-mix-test))
  (should (equal "" alchemist-last-run-test))
  (delay 1.2 (lambda ()
               (should (alchemist-test--last-run-successful-p)))))

(ert-deftest test-mix/run-mix-test-file ()
  (prepare-test-report-buffer)
  (cd "test/dummy_elixir/test/")
  (shut-up
    (alchemist-mix-test-file "dummy_elixir_test.exs"))
  (should (equal (expand-file-name "dummy_elixir_test.exs") alchemist-last-run-test))
  (delay 1.2 (lambda ()
               (should (alchemist-test--last-run-successful-p)))))

(provide 'alchemist-mix-test)

;;; alchemist-goto-test.el ends here
