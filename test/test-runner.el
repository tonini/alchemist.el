;;; test-runner.el --- The test runner for the alchemist test suites

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

;; Usage:
;;
;;   emacs -Q -l test/test-runner.el           # interactive mode
;;   emacs -batch -Q -l test/test-runner.el    # batch mode

;;; Code:

(let ((current-directory (file-name-directory load-file-name)))
  (setq alchemist-test-path (expand-file-name "." current-directory))
  (setq alchemist-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path alchemist-root-path)
(add-to-list 'load-path alchemist-test-path)

(require 'alchemist)

(load (expand-file-name "test-helper.el" alchemist-test-path) nil t)
(dolist (test-file (or argv (directory-files alchemist-test-path t "-test.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)

(provide 'test-runner)

;;; test-runner.el ends here
