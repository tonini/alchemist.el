;;; alchemist-hooks.el --- Run tests on save

;; Copyright © 2014 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com
;;         Dave Thomas <http://pragdave.me>

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

(defcustom alchemist-hooks-test-on-save nil
  "If t, run `alchemist-mix-test' on save."
  :type 'boolean
  :group 'alchemist-hooks)

(defun alchemist-hooks--test-on-save ()
  (when (and alchemist-hooks-test-on-save
             (eq major-mode 'elixir-mode))
    (alchemist-mix-test)))

(eval-after-load 'elixir-mode
  '(progn
     (add-hook 'after-save-hook 'alchemist-hooks--test-on-save nil nil)))

(provide 'alchemist-hooks)

;;; alchemist-hooks.el ends here
