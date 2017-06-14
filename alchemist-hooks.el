;;; alchemist-hooks.el --- Hooks functionality

;; Copyright © 2014-2017 Samuel Tonini

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

;; Hooks functionality

;;; Code:

(require 'alchemist-project)
(require 'alchemist-mix)
(require 'alchemist-report)
(require 'alchemist-test-mode)

(defgroup alchemist-hooks nil
  "Hooks"
  :prefix "alchemist-hooks-"
  :group 'alchemist)

(defcustom alchemist-hooks-test-on-save nil
  "If t, run `alchemist-mix-test' on save."
  :type 'boolean
  :group 'alchemist-hooks)

(defcustom alchemist-hooks-compile-on-save nil
  "If t, run `alchemist-mix-compile' on save."
  :type 'boolean
  :group 'alchemist-hooks)

(defun alchemist-hooks-test-on-save ()
  (when (and alchemist-hooks-test-on-save
             (alchemist-project-p))
    (alchemist-report-run "mix test"
                          alchemist-test-report-process-name
                          alchemist-test-report-buffer-name
                          #'alchemist-test-report-mode
                          #'alchemist-test--handle-exit
                          t)))

(defun alchemist-hooks-compile-on-save ()
  (when (and alchemist-hooks-compile-on-save
             (alchemist-project-p))
    (alchemist-report-run "mix compile"
                          alchemist-mix-process-name
                          alchemist-mix-buffer-name
                          #'alchemist-mix-mode
                          nil
                          t)))

(eval-after-load 'elixir-mode
  '(progn
     (add-hook 'after-save-hook 'alchemist-hooks-test-on-save nil nil)
     (add-hook 'after-save-hook 'alchemist-hooks-compile-on-save nil nil)))

(provide 'alchemist-hooks)

;;; alchemist-hooks.el ends here
