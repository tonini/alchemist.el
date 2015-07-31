;;; alchemist-company.el --- Elixir company-mode backend -*- lexical-binding: t -*-

;; Copyright © 2014-2015 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

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

;; Elixir company-mode backend.

;;; Code:

(require 'cl-lib)
(require 'alchemist-complete)
(require 'company)

;; Tell the byte compiler to assume that functions are defined
(eval-when-compile
  (declare-function alchemist-help--execute-without-complete "alchemist-help.el")
  (declare-function alchemist-help--exp-at-point "alchemist-help.el")
  (declare-function alchemist-goto--open-definition "alchemist-goto.el")
  (declare-function alchemist-server-complete-candidates "alchemist-server.el"))

(defgroup alchemist-company nil
  "Elixir company-mode backend."
  :prefix "alchemist-company-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-company-show-annotation t
  "Show an annotation inline with the candidate."
  :type 'boolean
  :group 'alchemist-company)

(defun alchemist-company--show-documentation ()
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates))
           (candidate (format "%s%s" selected (alchemist-company--annotation selected))))
      (alchemist-help--execute-without-complete candidate))))
(put 'alchemist-company--show-documentation 'company-keep t)

(defun alchemist-company--open-definition ()
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates)))
      (alchemist-goto--open-definition selected))))
(put 'alchemist-company--open-definition 'company-keep t)

(defun alchemist-company--annotation (candidate)
  (get-text-property 0 'meta candidate))

(defun alchemist-company (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Elixir."
  (interactive (list 'interactive))
  (when alchemist-company-show-annotation
    (set 'company-tooltip-align-annotations t))
  (cl-case command
    (interactive (company-begin-backend 'alchemist-company))
    (init (or (eq major-mode 'elixir-mode)
              (string= mode-name "Alchemist-IEx")))
    (prefix (and (or (eq major-mode 'elixir-mode)
                     (string= mode-name "Alchemist-IEx"))
                 (alchemist-help--exp-at-point)))
    (doc-buffer (alchemist-company--show-documentation))
    (location (alchemist-company--open-definition))
    (candidates (cons :async
                      (lambda (cb)
                        (setq alchemist-server-company-callback cb)
                        (alchemist-server-complete-candidates arg))))
    (annotation (when alchemist-company-show-annotation
                  (alchemist-company--annotation arg)))))

(add-to-list 'company-backends 'alchemist-company)

(provide 'alchemist-company)

;;; alchemist-company.el ends here
