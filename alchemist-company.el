;;; alchemist-company.el --- company-mode completion back-end for Elixir -*- lexical-binding: t -*-

;; Copyright © 2014 Samuel Tonini

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

;; company-mode completion back-end for Elixir

;;; Code:

(require 'company)

(defun alchemist-company (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Elixir."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'alchemist-company))
    (init (when (eq major-mode 'elixir-mode)))
    (prefix (and (eq major-mode 'elixir-mode)
                 (alchemist-help--exp-at-point)))
    (candidates (cons :async
        (lambda (cb) (alchemist-complete-candidates arg cb))))))

(add-to-list 'company-backends 'alchemist-company)

(provide 'alchemist-company)

;;; alchemist-company.el ends here
