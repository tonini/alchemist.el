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
(require 'alchemist-scope)
(require 'company)

;; Tell the byte compiler to assume that functions are defined
(eval-when-compile
  (declare-function alchemist-help--exp-at-point "alchemist-help.el")
  (declare-function alchemist-help--execute "alchemist-help.el")
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

(defvar alchemist-company-callback nil)
(defvar alchemist-company-filter-output nil)
(defvar alchemist-company-last-completion nil)

(defun alchemist-company--show-documentation ()
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates))
           (candidate (format "%s%s" selected (alchemist-company--annotation selected))))
      (alchemist-help--execute candidate))))
(put 'alchemist-company--show-documentation 'company-keep t)

(defun alchemist-company--open-definition ()
  (interactive)
  (company--electric-do
    (let* ((selected (nth company-selection company-candidates)))
      (alchemist-goto--open-definition selected))))
(put 'alchemist-company--open-definition 'company-keep t)

(defun alchemist-company--annotation (candidate)
  (get-text-property 0 'meta candidate))

(defun alchemist-company-build-scope-arg (arg)
  "Build informations about the current context."
  (let* ((modules (alchemist-utils--prepare-modules-for-elixir
                   (alchemist-scope-all-modules)))
         (aliases (alchemist-utils--prepare-aliases-for-elixir
                   (alchemist-scope-aliases))))
    (format "%s;%s;%s" arg modules aliases)))

(defun alchemist-company-build-server-arg (arg)
  (if (not (equal major-mode 'alchemist-iex-mode))
      (alchemist-company-build-scope-arg arg)
    (format "%s;[];[]" arg)))

(defun alchemist-company-filter (_process output)
  (setq alchemist-company-filter-output (cons output alchemist-company-filter-output))
  (if (string-match "END-OF-COMPLETE$" output)
      (let* ((candidates (alchmist-complete--build-candidates-from-process-output alchemist-company-filter-output))
             (candidates (if candidates
                             candidates
                           (alchemsit-complete--dabbrev-code-candidates))))
        (funcall alchemist-company-callback candidates))))

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
                        (setq alchemist-company-last-completion arg)
                        (setq alchemist-company-filter-output nil)
                        (setq alchemist-company-callback cb)
                        (alchemist-server-complete-candidates (alchemist-company-build-server-arg arg)
                                                              #'alchemist-company-filter))))
    (annotation (when alchemist-company-show-annotation
                  (alchemist-company--annotation arg)))))

(add-to-list 'company-backends 'alchemist-company)

(provide 'alchemist-company)

;;; alchemist-company.el ends here
