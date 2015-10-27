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
(require 'company)
(require 'alchemist-help)
(require 'alchemist-goto)
(require 'alchemist-scope)
(require 'alchemist-server)
(require 'alchemist-complete)

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
(defvar alchemist-company-doc-lookup-done nil)

(defun alchemist-company--wait-for-doc-buffer ()
  (while (not alchemist-company-doc-lookup-done)
    (sit-for 0.01)))

(defun alchemist-company-show-documentation (candidate)
  (interactive)
  (let* ((annotation (alchemist-company--annotation candidate))
         (candidate (if annotation
                        (format "%s%s" candidate annotation)
                      candidate))
         (candidate (alchemist-help--prepare-search-expr candidate)))
    (setq alchemist-company-doc-lookup-done nil)
    (alchemist-server-help (alchemist-help--server-arguments candidate) #'alchemist-company-doc-buffer-filter)
    (alchemist-company--wait-for-doc-buffer)
    (get-buffer alchemist-help-buffer-name)))

(defun alchemist-company-open-definition (candidate)
  (interactive)
  (alchemist-goto--open-definition candidate))

(defun alchemist-company--annotation (candidate)
  (get-text-property 0 'meta candidate))

(defun alchemist-company-build-scope-arg (arg)
  "Build informations about the current context."
  (let* ((modules (alchemist-utils-prepare-modules-for-elixir
                   (alchemist-scope-all-modules)))
         (aliases (alchemist-utils-prepare-aliases-for-elixir
                   (alchemist-scope-aliases))))
    (format "{ \"%s\", [ context: Elixir, imports: %s, aliases: %s ] }" arg modules aliases)))

(defun alchemist-company-build-server-arg (arg)
  (if (not (equal major-mode 'alchemist-iex-mode))
      (alchemist-company-build-scope-arg arg)
    (format "{ \"%s\", [ context: [], imports: [], aliases: [] ] }" arg)))

(defun alchemist-company-filter (_process output)
  (setq alchemist-company-filter-output (cons output alchemist-company-filter-output))
  (if (alchemist-server-contains-end-marker-p output)
      (let* ((candidates (alchemist-complete--build-candidates-from-process-output alchemist-company-filter-output)))
        (setq alchemist-company-filter-output nil)
        (alchemist-company-serve-candidates-to-callback (-distinct candidates)))))

(defun alchemist-company-doc-buffer-filter (_process output)
  (setq alchemist-company-filter-output (cons output alchemist-company-filter-output))
  (if (alchemist-server-contains-end-marker-p output)
      (let ((string (alchemist-server-prepare-filter-output alchemist-company-filter-output)))
        (setq alchemist-company-filter-output nil)
        (if (get-buffer alchemist-help-buffer-name)
            (kill-buffer alchemist-help-buffer-name))
        (with-current-buffer (get-buffer-create alchemist-help-buffer-name)
          (insert string)
          (ansi-color-apply-on-region (point-min) (point-max))
          (alchemist-help-minor-mode 1))
        (setq alchemist-company-doc-lookup-done t))))

(defun alchemist-company-serve-candidates-to-callback (candidates)
  (let* ((candidates (if candidates
                        candidates
                      (alchemsit-complete--dabbrev-code-candidates)))
         (candidates (if (eq (length candidates) 1)
                        (-insert-at 0 company-prefix candidates)
                       candidates))
         (candidates (-distinct candidates)))
    (funcall alchemist-company-callback candidates)))

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
                 (alchemist-scope-expression)))
    (doc-buffer (alchemist-company-show-documentation arg))
    (location (alchemist-company-open-definition arg))
    (candidates (cons :async
                      (lambda (cb)
                        (setq alchemist-company-last-completion arg)
                        (setq alchemist-company-callback cb)
                        (alchemist-server-complete-candidates (alchemist-company-build-server-arg arg)
                                                              #'alchemist-company-filter))))
    (annotation (when alchemist-company-show-annotation
                  (alchemist-company--annotation arg)))))

(add-to-list 'company-backends 'alchemist-company)

(provide 'alchemist-company)

;;; alchemist-company.el ends here
