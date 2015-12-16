;;; alchemist-info.el --- Getting informations from the server.  -*- lexical-binding: t -*-

;; Copyright © 2015 Samuel Tonini

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

;; Getting informations from the server.

;;; Code:

(require 'ansi-color)

(defgroup alchemist-info nil
  "Getting informations from the server."
  :prefix "alchemist-info-"
  :group 'alchemist)

(defconst alchemist-info-buffer-name "*alchemist-info-mode*"
  "Name of the Elixir info buffer.")

(defvar alchemist-info-filter-output nil)

(defvar alchemist-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `alchemist-info-mode'.")

(defun alchemist-info-datatype-filter (_process output)
  (setq alchemist-info-filter-output (cons output alchemist-info-filter-output))
  (when (alchemist-server-contains-end-marker-p output)
    (alchemist-interact-create-popup alchemist-info-buffer-name
                                     (alchemist-server-prepare-filter-output alchemist-info-filter-output)
                                     #'(lambda ()
                                         (alchemist-info-mode)
                                         (ansi-color-apply-on-region (point-min) (point-max))))
    (setq alchemist-info-filter-output nil)))

(defun alchemist-info-expression-at-point ()
  "Return the expression under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9.?!:@\'\"")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9.?!:@\'\"")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun alchemist-info-datatype-at-point ()
  "Return information about any datatype under the cursor."
  (interactive)
  (let ((expr (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (alchemist-info-expression-at-point))))
    (alchemist-server-info (format "{ :type, :info, '%s' }" expr) #'alchemist-info-datatype-filter)))

(defun alchemist-info-types-at-point ()
  "Return information of types under the cursor."
  (interactive)
  (let ((expr (alchemist-info-expression-at-point)))
    (alchemist-server-info (format "{ :type, :types, '%s' }" expr) #'alchemist-info-datatype-filter)))

(defun alchemist-info-close-popup ()
  "Quit the information buffer window."
  (interactive)
  (quit-windows-on alchemist-info-buffer-name))

(define-minor-mode alchemist-info-mode
  "Minor mode for Alchemist server information.

\\{alchemist-info-mode-map}"
  nil
  " Alchemist-Info"
  alchemist-info-mode-map)

(provide 'alchemist-info)

;;; alchemist-info.el ends here
