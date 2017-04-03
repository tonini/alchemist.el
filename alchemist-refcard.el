;;; alchemist-refcard.el --- Generates a refcard of alchemist functionality

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

;; Generates a refcard of alchemist functionality

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'tabulated-list)
(require 'alchemist-phoenix)

;; Tell the byte compiler about autoloaded functions from packages
(eval-when-compile
  (declare-function alchemist-mode "alchemist.el")
  (declare-function alchemist-version "alchemist.el"))

(defgroup alchemist-refcard nil
  "Generate a refcard of alchemist functionality."
  :prefix "alchemist-"
  :group 'applications)

(defconst alchemist-refcard--buffer-name "*alchemist-refcard*"
  "Name of Alchemist-Refcard mode buffer.")

(defconst alchemist-refcard-list-format
  [("" 55 t)
   ("" 35 t)]
  "List format.")

(defvar alchemist-refcard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'alchemist-refcard--describe-funtion-at-point)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `alchemist-refcard-mode'.")

(defun alchemist-refcard--get-keybinding (function-name)
  (let* ((keys (where-is-internal (intern function-name)))
         (keys (-map (lambda (k)
                       (let ((key (format "%s" k)))
                         (if (string-match-p "menu-bar" key)
                             nil
                           k))) keys))
         (keys (-remove 'null keys)))
    (if keys
        (progn
          (mapconcat (lambda (k) (key-description k)) keys " , "))
      "")))

(defun alchemist-refcard--tabulated-list-entries ()
  (alchemist-mode +1) ;; needs to be enabled for fetching current keybindings
  (let ((rows (list (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-refcard-title-row (format "Alchemist Refcard v%s" (alchemist-version)))
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Mix")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-compile")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-run")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Hex")
                    (alchemist-refcard--build-tabulated-row "alchemist-hex-info-at-point")
                    (alchemist-refcard--build-tabulated-row "alchemist-hex-releases-at-point")
                    (alchemist-refcard--build-tabulated-row "alchemist-hex-releases")
                    (alchemist-refcard--build-tabulated-row "alchemist-hex-info")
                    (alchemist-refcard--build-tabulated-row "alchemist-hex-search")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Testing")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-test")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-rerun-last-test")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-test-file")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-test-this-buffer")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-test-at-point")
                    (alchemist-refcard--build-tabulated-row "alchemist-mix-test-stale")
                    (alchemist-refcard--build-tabulated-row "alchemist-test-toggle-test-report-display")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Compilation")
                    (alchemist-refcard--build-tabulated-row "alchemist-compile")
                    (alchemist-refcard--build-tabulated-row "alchemist-compile-file")
                    (alchemist-refcard--build-tabulated-row "alchemist-compile-this-buffer")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Execution")
                    (alchemist-refcard--build-tabulated-row "alchemist-execute")
                    (alchemist-refcard--build-tabulated-row "alchemist-execute-file")
                    (alchemist-refcard--build-tabulated-row "alchemist-execute-this-buffer")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Documentation Lookup")
                    (alchemist-refcard--build-tabulated-row "alchemist-help")
                    (alchemist-refcard--build-tabulated-row "alchemist-help-history")
                    (alchemist-refcard--build-tabulated-row "alchemist-help-search-at-point")
                    (alchemist-refcard--build-tabulated-row "alchemist-refcard")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Definition Lookup")
                    (alchemist-refcard--build-tabulated-row "alchemist-goto-definition-at-point")
                    (alchemist-refcard--build-tabulated-row "alchemist-goto-jump-back")
                    (alchemist-refcard--build-tabulated-row "alchemist-goto-jump-to-previous-def-symbol")
                    (alchemist-refcard--build-tabulated-row "alchemist-goto-jump-to-next-def-symbol")
                    (alchemist-refcard--build-tabulated-row "alchemist-goto-list-symbol-definitions")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Project")
                    (alchemist-refcard--build-tabulated-row "alchemist-project-find-test")
                    (alchemist-refcard--build-tabulated-row "alchemist-project-toggle-file-and-tests")
                    (alchemist-refcard--build-tabulated-row "alchemist-project-toggle-file-and-tests-other-window")
                    (alchemist-refcard--build-tabulated-row "alchemist-project-run-tests-for-current-file")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "IEx")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-run")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-project-run")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-send-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-send-current-line-and-go")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-send-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-send-region-and-go")
                    (alchemist-refcard--build-tabulated-row "alchemist-iex-compile-this-buffer")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Eval")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-quoted-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-quoted-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-quoted-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-quoted-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-buffer")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-buffer")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-quoted-buffer")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-print-quoted-buffer")
                    (alchemist-refcard--build-tabulated-row "alchemist-eval-close-popup")
                    (alchemist-refcard--build-empty-tabulated-row)
                    (alchemist-refcard--build-tabulated-title-row "Macroexpand")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-once-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-once-print-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-print-current-line")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-once-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-once-print-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-print-region")
                    (alchemist-refcard--build-tabulated-row "alchemist-macroexpand-close-popup")
                    (alchemist-phoenix-mode) ;; needs to be enabled for fetching current keybindings
                    (alchemist-refcard--build-tabulated-title-row "Phoenix-Mode")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-web")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-views")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-controllers")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-channels")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-templates")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-models")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-static")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-router")
                    (alchemist-refcard--build-tabulated-row "alchemist-phoenix-find-routes")
                    (alchemist-refcard--build-empty-tabulated-row))))
    ;; disable it after getting the current keybindings
    (alchemist-phoenix-mode -1)
    (alchemist-mode -1)
    rows))

(defun alchemist-refcard--build-empty-tabulated-row ()
  (list "" `[,"" ""]))

(defun alchemist-refcard--build-tabulated-row (function-name)
  (list function-name `[,function-name
                        ,(propertize (alchemist-refcard--get-keybinding function-name) 'face font-lock-builtin-face)]))

(defun alchemist-refcard--build-tabulated-refcard-title-row (title)
  (list "" `[,(propertize title 'face font-lock-variable-name-face) ""]))

(defun alchemist-refcard--build-tabulated-title-row (title)
  (list "" `[,(propertize title 'face font-lock-constant-face) ""]))

(defun alchemist-refcard--describe-funtion-at-point ()
  (interactive)
  (let ((function-name (get-text-property (point) 'tabulated-list-id)))
    (when (not (s-blank? function-name))
      (describe-function (intern function-name)))))

(defun alchemist-refcard--buffer ()
  "Return alchemist-refcard buffer if it exists."
  (get-buffer alchemist-refcard--buffer-name))

(define-derived-mode alchemist-refcard-mode tabulated-list-mode "Alchemist"
  "Alchemist refcard mode."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Alchemist-Refcard")
  (setq-local alchemist-test-status-modeline nil)
  (use-local-map alchemist-refcard-mode-map)
  (setq tabulated-list-format alchemist-refcard-list-format)
  (setq tabulated-list-entries 'alchemist-refcard--tabulated-list-entries)
  (tabulated-list-print))

;;;###autoload
(defun alchemist-refcard ()
  "Generate an Alchemist refcard of all the features."
  (interactive)
  (let ((buffer-p (alchemist-refcard--buffer))
        (buffer (get-buffer-create alchemist-refcard--buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (alchemist-refcard-mode))))

(provide 'alchemist-refcard)

;;; alchemist-refcard.el ends here
