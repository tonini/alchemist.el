;;; alchemist-refcard.el ---

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

;;; Code:

(defconst alchemist-refcard-buffer-name "*alchemist-refcard*"
  "")

(defconst alchemist-refcard-list-format
  [("" 45 t)
   ("" 35 t)]
  "List format.")

(defvar alchemist-refcard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'alchmist-refcard--describe-funtion-at-point)
    map)
  "Keymap for `alchemist-refcard-mode'.")

(defun alchemist-refcard--get-keybinding (function-name)
  (let* ((keys (where-is-internal (intern function-name)))
         (keys (mapcar (lambda (k)
                         (let ((key (format "%s" k)))
                           (if (string-match-p "menu-bar" key)
                               nil
                             k))) keys))
         (keys (cl-remove-if nil keys)))
    (if keys
        (key-description (car keys))
      "")))

(defun alchemist-refcard-buffer ()
  "Return alchemist-refcard buffer if it exists."
  (get-buffer alchemist-refcard-buffer-name))

(defun alchemist-refcard--tabulated-list-entries ()
  (list
   (alchemist-refcard--build-empty-tabulated-row)
   (alchemist-refcard--build-tabulated-refcard-title-row (format "Alchemist Refcard v%s" alchemist--version))
   (alchemist-refcard--build-empty-tabulated-row)

   (alchemist-refcard--build-tabulated-title-row "Mix")
   (alchemist-refcard--build-tabulated-row "alchemist-mix")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-test")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-test-file")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-test-this-buffer")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-test-at-point")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-rerun-last-test")
   (alchemist-refcard--build-tabulated-row "alchemist-mix-compile")
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
   (alchemist-refcard--build-tabulated-row "alchemist-help-search-marked-region")
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
   (alchemist-refcard--build-tabulated-row "alchemist-eval-print-quoted-buffer")))

(defun alchemist-refcard--build-tabulated-row (function-name)
  (list function-name `[,(propertize function-name 'face 'font-lock-face)
                        ,(propertize (alchemist-refcard--get-keybinding function-name) 'face 'font-lock-builtin-face)]))

(defun alchemist-refcard--build-empty-tabulated-row ()
  (list "" `[,"" ""]))

(defun alchemist-refcard--build-tabulated-refcard-title-row (title)
  (list "" `[,(propertize title 'face '((t (:inherit font-lock-variable-name-face :weight bold)))) ""]))

(defun alchemist-refcard--build-tabulated-title-row (title)
  (list "" `[,(propertize title 'face '((t (:inherit font-lock-constant-face :weight bold)))) ""]))

(defun alchmist-refcard--describe-funtion-at-point ()
  (interactive)
  (let ((function-name (get-text-property (point) 'tabulated-list-id)))
    (when (not (alchemist-utils--empty-string-p function-name))
      (describe-function (intern function-name)))))

(define-derived-mode alchemist-refcard-mode tabulated-list-mode "Alchemist"
  "Special mode for listing Alchemist functionality."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Alchemist-Refcard")
  (setq-local alchemist-buffer-status-modeline nil)
  (use-local-map alchemist-refcard-mode-map)
  (alchemist-mode)
  (setq tabulated-list-format alchemist-refcard-list-format)
  (setq tabulated-list-entries 'alchemist-refcard--tabulated-list-entries)
  (tabulated-list-print))

;;;###autoload
(defun alchemist-refcard ()
  "Display a list of all available Alchemist functions."
  (interactive)
  (let ((buffer-p (alchemist-refcard-buffer))
        (buffer (get-buffer-create alchemist-refcard-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (alchemist-refcard-mode))))


(provide 'alchemist-refcard)

;;; alchemist-refcard.el ends here
