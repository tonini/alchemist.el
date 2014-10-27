;;; alchemist-buffer.el --- Define a custom compilation mode for Elixir executions

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

;;; Code:

(require 'compile)
(require 'ansi-color)

(defcustom alchemist-red-green-modeline t
  "If t, the modeline background is changed to green or red depending
   on the success or failure of commands such as 'mix test'."
  :type 'boolean
  :group 'alchemist-mix)

(defvar alchemist-buffer--mode-line-format-store nil)
(setq alchemist-buffer--mode-line-format-store
      mode-line-format)

(defface alchemist-buffer--success-face
  '((t (:inherit font-lock-variable-name-face :bold nil :background "darkgreen" :foreground "white")))
  ""
  :group 'alchemist-buffer)

(defface alchemist-buffer--failed-face
  '((t (:inherit font-lock-variable-name-face :bold nil :background "red" :foreground "white")))
  ""
  :group 'alchemist-buffer)

(defvar alchemist-buffer-test-mode-line-message
  "Elixir")

(defvar alchemist-buffer--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'alchemist-buffer--buffer-name)

(defvar alchemist-buffer--error-link-options
  '(elixir "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun alchemist-buffer--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode alchemist-buffer-mode "Elixir"
  "Elixir compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^Elixir.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq alchemist-buffer--buffer-name alchemist-buffer--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'alchemist-buffer--kill-any-orphan-proc)))

(defvar alchemist-buffer--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun alchemist-buffer--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'alchemist-buffer--handle-compilation-once t)
  (delete-matching-lines "\\(-*- mode:\\|elixir-compilation;\\)" (point-min) (point)))

(defun alchemist-buffer--handle-compilation ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun alchemist-buffer-run (cmdlist buffer-name)
  "run CMDLIST in `alchemist-buffer-mode'.
Returns the compilation buffer."
  (save-some-buffers (not compilation-ask-about-save) alchemist-buffer--save-buffers-predicate)
  (let* ((alchemist-buffer--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'shell-quote-argument cmdlist " ")
                           'alchemist-buffer-mode
                           (lambda (b) alchemist-buffer--buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons alchemist-buffer--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'elixir compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'alchemist-buffer--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'alchemist-buffer--handle-compilation-once nil t)
      (when alchemist-red-green-modeline
        (add-hook 'compilation-finish-functions 'alchemist-buffer--set-modeline-color nil t)))))

(defun alchemist-buffer--set-modeline-color (buffer status)
  (let ((status-font-face (if (string-prefix-p "finished" status)
                              'alchemist-buffer--success-face
                            'alchemist-buffer--failed-face)))
    (alchemist-buffer--reset-mode-line)
    (alchemist-buffer--add-to-modeline status-font-face)
    (remove-hook 'compilation-finish-functions 'alchemist-buffer--set-modeline-color)))

(defun alchemist-buffer--initalize-modeline ()
  (setq-default mode-line-format (append (list (propertize
                                                (concat " " alchemist-buffer-test-mode-line-message  " ")
                                                'face 'alchemist-buffer--success-face))
                                         mode-line-format))
    )

(defun alchemist-buffer--reset-mode-line ()
  (setq-default mode-line-format
                alchemist-buffer--mode-line-format-store))

(defun alchemist-buffer--add-to-modeline (status-font-face)
  (setq-default mode-line-format (append (list (propertize
                                                (concat " " alchemist-buffer-test-mode-line-message  " ")
                                                'face status-font-face))
                                         mode-line-format)))

(provide 'alchemist-buffer)

;;; alchemist-buffer.el ends here
