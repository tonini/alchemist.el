;;; alchemist-buffer.el --- Custom compilation mode for Alchemist

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

;;  Custom compilation mode for Alchemist

;;; Code:

(require 'compile)
(require 'ansi-color)

;; Variables

(defgroup alchemist-buffer nil
  "Custom compilation mode for Alchemist."
  :prefix "alchemist-buffer-"
  :group 'alchemist)

(defcustom alchemist-buffer-status-modeline t
  "if t, the face of local `mode-name' variable will change with compilation status.

For example, when `alchemist-mix-test' failes, the `mode-name' will be
formated with the `alchemist-buffer--failed-face' face, to symbolize failing tests."
  :type 'boolean
  :group 'alchemist-buffer)

(defvar alchemist-buffer--mode-name-face 'mode-line)

(defvar alchemist-buffer--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'alchemist-buffer--buffer-name)

(defvar alchemist-buffer--error-link-options
  '(elixir "^\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?$" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

;; Faces

(defface alchemist-buffer--success-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "darkgreen" :foreground "#e0ff00")))
  "Face for successful compilation run."
  :group 'alchemist-buffer)

(defface alchemist-buffer--failed-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "red" :foreground "white")))
  "Face for failed compilation run."
  :group 'alchemist-buffer)

(defface alchemist-buffer--running-face
  '((t (:inherit font-lock-variable-name-face :bold nil :background "gray" :foreground "black")))
  "Face for running compilation."
  :group 'alchemist-buffer)

(defun alchemist-buffer--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(defvar alchemist-buffer--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun alchemist-buffer--remove-dispensable-output ()
  (delete-matching-lines "\\(-*- mode:\\|Compiled \\|elixir-compilation;\\|Elixir started\\|^$\\)" (point-min) (point-max))
  (remove-hook 'compilation-filter-hook 'alchemist-buffer--remove-dispensable-output t))

(defun alchemist-buffer-remove-dispensable-output-after-finish (buffer msg)
  (delete-matching-lines "\\(Excluding tags\\|Including tags\\|Elixir exited\\|Elixir finished\\)" (point-min) (point-max)))

(defun alchemist-buffer--handle-compilation ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun alchemist-buffer--set-modeline-color (buffer status)
  (setq alchemist-buffer--mode-name-face
        (if (string-prefix-p "finished" status)
            'alchemist-buffer--success-face
          'alchemist-buffer--failed-face))

  (remove-hook 'compilation-finish-functions 'alchemist-buffer--set-modeline-color))

(defun alchemist-buffer-initialize-modeline ()
  "Initialize the mode-line face."
  (setq mode-name
         '(:eval (propertize "Elixir" 'face alchemist-buffer--mode-name-face))))

(defun alchemist-buffer-reset-modeline ()
  "Reset the current mode-line face to default."
  (setq mode-name "Elixir"))

(define-compilation-mode alchemist-buffer-mode "Elixir"
  "Elixir compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq alchemist-buffer--buffer-name alchemist-buffer--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'alchemist-buffer--kill-any-orphan-proc)))

(defun alchemist-buffer-run (cmdlist buffer-name)
  "Run CMDLIST in `alchemist-buffer-mode'.
Returns the compilation buffer.
Argument BUFFER-NAME for the compilation."
  (save-some-buffers (not compilation-ask-about-save) alchemist-buffer--save-buffers-predicate)
  (let* ((alchemist-buffer--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'concat cmdlist " ")
                           'alchemist-buffer-mode
                           (lambda (b) alchemist-buffer--buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons alchemist-buffer--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'elixir compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'alchemist-buffer--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'alchemist-buffer--remove-dispensable-output nil t)
      (add-to-list 'compilation-finish-functions 'alchemist-buffer-remove-dispensable-output-after-finish)
      (when alchemist-buffer-status-modeline
        (add-hook 'compilation-finish-functions 'alchemist-buffer--set-modeline-color nil t)))))

(provide 'alchemist-buffer)

;;; alchemist-buffer.el ends here
