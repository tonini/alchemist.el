;;; alchemist-help.el ---

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

(defface alchemist-help--key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  ""
  :group 'alchemist-help)

(defvar alchemist-help-search-history '()
  "Stores the search history")

(defun alchemist-help-module-sexp-at-point ()
  "Run `alchemist-help' with the module and sexp combination at point."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9A-z.")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9A-z.")
      (setq p2 (point))
      (alchemist-help (buffer-substring-no-properties p1 p2)))))

(defun alchemist-help-sexp-at-point ()
  "Run `alchemist-help' with the sexp at point."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9A-z")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9A-z")
      (setq p2 (point))
      (alchemist-help (buffer-substring-no-properties p1 p2)))))

(defun alchemist-help-search-marked-region (begin end)
  ""
  (interactive "r")
  (alchemist-help (filter-buffer-substring begin end)))

(defcustom alchemist-help-buffer-name "*elixir help*"
  "Name of the elixir help buffer."
  :type 'string
  :group 'alchemist-help)

(defun alchemist-help-build-code-for-search (string)
  (format "import IEx.Helpers

Application.put_env(:iex, :colors, [enabled: true])

h(%s)" string))

(defun alchemist-help--eval-string (string)
  (alchemist-help--execute-alchemist-with-code-eval-string string))

(defvar alchemist-help--temp-eval-filename "alchemist-help-tmp-eval-file.exs")

(defun alchemist-help--code-eval-string-command (file)
  (format "%s -e 'IO.puts inspect(elem(Code.eval_string(File.read!(\"%s\")), 0))'"
          alchemist-execute-command
          file))

(defun alchemist-help--execute-alchemist-with-code-eval-string (string)
  (with-temp-file alchemist-help--temp-eval-filename
    (insert string))
  (let ((content (shell-command-to-string (alchemist-help--code-eval-string-command alchemist-help--temp-eval-filename))))
    (delete-file alchemist-help--temp-eval-filename)
    (alchemist-help--initialize-buffer content)))

(defun alchemist-help--eval-doc (string)
  (alchemist-help--execute-alchemist-with-code-eval-string string))

(defun alchemist-help--initialize-buffer (content)
  (pop-to-buffer alchemist-help-buffer-name)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert content)
    (delete-matching-lines "do not show this result in output" (point-min) (point-max))
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only 1)
    (alchemist-help--minor-mode 1)))

(defun alchemist-help-minor-mode-key-binding-summary ()
  (interactive)
  (message
   (concat "[" (propertize "q" 'face 'alchemist-help--key-face)
           "]-quit ["
           (propertize "e" 'face 'alchemist-help--key-face)
           "]-sexp-at-point ["
           (propertize "E" 'face 'alchemist-help--key-face)
           "]-module-sexp-at-point ["
           (propertize "s" 'face 'alchemist-help--key-face)
           "]-search ["
           (propertize "?" 'face 'alchemist-help--key-face)
           "]-keys")))

(defun alchemist-help-next-search ()
  (interactive)
  (let ((current-position (- alchemist-help-search-history-index 1)))
    (message current-position)
    (unless (< current-position 0)
      (setq alchemist-help-search-history-index (- alchemist-help-search-history-index 1))
      (alchemist-help (nth current-position alchemist-help-search-history))
        )
    ))

(define-minor-mode alchemist-help--minor-mode
  "Minor mode for displaying elixir help."
  :group 'alchemist-help
  :keymap '(("q" . quit-window)
            ("e" . alchemist-help-sexp-at-point)
            ("E" . alchemist-help-module-sexp-at-point)
            ("s" . alchemist-help)
            ("?" . alchemist-help-minor-mode-key-binding-summary)
            ))

(defun alchemist-help (search)
  (interactive
   (list
    (ido-completing-read "Elixir help: " alchemist-help-search-history)))
  (unless (memq 'search alchemist-help-search-history)
    (add-to-list 'alchemist-help-search-history search))
  (alchemist-help--eval-string (alchemist-help-build-code-for-search search)))

(provide 'alchemist-help)
