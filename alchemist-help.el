;;; alchemist-help.el --- Interface to Elixir's documentation

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

;; Mode for searching through Elixir's documentation

;;; Code:

(defface alchemist-help--key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  "Fontface for the letter keys in the summary."
  :group 'alchemist-help)

(defvar alchemist-help-search-history '()
  "Stores the search history.")

(defvar alchemist-help-search-history-index 0
  "Stores the current position in the search history.")

(defvar alchemist-help-current-search-text '()
  "Stores the current search text.")

(defun alchemist-help-search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9A-z./?")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9A-z./?")
      (setq p2 (point))
      (alchemist-help (buffer-substring-no-properties p1 p2)))))

(defun alchemist-help-search-marked-region (begin end)
  "Run `alchemist-help' with the marked region.
Argument BEGIN where the mark starts.
Argument END where the mark ends."
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
  (alchemist-help--execute-alchemist-with-code-eval-string
   (alchemist-help-build-code-for-search string)))

(defun alchemist-help--eval-string-command (string)
  (format "%s -e 'IO.puts inspect(elem(Code.eval_string(\"%s\"), 0))'"
          alchemist-execute-command
          string))

(defun alchemist-help--execute-alchemist-with-code-eval-string (string)
  (let ((content (shell-command-to-string (alchemist-help--eval-string-command string))))
    (alchemist-help--initialize-buffer content)))

(defun alchemist-help--initialize-buffer (content)
  (pop-to-buffer alchemist-help-buffer-name)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (position-current-search-text (cl-position alchemist-help-current-search-text
                                                   alchemist-help-search-history)))
    (erase-buffer)
    (cond ((or (string-match-p (format "No documentation for %s was found"
                                       alchemist-help-current-search-text) content)
               (string-match-p "Invalid arguments for h helper" content)
               (string-match-p "\\*\\* " content)
               (string-match-p "Could not load module" content)
               )
           (insert (propertize
                    (format "No documentation for [ %s ] found." alchemist-help-current-search-text)
                    'face 'alchemist-help--key-face)))
          (t
           (insert content)
           (when (and alchemist-help-search-history
                      position-current-search-text)
             (setq alchemist-help-search-history-index position-current-search-text))
           (unless (memq 'alchemist-help-current-search-text alchemist-help-search-history)
             (add-to-list 'alchemist-help-search-history alchemist-help-current-search-text))))
    (delete-matching-lines "do not show this result in output" (point-min) (point-max))
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only 1)
    (alchemist-help-minor-mode 1)))

(defun alchemist-help-minor-mode-key-binding-summary ()
  (interactive)
  (message
   (concat "[" (propertize "q" 'face 'alchemist-help--key-face)
           "]-quit ["
           (propertize "e" 'face 'alchemist-help--key-face)
           "]-search-at-point ["
           (propertize "n" 'face 'alchemist-help--key-face)
           "]-next-search ["
           (propertize "p" 'face 'alchemist-help--key-face)
           "]-previous-search ["
           (propertize "s" 'face 'alchemist-help--key-face)
           "]-search ["
           (propertize "?" 'face 'alchemist-help--key-face)
           "]-keys")))

(defun alchemist-help-next-search ()
  (interactive)
  (let ((current-position (cl-position alchemist-help-current-search-text alchemist-help-search-history))
        (next-position (- alchemist-help-search-history-index 1)))
    (unless (< next-position 0)
      (setq alchemist-help-search-history-index next-position)
      (alchemist-help (nth next-position alchemist-help-search-history)))))

(defun alchemist-help-previous-search ()
  (interactive)
  (let ((current-position (cl-position alchemist-help-current-search-text alchemist-help-search-history))
        (next-position (+ alchemist-help-search-history-index 1)))
    (unless (> next-position (- (length alchemist-help-search-history) 1))
      (setq alchemist-help-search-history-index next-position)
      (alchemist-help (nth next-position alchemist-help-search-history)))))

(define-minor-mode alchemist-help-minor-mode
  "Minor mode for displaying elixir help."
  :group 'alchemist-help
  :keymap '(("q" . quit-window)
            ("e" . alchemist-help-search-at-point)
            ("s" . alchemist-help)
            ("n" . alchemist-help-next-search)
            ("p" . alchemist-help-previous-search)
            ("?" . alchemist-help-minor-mode-key-binding-summary)))

(defun alchemist-help (search)
  (interactive
   (list
    (completing-read "Elixir help: " alchemist-help-search-history)))
  (setq alchemist-help-current-search-text search)
  (alchemist-help--eval-string search))

;; These functions will not be available in the release of version 0.4.0
(define-obsolete-function-alias 'alchemist-help-sexp-at-point 'alchemist-help-search-at-point)
(define-obsolete-function-alias 'alchemist-help-module-sexp-at-point 'alchemist-help-search-at-point)

(provide 'alchemist-help)

;;; alchemist-help.el ends here
