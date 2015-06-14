;;; alchemist-help.el --- Functionality for Elixir documentation lookup -*- lexical-binding: t -*-

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

;; Functionality for Elixir documentation lookup.

;;; Code:

(defgroup alchemist-help nil
  "Functionality for Elixir documentation lookup."
  :prefix "alchemist-help-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-help-buffer-name "*elixir help*"
  "Name of the Elixir help buffer."
  :type 'string
  :group 'alchemist-help)

(defvar alchemist-help-search-history '()
  "Storage for the search history.")

(defvar alchemist-help-current-search-text '()
  "Stores the current search.")

;; Faces

(defface alchemist-help--key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  "Fontface for the letter keys in the summary."
  :group 'alchemist-help)

(defun alchemist-help--exp-at-point ()
  "Return the expression under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9.?!:")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9.?!:")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun alchemist-help--execute (search)
  (alchemist-server-help-with-complete search))

(defun alchemist-help--execute-without-complete (search)
  (setq alchemist-help-current-search-text search)
  (setq alchemist-server--output nil)
  (unless (alchemist-server-process-p)
    (alchemist-server-start))
  (setq alchemist-server--output nil)
  (set-process-filter (alchemist-server-process) #'alchemist-server-doc-filter)
  (process-send-string (alchemist-server-process) (format "DOC %s\n" search)))

(defun alchemist-help--bad-search-output-p (string)
  (let ((match (or (string-match-p "No documentation for " string)
                   (string-match-p "Invalid arguments for h helper" string)
                   (string-match-p "** (TokenMissingError)" string)
                   (string-match-p "** (SyntaxError)" string)
                   (string-match-p "** (FunctionClauseError)" string)
                   (string-match-p "** (CompileError)" string)
                   (string-match-p "Could not load module" string))))
    (if match
        t
      nil)))

(defun alchemist-help--initialize-buffer (content)
  (let ((default-directory (if (alchemist-project-root)
                               (alchemist-project-root)
                             default-directory)))
    (cond
     ((alchemist-help--bad-search-output-p content)
             (message (propertize
                       (format "No documentation for [ %s ] found." alchemist-help-current-search-text)
                       'face 'alchemist-help--key-face)))
     (t
      (if (get-buffer alchemist-help-buffer-name)
          (kill-buffer alchemist-help-buffer-name))
      (pop-to-buffer alchemist-help-buffer-name)
      (setq buffer-undo-list nil)
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (insert content)
        (unless (memq 'alchemist-help-current-search-text alchemist-help-search-history)
          (add-to-list 'alchemist-help-search-history alchemist-help-current-search-text))
        (delete-matching-lines "do not show this result in output" (point-min) (point-max))
        (delete-matching-lines "^Compiled lib\\/" (point-min) (point-max))
        (ansi-color-apply-on-region (point-min) (point-max))
        (read-only-mode 1)
        (alchemist-help-minor-mode 1))))))

(defun alchemist-help-minor-mode-key-binding-summary ()
  (interactive)
  (message
   (concat "[" (propertize "q" 'face 'alchemist-help--key-face)
           "]-quit ["
           (propertize "e" 'face 'alchemist-help--key-face)
           "]-search-at-point ["
           (propertize "m" 'face 'alchemist-help--key-face)
           "]-search-marked-region ["
           (propertize "s" 'face 'alchemist-help--key-face)
           "]-search ["
           (propertize "h" 'face 'alchemist-help--key-face)
           "]-history ["
           (propertize "?" 'face 'alchemist-help--key-face)
           "]-keys")))

(defun alchemist-help-search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor."
  (interactive)
  (alchemist-help--execute (alchemist-help--exp-at-point)))

(defun alchemist-help-search-marked-region (begin end)
  "Run `alchemist-help' with the marked region.
Argument BEGIN where the mark starts.
Argument END where the mark ends."
  (interactive "r")
  (let ((region (filter-buffer-substring begin end)))
    (alchemist-help--execute region)))

(defun alchemist-help--elixir-modules-to-list (str)
  (let* ((modules (split-string str))
         (modules (mapcar (lambda (m)
                            (when (string-match-p "Elixir\\." m)
                              (replace-regexp-in-string "Elixir\\." "" m))) modules))
         (modules (delete nil modules))
         (modules (cl-sort modules 'string-lessp :key 'downcase))
         (modules (delete-dups modules)))
    modules))

(defvar alchemist-help-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "e") #'alchemist-help-search-at-point)
    (define-key map (kbd "m") #'alchemist-help-search-marked-region)
    (define-key map (kbd "s") #'alchemist-help)
    (define-key map (kbd "h") #'alchemist-help-history)
    (define-key map (kbd "M-.") #'alchemist-goto-definition-at-point)
    (define-key map (kbd "?") #'alchemist-help-minor-mode-key-binding-summary)
    map)
  "Keymap for `alchemist-help-minor-mode'.")

(define-minor-mode alchemist-help-minor-mode
  "Minor mode for displaying elixir help."
  :group 'alchemist-help
  :keymap alchemist-help-minor-mode-map)

(defun alchemist-help ()
  "Load Elixir documentation for SEARCH."
  (interactive)
  (alchemist-server-help))

(defun alchemist-help-history (search)
  "Load Elixir from the documentation history for SEARCH."
  (interactive
   (list
    (completing-read "Elixir help history: " alchemist-help-search-history nil nil "")))
  (alchemist-help--execute-without-complete search))

(provide 'alchemist-help)

;;; alchemist-help.el ends here
