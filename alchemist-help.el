;;; alchemist-help.el --- Functionality for Elixir documentation lookup -*- lexical-binding: t -*-

;; Copyright © 2014-2017 Samuel Tonini

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

(require 'dash)
(require 's)
(require 'ansi-color)
(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-server)
(require 'alchemist-scope)
(require 'alchemist-goto)

(defgroup alchemist-help nil
  "Functionality for Elixir documentation lookup."
  :prefix "alchemist-help-"
  :group 'alchemist)

(defcustom alchemist-help-buffer-name "*alchemist help*"
  "Name of the Elixir help buffer."
  :type 'string
  :group 'alchemist-help)

(defvar alchemist-help-search-history '()
  "Storage for the search history.")

(defvar alchemist-help-current-search-text '()
  "Stores the current search.")

(defvar alchemist-help-filter-output nil)

(defface alchemist-help-key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  "Face for the letter keys in the summary."
  :group 'alchemist-help)

(defun alchemist-help-lookup-doc (search)
  "Lookup Elixir documentation for SEARCH."
  (setq alchemist-help-current-search-text search)
  (setq alchemist-help-filter-output nil)
  (if (not (s-blank? search))
      (alchemist-server-complete-candidates
       (alchemist-help--completion-server-arguments search)
       #'alchemist-help-complete-filter-output)
    (message "No documentation for [%s] found." search)))

(defun alchemist-help-no-doc-available-p (string)
  "Return non-nil if STRING contains Elixir no documentation message."
  (or (string-match-p "No documentation for" string)
      (string-match-p "Could not load module" string)
      (string-match-p "it does not have Elixir-style docs" string)
      (s-blank? string)))

(defun alchemist-help-store-search-in-history ()
  "Store the last `alchemist-help-current-search-text' in `alchemist-help-search-history'."
  (unless (memq 'alchemist-help-current-search-text alchemist-help-search-history)
    (add-to-list 'alchemist-help-search-history alchemist-help-current-search-text)))

(defun alchemist-help-display-doc (content)
  "Initialize the `alchemist-help-buffer-name' and insert CONTENT."
  (let ((default-directory (alchemist-project-root-or-default-dir))
        (buffer (get-buffer-create alchemist-help-buffer-name)))
    (cond
     ((alchemist-help-no-doc-available-p content)
      (message (format "No documentation for [%s] found."
                       alchemist-help-current-search-text)))
     (t
      (alchemist-help-store-search-in-history)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (erase-buffer)
          (insert content)
          (goto-char (point-min))
          (ansi-color-apply-on-region (point-min) (point-max))
          (alchemist-help-minor-mode)))
      (pop-to-buffer buffer)))))

(defun alchemist-help--search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor"
  (let* ((expr (alchemist-scope-expression)))
    (alchemist-help-lookup-doc (alchemist-help--prepare-search-expr expr))))

(defun alchemist-help--search-marked-region (begin end)
  "Run `alchemist-help' with the marked region.
Argument BEGIN where the mark starts.
Argument END where the mark ends."
  (let ((expr (buffer-substring-no-properties begin end)))
    (alchemist-help-lookup-doc (alchemist-help--prepare-search-expr expr))))

(defun alchemist-help--prepare-search-expr (expr)
  (let* ((module (alchemist-scope-extract-module expr))
         (module (alchemist-scope-alias-full-path module))
         (module (if module module ""))
         (function (alchemist-scope-extract-function expr))
         (function (if function function ""))
         (expr (cond
                ((and (not (s-blank? module))
                      (not (s-blank? function)))
                 (format "%s.%s" module function))
                ((not (s-blank? module))
                 module)
                (t
                 expr))))
    expr))

(defun alchemist-help--elixir-modules-to-list (str)
  (let* ((str (replace-regexp-in-string "^Elixir\\." "" str))
         (modules (split-string str))
         (modules (delete nil modules))
         (modules (cl-sort modules 'string-lessp :key 'downcase))
         (modules (-distinct modules)))
    modules))

(defun alchemist-help-minor-mode-key-binding-summary ()
  (interactive)
  (message
   (concat "[" (propertize "q" 'face 'alchemist-help-key-face)
           "]-quit ["
           (propertize "e" 'face 'alchemist-help-key-face)
           "]-search-at-point ["
           (propertize "m" 'face 'alchemist-help-key-face)
           "]-search-module ["
           (propertize "s" 'face 'alchemist-help-key-face)
           "]-search ["
           (propertize "h" 'face 'alchemist-help-key-face)
           "]-history ["
           (propertize "?" 'face 'alchemist-help-key-face)
           "]-keys")))

(defun alchemist-help--server-arguments (args)
  (if (and (not (equal major-mode 'alchemist-iex-mode))
           (not (bound-and-true-p alchemist-help-minor-mode)))
      (let* ((modules (alchemist-utils-prepare-modules-for-elixir
                       (alchemist-scope-all-modules))))
        (format "{ \"%s\", [ context: Elixir, imports: %s, aliases: [] ] }" args modules))
    (format "{ \"%s\", [ context: Elixir, imports: [], aliases: [] ] }" args)))

(defun alchemist-help--completion-server-arguments (args)
  "Build informations about the current context."
  (if (and (not (equal major-mode 'alchemist-iex-mode))
           (not (bound-and-true-p alchemist-help-minor-mode)))
      (let* ((modules (alchemist-utils-prepare-modules-for-elixir
                       (alchemist-scope-all-modules)))
             (aliases (alchemist-utils-prepare-aliases-for-elixir
                       (alchemist-scope-aliases))))
        (format "{ \"%s\", [ context: Elixir, imports: %s, aliases: %s ] }" args modules aliases))
    (format "{ \"%s\", [ context: Elixir, imports: [], aliases: [] ] }" args)))

(defun alchemist-help-complete-filter-output (_process output)
  (with-local-quit
    (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
    (if (alchemist-server-contains-end-marker-p output)
        (let* ((string (alchemist-server-prepare-filter-output alchemist-help-filter-output))
               (candidates (alchemist-complete--output-to-list
                            (ansi-color-filter-apply string)))
               (candidates (if (= (length candidates) 2)
                               nil
                             candidates)))
          (setq alchemist-help-filter-output nil)
          (if candidates
              (let* ((search (alchemist-complete--completing-prompt alchemist-help-current-search-text candidates)))
                (setq alchemist-help-current-search-text search)
                (alchemist-server-help (alchemist-help--server-arguments search) #'alchemist-help-filter-output))
            (alchemist-server-help (alchemist-help--server-arguments alchemist-help-current-search-text) #'alchemist-help-filter-output))))))

(defun alchemist-help-filter-output (_process output)
  (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
  (if (alchemist-server-contains-end-marker-p output)
      (let ((string (alchemist-server-prepare-filter-output alchemist-help-filter-output)))
        (alchemist-help-display-doc string)
        (setq alchemist-help-current-search-text nil)
        (setq alchemist-help-filter-output nil))))

(defun alchemist-help-modules-filter (_process output)
  (with-local-quit
    (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
    (if (alchemist-server-contains-end-marker-p output)
        (let* ((output (alchemist-server-prepare-filter-output alchemist-help-filter-output))
               (modules (alchemist-help--elixir-modules-to-list output))
               (search (completing-read
                        "Elixir help: "
                        modules
                        nil
                        nil
                        nil))
               (module (alchemist-scope-extract-module search))
               (function (alchemist-scope-extract-function search))
               (search (cond
                        ((and module function)
                         search)
                        ((and module
                              (not (string-match-p "[\/0-9]+$" module)))
                         (concat module "."))
                        (t
                         search))))
          (alchemist-help-lookup-doc (alchemist-utils-remove-dot-at-the-end search))))))

;; Public functions

(defun alchemist-help-search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor.

If the buffer local variable `mark-active' is non-nil,
the actively marked region will be used for passing to `alchemist-help'."
  (interactive)
  (if mark-active
      (alchemist-help--search-marked-region (region-beginning) (region-end))
      (alchemist-help--search-at-point)))

(defun alchemist-help-module ()
  "Load Elixir documentation for the module of the most recent SEARCH.

This is helpful to jump from the documentation of, say, the String.split/1
function to the documetation of the String module."
  (interactive)
  (let* ((current-search (car alchemist-help-search-history))
         (module (alchemist-scope-extract-module current-search))
         (module (alchemist-scope-alias-full-path module)))

    (if module
        (alchemist-help-lookup-doc (alchemist-help--prepare-search-expr module))
      (message "No module found"))))

(defvar alchemist-help-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "e") #'alchemist-help-search-at-point)
    (define-key map (kbd "m") #'alchemist-help-module)
    (define-key map (kbd "s") #'alchemist-help)
    (define-key map (kbd "h") #'alchemist-help-history)
    (define-key map (kbd "M-.") #'alchemist-goto-definition-at-point)
    (define-key map (kbd "?") #'alchemist-help-minor-mode-key-binding-summary)
    map)
  "Keymap for `alchemist-help-minor-mode'.")

(define-minor-mode alchemist-help-minor-mode
  "Minor mode for displaying elixir help."
  :group 'alchemist-help
  :keymap alchemist-help-minor-mode-map
  (cond (alchemist-help-minor-mode
         (setq buffer-read-only t))
        (t
         (setq buffer-read-only nil))))

(defun alchemist-help ()
  "Load Elixir documentation for SEARCH."
  (interactive)
  (setq alchemist-help-filter-output nil)
  (alchemist-server-info "{ :type, :modules }" #'alchemist-help-modules-filter))

(defun alchemist-help-history (search)
  "Load Elixir from the documentation history for SEARCH."
  (interactive
   (list
    (completing-read "Elixir help history: " alchemist-help-search-history nil nil "")))
  (alchemist-help-lookup-doc search))

(provide 'alchemist-help)

;;; alchemist-help.el ends here
