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

(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-scope)

;; Tell the byte compiler to assume that functions are defined
(eval-when-compile
  (declare-function alchemist-server-help-with-modules "alchemist-server.el")
  (declare-function alchemist-server-complete-candidates "alchemist-server.el")
  (declare-function alchemist-company-build-scope-arg "alchemist-company.el")
  (declare-function alchemist-company-build-server-arg "alchemist-company.el")
  (declare-function alchemist-server-help "alchemist-server.el")
  (declare-function alchemist-goto-definition-at-point "alchemist-goto.el"))

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

(defvar alchemist-help-filter-output nil)

;; Faces

(defface alchemist-help--key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  "Fontface for the letter keys in the summary."
  :group 'alchemist-help)

;; Private functions

(defun alchemist-help--exp-at-point ()
  "Return the expression under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9.?!:@")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9.?!:@")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun alchemist-help--execute (search)
  (setq alchemist-help-current-search-text search)
  (setq alchemist-help-filter-output nil)
  (if (not (alchemist-utils--empty-string-p search))
      (alchemist-server-complete-candidates
       (alchemist-company-build-server-arg search)
       #'alchemist-help-complete-filter-output)
    (message "No documentation for [%s] found." search)))

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
      (with-current-buffer alchemist-help-buffer-name
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
          (alchemist-help-minor-mode 1)))))))

(defun alchemist-help--search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor"
  (let* ((expr (alchemist-help--exp-at-point)))
    (alchemist-help--execute (alchemist-help--prepare-search-expr expr))))

(defun alchemist-help--search-marked-region (begin end)
  "Run `alchemist-help' with the marked region.
Argument BEGIN where the mark starts.
Argument END where the mark ends."
  (let ((expr (buffer-substring-no-properties begin end)))
    (alchemist-help--execute (alchemist-help--prepare-search-expr expr))))

(defun alchemist-help--prepare-search-expr (expr)
  (let* ((module (alchemist-scope-extract-module expr))
         (module (alchemist-scope-alias-full-path module))
         (module (if module module ""))
         (function (alchemist-scope-extract-function expr))
         (function (if function function ""))
         (expr (cond
                ((and (not (alchemist-utils--empty-string-p module))
                      (not (alchemist-utils--empty-string-p function)))
                 (format "%s.%s" module function))
                ((not (alchemist-utils--empty-string-p module))
                 module)
                (t
                 expr))))
    expr))

(defun alchemist-help--elixir-modules-to-list (str)
  (let* ((modules (split-string str))
         (modules (mapcar (lambda (m)
                            (when (string-match-p "Elixir\\." m)
                              (replace-regexp-in-string "Elixir\\." "" m))) modules))
         (modules (delete nil modules))
         (modules (cl-sort modules 'string-lessp :key 'downcase))
         (modules (delete-dups modules)))
    modules))


(defun alchemist-help-minor-mode-key-binding-summary ()
  (interactive)
  (message
   (concat "[" (propertize "q" 'face 'alchemist-help--key-face)
           "]-quit ["
           (propertize "e" 'face 'alchemist-help--key-face)
           "]-search-at-point ["
           (propertize "s" 'face 'alchemist-help--key-face)
           "]-search ["
           (propertize "h" 'face 'alchemist-help--key-face)
           "]-history ["
           (propertize "?" 'face 'alchemist-help--key-face)
           "]-keys")))

(defun alchemist-help-build-scope-arg (arg)
  "Build informations about the current context."
  (let* ((modules (alchemist-utils--prepare-modules-for-elixir
                   (alchemist-scope-all-modules))))
    (format "%s;%s" arg modules)))

(defun alchemist-help-build-server-arg (arg)
  (if (not (equal major-mode 'alchemist-iex-mode))
      (alchemist-help-build-scope-arg arg)
    (format "%s;[];" arg)))

(defun alchemist-help-complete-filter-output (_process output)
  (with-local-quit
    (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
    (if (string-match "END-OF-COMPLETE$" output)
        (let* ((string (apply #'concat (reverse alchemist-help-filter-output)))
               (string (replace-regexp-in-string "END-OF-COMPLETE$" "" string))
               (candidates (alchemist-complete--output-to-list
                            (alchemist--utils-clear-ansi-sequences string)))
               (candidates (if (= (length candidates) 2)
                               nil
                             candidates)))
          (setq alchemist-help-filter-output nil)
          (if candidates
              (let* ((search (alchemist-complete--completing-prompt alchemist-help-current-search-text candidates)))
                (setq alchemist-help-current-search-text search)
                (alchemist-server-help (alchemist-help-build-server-arg search) #'alchemist-help-filter-output))
            (alchemist-server-help (alchemist-help-build-server-arg alchemist-help-current-search-text) #'alchemist-help-filter-output))))))

(defun alchemist-help-filter-output (_process output)
  (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
  (if (string-match "END-OF-DOC$" output)
      (let* ((string (apply #'concat (reverse alchemist-help-filter-output)))
             (string (replace-regexp-in-string "END-OF-DOC$" "" string))
             (string (replace-regexp-in-string "\n+$" "" string)))
        (if (alchemist-utils--empty-string-p string)
            (message "No documentation for [%s] found." alchemist-help-current-search-text)
          (alchemist-help--initialize-buffer string)))))

(defun alchemist-help-modules-filter (_process output)
  (with-local-quit
    (setq alchemist-help-filter-output (cons output alchemist-help-filter-output))
    (if (string-match "END-OF-MODULES$" output)
        (let* ((output (apply #'concat (reverse alchemist-help-filter-output)))
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
                        (module
                         (concat module "."))
                        (t
                         search))))
          (alchemist-help--execute search)))))

;; Public functions

(defun alchemist-help-search-at-point ()
  "Search through `alchemist-help' with the expression under the cursor.

If the buffer local variable `mark-active' is non-nil,
the actively marked region will be used for passing to `alchemist-help'."
  (interactive)
  (if mark-active
      (alchemist-help--search-marked-region (region-beginning) (region-end))
      (alchemist-help--search-at-point)))

(defvar alchemist-help-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "e") #'alchemist-help-search-at-point)
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
  (setq alchemist-help-filter-output nil)
  (alchemist-server-help-with-modules #'alchemist-help-modules-filter))

(defun alchemist-help-history (search)
  "Load Elixir from the documentation history for SEARCH."
  (interactive
   (list
    (completing-read "Elixir help history: " alchemist-help-search-history nil nil "")))
  (alchemist-help--execute search))

;; Deprecated functions; these will get removed in v1.5.0
(defun alchemist-help-search-marked-region () (interactive)
       (alchemist-utils-deprecated-message "alchemist-help-search-marked-region" "alchemist-help-search-at-point"))

(provide 'alchemist-help)

;;; alchemist-help.el ends here
