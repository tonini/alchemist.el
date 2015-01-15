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

;; Functionality for Elixir documentation lookup

;;; Code:

(defgroup alchemist-help nil
  "Functionality for Elixir documentation lookup."
  :prefix "alchemist-help-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-help-ansi-color-docs t
  "If t, `alchemist-help' will present ansi colored documentation."
  :type 'boolean
  :group 'alchemist-help)

(defcustom alchemist-help-buffer-name "*elixir help*"
  "Name of the Elixir help buffer."
  :type 'string
  :group 'alchemist-help)

(defvar alchemist-help-mix-run-command "mix run"
  "The shell command for 'mix run'.")

(defvar alchemist-help-search-history '()
  "Storage for the search history.")

(defvar alchemist-help-current-search-text '()
  "Stores the current search.")

;; Faces

(defface alchemist-help--key-face
  '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
  "Fontface for the letter keys in the summary."
  :group 'alchemist-help)

(defun alchemist-help--load-ansi-color-setting ()
  (let ((config (gethash "ansi-color-docs" (alchemist-project-config))))
    (if config
        (intern config)
      alchemist-help-ansi-color-docs)))

(defun alchemist-help--exp-at-point ()
  "Return the expression under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9.?!:")
      (setq p1 (point))
      (skip-chars-forward "-_A-Za-z0-9.?!:")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun alchemist-help--start-help-process (exp callback)
  (let* ((buffer (get-buffer-create "alchemist-help-buffer"))
         (command (alchemist-help--eval-string-command (alchemist-help--build-code-for-search exp)))
         (proc (start-process-shell-command "alchemist-help-proc" buffer command)))
    (set-process-sentinel proc (lambda (process signal)
                                 (when (equal signal "finished\n")
                                   (funcall callback (alchemist-utils--get-buffer-content (process-buffer process))))
                                 (alchemist-utils--erase-buffer (process-buffer process))))))

(defun alchemist-help--execute (search)
  (let ((last-directory default-directory)
        (last-buffer (current-buffer)))
    (alchemist-complete search (lambda (candidates)
                                 (if candidates
                                     (let* ((search (alchemist-complete--completing-prompt search candidates)))
                                       (setq alchemist-help-current-search-text search)
                                       (alchemist-help--start-help-process search (lambda (output)
                                                                                    (alchemist-help--initialize-buffer output)
                                                                                    (with-current-buffer last-buffer
                                                                                      (cd last-directory)))))
                                   (message "No documentation found for '%s'" search))))))

(defun alchemist-help--execute-without-complete (search)
  (setq alchemist-help-current-search-text search)
  (let ((last-directory default-directory)
        (last-buffer (current-buffer)))
    (alchemist-help--start-help-process search (lambda (output)
                                                 (alchemist-help--initialize-buffer output)
                                                 (with-current-buffer last-buffer
                                                   (cd last-directory))))))

(defun alchemist-help--build-code-for-search (string)
  (format "import IEx.Helpers

Application.put_env(:iex, :colors, [enabled: %s])

h(%s)" (if (alchemist-help--load-ansi-color-setting) "true" "false") string))

(defun alchemist-help--eval-string-command (string)
  (when (alchemist-project-p)
    (alchemist-project--establish-root-directory))
  (let* ((compile-option (if (and (alchemist-project-p)
                                  (alchemist-project--load-compile-when-needed-setting))
                             ""
                           "--no-compile"))
         (command (if (alchemist-project-p)
                      (format "%s %s -e \"%s\"" alchemist-help-mix-run-command compile-option string)
                    (format "%s -e \"%s\"" alchemist-execute-command string))))
    command))

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
  (pop-to-buffer alchemist-help-buffer-name)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (cond ((alchemist-help--bad-search-output-p content)
           (message (propertize
                     (format "No documentation for [ %s ] found." alchemist-help-current-search-text)
                     'face 'alchemist-help--key-face)))
          (t
           (erase-buffer)
           (insert content)
           (unless (memq 'alchemist-help-current-search-text alchemist-help-search-history)
             (add-to-list 'alchemist-help-search-history alchemist-help-current-search-text))))
    (delete-matching-lines "do not show this result in output" (point-min) (point-max))
    (delete-matching-lines "^Compiled lib\\/" (point-min) (point-max))
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
    modules)
  )

(defun alchemist-help--get-modules ()
  (let* ((elixir-code "
defmodule AlchemistModule do
  def get_modules do
    modules = Enum.map(:code.all_loaded, fn({m, _}) -> Atom.to_string(m) end)

    if :code.get_mode() === :interactive do
      modules ++ get_modules_from_applications()
    else
      modules
    end
  end

  defp get_modules_from_applications do
    for {app, _, _} <- :application.loaded_applications,
        {_, modules} = :application.get_key(app, :modules),
             module <- modules,
             has_doc = Code.get_docs(module, :moduledoc), elem(has_doc, 1) do
      Atom.to_string(module)
    end
  end
end

AlchemistModule.get_modules |> Enum.map &IO.puts/1
")
         (command (if (alchemist-project-p)
                      (format "%s -e \"%s\"" alchemist-help-mix-run-command elixir-code)
                    (format "%s -e \"%s\"" alchemist-execute-command elixir-code))))
    (when (alchemist-project-p)

      (alchemist-project--establish-root-directory))
    (alchemist-help--elixir-modules-to-list (shell-command-to-string command))))

(define-minor-mode alchemist-help-minor-mode
  "Minor mode for displaying elixir help."
  :group 'alchemist-help
  :keymap '(("q" . quit-window)
            ("e" . alchemist-help-search-at-point)
            ("m" . alchemist-help-search-marked-region)
            ("s" . alchemist-help)
            ("h" . alchemist-help-history)
            ("?" . alchemist-help-minor-mode-key-binding-summary)))

(defun alchemist-help (search)
  "Load Elixir documentation for SEARCH."
  (interactive
   (list (completing-read
          "Elixir help: "
          (alchemist-help--get-modules)
          nil
          nil
          nil)))
  (alchemist-help--execute (if (string-match-p "\\.$" search)
                               search
                             (concat search "."))))

(defun alchemist-help-history (search)
  "Load Elixir from the documentation history for SEARCH."
  (interactive
   (list
    (completing-read "Elixir help history: " alchemist-help-search-history nil nil "")))
  (alchemist-help--execute-without-complete search))

(provide 'alchemist-help)

;;; alchemist-help.el ends here
