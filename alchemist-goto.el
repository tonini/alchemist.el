;;; alchemist-goto.el --- Functionality to jump modules and function definitions

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

;; Functionality to jump modules and function definitions

;;; Code:

(require 'etags)

(defgroup alchemist-goto nil
  "Functionality to jump modules and function definitions."
  :prefix "alchemist-goto-"
  :group 'alchemist)

(defcustom alchemist-goto-erlang-source-dir ""
  "Path to the erlang source code."
  :type 'string
  :group 'alchemist-goto)

(defcustom alchemist-goto-elixir-source-dir ""
  "Path to the elixir source code."
  :type 'string
  :group 'alchemist-goto)

(defvar alchemist-goto--symbol-list '())
(defvar alchemist-goto--symbol-name-and-pos '())
(defvar alchemist-goto--symbol-list-bare '())
(defvar alchemist-goto--symbol-name-and-pos-bare '())

(defun alchemist-goto--extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z_\?!]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z_\?!]+" (car parts))
      (replace-regexp-in-string "\\.$" "" (mapconcat 'concat parts ".")))))

(defun alchemist-goto--extract-function (code)
  "Extract function from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z_\?!]+" function))
      function)))

(defun alchemist-goto--build-elixir-ex-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/lib\\)\\/.+\.ex$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-elixir-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--build-elixir-erl-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-elixir-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--build-erlang-core-file (file)
  (when (string-match "\\/\\(lib\\/.+\\/src\\)\\/.+\.erl$" file)
    (let* ((file (substring-no-properties file (match-beginning 1)))
           (source-directory (expand-file-name alchemist-goto-erlang-source-dir)))
      (concat source-directory file))))

(defun alchemist-goto--elixir-file-p (file)
  (string-match-p  "\\.ex\\(s\\)?$" file))

(defun alchemist-goto--erlang-file-p (file)
  (string-match-p  "\\.erl$" file))

(defun alchemist-goto--get-full-path-of-alias (module)
  (let* ((aliases (mapcar (lambda (m)
                            (when (string= module (car (cdr m)))
                              (car m))) (alchemist-goto--alises-of-current-buffer)))
         (aliases (delete nil aliases)))
    (if aliases
        (car aliases)
      module)))

(defun alchemist-goto--string-at-point-p ()
  "Return non-nil if cursor is at a string."
  (or (and (nth 3 (parse-partial-sexp 1 (point)))
           (nth 8 (parse-partial-sexp 1 (point))))
      (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")
           (match-beginning 0))))

(defun alchemist-goto--symbol-definition-p (symbol)
  (alchemist-goto--fetch-symbol-definitions)
  (if (member symbol alchemist-goto--symbol-list-bare)
      t
    nil))

(defun alchemist-goto--goto-symbol (symbol)
  (let ((position (cdr (assoc symbol alchemist-goto--symbol-name-and-pos-bare))))
    (goto-char (if (overlayp position) (overlay-start position) position))))

(defun alchemist-goto-list-symbol-definitions ()
  "List all symbol definitions in the current file like functions/macros/modules.

It will jump to the position of the symbol definition after selection."
  (interactive)
  (alchemist-goto--fetch-symbol-definitions)
  (ring-insert find-tag-marker-ring (point-marker))
  (let* ((selected-def (completing-read "Symbol definitions:" alchemist-goto--symbol-list))
        (position (cdr (assoc selected-def alchemist-goto--symbol-name-and-pos))))
    (goto-char (if (overlayp position) (overlay-start position) position))))

(defun alchemist-goto--fetch-symbol-definitions ()
  (alchemist-goto--search-for-symbols "^\\s-*\\(defp?\\|defmacrop?\\|defmodule\\)\s.*"))

(defface alchemist-goto--def-face
  '((t (:inherit font-lock-constant-face)))
  ""
  :group 'alchemist-goto)

(defface alchemist-goto--name-face
  '((t (:bold t)))
  ""
  :group 'alchemist-goto)

(defvar alchemist-goto--symbol-def-extract-regex
  "^\\s-*\\(defp?\\|defmacrop?\\|defmodule\\)[ \n\t]+\\([a-z_\?!]+\\)\\(.*\\)\\(do\\|\n\\)?$")

(defun alchemist-goto--extract-symbol (str)
  (save-match-data
    (when (string-match alchemist-goto--symbol-def-extract-regex str)
      (let ((type (substring str (match-beginning 1) (match-end 1)))
            (name (substring str (match-beginning 2) (match-end 2)))
            (arguments (substring str (match-beginning 3) (match-end 3))))
        (concat
         (propertize type
                     'face 'alchemist-goto--def-face)
         " "
         (propertize name
                     'face 'alchemist-goto--name-face)
         (replace-regexp-in-string " do:.*$" "" (replace-regexp-in-string " do$" "" arguments)))))))

(defun alchemist-goto--extract-symbol-bare (str)
  (save-match-data
    (when (string-match alchemist-goto--symbol-def-extract-regex str)
      (let ((type (substring str (match-beginning 1) (match-end 1)))
            (name (substring str (match-beginning 2) (match-end 2)))
            (arguments (substring str (match-beginning 3) (match-end 3))))
        name))))

(defun alchemist-goto--get-symbol-from-position (position)
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char position)
      (end-of-line)
      (let* ((end-position (point))
             (line (buffer-substring-no-properties position end-position)))
        (alchemist-goto--extract-symbol line)))))

(defun alchemist-goto--get-symbol-from-position-bare (position)
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char position)
      (end-of-line)
      (let* ((end-position (point))
             (line (buffer-substring-no-properties position end-position)))
        (alchemist-goto--extract-symbol-bare line)))))


(defun alchemist-goto--search-for-symbols (regex)
  (setq alchemist-goto--symbol-list '())
  (setq alchemist-goto--symbol-name-and-pos '())
  (with-current-buffer (buffer-name)
    (save-excursion
      (goto-char (point-max))
      (goto-char (point-min))
      (let ()
        (save-match-data
          (while (re-search-forward regex nil t)
            (when (not (alchemist-goto--string-at-point-p))
              (when (alchemist-goto--get-symbol-from-position (car (match-data)))
                (let* ((position (car (match-data)))
                       (symbol (alchemist-goto--get-symbol-from-position position))
                       (symbol-bare (alchemist-goto--get-symbol-from-position-bare position)))
                  (setq alchemist-goto--symbol-list (append alchemist-goto--symbol-list (list symbol)))
                  (setq alchemist-goto--symbol-name-and-pos (append alchemist-goto--symbol-name-and-pos (list (cons symbol position))))
                  (setq alchemist-goto--symbol-list-bare (append alchemist-goto--symbol-list-bare (list symbol-bare)))
                  (setq alchemist-goto--symbol-name-and-pos-bare (append alchemist-goto--symbol-name-and-pos-bare (list (cons symbol-bare position)))))))))))))

(defun alchemist-goto--open-definition (expr)
  (let* ((module (alchemist-goto--extract-module expr))
         (module (alchemist-goto--get-full-path-of-alias module))
         (module (if module module "nil"))
         (function (alchemist-goto--extract-function expr))
         (function (if function function "\"\"")))
    (ring-insert find-tag-marker-ring (point-marker))
    (cond
     ((and (string-equal module "nil")
           (string-equal major-mode "elixir-mode")
           (alchemist-goto--symbol-definition-p function))
      (alchemist-goto--goto-symbol function))
     (t (let* ((file (alchemist-goto--get-module-source module function)))
          (cond ((equal file nil)
                 (message "Don't know how to find: %s" expr))
                ((file-exists-p file)
                 (alchemist-goto--open-file file module function))
                ((alchemist-goto--elixir-file-p file)
                 (let* ((elixir-source-file (alchemist-goto--build-elixir-ex-core-file file)))
                   (if (file-exists-p elixir-source-file)
                       (alchemist-goto--open-file elixir-source-file module function)
                     (message "Don't know how to find: %s" expr))))
                ((alchemist-goto--erlang-file-p file)
                 (let* ((elixir-source-file (alchemist-goto--build-elixir-erl-core-file file))
                        (erlang-source-file (alchemist-goto--build-erlang-core-file file)))
                   (cond ((file-exists-p elixir-source-file)
                          (alchemist-goto--open-file elixir-source-file module function))
                         ((file-exists-p erlang-source-file)
                          (alchemist-goto--open-file erlang-source-file module function))
                         (t
                          (message "Don't know how to find: %s" expr)))))
                (t
                 (pop-tag-mark)
                 (message "Don't know how to find: %s" expr))))))))

(defun alchemist-goto--open-file (file module function)
  (let* ((buf (find-file-noselect file)))
    (switch-to-buffer buf)
    (beginning-of-buffer)
    (cond ((alchemist-goto--elixir-file-p file)
           (alchemist-goto--jump-to-elixir-source module function))
          ((alchemist-goto--erlang-file-p file)
           (alchemist-goto--jump-to-erlang-source module function)))))

(defun alchemist-goto--jump-to-elixir-source (module function)
  (let ((function (replace-regexp-in-string "\?" "\\?" function)))
    (when (re-search-forward (format "^\s+\\(defp?\s+%s\(\\|defmacrop?\s+%s\(\\)" function function) nil t)
      (goto-char (match-beginning 0)))
    (when (re-search-forward (format "\\(defmodule\\|defimpl\\|defprotocol\\)\s+%s\s+do" module) nil t)
      (goto-char (match-beginning 0)))))

(defun alchemist-goto--jump-to-erlang-source (module function)
  (when (re-search-forward (format "\\(^%s\(\\)" function) nil t)
    (goto-char (match-beginning 0)))
  (when (re-search-forward (format "\\(^-module\(%s\)\\)" (substring module 1)) nil t)
    (goto-char (match-beginning 0))))

(defun alchemist-goto--clear-output (output)
  (let* ((output (replace-regexp-in-string "source-file-path:" "" output))
         (output (replace-regexp-in-string "\n" "" output))
         (output (alchemist--utils-clear-ansi-sequences output))
         (output (if (string= output "") nil output)))
    output))

(defun alchemist-goto--debug-message (output)
  (alchemist-message (format "== ALCHEMIST GOTO FAILED ==\n== OUTPUT BEGIN:\n%s== OUTPUT END:"
                             output)))

(defun alchemist-goto--report-errors (output)
  (when (and (not (string-match-p "source-file-path:" output))
             (not (string= (alchemist--utils-clear-ansi-sequences
                            (replace-regexp-in-string "\n" "" output)) "")))
    (when alchemist-complete-debug-mode
      (alchemist-goto--debug-message output))))

(defun alchemist-goto--runner ()
  (if (alchemist-project-p)
      (format "%s run --no-compile" alchemist-mix-command)
    alchemist-execute-command))

(defun alchemist-goto--get-module-source (module function)
  (let* ((default-directory (if (alchemist-project-p)
                                (alchemist-project-root)
                              default-directory))
         (source-file (shell-command-to-string (format "%s -e '%s'"
                                                       (alchemist-goto--runner)
                                                       (alchemist-goto--get-module-source-code module function)))))
    (alchemist-goto--report-errors source-file)
    (alchemist-goto--clear-output source-file)))

(defun alchemist-goto--get-module-source-code (module function)
  (format "
defmodule Source do

  def find(nil, function) do
    cond do
      List.keymember?(get_module_funs(Kernel), function, 0) ->
        IO.puts source(Kernel)
      List.keymember?(get_module_funs(Kernel.SpecialForms), function, 0) ->
        IO.puts source(Kernel.SpecialForms)
      true ->
        IO.puts \"\"
    end
  end

  def find(module, function) do
    cond do
      Code.ensure_loaded?(module) ->
        IO.puts source(module)
      List.keymember?(Kernel.module_info[:exports], function, 0) ->
        IO.puts source(Kernel)
      List.keymember?(Kernel.SpecialForms.module_info[:exports], function, 0) ->
        IO.puts source(Kernel.SpecialForms)
      true ->
        IO.puts \"\"
    end
  end

  defp source(module) do
    source = module.module_info(:compile)[:source]

    case source do
      nil -> nil
      source -> \"source-file-path:\" <> List.to_string(source)
    end
  end

  defp get_module_funs(mod) do
    if function_exported?(mod, :__info__, 1) do
      if docs = Code.get_docs(mod, :docs) do
        for {tuple, _line, _kind, _sign, doc} <- docs, doc != false, do: tuple
      else
        (mod.__info__(:functions) -- [__info__: 1]) ++ mod.__info__(:macros)
      end
    else
      mod.module_info(:exports)
    end
  end
end

Source.find(%s, :%s)" module function))

(defun alchemist-goto--alises-of-current-buffer ()
  (let* ((aliases '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\s+alias\s+\\([-_A-Za-z0-9,\.\?!\]+\\)\\(\s*,\s*as:\s*\\)?\\([-_A-Za-z0-9,\.\?!\]+\\)?\n" nil t)
        (let* ((alias (match-string 1))
               (as (if (match-string 3) (match-string 3) nil))
               (as (if as as (car (last (split-string alias "\\."))))))
          (setq aliases (append aliases (list (list alias as)))))))
    aliases))

(defun alchemist-goto-definition-at-point ()
  "Jump to the elixir expression definition at point."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "-_A-Za-z0-9.?!:")
    (setq p1 (point))
    (skip-chars-forward "-_A-Za-z0-9.?!:")
    (setq p2 (point))
    (alchemist-goto--open-definition (buffer-substring-no-properties p1 p2))))

(defalias 'alchemist-goto-jump-back 'pop-tag-mark)

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
