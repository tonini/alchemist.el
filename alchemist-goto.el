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

(defun alchemist-goto--extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z]+" (car parts))
      (mapconcat 'concat parts "."))))

(defun alchemist-goto--extract-function (code)
  "Extract function from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z]+" function))
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

(defun alchemist-goto--open-definition (expr)
  (let* ((module (alchemist-goto--extract-module expr))
         (module (if module module "AlchemistGoto"))
         (function (alchemist-goto--extract-function expr))
         (function (if function function "\"\""))
         (file (alchemist-goto--get-module-source module function)))
    (ring-insert find-tag-marker-ring (point-marker))
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
           (message "Don't know how to find: %s" expr)))))

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
    (when (re-search-forward (format "^\s+\\(defp?\s+%s\(\\|defmacrop?\s+%s\(\\)" function function function) nil t)
      (goto-char (match-beginning 0)))
    (when (re-search-forward (format "\\(defmodule %s\s+do\\)" module) nil t)
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
  def find(module, function) do
    cond do
      Code.ensure_loaded?(module) ->
        IO.puts source(module)
      List.keymember?(Kernel.module_info[:exports], function, 0) ->
        IO.puts source(Kernel)
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
end

Source.find(%s, :%s)" module function))

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
