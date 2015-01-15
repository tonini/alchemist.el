;;; alchemist-goto.el ---

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

;;

;;; Code:

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

(defun alchemist-goto-open-definition (expr)
  (let* ((module (alchemist-goto--extract-module expr))
         (function (alchemist-goto--extract-function expr)))
    (if module
        (progn
          (let* ((file (alchemist-goto-get-module-source module)))
            (cond ((equal file nil)
                   (message "No source file available."))
                  ((file-exists-p file)
                   (alchemist-goto--open-file file module function))
                  ((alchemist-goto--elixir-file-p file)
                   (let* ((elixir-source-file (alchemist-goto--build-elixir-ex-core-file file)))
                     (if (file-exists-p elixir-source-file)
                         (alchemist-goto--open-file elixir-source-file module function)
                       (message "File does not exists: %s" elixir-source-file))))
                  ((alchemist-goto--erlang-file-p file)
                   (let* ((elixir-source-file (alchemist-goto--build-elixir-erl-core-file file))
                          (erlang-source-file (alchemist-goto--build-erlang-core-file file)))
                     (cond ((file-exists-p elixir-source-file)
                            (alchemist-goto--open-file elixir-source-file module function))
                           ((file-exists-p erlang-source-file)
                            (alchemist-goto--open-file erlang-source-file module function))
                           (t
                            (message "Source file does not exists for:" module)))))
                  (t (message "File does not exists: %s" file)))))
      (message "Could not find source for module: %s" file))))

(defun alchemist-goto--elixir-file-p (file)
  (string-match-p  "\\.ex\\(s\\)?$" file))

(defun alchemist-goto--erlang-file-p (file)
  (string-match-p  "\\.erl$" file))

(defun alchemist-goto--open-file (file module function)
  (find-file-other-window file)
  (beginning-of-buffer)
  (cond ((alchemist-goto--elixir-file-p file)
         (if function
             (when (re-search-forward (format "^\s+\\(defp %s\(\\|def %s\(\\|defmacro %s\(\\)" function function function) nil t)
               (goto-char (match-beginning 0)))
           (when (re-search-forward (format "\\(defmodule %s\s+do\\)" module) nil t)
             (goto-char (match-beginning 0)))))
        ((alchemist-goto--erlang-file-p file)
         (if function
             (when (re-search-forward (format "\\(^%s\(\\)" function) nil t)
               (goto-char (match-beginning 0)))
           (when (re-search-forward (format "\\(^-module\(%s\)\\)" (substring module 1)) nil t)
             (goto-char (match-beginning 0)))))))

(defun alchemist-goto--runner ()
  (if (alchemist-project-p)
      (format "%s run --no-compile" alchemist-mix-command)
    alchemist-execute-command))

(defun alchemist-goto-get-module-source (module)
  (let* ((default-directory (if (alchemist-project-p)
                                (alchemist-project-root)
                              default-directory))
         (source (replace-regexp-in-string "\n" "" (alchemist--utils-clear-ansi-sequences
                                                    (shell-command-to-string (format "%s -e '%s'"
                                                                                     (alchemist-goto--runner)
                                                                                     (alchemist-goto--get-module-source module)))))))
    (if (string= source "")
        nil
      source)))

(defun alchemist-goto--get-module-source (module)
  (format "
defmodule Source do
  def find(module) do
    if Code.ensure_loaded?(module) do
      IO.puts source(module)
    end
  end

  defp source(module) do
    source = module.module_info(:compile)[:source]

    case source do
      nil -> nil
      source -> List.to_string(source)
    end
  end
end

Source.find(%s)" module))

(defun alchemist-goto-definition-at-point ()
  "Jump to the elixir expression definition at point."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9A-z./?!:")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9A-z./?!:")
      (setq p2 (point))
      (alchemist-goto-open-definition (buffer-substring-no-properties p1 p2)))))

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
