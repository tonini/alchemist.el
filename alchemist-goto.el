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

(defun alchemist-goto-extract-function (code)
  "Extract function from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (and function
               (string-match-p "^[a-z]+" function))
      function)))

(defvar alchemist-goto-elixir-source "~/Projects/elixir/")

(defun alchemist-goto-elixir-source-relative-file (file)
  (string-match "\\(elixir-[0-9]+\\.[0-9]+\\.[0-9a-z-_]+\\/\\)" file)
  (substring-no-properties file (match-end 1)))

(defun alchemist-goto-build-elixir-source-file (file)
  (save-match-data
    (when (or (string-match "\\/\\(lib\\/elixir\\/lib\\)" file)
              (string-match "\\/\\(lib\\/iex\\/lib\\)" file)
              (string-match "\\/\\(lib\\/mix\\/lib\\)" file)
              (string-match "\\/\\(lib\\/eex\\/lib\\)" file)
              (string-match "\\/\\(lib\\/ex_unit\\/lib\\)" file)
              (string-match "\\/\\(lib\\/logger\\/lib\\)" file))
      (concat alchemist-goto-elixir-source
              (substring-no-properties file (match-beginning 1)))

      )
    )
  )

(defun alchemist-goto-elixir-core-source-file-p (file)
  (or (string-match-p "\\/lib\\/elixir\\/lib" file)
      (string-match-p "\\/lib\\/iex\\/lib" file)
      (string-match-p "\\/lib\\/mix\\/lib" file)
      (string-match-p "\\/lib\\/eex\\/lib" file)
      (string-match-p "\\/lib\\/ex_unit\\/lib" file)
      (string-match-p "\\/lib\\/logger\\/lib" file)))

(defun alchemist-goto-extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z]+" (car parts))
      (mapconcat 'concat parts "."))))

(defun alchemist-goto-definition-at-point ()
  "Return the expression under the cursor."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9A-z./?!:")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9A-z./?!:")
      (setq p2 (point))
      (alchemist-goto-open-definition (buffer-substring-no-properties p1 p2)))))

(defun alchemist-goto-open-definition (expr)
  (let ((module (alchemist-goto-extract-module expr))
        (function (alchemist-goto-extract-function expr)))
    (if module
        (progn
          (let ((source (alchemist-goto-get-module-source module)))
            (cond ((equal source nil)
                   (message "No source file available for: %s" expr))
                  ((file-exists-p source)
                   (progn
                     (find-file-other-window source)
                     (beginning-of-buffer)
                     (when function
                       (when (re-search-forward (format "^\s+\\(defp %s\(\\|def %s\(\\|defmacro %s\(\\)" function function function) nil t)
                         (goto-char (match-beginning 0))))))
                  ((alchemist-goto-elixir-core-source-file-p source)
                   (if alchemist-goto-elixir-source
                       (progn
                         (if (file-exists-p (alchemist-goto-build-elixir-source-file source))
                             (find-file-other-window (alchemist-goto-build-elixir-source-file source))
                           (message "File does not exists: %s" (alchemist-goto-build-elixir-source-file source)))

                         (beginning-of-buffer)
                         (when function
                           (when (re-search-forward (format "^\s+\\(defp %s\(\\|def %s\(\\|defmacro %s\(\\)" function function function) nil t)
                             (goto-char (match-beginning 0)))))
                     (message "No information about elixir source directory."))
                   )
                  (t (message "File does not exists: %s" source)))
            )
          )
      (message "Could not find source for module: %s" module))))

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
    (if (string-empty-p source)
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

(defun alchemist-goto-get-source-path ()
  (message "%s" (shell-command-to-string "elixir -e 'List.module_info[:exports] |> Enum.map &IO.inspect/1'")))

;; (alchemist-goto-get-module-source)

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
