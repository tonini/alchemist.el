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

(defun alchemist-goto-extract-module (code)
  "Extract module from CODE."
  (let* ((parts (split-string code "\\."))
         (function (car (last parts)))
         (case-fold-search nil))
    (when (string-match-p "^[a-z]+" function)
      (delete function parts))
    (unless (string-match-p "^[a-z]+" (car parts))
      (mapconcat 'concat parts "."))))

(alchemist-goto-extract-function ":gen_tcp.accept")

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
          (let ((source (alchemist--utils-clear-ansi-sequences
                         (alchemist-goto-get-module-source module))))
            (if (file-exists-p source)
                (find-file-other-window source)
              (message "File does not exists: %s" source))
            )
          )
      (message "Could not find source for module: %s" module))))

(defun alchemist-goto--runner ()
  (if (alchemist-project-p)
      (format "%s run --no-compile" alchemist-mix-command)
    alchemist-execute-command))

(defun alchemist-goto-get-module-source (module)
  (let ((default-directory (if (alchemist-project-p)
                               (alchemist-project-root)
                             default-directory)))
    (replace-regexp-in-string "\n" "" (alchemist--utils-clear-ansi-sequences
     (shell-command-to-string (format "%s -e '%s'"
                                      (alchemist-goto--runner)
                                      (alchemist-goto--get-module-source module)))))))

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
