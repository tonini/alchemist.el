;;; alchemist-eval.el ---

;; Copyright © 2014 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar alchemist-eval--eval-filename "alchemist-eval-tmp-eval-file.exs")

(defvar alchemist-eval--quoted-buffer-name "*elixir-quoted*")

(defun alchemist-eval--code-eval-string-command (file)
  (format "%s -e 'IO.puts inspect(elem(Code.eval_string(File.read!(\"%s\")), 0))'"
          alchemist-execute-command
          file))

(defun alchemist-eval--code-string-to-quoted-command (file)
  (format "%s -e 'IO.puts inspect(elem(Code.string_to_quoted(File.read!(\"%s\")), 1), pretty: true)'"
          alchemist-execute-command
          file))

(defun alchemist-eval--execute-elixir-with-code-eval-string (string)
  (with-temp-file alchemist-eval--eval-filename
    (insert string))
  (let ((output (shell-command-to-string (alchemist-eval--code-eval-string-command alchemist-eval--eval-filename))))
    (delete-file alchemist-eval--eval-filename)
    output))

(defun alchemist-eval--execute-elixir-with-code-string-to-quoted (string)
  (with-temp-file alchemist-eval--eval-filename
    (insert string))
  (let ((output (shell-command-to-string (alchemist-eval--code-string-to-quoted-command alchemist-eval--eval-filename))))
    (delete-file alchemist-eval--eval-filename)
    output))

(defun alchemist-eval--eval-string (string)
  (let ((output (alchemist-eval--execute-elixir-with-code-eval-string string)))
    (message output)))

(defun alchemist-eval--string-to-quoted (string)
  (let* ((output (alchemist-eval--execute-elixir-with-code-string-to-quoted string)))
    (alchemist-eval--quoted-initialize-buffer output)))

(defun alchemist-eval-eval-on-region (beg end)
  "Evaluate the Elixir code on the marked region.
Argument BEG Start of the region.
Argument END End of the region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let* ((region (buffer-substring-no-properties beg end)))
    (alchemist-eval--eval-string region)))

(defun alchemist-eval-eval-on-current-line ()
  "Evaluate the Elixir code on the current line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (alchemist-eval--eval-string current-line)))

(defun alchemist-eval-eval-on-current-buffer ()
  "Evaluate the Elixir code on the current buffer."
  (interactive)
  (let ((current-buffer (buffer-substring-no-properties (point-max) (point-min))))
    (alchemist-eval--eval-string current-buffer)))

(defun alchemist-eval-string-to-quoted-on-region (beg end)
  "Get the representation of the expression on the marked region.
Argument BEG Start of the region.
Argument END End of the region."
  (interactive (list (point) (mark)))
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))
  (let ((region (buffer-substring-no-properties beg end)))
    (alchemist-eval--string-to-quoted region)))

(defun alchemist-eval-string-to-quoted-on-current-line ()
  "Get the representation of the expression on the current line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (alchemist-eval--string-to-quoted current-line)))

(defun alchemist-eval--quoted-initialize-buffer (quoted)
  (pop-to-buffer alchemist-eval--quoted-buffer-name)
  (setq buffer-undo-list nil) ; Get rid of undo information from
                                        ; previous expansions
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Ignore undo information
    (erase-buffer)
    (insert quoted)
    (goto-char (point-min))
    (when (package-installed-p 'elixir-mode)
      (elixir-mode))
    (alchemist-eval-quoted-minor-mode 1)))

(define-minor-mode alchemist-eval-quoted-minor-mode
  "Minor mode for displaying elixir quoted expressions"
  :group 'alchemist-eval-quoted :lighter " quoted"
  :keymap '(("q" . quit-window))
  (setq buffer-read-only t))

(provide 'alchemist-eval)

;;; alchemist-eval.el ends here
