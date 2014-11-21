;;; alchemist-complete.el ---  -*- lexical-binding: t -*-

;; Copyright © 2014 Samuel Tonini

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

(defun alchemist-complete--output-to-list (string)
  (let* ((search-text (replace-regexp-in-string "\"" "" string))
         (search-text (replace-regexp-in-string "\\[" "" search-text))
         (search-text (replace-regexp-in-string "\\]" "" search-text))
         (search-text (replace-regexp-in-string "'" "" search-text))
         (search-text (replace-regexp-in-string "\n" "" search-text))
         (search-text (replace-regexp-in-string " " "" search-text)))
    (split-string search-text ",")))

(defun alchemist-complete--command (exp)
  (let* ((elixir-code (format "
defmodule Alchemist do
  def expand(exp) do
    {status, result, list } = IEx.Autocomplete.expand(Enum.reverse(exp))

    case { status, result, list } do
      { :yes, [], _ } -> List.insert_at(list, 0, exp)
      { :yes, _,  _ } -> expand(exp ++ result)
                  _t  -> exp
    end
  end
end

IO.inspect Alchemist.expand('%s')
" exp))
         (command (if (alchemist-project-p)
                      (format "%s --no-compile -e \"%s\"" alchemist-help-mix-run-command elixir-code)
                    (format "%s -e \"%s\"" alchemist-execute-command elixir-code))))
    (when (alchemist-project-p)
      (alchemist-project--establish-root-directory))
    command))

(defun alchemist-complete (exp callback)
  ""
  (interactive)
  (let* ((buffer (get-buffer-create "alchemist-complete-buffer"))
         (command (alchemist-complete--command exp))
         (proc (start-process-shell-command "alchemist-complete-proc" buffer command)))
    (set-process-sentinel proc (lambda (process signal)
                                 (cond
                                  ((equal signal "finished\n")
                                   (funcall callback (with-current-buffer (process-buffer process)
                                                       (buffer-substring (point-min) (point-max))))
                                   (kill-buffer (process-buffer process)))
                                  ('t
                                   (message "signal: %s" signal)
                                   (kill-buffer (process-buffer process))))))))

(defun alchemist-complete--completing-prompt (initial completing-collection)
  (let* ((search-term (when (> (length completing-collection) 0)
                        (car completing-collection)))
         (completing-collection (cdr completing-collection))
         (search-term (if (and (> (length completing-collection) 1)
                               (string-match-p ".\\.." search-term))
                          (concat (car (split-string search-term "\\.")) ".")
                        search-term))
         (completing-collection (if (and (equal 1 (length completing-collection))
                                         (string-match-p ".\\.." search-term))
                                    '()
                                  completing-collection))
         (completing-collection (if (string-match-p "\\.$" search-term)
                                    (mapcar (lambda (fn) (concat search-term fn)) completing-collection)
                                  completing-collection))
         (search-term (if (equal (length completing-collection) 1)
                          (car completing-collection)
                        search-term)))
    (cond  ((equal (length completing-collection) 1)
            (car completing-collection))
           (completing-collection
            (completing-read
             "Elixir help: "
             completing-collection
             nil
             nil
             initial))
           (t search-term))))

(provide 'alchemist-complete)

;;; alchemist-complete.el ends here
