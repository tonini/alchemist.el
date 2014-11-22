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


(defun alchemist-complete--build-candidates (a-list)
  (let* ((search-term (car a-list))
         (candidates (cdr a-list))
         (candidates (mapcar (lambda (c) (replace-regexp-in-string "/[0-9]$" "" c)) a-list))
         (candidates (cond ((string-match-p "\\.$" search-term)
                            (let* ((candidates (mapcar (lambda (c) (concat search-term c)) (cdr candidates)))
                                   (search-term (replace-regexp-in-string "\\.$" "" search-term)))
                              candidates))
                           ((string-match-p "\\..+" search-term)
                            (let* ((search-term (mapconcat 'concat (butlast (split-string search-term "\\.") 1) "."))
                                   (candidates (mapcar (lambda (c) (concat search-term "." c)) (cdr candidates))))
                              candidates))
                           (t (cdr candidates))))
         (candidates (delete-dups candidates))
         )
    candidates))

(defun alchemist-complete--build-help-candidates (a-list)
  (let* ((search-term (car a-list))
         (candidates (cond ((string-match-p "\\.$" search-term)
                            (let* ((candidates (mapcar (lambda (c) (concat search-term c)) (cdr a-list)))
                                   (search-term (replace-regexp-in-string "\\.$" "" search-term)))
                              (push search-term candidates)))
                           ((string-match-p "\\..+" search-term)
                            (let* ((search-term (mapconcat 'concat (butlast (split-string search-term "\\.") 1) "."))
                                   (candidates (mapcar (lambda (c) (concat search-term "." c)) (cdr a-list))))
                              candidates))
                           (t (cdr a-list)))))
    (delete-dups candidates)))

(defun alchemist-complete--elixir-output-to-list (output)
  (let* ((output (replace-regexp-in-string "\"" "" output))
         (output (replace-regexp-in-string "\\[" "" output))
         (output (replace-regexp-in-string "\\]" "" output))
         (output (replace-regexp-in-string "'" "" output))
         (output (replace-regexp-in-string "\n" "" output))
         (output (replace-regexp-in-string " " "" output))
         (a-list (split-string output ",")))
    a-list))

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
  (let* ((buffer (get-buffer-create "alchemist-complete-buffer"))
         (command (alchemist-complete--command exp))
         (proc (start-process-shell-command "alchemist-complete-proc" buffer command)))
    (set-process-sentinel proc (lambda (process signal)
                                 (cond
                                  ((equal signal "finished\n")
                                   (let ((output (alchemist-complete--elixir-output-to-list (with-current-buffer (process-buffer process)
                                                                                              (set-buffer-modified-p nil)
                                                                                              (buffer-substring (point-min) (point-max))))))
                                     (funcall callback output)
                                     (kill-buffer (process-buffer process))))
                                  ('t
                                   (with-current-buffer (process-buffer process)
                                     (set-buffer-modified-p nil))
                                   (kill-buffer (process-buffer process))
                                   ))))))

(defun alchemist-complete-candidates (exp callback)
  (let* ((buffer (get-buffer-create "alchemist-complete-buffer"))
         (command (alchemist-complete--command exp))
         (proc (start-process-shell-command "alchemist-complete-proc" buffer command)))
    (set-process-sentinel proc (lambda (process signal)
                                 (cond
                                  ((equal signal "finished\n")
                                   (let* ((output (alchemist-complete--elixir-output-to-list (with-current-buffer (process-buffer process)
                                                                                               (set-buffer-modified-p nil)
                                                                                               (buffer-substring (point-min) (point-max)))))
                                          (candidates (alchemist-complete--build-candidates output)))
                                     (funcall callback candidates)
                                     (kill-buffer (process-buffer process))))
                                  ('t
                                   (with-current-buffer (process-buffer process)
                                     (set-buffer-modified-p nil))
                                   (kill-buffer (process-buffer process))
                                   ))))))

(defun alchemist-complete--completing-prompt (initial completing-collection)
  (let* ((completing-collection (alchemist-complete--build-help-candidates completing-collection)))
    (cond ((equal (length completing-collection) 1)
           (car completing-collection))
          (completing-collection
           (completing-read
            "Elixir help: "
            completing-collection
            nil
            nil
            initial))
          (t initial))))

(provide 'alchemist-complete)

;;; alchemist-complete.el ends here
