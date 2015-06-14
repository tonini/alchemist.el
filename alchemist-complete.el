;;; alchemist-complete.el --- Complete functionality for Elixir source code -*- lexical-binding: t -*-

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

;; Complete functionality for Elixir and Erlang source code.

;;; Code:

(defgroup alchemist-complete nil
  "Complete functionality for Elixir source code."
  :prefix "alchemist-complete-"
  :group 'alchemist)

(defun alchemist-complete--concat-prefix-with-functions (prefix functions &optional add-prefix)
  (let* ((prefix (mapconcat 'concat (butlast (split-string prefix "\\.") 1) "."))
         (candidates (mapcar (lambda (c) (concat prefix "." c)) (cdr functions))))
    (if add-prefix
        (push prefix candidates)
      candidates)))

(defun alchemist-complete--add-prefix-to-function (prefix function)
  (let* ((prefix (mapconcat 'concat (butlast (split-string prefix "\\.") 1) "."))
         (candidate (concat prefix "." function)))
    candidate))

(defun alchemist-complete--build-candidates (a-list)
  (let* ((search-term (car a-list))
         (candidates (if (string-match-p "^.+\/" search-term)
                         a-list
                       (cdr a-list)))
         (candidates (mapcar (lambda (f)
                               (let* ((candidate f)
                                      (meta (if (string-match-p "^.+/" f)
                                                (replace-regexp-in-string "^.+/" "/" f)
                                              "")))
                                 (cond
                                  ((and (string-match-p "^:" search-term)
                                        (not (string-match-p "\\.$" search-term)))
                                   (propertize (concat ":" candidate)))
                                  ((string-match-p "\\." search-term)
                                   (propertize (alchemist-complete--add-prefix-to-function search-term
                                                                                           (replace-regexp-in-string "/[0-9]$" "" candidate)) 'meta meta))
                                  (t (propertize (replace-regexp-in-string "/[0-9]$" "" candidate) 'meta meta)))))
                             candidates)))
    candidates))

(defun alchemist-complete--build-help-candidates (a-list)
  (let* ((search-term (car a-list))
         (candidates (cond ((> (alchemist-utils--count-char-in-str "\\." search-term) 1)
                            (let ((search (if (string-match-p "\\.[a-z0-9_\?!]+$" search-term)
                                              (list (replace-regexp-in-string "\\.[a-z0-9_\?!]+$" "" search-term))
                                            (list (replace-regexp-in-string "\\.$" "" search-term))))
                                  (candidates (mapcar (lambda (c)
                                                        (if (string-match-p "\\.[a-z0-9_\?!]+$" search-term)
                                                            (concat (replace-regexp-in-string "\\.[a-z0-9_\?!]+$" "." search-term) c)
                                                          (concat search-term c)))
                                                      (cdr a-list))))
                              (append search candidates)))
                            ((string-match-p "\\.$" search-term)
                            (alchemist-complete--concat-prefix-with-functions search-term a-list t))
                           ((string-match-p "\\.[a-z0-9_\?!]+$" search-term)
                            (alchemist-complete--concat-prefix-with-functions search-term a-list))
                           (t
                            a-list))))
    (delete-dups candidates)))

(defun alchemist-complete--output-to-list (output)
  (let* ((output (replace-regexp-in-string "^cmp:" "" output))
         (output (split-string output))
         (output (delete nil output)))
    output)
  )

(defun alchemist-complete--clear-buffer (buffer)
  "Clears the BUFFER from not used lines."
  (with-current-buffer buffer
    (delete-non-matching-lines "^cmp:" (point-min) (point-max))))

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
            (replace-regexp-in-string "\\.$" "" initial)))
          (t initial))))

(provide 'alchemist-complete)

;;; alchemist-complete.el ends here
