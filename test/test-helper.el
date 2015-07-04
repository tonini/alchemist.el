;;; test-helper.el --- Test helper for test suites

;; Copyright © 2014-2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

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

(require 'f)

(defvar alchemist-test-path
  (f-parent (f-this-file)))

(defvar alchemist-root-path
  (f-parent alchemist-test-path))

(defvar alchemist-sandbox-path
  (file-name-as-directory (f-expand "sandbox" alchemist-test-path)))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory alchemist-sandbox-path))
     (when (f-dir? alchemist-sandbox-path)
       (f-delete alchemist-sandbox-path :force))
     (f-mkdir alchemist-sandbox-path)
     ,@body))

(defmacro with-current-variable (name value &rest body)
  "Evaluate BODY after temporarily set variable NAME with VALUE."
`(let ((,name ,value))
   ,@body))

(defmacro capture-message (&rest form)
  (declare (debug (&rest form))
           (indent 0))
  `(let ((start (make-marker))
         (message-buffer (get-buffer "*Messages*")))
     (with-current-buffer message-buffer
       (set-marker start (point-max)))
     (progn ,@form)
     (with-current-buffer message-buffer
       (buffer-substring start (point-max)))))

(defun wait(amount)
  (let* ((amount (/ (float amount) 4)))
    (sleep-for amount)
    (sleep-for amount)
    (sleep-for amount)
    (sleep-for amount)))

(defun delay (seconds callback)
  "Wait SECONDS, then run function CALLBACK."
  (declare (indent 1))
  (run-at-time seconds nil callback))

(add-to-list 'load-path alchemist-root-path)

(require 'alchemist)
(require 'ert)

(provide 'test-helper)

;;; test-helper.el ends here
