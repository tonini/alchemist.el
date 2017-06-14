;;; alchemist-message.el --- Internal message functionality

;; Copyright © 2014-2017 Samuel Tonini

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

;; Internal message functionality.

;;; Code:

(defgroup alchemist-message nil
  "Internal message functionality."
  :prefix "alchemist-message-"
  :group 'alchemist)

;; Variables

(defvar alchemist-message--buffer-name "*alchemist message*")

(defun alchemist-message (message)
  (alchemist-message--initialize-buffer message))

(defun alchemist-message--initialize-buffer (message)
  (display-buffer (get-buffer-create alchemist-message--buffer-name))
  (with-current-buffer alchemist-message--buffer-name
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (insert message)
      (goto-char (point-min))
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode)
      (alchemist-message-mode 1))))

(define-minor-mode alchemist-message-mode
  "Minor mode for displaying alchemist messages"
  :group 'alchemist-message
  :lighter " alchemist-msg"
  :keymap '(("q" . quit-window)))

(provide 'alchemist-message)

;;; alchemist-message.el ends here
