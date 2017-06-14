;;; alchemist-key.el --- Key prefix setup for Alchemist related key commands

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

;; Key prefix setup for Alchemist related key commands.

;;; Code:

(defgroup alchemist-key nil
  "Key prefix setup for Alchemist related key commands."
  :prefix "alchemist-key-"
  :group 'alchemist)

(defcustom alchemist-key-command-prefix (kbd "C-c a")
  "The prefix for Alchemist related key commands."
  :type 'string
  :group 'alchemist)

(provide 'alchemist-key)

;;; alchemist-key.el ends here
