;;; alchemist.el --- Elixir tooling integration into emacs

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: languages, mix, elixir, elixirc, hex

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

;;; Installation:

;;    alchemist.el is available on both community maintained repositories - Marmalade and MELPA

;;    (add-to-list 'package-archives
;;                 '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;;    or
;;
;;    (add-to-list 'package-archives
;;                 '("melpa" . "http://melpa.org/packages/") t)
;;
;;    M-x package-install alchemist

;;   Manual Installation:
;;
;;    (add-to-list 'load-path "~/path/to/alchemist.el/")
;;    (require 'alchemist)
;;    (alchemist-mode 1)

;;; Usage:

;;     Add the following in your .emacs file:

;;    (require alchemist)
;;    (alchemist-mode 1)

;;; Code:

(require 'alchemist-utils)
(require 'alchemist-buffer)
(require 'alchemist-compile)
(require 'alchemist-execute)
(require 'alchemist-mix)

(defvar alchemist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a t") 'alchemist-mix-test)
    (define-key map (kbd "C-c a m t f") 'alchemist-mix-test-file)
    (define-key map (kbd "C-c a m t b") 'alchemist-mix-test-this-buffer)
    (define-key map (kbd "C-c a m t .") 'alchemist-mix-test-at-point)
    (define-key map (kbd "C-c a c c") 'alchemist-compile)
    (define-key map (kbd "C-c a c f") 'alchemist-compile-file)
    (define-key map (kbd "C-c a c b") 'alchemist-compile-this-buffer)
    (define-key map (kbd "C-c a e e") 'alchemist-execute)
    (define-key map (kbd "C-c a e f") 'alchemist-execute-file)
    (define-key map (kbd "C-c a e b") 'alchemist-execute-this-buffer)
    map)
  "The keymap used when `alchemist-mode' is active.")

;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}"
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :global t
  :keymap 'alchemist-mode-map)

(provide 'alchemist)

;;; alchemist.el ends here
