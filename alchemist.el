;;; alchemist.el --- Elixir tooling integration into emacs

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 0.1.0
;; Package-Requires:
;; Keywords: mix, elixir, elixirc, hex

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

;;; Installation:

;;    alchemist.el is available on both community maintained repositories - Marmalade and MELPA

;;    (add-to-list 'package-archives
;;                 '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;;    or
;;
;;    (add-to-list 'package-archives
;;                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode.

When alchemist mode is enabled, the follwing elixir modes will be loaded:
* alchemist-buffer
* alchemist-compile
* alchemist-execute
* alchemist-eval
* alchemist-mix"
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :group 'alchemist
  (cond (alchemist-mode
         (require 'alchemist-utils)
         (require 'alchemist-buffer)
         (require 'alchemist-compile)
         (require 'alchemist-execute)
         (require 'alchemist-eval)
         (require 'alchemist-mix))))

(provide 'alchemist)

;;; alchemist.el ends here
