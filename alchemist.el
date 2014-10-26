;;; alchemist.el --- Elixir tooling integration into emacs

;; Copyright © 2014 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 0.2.0
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

;;    alchemist.el is available on both community maintained repositories -
;;    Marmalade and MELPA.
;;
;;    (add-to-list 'package-archives
;;                 '("marmalade" . "http://marmalade-repo.org/packages/"))
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
(require 'alchemist-hooks)

;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode."
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :group 'alchemist
  (cond (alchemist-mode
         (alchemist-buffer--initalize-modeline))
        (t
         ;; Reset the mode-line
         (alchemist-buffer--reset-mode-line))))

(provide 'alchemist)

;;; alchemist.el ends here
