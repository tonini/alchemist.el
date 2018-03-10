;;; alchemist-complete.el --- Complete functionality for Elixir source code -*- lexical-binding: t -*-

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

;; Complete functionality for Elixir and Erlang source code.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'ansi-color)

(require 'company)
(require 'company-lsp)

(add-hook 'alchemist-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-lsp)))

(add-hook 'alchemist-iex-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-lsp)))

(provide 'alchemist-complete)

;;; alchemist-complete.el ends here
