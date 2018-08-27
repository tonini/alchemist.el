;;; alchemist-goto.el --- Functionality to jump modules and function definitions -*- lexical-binding: t -*-

;; Copyright © 2015 Samuel Tonini

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

;; Functionality to jump modules and function definitions

;;; Code:

(require 'xref)
(require 'etags)
(require 'lsp-imenu)
(require 'alchemist-elixir-ls)

(defalias 'alchemist-goto-definition-at-point '(lambda ()
                                                 (interactive)
                                                 (xref-find-definitions (alchemist-scope-expression))))
(defalias 'alchemist-goto-jump-back 'pop-tag-mark)

(defalias 'alchemist-goto-list-symbol-definitions 'imenu)

(provide 'alchemist-goto)

;;; alchemist-goto.el ends here
