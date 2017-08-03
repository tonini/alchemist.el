;;; alchemist-phoenix.el --- Minor mode for the Phoenix web framework

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

;; Minor mode for the Phoenix web framework

;;; Code:

(require 'alchemist-key)
(require 'alchemist-project)

(defgroup alchemist-phoenix nil
  "Minor mode for the Phoenix web framework."
  :prefix "alchemist-phoenix-"
  :group 'alchemist)

;;;###autoload
(defun alchemist-phoenix-project-p ()
  "Return non-nil if `default-directory' is inside a Phoenix project."
  (and (alchemist-project-p)
       (file-directory-p (concat (alchemist-project-root) "web"))))

(defun alchemist-phoenix-find-dir (directory)
  (unless (alchemist-phoenix-project-p)
    (error "Could not find a Phoenix Mix project root."))
  (alchemist-file-find-files (alchemist-project-root) directory))

(defun alchemist-phoenix-find-web ()
  (interactive)
  (alchemist-phoenix-find-dir "web"))

(defun alchemist-phoenix-find-views ()
  (interactive)
  (alchemist-phoenix-find-dir "web/views"))

(defun alchemist-phoenix-find-controllers ()
  (interactive)
  (alchemist-phoenix-find-dir "web/controllers"))

(defun alchemist-phoenix-find-channels ()
  (interactive)
  (alchemist-phoenix-find-dir "web/channels"))

(defun alchemist-phoenix-find-templates ()
  (interactive)
  (alchemist-phoenix-find-dir "web/templates"))

(defun alchemist-phoenix-find-models ()
  (interactive)
  (alchemist-phoenix-find-dir "web/models"))

(defun alchemist-phoenix-find-static ()
  (interactive)
  (alchemist-phoenix-find-dir "web/static"))

(defun alchemist-phoenix-routes (&optional prefix)
  (interactive)
  "Run the Mix task 'phoenix.routes' and list all available Phoenix routes."
  (alchemist-mix-execute '("phoenix.routes") prefix))

(defun alchemist-phoenix-router ()
  "Open the 'router.ex' file from 'web' directory."
  (interactive)
  (unless (alchemist-phoenix-project-p)
    (error "Could not find an Phoenix Mix project root."))
  (find-file (concat (alchemist-project-root) "web/router.ex")))

(defvar alchemist-phoenix-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n w") #'alchemist-phoenix-find-web)
    (define-key map (kbd "n v") #'alchemist-phoenix-find-views)
    (define-key map (kbd "n c") #'alchemist-phoenix-find-controllers)
    (define-key map (kbd "n l") #'alchemist-phoenix-find-channels)
    (define-key map (kbd "n t") #'alchemist-phoenix-find-templates)
    (define-key map (kbd "n m") #'alchemist-phoenix-find-models)
    (define-key map (kbd "n s") #'alchemist-phoenix-find-static)
    (define-key map (kbd "n r") #'alchemist-phoenix-router)
    (define-key map (kbd "n R") #'alchemist-phoenix-routes)
    map)
  "Keymap for Alchemist Phoenix commands after `alchemist-key-command-prefix'.")
(fset 'alchemist-phoenix-command-map alchemist-phoenix-command-map)

(defvar alchemist-phoenix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map alchemist-key-command-prefix 'alchemist-phoenix-command-map)
    map)
  "Keymap for Alchemist Phoenix minor mode.")

(easy-menu-define alchemist-mode-menu alchemist-phoenix-mode-map
  "Menu for Alchemist-Phoenix mode."
  '("Phoenix"
    ("Directory lookup"
     ["Lookup 'web' " alchemist-phoenix-find-web]
     ["Lookup 'web/views' " alchemist-phoenix-find-views]
     ["Lookup 'web/controllers' " alchemist-phoenix-find-controllers]
     ["Lookup 'web/channels' " alchemist-phoenix-find-channels]
     ["Lookup 'web/templates' " alchemist-phoenix-find-templates]
     ["Lookup 'web/models' " alchemist-phoenix-find-models]
     ["Lookup 'web/static'" alchemist-phoenix-find-static])
    ("Mix tasks"
     ["Run 'phoenix.routes'" alchemist-phoenix-routes])
    ["Open the 'router.ex' file" alchemist-phoenix-router]))

;;;###autoload
(define-minor-mode alchemist-phoenix-mode
  "Minor mode for Elixir Phoenix web framework projects.

The following commands are available:

\\{alchemist-phoenix-mode-map}"
  :lighter " alchemist-phoenix"
  :keymap alchemist-phoenix-mode-map
  :group 'alchemist)

;;;###autoload
(defun alchemist-phoenix-enable-mode ()
  (when (alchemist-phoenix-project-p)
    (alchemist-phoenix-mode)))

;;;###autoload
(dolist (hook '(alchemist-mode-hook))
  (add-hook hook 'alchemist-phoenix-enable-mode))

(provide 'alchemist-phoenix)

;;; alchemist-phoenix.el ends here
