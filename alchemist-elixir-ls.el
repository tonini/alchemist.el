;;; alchemist-elixir-ls.el --- Glue between emacs and a LSP server

;; Copyright © 2018 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>

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

;; Microsoft's Language Server Protocol is a spec for communicating between an
;; editor and a project-analyzing server.
;; This code provides the emacs-side configuration to connect to the provided
;; server.

;;; Code:

(require 'alchemist-project)
(require 'lsp-mode)
(require 'lsp-imenu)

(defconst alchemist-server-root-path
  (concat (file-name-directory load-file-name) "elixir-ls/"))

(defvar alchemist--project-settings nil
  "Where alchemist keeps its project-level settings")

(lsp-define-stdio-client lsp-elixir-mode
                         "elixir"
                         (lambda () (alchemist-project-root-or-default-dir))
                         (alchemist--lsp-server-path-for-current-project))

(defun alchemist--lsp-server-path-for-current-project ()
  `(,(concat alchemist-server-root-path
           "erl"
           (alchemist--server-erlang-version (alchemist-project-root-or-default-dir))
           "/"
           "language_server"
           "."
           (alchemist--server-extension))))


(defun alchemist--server-erlang-version (project-path)
  (let* ((project-settings-map (alchemist--project-settings))
         (project-erlang-version (or (gethash project-path
                                              (gethash "alchemist-projects" project-settings-map))
                                     (completing-read "Choose which version of Erlang the LSP server should use: "
                                                      '("19" "20")
                                                      nil
                                                      t
                                                      ))))
    (puthash project-path project-erlang-version (gethash "alchemist-projects" project-settings-map))
    (setq alchemist--project-settings project-settings-map)
    (alchemist--project-save-settings project-settings-map)
    project-erlang-version))

(defun alchemist--server-extension ()
  (make-local-variable 'alchemist--system-extension)
  (cond	((eq system-type 'windows-nt) (setq alchemist--system-extension "bat"))
                ((eq system-type 'gnu/linux) (setq alchemist--system-extension "sh"))
                ((eq system-type 'gnu) (setq alchemist--system-extension "sh"))
                ((eq system-type 'gnu/kfreebsd) (setq alchemist--system-extension "sh"))
                ((eq system-type 'darwin) (setq alchemist--system-extension "sh"))
                ((eq system-type 'cygwin) (setq alchemist--system-extension "sh"))
                (t (error "Alchemist.el: Failed to set extension for elixir-ls, you seem to be on an unsupported OS"))))

(defun alchemist--project-settings ()
  (or alchemist--project-settings
      (setq alchemist--project-settings
            (or (and (file-exists-p (alchemist--config-file-path))
                     (alchemist--project-read-file))
                (alchemist--project-init-settings-file)))))

(defun alchemist--project-read-file ()
  (with-temp-buffer
    (insert-file-contents (alchemist--config-file-path))
    (goto-char (point-min)) (read (current-buffer))))

(defun alchemist--project-init-settings-file ()
  (alchemist--project-save-settings (alchemist--default-project-settings-map)))

(defun alchemist--project-save-settings (project-settings-map)
  (with-temp-file (alchemist--config-file-path)
    (prin1 project-settings-map (current-buffer))))

(defun alchemist--default-project-settings-map ()
  (let ((default-map (make-hash-table :test 'equal)))
    (puthash "alchemist-projects-version" 1 default-map)
    (puthash "alchemist-projects" (make-hash-table :test 'equal) default-map)
    default-map))

(defun alchemist--config-file-path ()
  (locate-user-emacs-file "alchemist-project-settings.el"))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'alchemist-mode-hook 'lsp-elixir-mode-enable)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(defalias 'alchemist-format-buffer 'lsp-format-buffer)

(provide 'alchemist-elixir-ls)

;;; alchemist-elixir-ls.el ends here
