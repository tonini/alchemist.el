;;; alchemist-elixir-ls.el --- Functionality to jump modules and function definitions -*- lexical-binding: t -*-

(require 'alchemist-project)
(require 'lsp-imenu)
(require 'lsp-mode)
(require 'elixir-mode)
(require 'dash)
(require 's)

(defgroup alchemist-server nil
  "Which Language Server Protocol alchemist will use"
  :prefix "alchemist-server-"
  :group 'alchemist)

(defconst alchemist-server-root-path
  (concat (file-name-directory load-file-name) "elixir-ls/"))

(defvar alchemist--project-settings nil
  "Where alchemist keeps its project-level settings")

(lsp-define-stdio-client
 lsp-elixir-mode
 "elixir"
 (lambda () (alchemist-project-root-or-default-dir))
 (alchemist--lsp-server-path-for-current-project))
;; '("~/src/projects/alchemist.el/elixir-ls/erl19/language_server.sh")

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

(defun alchemist-macro-expand (start-pos end-pos)
  "Expands the selected code once.

This function has some string manipulation logic because elixir_sense returns
a string that begins and ends with parens, so we get rid of them to print something
meaningful to the user."
  (interactive "r")
  (lsp--cur-workspace-check)
  (let* ((selected-code (buffer-substring-no-properties start-pos end-pos))
         (response (lsp-send-request
                    (lsp-make-request
                     "elixirDocument/macroExpansion"
                     `(:context (:selection ,selected-code)
                                :position ,(lsp--cur-position)
                                :textDocument ,(lsp--make-text-document-item)))))
         (expansion (gethash "expand" response))
         (lines (cdr (butlast (split-string expansion "\n"))))
         (insertable (string-join
                      (mapcar (lambda (x) (concat "# " x)) lines)
                      "\n")))
    (save-excursion (goto-char start-pos)
                    (previous-line)
                    (insert insertable))))

(defalias 'alchemist-format-buffer 'lsp-format-buffer)

(provide 'alchemist-elixir-ls)

;;; alchemist-elixir-ls.el ends here
