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

(defcustom alchemist-server-extension 'sh
  "Help alchemist decide if you are running on a *nix or Microsoft machine"
  :type '(choice (const :tag "Windows" exe)
                 (const :tag "*nix" sh))
  :group 'alchemist-server)

(defconst alchemist-server-root-path
  (lambda () (concat (file-name-directory load-file-name) "elixir-ls/")))

(defun alchemist-server-erlang-version (project-path)
  (locate-user-emacs-file "alchemist-config")
  ;; load file
  ;; if empty, initialize hashmap ({alchemist: {version: 1, projects: {}}})
  ;; load hashmap's projects key
  ;; find key related to project
  ;; if key not there, use completing-read to ask for an erlang version
  ;; store key in file
  ;; return erlang version
  )

(defun alchemist-lsp-server-full-path ()
  (concat alchemist-server-root-path
          "erl/"
          alchemist-server-erlang-version
          "/"
          "language_server"
          "."
          alchemist-server-extension))

(lsp-define-stdio-client
  lsp-elixir-mode
  "elixir"
   (lambda () (alchemist-project-root-or-default-dir))
   '("~/src/projects/alchemist.el/elixir-ls/erl19/language_server.sh"))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'alchemist-mode-hook 'lsp-elixir-mode-enable)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(defun)

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
