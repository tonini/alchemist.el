;;; alchemist-elixir-ls.el --- Functionality to jump modules and function definitions -*- lexical-binding: t -*-

(require 'alchemist-project)
(require 'lsp-mode)
(require 'elixir-mode)
(require 'dash)
(require 's)

(lsp-define-stdio-client
  lsp-elixir-mode
  "elixir"
   (lambda () (alchemist-project-root-or-default-dir))
   '("~/src/projects/alchemist.el/elixir-ls/erl19/language_server.sh"))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'alchemist-mode-hook 'lsp-elixir-mode-enable)

(defun alchemist-macro-expand ()
  (interactive)
  (lsp--cur-workspace-check)
  (let (x (lsp-send-request (lsp-make-request
                           "elixirDocument/macroExpansion"
                           `(
                             :position ,(lsp--cur-position)
                             :textDocument ,(lsp--make-text-document-item)))))
    (message "%s" x)
    ))

(defalias 'alchemist-format-buffer 'lsp-format-buffer)

(provide 'alchemist-elixir-ls)

;;; alchemist-elixir-ls.el ends here
