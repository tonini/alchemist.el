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
   '("~/src/projects/alchemist.el/elixir-ls/erl20/language_server.sh"))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'alchemist-mode-hook 'lsp-elixir-mode-enable)

(defalias 'alchemist-format-buffer 'lsp-format-buffer)

(provide 'alchemist-elixir-ls)

;;; alchemist-elixir-ls.el ends here
