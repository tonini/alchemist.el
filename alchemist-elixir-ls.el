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

(add-hook 'elixir-mode-hook 'lsp-elixir-mode-enable)
