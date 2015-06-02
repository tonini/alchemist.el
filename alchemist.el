;;; alchemist.el --- Elixir tooling integration into Emacs

;; Copyright © 2014-2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/alchemist.el
;; Version: 0.17.0-cvs
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

;; Alchemist integrate Elixir's tooling into Emacs

;;; Code:

(defgroup alchemist nil
  "Elixir Tooling Integration Into Emacs."
  :prefix "alchemist-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/tonini/alchemist.el")
  :link '(emacs-commentary-link :tag "Commentary" "alchemist"))

(require 'alchemist-utils)
(require 'alchemist-project)
(require 'alchemist-buffer)
(require 'alchemist-compile)
(require 'alchemist-execute)
(require 'alchemist-mix)
(require 'alchemist-hooks)
(require 'alchemist-help)
(require 'alchemist-complete)
(require 'alchemist-message)
(require 'alchemist-iex)
(require 'alchemist-eval)
(require 'alchemist-goto)
(require 'alchemist-test-mode)

(eval-after-load 'company
  '(progn
     (require 'alchemist-company)))

(defun alchemist-mode-hook ()
  "Hook which enables `alchemist-mode'"
  (alchemist-mode 1))

(defvar alchemist--version "0.17.0-cvs")

;;;###autoload
(defun alchemist-version (&optional show-version)
  "Display Alchemist's version."
  (interactive)
  (message "Alchemist %s" (replace-regexp-in-string "-cvs" "snapshot" alchemist--version)))

(defcustom alchemist-key-command-prefix
  (kbd "C-c a")
  "The prefix for alchemist related key commands."
  :type 'string
  :group 'alchemist)

(define-prefix-command 'alchemist-mode-keymap)

;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}"
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :group 'alchemist
  :global nil
  :keymap `((,alchemist-key-command-prefix . alchemist-mode-keymap))
  (cond (alchemist-mode
         (alchemist-buffer-initialize-modeline))
        (t
         (alchemist-buffer-reset-modeline))))

(let ((map alchemist-mode-keymap))
  (define-key map (kbd "x") 'alchemist-mix)
  (define-key map (kbd "t") 'alchemist-mix-test)
  (define-key map (kbd "m c") 'alchemist-mix-compile)
  (define-key map (kbd "m t f") 'alchemist-mix-test-file)
  (define-key map (kbd "m t b") 'alchemist-mix-test-this-buffer)
  (define-key map (kbd "m t .") 'alchemist-mix-test-at-point)
  (define-key map (kbd "c c") 'alchemist-compile)
  (define-key map (kbd "c f") 'alchemist-compile-file)
  (define-key map (kbd "c b") 'alchemist-compile-this-buffer)
  (define-key map (kbd "e e") 'alchemist-execute)
  (define-key map (kbd "e f") 'alchemist-execute-file)
  (define-key map (kbd "e b") 'alchemist-execute-this-buffer)
  (define-key map (kbd "h h") 'alchemist-help)
  (define-key map (kbd "h e") 'alchemist-help-search-at-point)
  (define-key map (kbd "h m") 'alchemist-help-search-marked-region)
  (define-key map (kbd "p f") 'alchemist-project-find-test)
  (define-key map (kbd "p t") 'alchemist-project-open-tests-for-current-file)
  (define-key map (kbd "p s") 'alchemist-project-toggle-file-and-tests)
  (define-key map (kbd "p o") 'alchemist-project-toggle-file-and-tests-other-window)
  (define-key map (kbd "i i") 'alchemist-iex-run)
  (define-key map (kbd "i p") 'alchemist-iex-project-run)
  (define-key map (kbd "i l") 'alchemist-iex-send-current-line)
  (define-key map (kbd "i c") 'alchemist-iex-send-current-line-and-go)
  (define-key map (kbd "i r") 'alchemist-iex-send-region)
  (define-key map (kbd "i m") 'alchemist-iex-send-region-and-go)
  (define-key map (kbd "i b") 'alchemist-iex-compile-this-buffer)
  (define-key map (kbd "v l") 'alchemist-eval-current-line)
  (define-key map (kbd "v k") 'alchemist-eval-print-current-line)
  (define-key map (kbd "v j") 'alchemist-eval-quoted-current-line)
  (define-key map (kbd "v h") 'alchemist-eval-print-quoted-current-line)
  (define-key map (kbd "v o") 'alchemist-eval-region)
  (define-key map (kbd "v i") 'alchemist-eval-print-region)
  (define-key map (kbd "v u") 'alchemist-eval-quoted-region)
  (define-key map (kbd "v y") 'alchemist-eval-print-quoted-region)
  (define-key map (kbd "v q") 'alchemist-eval-buffer)
  (define-key map (kbd "v w") 'alchemist-eval-print-buffer)
  (define-key map (kbd "v e") 'alchemist-eval-quoted-buffer)
  (define-key map (kbd "v r") 'alchemist-eval-print-quoted-buffer))

(define-key alchemist-mode-map (kbd "M-.") 'alchemist-goto-definition-at-point)
(define-key alchemist-mode-map (kbd "M-,") 'alchemist-goto-jump-back)
(define-key alchemist-mode-map (kbd "C-c , .") 'alchemist-goto-definitions-in-current-file)

;; (easy-menu-define alchemist-mode-menu alchemist-mode-map
;;   "Alchemist mode menu."
;;   '("Alchemist"
;;     ("Goto"
;;      ["Jump to definiton at point" alchemist-goto-definition-at-point]
;;      ["Jump back" alchemist-goto-jump-back])
;;     ("Evaluate"
;;      ["Evaluate current line" alchemist-eval-current-line]
;;      ["Evaluate current line and print" alchemist-eval-print-current-line]
;;      ["Evaluate quoted current line" alchemist-eval-quoted-current-line]
;;      ["Evaluate quoted current line and print" alchemist-eval-print-quoted-current-line]
;;      "---"
;;      ["Evaluate region" alchemist-eval-region]
;;      ["Evaluate region and print" alchemist-eval-print-region]
;;      ["Evaluate quoted region" alchemist-eval-quoted-region]
;;      ["Evaluate quoted region and print" alchemist-eval-print-quoted-region]
;;      "---"
;;      ["Evaluate buffer" alchemist-eval-buffer]
;;      ["Evaluate buffer and print" alchemist-eval-print-buffer]
;;      ["Evaluate quoted buffer" alchemist-eval-quoted-buffer]
;;      ["Evaluate quoted buffer and print" alchemist-eval-print-quoted-buffer])
;;     ("Compile"
;;      ["Compile..." alchemist-compile]
;;      ["Compile this buffer" alchemist-compile-this-buffer]
;;      ["Compile file" alchemist-compile-file])
;;     ("Execute"
;;      ["Execute..." alchemist-compile]
;;      ["Execute this buffer" alchemist-execute-this-buffer]
;;      ["Execute file" alchemist-execute-file])
;;     ("Mix"
;;      ["Mix deps..." alchemist-mix-deps-with-prompt]
;;      ["Mix compile..." alchemist-mix-compile]
;;      ["Mix run..." alchemist-mix-run]
;;      "---"
;;      ["Mix test this buffer" alchemist-mix-test-this-buffer]
;;      ["Mix test file..." alchemist-mix-test-file]
;;      ["Mix test at point" alchemist-mix-test-at-point]
;;      "---"
;;      ["Mix..." alchemist-mix]
;;      ["Mix new..." alchemist-mix-new]
;;      ["Mix hex search..." alchemist-mix-hex-search]
;;      "---"
;;      ["Mix local..." alchemist-mix-local-with-prompt]
;;      ["Mix local install..." alchemist-mix-local-install]
;;      ["Mix local install (Path)..." alchemist-mix-local-install-with-path]
;;      ["Mix local install (URL)..." alchemist-mix-local-install-with-url]
;;      "---"
;;      ["Display mix buffer" alchemist-mix-display-mix-buffer]
;;      "---"
;;      ["Mix help..." alchemist-mix-help])
;;     ("IEx"
;;      ["IEx send current line" alchemist-iex-send-current-line]
;;      ["IEx send current line and go" alchemist-iex-send-current-line-and-go]
;;      "---"
;;      ["IEx send last region" alchemist-iex-send-last-sexp]
;;      ["IEx send region" alchemist-iex-send-region]
;;      ["IEx send region and go" alchemist-iex-send-region-and-go]
;;      "---"
;;      ["IEx compile this buffer" alchemist-iex-compile-this-buffer]
;;      ["IEx recompile this buffer" alchemist-iex-recompile-this-buffer]
;;      "---"
;;      ["IEx run" alchemist-iex-run])
;;     ("Project"
;;      ["Project find all tests" alchemist-project-find-test]
;;      ["Project find tests for current file" alchemist-project-open-tests-for-current-file]
;;      ["Project toggle between file and test" alchemist-project-toggle-file-and-tests]
;;      ["Project toggle between file and test in other window" alchemist-project-toggle-file-and-tests-other-window]
;;      "---"
;;      ["Project toggle compile when needed" alchemist-project-toggle-compile-when-needed
;;       :style toggle :selected alchemist-project-compile-when-needed])
;;     ("Documentation"
;;      ["Documentation search..." alchemist-help]
;;      ["Documentation search history..." alchemist-help-history]
;;      "---"
;;      ["Documentation search at point..." alchemist-help-search-at-point]
;;      ["Documentation search marked region..." alchemist-help-search-marked-region])
;;     ))


(and (ignore-errors (require 'easymenu) t)
     (easy-menu-define alchemist-menu alchemist-mode-map "Alchemist Mode menu"
       `("Alchemist"
	 ("Iex"
	  ["Alchemist iex run" alchemist-iex-run
	   :help " `alchemist-iex-run'
Start an IEx process.
Show the IEx buffer if an IEx process is already run."]
	  ["Alchemist iex clear buffer" alchemist-iex-clear-buffer
	   :help " `alchemist-iex-clear-buffer'
Clear the current iex process buffer."]
	  ["Alchemist iex compile this buffer" alchemist-iex-compile-this-buffer
	   :help " `alchemist-iex-compile-this-buffer'
Compiles the current buffer in the IEx process."]
	  ["Alchemist iex mode" alchemist-iex-mode
	   :help " `alchemist-iex-mode'
Major mode for interacting with an Elixir IEx process."]
	  ["Alchemist iex project run" alchemist-iex-project-run
	   :help " `alchemist-iex-project-run'
Start an IEx process with mix 'iex -S mix' in the
context of an Elixir project.
Show the IEx buffer if an IEx process is already run."]
	  ["Alchemist iex recompile this buffer" alchemist-iex-recompile-this-buffer
	   :help " `alchemist-iex-recompile-this-buffer'
Recompiles and reloads the current buffer in the IEx process."]
	  ["Alchemist iex send current line" alchemist-iex-send-current-line
	   :help " `alchemist-iex-send-current-line'
Sends the current line to the IEx process."]
	  ["Alchemist iex send current line and go" alchemist-iex-send-current-line-and-go
	   :help " `alchemist-iex-send-current-line-and-go'
Sends the current line to the inferior IEx process
and jump to the buffer."]
	  ["Alchemist iex send last sexp" alchemist-iex-send-last-sexp
	   :help " `alchemist-iex-send-last-sexp'
Send the previous sexp to the inferior IEx process."]
	  ["Alchemist iex send region" alchemist-iex-send-region
	   :help " `alchemist-iex-send-region'
Sends the marked region to the IEx process."]
	  ["Alchemist iex send region and go" alchemist-iex-send-region-and-go
	   :help " `alchemist-iex-send-region-and-go'
Sends the marked region to the inferior IEx process
and jump to the buffer."]
	  ["Alchemist iex start process" alchemist-iex-start-process
	   :help " `alchemist-iex-start-process'
Start an IEX process.
With universal prefix C-u, prompts for a COMMAND,
otherwise uses `alchemist-iex-program-name'.
It runs the hook `alchemist-iex-mode-hook' after starting the process and
setting up the IEx buffer."]
	  )

	 ("Eval"
	  ["Alchemist eval buffer" alchemist-eval-buffer
	   :help " `alchemist-eval-buffer'
Evaluate the Elixir code in the current buffer."]
	  ["Alchemist eval current line" alchemist-eval-current-line
	   :help " `alchemist-eval-current-line'
Evaluate the Elixir code on the current line."]
	  ["Alchemist eval print buffer" alchemist-eval-print-buffer
	   :help " `alchemist-eval-print-buffer'
Evaluate the Elixir code in the current buffer and insert the result."]
	  ["Alchemist eval print current line" alchemist-eval-print-current-line
	   :help " `alchemist-eval-print-current-line'
Evaluate the Elixir code on the current line and insert the result."]
	  ["Alchemist eval print quoted buffer" alchemist-eval-print-quoted-buffer
	   :help " `alchemist-eval-print-quoted-buffer'
Get the Elixir code representation of the expression in the current buffer and insert result."]
	  ["Alchemist eval print quoted current line" alchemist-eval-print-quoted-current-line
	   :help " `alchemist-eval-print-quoted-current-line'
Get the Elixir code representation of the expression on the current line and insert the result."]
	  ["Alchemist eval print quoted region" alchemist-eval-print-quoted-region
	   :help " `alchemist-eval-print-quoted-region'
Get the Elixir code representation of the expression on marked region and insert the result."]
	  ["Alchemist eval print region" alchemist-eval-print-region
	   :help " `alchemist-eval-print-region'
Evaluate the Elixir code on marked region and insert the result."]
	  ["Alchemist eval quoted buffer" alchemist-eval-quoted-buffer
	   :help " `alchemist-eval-quoted-buffer'
Get the Elixir code representation of the expression in the current buffer."]
	  ["Alchemist eval quoted current line" alchemist-eval-quoted-current-line
	   :help " `alchemist-eval-quoted-current-line'
Get the Elixir code representation of the expression on the current line."]
	  ["Alchemist eval quoted region" alchemist-eval-quoted-region
	   :help " `alchemist-eval-quoted-region'
Get the Elixir code representation of the expression on marked region."]
	  ["Alchemist eval region" alchemist-eval-region
	   :help " `alchemist-eval-region'
Evaluate the Elixir code on marked region."]
	  )

	 ("Execute"
	  ["Alchemist execute" alchemist-execute
	   :help " `alchemist-execute'
Run a elixir with CMDLIST."]
	  ["Alchemist execute file" alchemist-execute-file
	   :help " `alchemist-execute-file'
Run elixir with the given FILENAME."]
	  ["Alchemist execute this buffer" alchemist-execute-this-buffer
	   :help " `alchemist-execute-this-buffer'
Run the current buffer through elixir."]
	  )

	 ("Goto"
	  ["Alchemist goto definition at point" alchemist-goto-definition-at-point
	   :help " `alchemist-goto-definition-at-point'
Jump to the elixir expression definition at point."]
	  ["Alchemist goto jump back" alchemist-goto-jump-back
	   :help " `alchemist-goto-jump-back'
Pop back to where M-x find-tag was last invoked.

This is distinct from invoking M-x find-tag with a negative argument
since that pops a stack of markers at which tags were found, not from
where they were found."]
	  ["Alchemist goto list symbol definitions" alchemist-goto-list-symbol-definitions
	   :help " `alchemist-goto-list-symbol-definitions'
List all symbol definitions in the current file like functions/macros/modules.

It will jump to the position of the symbol definition after selection."]
	  )

	 ("Compile"
	  ["Alchemist compile" alchemist-compile
	   :help " `alchemist-compile'
Compile CMDLIST with elixirc."]
	  ["Alchemist compile file" alchemist-compile-file
	   :help " `alchemist-compile-file'
Compile the given FILENAME."]
	  ["Alchemist compile this buffer" alchemist-compile-this-buffer
	   :help " `alchemist-compile-this-buffer'
Compile the current buffer with elixirc."]
	  )

	 ("Complete"
	  ["Alchemist complete debug mode" alchemist-complete-debug-mode
	   :help " `alchemist-complete-debug-mode'
Enables the debug mode for completion if `alchemist-complete-debug-mode'
is `nil', otherwise it disable it."]
	  )

	 ("Help"
	  ["Alchemist help" alchemist-help
	   :help " `alchemist-help'
Load Elixir documentation for SEARCH."]
	  ["Alchemist help history" alchemist-help-history
	   :help " `alchemist-help-history'
Load Elixir from the documentation history for SEARCH."]
	  ["Alchemist help minor mode" alchemist-help-minor-mode
	   :help " `alchemist-help-minor-mode'
Minor mode for displaying elixir help."]
	  ["Alchemist help minor mode key binding summary" alchemist-help-minor-mode-key-binding-summary
	   :help " `alchemist-help-minor-mode-key-binding-summary'"]
	  ["Alchemist help search at point" alchemist-help-search-at-point
	   :help " `alchemist-help-search-at-point'
Search through `alchemist-help' with the expression under the cursor."]
	  ["Alchemist help search marked region" alchemist-help-search-marked-region
	   :help " `alchemist-help-search-marked-region'
Run `alchemist-help' with the marked region.
Argument BEGIN where the mark starts.
Argument END where the mark ends."]
	  )

	 ("Message"
	  ["Alchemist message mode" alchemist-message-mode
	   :help " `alchemist-message-mode'
Minor mode for displaying alchemist messages"]
	  )

	 ("Mix"
	  ["Alchemist mix" alchemist-mix
	   :help " `alchemist-mix'
Prompt for mix commands. Prompt for the mix env if the prefix arg is set."]
	  ["Alchemist mix compile" alchemist-mix-compile
	   :help " `alchemist-mix-compile'
Compile the whole elixir project. Prompt for the mix env if the prefix
arg is set."]
	  ["Alchemist mix deps with prompt" alchemist-mix-deps-with-prompt
	   :help " `alchemist-mix-deps-with-prompt'
Prompt for mix deps commands."]
	  ["Alchemist mix display mix buffer" alchemist-mix-display-mix-buffer
	   :help " `alchemist-mix-display-mix-buffer'
Display the mix buffer when exists."]
	  ["Alchemist mix execute" alchemist-mix-execute
	   :help " `alchemist-mix-execute'
Run a mix command. Prompt for the mix env if the prefix arg is set."]
	  ["Alchemist mix help" alchemist-mix-help
	   :help " `alchemist-mix-help'
Show help output for a specific mix command. Prompt for the mix env if
the prefix arg is set."]
	  ["Alchemist mix hex search" alchemist-mix-hex-search
	   :help " `alchemist-mix-hex-search'
Display packages matching the given search query. Prompt for the mix env
if the prefix arg is set."]
	  ["Alchemist mix local install" alchemist-mix-local-install
	   :help " `alchemist-mix-local-install'
Prompt for mix local.install PATH-OR-URL."]
	  ["Alchemist mix local install with path" alchemist-mix-local-install-with-path
	   :help " `alchemist-mix-local-install-with-path'
Runs local.install and prompt for a PATH as argument."]
	  ["Alchemist mix local install with url" alchemist-mix-local-install-with-url
	   :help " `alchemist-mix-local-install-with-url'
Runs local.install and prompt for a URL as argument."]
	  ["Alchemist mix local with prompt" alchemist-mix-local-with-prompt
	   :help " `alchemist-mix-local-with-prompt'
Prompt for mix local commands."]
	  ["Alchemist mix new" alchemist-mix-new
	   :help " `alchemist-mix-new'
Create a new elixir project named by NAME."]
	  ["Alchemist mix run" alchemist-mix-run
	   :help " `alchemist-mix-run'
Runs the given file or expression in the context of the application.
Prompt for the mix env if the prefix arg is set."]
	  ["Alchemist mix test" alchemist-mix-test
	   :help " `alchemist-mix-test'
Run the whole elixir test suite."]
	  ["Alchemist mix test at point" alchemist-mix-test-at-point
	   :help " `alchemist-mix-test-at-point'
Run the test at point."]
	  ["Alchemist mix test file" alchemist-mix-test-file
	   :help " `alchemist-mix-test-file'
Run `alchemist-mix--test-file' with the FILENAME."]
	  ["Alchemist mix test this buffer" alchemist-mix-test-this-buffer
	   :help " `alchemist-mix-test-this-buffer'
Run the current buffer through mix test."]
	  )

	 ("Project"
	  ["Alchemist project find test" alchemist-project-find-test
	   :help " `alchemist-project-find-test'
Open project test directory and list all test files."]
	  ["Alchemist project open tests for current file" alchemist-project-open-tests-for-current-file
	   :help " `alchemist-project-open-tests-for-current-file'
Open the appropriate test file for the current buffer file in a new window."]
	  ["Alchemist project toggle compile when needed" alchemist-project-toggle-compile-when-needed
	   :help " `alchemist-project-toggle-compile-when-needed'"]
	  ["Alchemist project toggle file and tests" alchemist-project-toggle-file-and-tests
	   :help " `alchemist-project-toggle-file-and-tests'
Toggle between a file and its tests in the current window."]
	  ["Alchemist project toggle file and tests other window" alchemist-project-toggle-file-and-tests-other-window
	   :help " `alchemist-project-toggle-file-and-tests-other-window'
Toggle between a file and its tests in other window."]
	  )

	 ("Test"
	  ["Alchemist test mode" alchemist-test-mode
	   :help " `alchemist-test-mode'
Minor mode for Elixir ExUnit files.

The following commands are available:

key             binding
---             -------"]
	  ["Alchemist test mode jump to next test" alchemist-test-mode-jump-to-next-test
	   :help " `alchemist-test-mode-jump-to-next-test'
Jump to the next ExUnit test. If there are no tests after the current
position, jump to the first test in the buffer. Do nothing if there are no tests
in this buffer."]
	  ["Alchemist test mode jump to previous test" alchemist-test-mode-jump-to-previous-test
	   :help " `alchemist-test-mode-jump-to-previous-test'
Jump to the previous ExUnit test. If there are no tests before the current
position, jump to the last test in the buffer. Do nothing if there are no tests
in this buffer."]
	  )

	 ("Version"
	  ["Alchemist version" alchemist-version
	   :help " `alchemist-version'
Display Alchemist's version."]
	  )

	 ("Buffer"
	  ["Alchemist buffer mode" alchemist-buffer-mode
	   :help " `alchemist-buffer-mode'
Elixir compilation mode.

In addition to any hooks its parent mode `compilation-mode' might have run,
this mode runs the hook `alchemist-buffer-mode-hook', as the final step
during initialization."]
	  ))))

(add-hook 'elixir-mode-hook 'alchemist-mode-hook)

(provide 'alchemist)

;;; alchemist.el ends here
