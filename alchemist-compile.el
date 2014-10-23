;;; alchemist-compile.el --- Elixir's compile integration

;;; Commentary:
;;

;;; Code:

(defcustom alchemist-compile-command "elixirc"
  "The shell command for elixirc."
  :type 'string
  :group 'elixir-compile)

(defvar alchemist-compile-buffer-name "*elixirc*"
  "Name of the elixir output buffer.")

(defun alchemist-compile-this-buffer ()
  "Run the current buffer through elixirc."
  (interactive)
  (elixir-compile--file buffer-file-name))

(defun alchemist-compile-file (filename)
  "Run elixir with the given `FILENAME`."
  (interactive "Felixirc: ")
  (alchemist-compile--file (expand-file-name filename)))

(defun alchemist-compile--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (alchemist-compile-run (list alchemist-compile-command (expand-file-name filename))))

(defun alchemist-compile--read-command (command)
  (read-shell-command "elixirc command: " (concat command " ")))

(defun alchemist-compile-run (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive (list (alchemist-compile--read-command elixir-compile-command)))
  (alchemist-buffer-run (alchemist-utils-build-runner-cmdlist cmdlist)
                        alchemist-compile-buffer-name))

(provide 'alchemist-compile)

;;; alchemist-compile.el ends here
