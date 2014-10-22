;;; elixir-compile.el --- Elixir's compile integration

;;; Commentary:
;;

;;; Code:

(defcustom elixir-compile-command "elixirc"
  "The shell command for elixirc."
  :type 'string
  :group 'elixir-compile)

(defvar elixir-compile-buffer-name "*elixirc*"
  "Name of the elixir output buffer.")

(defun elixir-compile-this-buffer ()
  "Run the current buffer through elixirc."
  (interactive)
  (elixir-compile--file buffer-file-name))

(defun elixir-compile-file (filename)
  "Run elixir with the given `FILENAME`."
  (interactive "Felixirc: ")
  (elixir-compile--file (expand-file-name filename)))

(defun elixir-compile--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (elixir-compile-run (list elixir-compile-command (expand-file-name filename))))

(defun elixir-compile--read-command (command)
  (read-shell-command "elixirc command: " (concat command " ")))

(defun elixir-compile-run (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive (list (elixir-compile--read-command elixir-compile-command)))
  (elixir-compilation-run (elixir-utils-build-runner-cmdlist cmdlist)
                          elixir-compile-buffer-name))

(provide 'elixir-compile)

;;; elixir-compile.el ends here
