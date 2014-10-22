;;; elixir-execute.el --- Elixir's script execution integration

;;; Commentary:
;;

;;; Code:

(defcustom elixir-execute-command "elixir"
  "The shell command for elixir."
  :type 'string
  :group 'elixir-execute)

(defvar elixir-execute-buffer-name "*elixir*"
  "Name of the elixir output buffer.")

(defun elixir-execute-this-buffer ()
  "Run the current buffer through elixir."
  (interactive)
  (elixir-execute--file buffer-file-name))

(defun elixir-execute-file (filename)
  "Run elixir with the given `FILENAME`."
  (interactive "Felixir: ")
  (elixir-execute--file (expand-file-name filename)))

(defun elixir-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (elixir-execute-run (list elixir-execute-command (expand-file-name filename))))

(defun elixir-execute--read-command (command)
  (read-shell-command "elixir command: " (concat command " ")))

(defun elixir-execute-run (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive (list (elixir-execute--read-command elixir-execute-command)))
  (elixir-compilation-run (elixir-utils-build-runner-cmdlist cmdlist)
                          elixir-execute-buffer-name))

(provide 'elixir-execute)

;;; elixir-execute.el ends here
