;;; alchemist-execute.el --- Elixir's script execution integration

;;; Commentary:
;;

;;; Code:

(defcustom alchemist-execute-command "elixir"
  "The shell command for elixir."
  :type 'string
  :group 'alchemist-execute)

(defvar alchemist-execute-buffer-name "*elixir*"
  "Name of the elixir output buffer.")

(defun alchemist-execute-this-buffer ()
  "Run the current buffer through elixir."
  (interactive)
  (alchemist-execute--file buffer-file-name))

(defun alchemist-execute-file (filename)
  "Run elixir with the given `FILENAME`."
  (interactive "Felixir: ")
  (alchemist-execute--file (expand-file-name filename)))

(defun alchemist-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (alchemist-execute-run (list alchemist-execute-command (expand-file-name filename))))

(defun alchemist-execute--read-command (command)
  (read-shell-command "elixir command: " (concat command " ")))

(defun alchemist-execute-run (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive (list (alchemist-execute--read-command alchemist-execute-command)))
  (alchemist-buffer-run (alchemist-utils-build-runner-cmdlist cmdlist)
                        alchemist-execute-buffer-name))

(provide 'alchemist-execute)

;;; alchemist-execute.el ends here
