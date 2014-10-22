;;; elixir-execute.el --- Elixir's script execution integration

;;; Commentary:
;;

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
  "Run elixir with the given `filename`"
  (interactive "Felixir: ")
  (elixir-execute--file (expand-file-name filename)))

(defun elixir-execute--file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (elixir-execute-run (list elixir-execute-command (expand-file-name filename))))

(defun elixir-execute-run (cmdlist)
  "Run a elixir with `CMDLIST`."
  (interactive "Melixir: ")
  (elixir-compilation-run (elixir-utils-build-runner-cmdlist cmdlist)
                          elixir-execute-buffer-name))

(provide 'elixir-execute)

;;; elixir-execute.el ends here
