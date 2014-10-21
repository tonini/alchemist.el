;;; elixir-compilation.el --- Define a custom compilation mode for Elixir executions

;;; Commentary:
;;

(require 'compile)
(require 'ansi-color)

(defvar elixir-compilation--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'elixir-compilation--buffer-name)

(defvar elixir-compilation--error-link-options
  '(elixir "\\([a-z./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun elixir-compilation--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode elixir-compilation-mode "Elixir"
  "Elixir compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^Elixir.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq elixir-compilation--buffer-name elixir-compilation--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'elixir-compilation--kill-any-orphan-proc)))

(defvar elixir-compilation--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun elixir-compilation--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'elixir-compilation--handle-compilation-once t)
  (delete-matching-lines "\\(elixir-compilation;\\|Elixir started\\|^$\\)" (point-min) (point)))

(defun elixir-compilation--handle-compilation ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun elixir-compilation-run (cmdlist buffer-name)
  "run CMDLIST in `elixir-compilation-compilation-mode'.
Returns the compilation buffer."
  (save-some-buffers (not compilation-ask-about-save) elixir-compilation--save-buffers-predicate)

  (let* ((elixir-compilation--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'shell-quote-argument cmdlist " ")
                           'elixir-compilation-mode
                           (lambda (b) elixir-compilation--buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons elixir-compilation--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'elixir compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'elixir-compilation--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'elixir-compilation--handle-compilation-once nil t))))

(provide 'elixir-compilation)

;;; elixir-compilation.el ends here
