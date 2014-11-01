(require 'f)
(require 'ert)

(defvar alchemist-test-path
  (f-dirname (f-this-file)))

(defvar alchemist-sandbox-path
  (f-slash (f-expand "sandbox" alchemist-test-path)))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory alchemist-sandbox-path))
     (when (f-dir? alchemist-sandbox-path)
       (f-delete alchemist-sandbox-path :force))
     (f-mkdir alchemist-sandbox-path)
     ,@body))

(provide 'test-helper)
