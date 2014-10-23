(require 'f)

(defvar alchemist-test-path
  (f-dirname (f-this-file)))

(defvar alchemist-sandbox-path
  (f-expand "sandbox" alchemist-test-path))

(defmacro within-sandbox (&optional current &rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory
           (f-join alchemist-sandbox-path (format "%s" ,current))))
     (f-mkdir alchemist-sandbox-path)
     ,@body
     (f-delete alchemist-sandbox-path :force)))

(provide 'test-helper)
