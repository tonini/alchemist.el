;; Usage:
;;
;;   emacs -Q -l test/test-runner.el           # interactive mode
;;   emacs -batch -Q -l test/test-runner.el    # batch mode

(let ((current-directory (file-name-directory load-file-name)))
  (setq alchemist-test-path (expand-file-name "." current-directory))
  (setq alchemist-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path alchemist-root-path)
(add-to-list 'load-path alchemist-test-path)

(require 'alchemist)

(load (expand-file-name "test-helper.el" alchemist-test-path) nil t)
(dolist (test-file (or argv (directory-files alchemist-test-path t "-tests.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)
