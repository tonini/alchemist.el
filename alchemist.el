;;; alchemist.el --- Elixir tooling integration into emacs

;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Version: 0.0.1

;;; Commentary:
;;


;;;###autoload
(define-minor-mode alchemist-mode
  "Toggle alchemist mode.

When alchemist mode is enabled, the follwing elixir modes will be loaded:
* alchemist-buffer
* alchemist-compile
* alchemist-execute
* alchemist-eval
* alchemist-mix
* alchemist-iex"
  nil
  ;; The indicator for the mode line.
  " alchemist"
  :group 'alchemist
  (cond (alchemist-mode
         (require 'alchemist-utils)
         (require 'alchemist-buffer)
         (require 'alchemist-compile)
         (require 'alchemist-execute)
         (require 'alchemist-eval)
         (require 'alchemist-mix))))

(provide 'alchemist)

;;; alchemist.el ends here
