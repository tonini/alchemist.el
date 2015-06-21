;;; alchemist-test.el --- Minor mode for Elixir test files.

;; Copyright © 2015 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for Elixir test files.

;;; Code:

(defgroup alchemist-test nil
  "Minor mode for Elixir ExUnit files."
  :prefix "alchemist-test-"
  :group 'alchemist)

;; Variables

(defvar alchemist-test-buffer-name "*alchemist-test-report*"
  "Name of the test report buffer.")

(defcustom alchemist-test-highlight-tests t
  "Non-nil means that specific functions for testing will
be highlighted with more significant font faces."
  :type 'boolean
  :group 'alchemist-test-mode)

(defvar alchemist-test-at-point #'alchemist-mix-test-at-point)
(defvar alchemist-test-this-buffer #'alchemist-mix-test-this-buffer)
(defvar alchemist-test #'alchemist-mix-test)
(defvar alchemist-test-file #'alchemist-mix-test-file)
(defvar alchemist-test-jump-to-previous-test #'alchemist-test-jump-to-previous-test)
(defvar alchemist-test-jump-to-next-test #'alchemist-test-jump-to-next-test)

(defvar alchemist-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , s") alchemist-test-at-point)
    (define-key map (kbd "C-c , v") alchemist-test-this-buffer)
    (define-key map (kbd "C-c , a") alchemist-test)
    (define-key map (kbd "C-c , f") alchemist-test-file)
    (define-key map (kbd "C-c , p") alchemist-test-jump-to-previous-test)
    (define-key map (kbd "C-c , n") alchemist-test-jump-to-next-test)
    map)
  "Keymap for `alchemist-test-mode'.")

(setq alchemist-test--test-regex
  "\\(^[[:space:]]*test .+ do[[:space:]]*$\\|^[[:space:]]* [0-9]+) test .+\\)")

;; Private functions

(defun alchemist-test--buffer-contains-tests-p ()
  "Return nil if the current buffer contains no tests, non-nil if it does."
  (save-excursion
    (save-match-data
      (beginning-of-buffer)
      (re-search-forward alchemist-test--test-regex nil t))))

(defun alchemist-test--jump-to-test (search-fn reset-fn)
  "Move the point to the next/previous test, based on `search-fn' (which is the
function that searches for the next test, can be re-search-forward or
re-search-backward) and `reset-fn' (which is used when wrapping at the
beginning/end of the buffer if no results were found)."
  (when (alchemist-test--buffer-contains-tests-p)
    (save-match-data
      (unless (funcall search-fn alchemist-test--test-regex nil t)
        (funcall reset-fn)
        (funcall search-fn alchemist-test--test-regex nil t))
      (back-to-indentation))))

;; Public functions

(defun alchemist-test-jump-to-next-test ()
  "Jump to the next ExUnit test. If there are no tests after the current
position, jump to the first test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (alchemist-test--jump-to-test 're-search-forward 'beginning-of-buffer))

(defun alchemist-test-jump-to-previous-test ()
  "Jump to the previous ExUnit test. If there are no tests before the current
position, jump to the last test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (alchemist-test--jump-to-test 're-search-backward 'end-of-buffer))

(defun alchemist-test--highlight-syntax ()
  (if alchemist-test-highlight-tests
      (font-lock-add-keywords nil
                              '(("^\s+\\(test\\)\s+" 1
                                 font-lock-variable-name-face t)
                                ("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\)\s+" 1
                                 font-lock-type-face t)
                                ("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\)\(" 1
                                 font-lock-type-face t)))))

;;;###autoload
(define-minor-mode alchemist-test-mode
  "Minor mode for Elixir ExUnit files.

The following commands are available:

\\{alchemist-test-mode-map}"
  :lighter ""
  :group 'alchemist
  :keymap alchemist-test-mode-map
  (when alchemist-test-mode
    (alchemist-test--highlight-syntax)))

;;;###autoload
(defun alchemist-test-enable-mode ()
  (if (alchemist-utils--is-test-file-p)
      (alchemist-test-mode)))

;;;###autoload
(dolist (hook '(alchemist-mode-hook))
  (add-hook hook 'alchemist-test-enable-mode))

(provide 'alchemist-test)

;;; alchemist-test.el ends here
