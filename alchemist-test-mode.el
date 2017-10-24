;;; alchemist-test-mode.el --- Minor mode for Elixir test files.

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

(require 'dash)
(require 'alchemist-project)
(require 'compile)

(defgroup alchemist-test-mode nil
  "Minor mode for Elixir ExUnit files."
  :prefix "alchemist-test-mode-"
  :group 'alchemist)

;; Variables

(defcustom alchemist-test-mode-highlight-tests t
  "Non-nil means that specific functions for testing will
be highlighted with more significant font faces."
  :type 'boolean
  :group 'alchemist-test-mode)

(defcustom alchemist-test-display-compilation-output nil
  "if Non-nil, compilation informations will be displayed
in the test report buffer."
  :type 'boolean
  :group 'alchemist-test-mode)

(defcustom alchemist-test-truncate-lines t
  "The value of this variable is used to set the value of
truncate-lines in the test report window."
  :type 'boolean
  :group 'alchemist-test-mode)

(defcustom alchemist-test-status-modeline t
  "if Non-nil, the face of local `mode-name' variable will change with test run status.

For example, when `alchemist-mix-test' fails, the `mode-name' will be
formatted with the `alchemist-test--failed-face' face, to symbolize failing tests."
  :type 'boolean
  :group 'alchemist-test)

(defcustom alchemist-test-ask-about-save t
  "Non-nil means 'alchemist-test-excute` asks which buffers to save before running.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'alchemist-test)

(defvar alchemist-test--last-run-status "")

(defconst alchemist-test-report-buffer-name "*alchemist test report*"
  "Name of the test report buffer.")

(defconst alchemist-test-report-process-name "alchemist-test-process"
  "Name of the test report process.")

;; Faces

(defface alchemist-test--test-file-and-location-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for the file where the failed test are."
  :group 'alchemist-test)

(defface alchemist-test--stacktrace-file-and-location-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for the stacktrace files."
  :group 'alchemist-test)

(defface alchemist-test--success-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "darkgreen" :foreground "white")))
  "Face for successful compilation run."
  :group 'alchemist-test)

(defface alchemist-test--failed-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "red" :foreground "white")))
  "Face for failed compilation run."
  :group 'alchemist-test)

(defvar alchemist-test--mode-name-face 'mode-line)

(defvar alchemist-test-at-point #'alchemist-mix-test-at-point)
(defvar alchemist-test-this-buffer #'alchemist-mix-test-this-buffer)
(defvar alchemist-test #'alchemist-mix-test)
(defvar alchemist-test-file #'alchemist-mix-test-file)
(defvar alchemist-test-jump-to-previous-test #'alchemist-test-mode-jump-to-previous-test)
(defvar alchemist-test-jump-to-next-test #'alchemist-test-mode-jump-to-next-test)
(defvar alchemist-test-list-tests #'alchemist-test-mode-list-tests)

(defvar alchemist-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , s") alchemist-test-at-point)
    (define-key map (kbd "C-c , v") alchemist-test-this-buffer)
    (define-key map (kbd "C-c , a") alchemist-test)
    (define-key map (kbd "C-c , f") alchemist-test-file)
    (define-key map (kbd "C-c , p") alchemist-test-jump-to-previous-test)
    (define-key map (kbd "C-c , n") alchemist-test-jump-to-next-test)
    (define-key map (kbd "C-c , l") alchemist-test-list-tests)
    map)
  "Keymap for `alchemist-test-mode'.")

(defconst alchemist-test-mode--test-regex
  (let ((whitespace-opt "[[:space:]]*")
        (whitespace "[[:space:]]+"))
    (concat "\\(^" whitespace-opt "test" whitespace "\\(?10:.+\\)" whitespace "do" whitespace-opt "$"
            "\\|"
            whitespace " [0-9]+) test .+\\)")))

;; Private functions

(defun alchemist-test--set-modeline-color (status)
  (setq alchemist-test--mode-name-face
        (if (string-prefix-p "finished" status)
            'alchemist-test--success-face
          'alchemist-test--failed-face)))

(defun alchemist-test-mode--buffer-contains-tests-p ()
  "Return nil if the current buffer contains no tests, non-nil if it does."
  (alchemist-utils-occur-in-buffer-p (current-buffer) alchemist-test-mode--test-regex))

(defun alchemist-test-mode--tests-in-buffer ()
  "Return an alist of tests in this buffer.

The keys in the list are the test names (e.g., the string passed to the test/2
macro) while the values are the position at which the test matched."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((tests '()))
        (while (re-search-forward alchemist-test-mode--test-regex nil t)
          (let* ((position (car (match-data)))
                 (matched-string (match-string 10)))
            (set-text-properties 0 (length matched-string) nil matched-string)
            (add-to-list 'tests (cons matched-string position) t)))
        tests))))

(defun alchemist-test-mode--highlight-syntax ()
  (if alchemist-test-mode-highlight-tests
      (font-lock-add-keywords nil
                              '(("^\s+\\(test\\)\s+" 1
                                 font-lock-variable-name-face t)
                                ("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\|flunk\\)\s+" 1
                                 font-lock-type-face t)
                                ("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\|flunk\\)\(" 1
                                 font-lock-type-face t)))))

;; Public functions

(defun alchemist-test-save-buffers ()
  "Save some modified file-visiting buffers."
  (save-some-buffers (not alchemist-test-ask-about-save) nil))

(defun alchemist-test-clean-compilation-output (output)
  (if (not alchemist-test-display-compilation-output)
      (with-temp-buffer
        (insert output)
        (delete-matching-lines "^Compiled .+" (point-min) (point-max))
        (delete-matching-lines "^Generated .+" (point-min) (point-max))
        (buffer-substring-no-properties (point-min) (point-max)))
  output))

(defun alchemist-test-execute (command-list)
  (message "Testing...")
  (let* ((default-directory (or (alchemist-project-root) default-directory))
         (command (mapconcat 'concat (-flatten command-list) " ")))
    (alchemist-test-save-buffers)
    (compile command 'alchemist-test-compilation-mode)))

(define-compilation-mode alchemist-test-compilation-mode "Alchemist Test Compilation"
  "Compilation mode for mix test output from Alchemist"
  (add-hook 'compilation-filter-hook 'alchemist-test-colorize-compilation-buffer nil t))

(defun alchemist-test-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun alchemist-test-initialize-modeline ()
  "Initialize the mode-line face."
  (when alchemist-test-status-modeline
    (setq mode-name
          '(:eval (propertize "Elixir" 'face alchemist-test--mode-name-face)))))

(defun alchemist-test-reset-modeline ()
  "Reset the current mode-line face to default."
  (setq mode-name "Elixir"))

(defun alchemist-test-mode-jump-to-next-test ()
  "Jump to the next ExUnit test. If there are no tests after the current
position, jump to the first test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (alchemist-utils-jump-to-next-matching-line alchemist-test-mode--test-regex 'back-to-indentation))

(defun alchemist-test-mode-jump-to-previous-test ()
  "Jump to the previous ExUnit test. If there are no tests before the current
position, jump to the last test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (alchemist-utils-jump-to-previous-matching-line alchemist-test-mode--test-regex 'back-to-indentation))

(defun alchemist-test-mode-list-tests ()
  "List ExUnit tests (calls to the test/2 macro) in the current buffer and jump
to the selected one."
  (interactive)
  (let* ((tests (alchemist-test-mode--tests-in-buffer))
         (selected (completing-read "Test: " tests))
         (position (cdr (assoc selected tests))))
    (goto-char position)
    (back-to-indentation)))

(defun alchemist-test-toggle-test-report-display ()
  "Toggle between display or hidding `alchemist-test-report-buffer-name' buffer."
  (interactive)
  (let* ((buffer (get-buffer alchemist-test-report-buffer-name))
         (window (get-buffer-window buffer)))
    (if buffer
        (if window
            (quit-window nil window)
          (display-buffer buffer))
      (message "No Alchemist test report buffer exists."))))

;;;###autoload
(define-minor-mode alchemist-test-mode
  "Minor mode for Elixir ExUnit files.

The following commands are available:

\\{alchemist-test-mode-map}"
  :lighter ""
  :keymap alchemist-test-mode-map
  :group 'alchemist
  (when alchemist-test-mode
    (alchemist-test-mode--highlight-syntax)))

;;;###autoload
(defun alchemist-test-enable-mode ()
  (if (alchemist-utils-test-file-p)
      (alchemist-test-mode)))

;;;###autoload
(dolist (hook '(alchemist-mode-hook))
  (add-hook hook 'alchemist-test-enable-mode))

(provide 'alchemist-test-mode)

;;; alchemist-test-mode.el ends here
