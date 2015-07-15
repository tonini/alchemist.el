;;; alchemist-report.el ---

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

;;

;;; Code:

(defvar alchemist-report-on-exit nil)
(defvar alchemist-report-on-exit-function nil)
(defvar alchemist-report-on-render nil)
(defvar alchemist-report-on-render-function nil)
(defvar alchemist-report--last-run-status nil)

(defun alchemist-report--cleanup-buffer (buffer)
  (let ((buffer (get-buffer buffer)))
    (when buffer
      (kill-buffer buffer))))

(defun alchemist-report--display-buffer (buffer mode)
  (with-current-buffer buffer
    (funcall mode))
  (display-buffer buffer))

(defun alchemist-report--sentinel (process status)
  "Sentinel for test report buffer."
  (if (memq (process-status process) '(exit signal))
      (let ((buffer (process-buffer process)))
        (if (null (buffer-name buffer))
            (set-process-buffer process nil)
          (progn
            (alchemist-report--render-report status)
            (alchemist-report--handle-exit status)
            (delete-process process))))))

(defun alchemist-report--render-report (buffer)
  (when alchemist-report-on-render-function
    (funcall alchemist-report-on-render-function buffer)))

(defun alchemist-report--handle-exit (status)
  (alchemist-report--store-process-status status)
  (when alchemist-report-on-exit-function
    (funcall alchemist-report-on-exit-function status)))

(defun alchemist-report--store-process-status (status)
  (setq alchemist-report--last-run-status status))

(defun alchemist-report--last-run-successful-p ()
  (when (string-prefix-p "finished" alchemist-report--last-run-status) t))

(defun alchemist-report--ansi-color-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let* ((buffer-read-only nil)
           (moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (if moving (goto-char (process-mark proc))))))

(defun alchemist-report-run (command process-name buffer-name mode &optional on-exit on-render)
  (alchemist-report--cleanup-buffer buffer-name)
  (let* ((buffer (get-buffer-create buffer-name))
         (project-root (alchemist-project-root))
         (default-directory (if project-root
                                project-root
                              default-directory))
         (process (start-process-shell-command process-name buffer command)))
    (when on-exit
      (setq alchemist-report-on-exit-function on-exit))
    (when on-render
      (setq alchemist-report-on-render-function on-render))
    (set-process-sentinel process 'alchemist-report--sentinel)
    (set-process-filter process 'alchemist-report--ansi-color-insertion-filter)
    (alchemist-report--display-buffer buffer mode)))

(provide 'alchemist-report)

;;; alchemist-report.el ends here
