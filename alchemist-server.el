;;; alchemist-server.el --- -*- lexical-binding: t -*-

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

(defvar alchemist-server
  (concat (file-name-directory load-file-name) "alchemist.exs")
  "Script file with alchemist server.")

(defvar alchemist-server-processes '())
(defvar alchemist-server-env "dev")

(defvar alchemist-server-command
  (format "elixir %s %s" alchemist-server alchemist-server-env))

(defun alchemist-server-start ()
  (let* ((process-name (alchemist-server-process-name))
         (default-directory (if (string= process-name "alchemist-server")
                                default-directory
                              process-name))
         (process (start-process-shell-command process-name "*alchemist-server*" alchemist-server-command)))
    (add-to-list 'alchemist-server-processes (cons process-name process))))

(defun alchemist-server-process-p ()
  (process-live-p (alchemist-server-process)))

(defun alchemist-server-process ()
  (cdr (assoc (alchemist-server-process-name) alchemist-server-processes)))

(defun alchemist-server-process-name ()
  (let* ((process-name (alchemist-project-root))
         (process-name (if process-name
                           process-name
                         "alchemist-server")))
    process-name))

(defun alchemist-server-doc-filter (process output)
  (setq alchemist-server--output (cons output alchemist-server--output))
  (if (string-match "END-OF-DOC$" output)
      (let* ((string (apply #'concat (reverse alchemist-server--output)))
             (string (replace-regexp-in-string "END-OF-DOC$" "" string)))
        (alchemist-help--initialize-buffer string))))

(defun alchemist-server-complete-canidates-filter (process output)
  (setq alchemist-server--output (cons output alchemist-server--output))
  (if (string-match "END-OF-COMPLETE$" output)
      (let* ((string (apply #'concat (reverse alchemist-server--output)))
            (string (replace-regexp-in-string "END-OF-COMPLETE$" "" string))
            (candidates (alchemist-complete--output-to-list
                         (alchemist--utils-clear-ansi-sequences string)))
            (candidates (alchemist-complete--build-candidates candidates)))
        (funcall alchemist-server-company-callback candidates))))

(defun alchemist-server-complete-filter (process output)
  (setq alchemist-server--output (cons output alchemist-server--output))
  (if (string-match "END-OF-COMPLETE$" output)
      (let* ((string (apply #'concat (reverse alchemist-server--output)))
            (string (replace-regexp-in-string "END-OF-COMPLETE$" "" string))
            (candidates (alchemist-complete--output-to-list
                         (alchemist--utils-clear-ansi-sequences string))))
        (funcall alchemist-server-help-callback candidates))))

(defun alchemist-server-help-complete-modules-filter (process output)
  (setq alchemist-server--output (cons output alchemist-server--output))
  (if (string-match "END-OF-MODULES$" output)
      (let* ((output (apply #'concat (reverse alchemist-server--output)))
             (modules (alchemist-help--elixir-modules-to-list output))
             (search (completing-read
                      "Elixir help: "
                      modules
                      nil
                      nil
                      nil)))
        (alchemist-help--execute (if (string-match-p "\\.$" search)
                                     search
                                   (concat search "."))))))

(defun alchemist-server-goto-filter (process output)
  (setq alchemist-server--output (cons output alchemist-server--output))
  (if (string-match "END-OF-SOURCE$" output)
      (let* ((output (apply #'concat (reverse alchemist-server--output)))
             (output (replace-regexp-in-string "END-OF-SOURCE" "" output))
             (output (replace-regexp-in-string "\n" "" output))
             (file (replace-regexp-in-string "source-file-path:" "" output)))
        (funcall alchemist-server-goto-callback file))))

(defun alchemist-server-goto (module function expr)
  (setq alchemist-server--output nil)
  (unless (alchemist-server-process-p)
    (alchemist-server-start))
  (setq alchemist-server-goto-callback (lambda (file)
                                         (cond ((equal file nil)
                                                (message "Don't know how to find: %s" expr))
                                               ((file-exists-p file)
                                                (alchemist-goto--open-file file module function))
                                               ((alchemist-goto--elixir-file-p file)
                                                (let* ((elixir-source-file (alchemist-goto--build-elixir-ex-core-file file)))
                                                  (if (file-exists-p elixir-source-file)
                                                      (alchemist-goto--open-file elixir-source-file module function)
                                                    (message "Don't know how to find: %s" expr))))
                                               ((alchemist-goto--erlang-file-p file)
                                                (let* ((elixir-source-file (alchemist-goto--build-elixir-erl-core-file file))
                                                       (erlang-source-file (alchemist-goto--build-erlang-core-file file)))
                                                  (cond ((file-exists-p elixir-source-file)
                                                         (alchemist-goto--open-file elixir-source-file module function))
                                                        ((file-exists-p erlang-source-file)
                                                         (alchemist-goto--open-file erlang-source-file module function))
                                                        (t
                                                         (message "Don't know how to find: %s" expr)))))
                                               (t
                                                (pop-tag-mark)
                                                (message "Don't know how to find: %s" expr)))))
  (set-process-filter (alchemist-server-process) #'alchemist-server-goto-filter)
  (process-send-string (alchemist-server-process) (format "SOURCE %s,%s\n" module function)))

(defun alchemist-server-help ()
  (setq alchemist-server--output nil)
  (unless (alchemist-server-process-p)
    (alchemist-server-start))
  (set-process-filter (alchemist-server-process) #'alchemist-server-help-complete-modules-filter)
  (process-send-string (alchemist-server-process) "MODULES\n"))

(defun alchemist-server-complete-candidates (exp)
  (setq alchemist-server--output nil)
  (unless (alchemist-server-process-p)
    (alchemist-server-start))
  (set-process-filter (alchemist-server-process) #'alchemist-server-complete-canidates-filter)
  (process-send-string (alchemist-server-process) (format "COMPLETE %s\n" exp)))

(defun alchemist-server-help-with-complete (search)
  (setq alchemist-server--output nil)
  (unless (alchemist-server-process-p)
    (alchemist-server-start))
  (setq alchemist-server-help-callback (lambda (candidates)
                                         (if candidates
                                             (let* ((search (alchemist-complete--completing-prompt search candidates)))
                                               (setq alchemist-help-current-search-text search)
                                               (setq alchemist-server--output nil)
                                               (set-process-filter (alchemist-server-process) #'alchemist-server-doc-filter)
                                               (process-send-string (alchemist-server-process) (format "DOC %s\n" search))))
                                         (message "No documentation found for '%s'" search)))
  (set-process-filter (alchemist-server-process) #'alchemist-server-complete-filter)
  (process-send-string (alchemist-server-process) (format "COMPLETE %s\n" search)))

;; (rplacd (assoc 'y values) 201)

;; kill-emacs-hook
;; Kill running process before kill emacs
;; handle quit functionality inside filters

(provide 'alchemist-server)

;;; alchemist-server.el ends here
