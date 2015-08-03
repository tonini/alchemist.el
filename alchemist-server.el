;;; alchemist-server.el --- Interface to the Alchemist Elixir server. -*- lexical-binding: t -*-

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

;; Interface to the Alchemist Elixir server.

;;; Code:

(require 'alchemist-execute)

(defgroup alchemist-server nil
  "Interface to the Alchemist Elixir server."
  :prefix "alchemist-server-"
  :group 'alchemist)

(defvar alchemist-server--envs '("dev" "prod" "test" "shared")
  "The list of server environments to use.")

(defconst alchemist-server
  (concat (file-name-directory load-file-name) "server/run.exs")
  "Script file with alchemist server.")

(defvar alchemist-server--processes '())
(defvar alchemist-server--env "dev")

(defconst alchemist-server-codes '((evaluate "EVAL")
                                   (eval-quote "QUOTE")
                                   (source "SOURCE")
                                   (mixtasks "MIXTASKS")
                                   (modules "MODULES")
                                   (doc "DOC")
                                   (complete "COMPLETE")))

(defconst alchemist-server-command
  (format "%s %s %s" alchemist-execute-command alchemist-server alchemist-server--env))

(defun alchemist-server-start (env)
  "Start alchemist server for the current mix project in specific ENV.

If a server already running, the current one will be killed and new one
will be started instead."
  (interactive (list
                (completing-read (format "(Alchemist-Server) run in environment: (default: %s) " alchemist-server--env)
                                 alchemist-server--envs nil nil nil)))
  (when (alchemist-server--process-p)
    (kill-process (alchemist-server--process)))
  (alchemist-server--start-with-env env))

(defun alchemist-server--start ()
  (unless (alchemist-server--process-p)
    (alchemist-server--start-with-env alchemist-server--env)))

(defun alchemist-server--start-with-env (env)
  (let* ((process-name (alchemist-server--process-name))
         (default-directory (if (string= process-name "alchemist-server")
                                default-directory
                              process-name))
         (server-command (format "elixir %s %s" alchemist-server env))
         (process (start-process-shell-command process-name "*alchemist-server*" server-command)))
    (set-process-query-on-exit-flag process nil)
    (alchemist-server--store-process process)))

(defun alchemist-server--store-process (process)
  (let ((process-name (alchemist-server--process-name)))
    (if (cdr (assoc process-name alchemist-server--processes))
        (setq alchemist-server--processes
              (delq (assoc process-name alchemist-server--processes) alchemist-server--processes)))
    (add-to-list 'alchemist-server--processes (cons process-name process))))

(defun alchemist-server--process-p ()
  (process-live-p (alchemist-server--process)))

(defun alchemist-server--process ()
  (cdr (assoc (alchemist-server--process-name) alchemist-server--processes)))

(defun alchemist-server--process-name ()
  (let* ((process-name (alchemist-project-root))
         (process-name (if process-name
                           process-name
                         "alchemist-server")))
    process-name))

(defun alchemist-server--build-request-id (code &optional args)
  (let* ((code (car (cdr (assoc code alchemist-server-codes)))))
    (if args
        (format "%s %s\n" code args)
      (format "%s\n" code))))

(defun alchemist-server-send-request (id filter)
  (alchemist-server--start)
  (set-process-filter (alchemist-server--process) filter)
  (process-send-string (alchemist-server--process) id))

(defun alchemist-server-goto (args filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'source args) filter))

(defun alchemist-server--mix (filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'mixtasks) filter))

(defun alchemist-server-help-with-modules (filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'modules) filter))

(defun alchemist-server-help (args filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'doc args) filter))

(defun alchemist-server-eval (file filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'evaluate file) filter))

(defun alchemist-server-eval-quote (file filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'eval-quote file) filter))

(defun alchemist-server-complete-candidates (args filter)
  (alchemist-server--start)
  (alchemist-server-send-request (alchemist-server--build-request-id 'complete args) filter))

(defun alchemist-server-status ()
  "Report the server status for the current Elixir project."
  (interactive)
  (message "Alchemist-Server-Status: [Project: %s Status: %s]"
           (alchemist-server--process-name)
           (if (alchemist-server--process-p)
               "Connected"
             "Not Connected")))

(provide 'alchemist-server)

;;; alchemist-server.el ends here
