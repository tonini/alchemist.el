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

(defvar alchemist-server-processes '()
  "Store running Alchemist server processes.")

(defvar alchemist-server-env "dev"
  "Default environment in for the Alchemist server.")

(defvar alchemist-server-envs '("dev" "prod" "test" "shared")
  "List of available Alchemist server environments.")

(defconst alchemist-server
  (concat (file-name-directory load-file-name) "alchemist-server/run.exs")
  "Path to the Alchemist server file.")

(defconst alchemist-server-command
  (format "%s %s %s"
          alchemist-execute-command
          alchemist-server
          alchemist-server-env)
  "Alchemist server command.")

(defconst alchemist-server-codes '((server-eval "EVAL")
                                   (server-defl "DEFL")
                                   (server-info "INFO")
                                   (server-docl "DOCL")
                                   (server-comp "COMP"))
  "Alchemist server API codes.")

(defun alchemist-server-start (env)
  "Start alchemist server for the current mix project in specific ENV.

If a server already running, the current one will be killed and new one
will be started instead."
  (interactive (list
                (completing-read (format "(Alchemist-Server) run in environment: (default: %s) " alchemist-server-env)
                                 alchemist-server-envs nil nil nil)))
  (when (alchemist-server-process-p)
    (kill-process (alchemist-server-process)))
  (alchemist-server-start-in-env env))

(defun alchemist-server-start-if-not-running ()
  "Start a new Alchemist server if not already running.

An Alchemist server will be started for the current Elixir mix project."
  (unless (alchemist-server-process-p)
    (alchemist-server-start-in-env alchemist-server-env)))

(defun alchemist-server-start-in-env (env)
  "Start an Alchemist server with the ENV."
  (let* ((process-name (alchemist-server-process-name))
         (default-directory (if (string= process-name "alchemist-server")
                                default-directory
                              process-name))
         (server-command (format "%s %s %s"
                                 alchemist-execute-command
                                 (shell-quote-argument alchemist-server)
                                 (shell-quote-argument env)))
         (process (start-process-shell-command process-name "*alchemist-server*" server-command)))
    (set-process-query-on-exit-flag process nil)
    (alchemist-server--store-process process)))

(defun alchemist-server--store-process (process)
  "Store PROCESS in `alchemist-server-processes'."
  (let ((process-name (alchemist-server-process-name)))
    (if (cdr (assoc process-name alchemist-server-processes))
        (setq alchemist-server-processes
              (delq (assoc process-name alchemist-server-processes) alchemist-server-processes)))
    (add-to-list 'alchemist-server-processes (cons process-name process))))

(defun alchemist-server-process-p ()
  "Return non-nil if a process for the current
Elixir mix project is live."
  (process-live-p (alchemist-server-process)))

(defun alchemist-server-process ()
  "Return process for the current Elixir mix project."
  (cdr (assoc (alchemist-server-process-name) alchemist-server-processes)))

(defun alchemist-server-process-name ()
  "Return process name for the current Elixir mix project."
  (let* ((process-name (if (alchemist-project-elixir-p)
                           "alchemist-server"
                         (alchemist-project-root)))
         (process-name (if process-name
                           process-name
                         "alchemist-server")))
    process-name))

(defun alchemist-server-api-code (symbol)
  "Return Alchemist server API code for SYMBOL."
  (car (cdr (assoc symbol alchemist-server-codes))))

(defconst alchemist-server-code-end-marker-regex
  (format "END-OF-\\(%s\\|%s\\|%s\\|%s\\|%s\\)$"
          (alchemist-server-api-code 'server-eval)
          (alchemist-server-api-code 'server-defl)
          (alchemist-server-api-code 'server-info)
          (alchemist-server-api-code 'server-docl)
          (alchemist-server-api-code 'server-comp))
  "Regular expression to identify Alchemist server API end markers.")

(defun alchemist-server-contains-end-marker-p (string)
  "Return non-nil if STRING contain an Alchemist server API end marker."
  (when string
    (string-match-p alchemist-server-code-end-marker-regex string)))

(defun alchemist-server-build-request-string (code &optional args)
  "Build Alchemist server request string for CODE.

If ARGS available add them to the request string."
  (let* ((code (car (cdr (assoc code alchemist-server-codes)))))
    (if args
        (format "%s %s\n" code args)
      (format "%s\n" code))))

(defun alchemist-server-prepare-filter-output (output)
  "Clean OUTPUT by remove Alchemist server API end markes."
  (let* ((output (apply #'concat (reverse output)))
         (output (replace-regexp-in-string alchemist-server-code-end-marker-regex "" output))
         (output (replace-regexp-in-string "\n+$" "" output)))
    output))

(defun alchemist-server-send-request (string filter)
  "Send STRING to Alchemist server API and set FILTER to process."
  (alchemist-server-start-if-not-running)
  (set-process-filter (alchemist-server-process) filter)
  (process-send-string (alchemist-server-process) string))

(defun alchemist-server-goto (args filter)
  "Make an Alchemist server source request with ARGS.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-defl args) filter))

(defun alchemist-server-info (args filter)
  "Make an Alchemist server mix request.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-info args) filter))

(defun alchemist-server-help-with-modules (filter)
  "Make an Alchemist server modules request.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-info) filter))

(defun alchemist-server-help (args filter)
  "Make an Alchemist server doc request with ARGS.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-docl args) filter))

(defun alchemist-server-eval (args filter)
  "Make an Alchemist server evaluate request with FILE.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-eval args) filter))

(defun alchemist-server-complete-candidates (args filter)
  "Make an Alchemist server complete request with ARGS.

Process server respond with FILTER."
  (alchemist-server-start-if-not-running)
  (alchemist-server-send-request (alchemist-server-build-request-string 'server-comp args) filter))

(defun alchemist-server-status ()
  "Report the server status for the current Elixir project."
  (interactive)
  (message "Alchemist-Server-Status: [Project: %s Status: %s]"
           (alchemist-server-process-name)
           (if (alchemist-server-process-p)
               "Connected"
             "Not Connected")))

(provide 'alchemist-server)

;;; alchemist-server.el ends here
