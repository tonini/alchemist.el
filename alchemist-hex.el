;;; alchemist-hex.el --- Interface to the Hex package manager API. -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Samuel Tonini

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

;;  Interface to the Hex package manager API.

;;; Code:

(require 'json)
(require 'dash)

(defgroup alchemist-hex nil
  "Interface to the Hex package manager API."
  :prefix "alchemist-test-mode-"
  :group 'alchemist)

(defconst alchemist-hex-api-url "https://hex.pm/api/packages/"
  "Hex package manager API url.")

(defconst alchemist-hexdoc-url "https://hexdocs.pm"
  "Hexdocs url.")

(defconst alchemist-hex-buffer-name "*alchemist-hex*"
  "Name of the hex output buffer.")

(defvar alchemist-hex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map))

(defun alchemist-hex--fetch-package-info (pkg-name)
  (let* ((inhibit-message t)
         (url (concat alchemist-hex-api-url pkg-name))
         (string
          (with-current-buffer (url-retrieve-synchronously url t)
	    (goto-char (point-min))
	    (search-forward "\n\n")
	    (delete-region (point-min) (point))
            (buffer-string))))
    (when (string-match-p "\"status\":404" string)
      (error (format "There is no hex package [%s] available" pkg-name)))
    (json-read-from-string string)))

(defun alchemist-hex--fetch-search-packages (pkg-name)
  (let* ((inhibit-message t)
         (url (concat alchemist-hex-api-url "?search=" pkg-name))
         (string
          (with-current-buffer (url-retrieve-synchronously url t)
            (goto-char (point-min))
            (search-forward "\n\n")
            (delete-region (point-min) (point))
            (buffer-string))))
    (json-read-from-string string)))

(defun alchemist-hex--deps-name-at-point ()
  "Return the dependency name under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-_a-z0-9")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun alchemist-hex--display-releases-for (package-name)
  (let* ((infos (alchemist-hex--fetch-package-info package-name))
         (releases (cdr (assoc 'releases infos)))
         (latest-version (cdr (assoc 'version (aref releases 0))))
         (latest-version-url (cdr (assoc 'url (aref releases 0))))
         (buffer (get-buffer-create alchemist-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (erase-buffer)
        (insert (propertize (format "%s versions  (latest version " package-name)
                            'face font-lock-variable-name-face))
        (insert-button latest-version
                       'action (lambda (x) (browse-url (button-get x 'url)))
                       'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-version-url))
        (insert ")\n\n")
        (-map (lambda (release)
                (let ((version (cdr (assoc 'version release)))
                      (url (cdr (assoc 'url release)))
                      (date (date-to-time (cdr (assoc 'inserted_at release)))))
                  (insert-button version
                                 'action (lambda (x) (browse-url (button-get x 'url)))
                                 'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" url))
                  (insert (format "     (%s %s)"
                                  (propertize "released on" 'face font-lock-string-face)
                                  (format-time-string "%Y-%m-%d" date)))
                  (insert "   (")
                  (insert-button "docs"
                                 'face font-lock-constant-face
                                 'action (lambda (x) (browse-url (button-get x 'url)))
                                 'url (format "%s/%s/%s" alchemist-hexdoc-url package-name version))
                  (insert ")\n"))) releases)
        (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "    (") 1 1 t)
        (goto-char (point-min))
        (alchemist-hex-mode)))
    (pop-to-buffer buffer)))

(defun alchemist-hex--display-info-for (package-name)
  (let* ((infos (alchemist-hex--fetch-package-info package-name))
         (releases (cdr (assoc 'releases infos)))
         (latest-release (cdr (assoc 'version (aref releases 0))))
         (meta (assoc 'meta infos))
         (buffer (get-buffer-create alchemist-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (latest-version (cdr (assoc 'version (aref releases 0))))
            (latest-version-url (cdr (assoc 'url (aref releases 0)))))
        (goto-char (point-min))
        (erase-buffer)
        (insert (cdr (assoc 'description meta)))
        (insert "\n\n")
        (insert (propertize "Config: "
                            'face font-lock-string-face))
        (insert (format "{:%s, => \"~> %s\"}" package-name latest-release))
        (insert "\n")
        (insert (propertize "Latest release: " 'face font-lock-string-face))
        (insert-button latest-version
                       'action (lambda (x) (browse-url (button-get x 'url)))
                       'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-version-url))
        (insert "\n\n")
        (insert (propertize "Maintainers: " 'face font-lock-string-face))
        (-map (lambda (maintainer)
                  (insert (format "\n  - %s" (decode-coding-string maintainer 'utf-8-auto)))
                  ) (cdr (assoc 'maintainers meta)))
        (insert "\n")
        (insert (propertize "Licenses: " 'face font-lock-string-face))
        (-map (lambda (license)
                  (insert (format "%s" (decode-coding-string license 'utf-8-auto)))
                ) (cdr (assoc 'licenses meta)))
        (insert "\n")
        (insert (propertize "Links: " 'face font-lock-string-face))
        (-map (lambda (link)
                (let ((link-name (car link))
                      (url (decode-coding-string (cdr link) 'utf-8-auto)))
                  (insert (format "\n  %s: " link-name))
                  (insert-button url
                                 'action (lambda (x) (browse-url (button-get x 'url)))
                                 'url url))) (cdr (assoc 'links meta)))
        (insert "\n")
        (insert (propertize "Releases: \n" 'face font-lock-string-face))
        (-map (lambda (release)
                (let ((version (cdr (assoc 'version release)))
                      (url (cdr (assoc 'url release))))
                (insert "  - ")
                (insert-button version
                               'action (lambda (x) (browse-url (button-get x 'url)))
                               'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" url))
                (insert "     (")
                (insert-button "docs"
                               'face font-lock-constant-face
                               'action (lambda (x) (browse-url (button-get x 'url)))
                               'url (format "%s/%s/%s" alchemist-hexdoc-url package-name version))
                (insert ")\n"))) releases)
        (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "     (") 1 1 t)
        (goto-char (point-min))
        (alchemist-hex-mode)))
    (pop-to-buffer buffer)))

(defun alchemist-hex-search (package-name)
  "Search for Hex packages."
  (interactive "Mhex search: \n")
  (let* ((packages (alchemist-hex--fetch-search-packages package-name))
         (buffer (get-buffer-create alchemist-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (erase-buffer)
        (insert (propertize "search results for: "
                            'face font-lock-variable-name-face))
        (insert (propertize (format "%s \n\n" package-name)
                            'face font-lock-builtin-face))
        (-map (lambda (package)
                (let ((name (cdr (assoc 'name package)))
                      (date (date-to-time (cdr (assoc 'inserted_at package))))
                      (version (cdr (assoc 'version (aref (cdr (assoc 'releases package)) 0))))
                      (url (cdr (assoc 'url package)))
                      (latest-release-url (cdr (assoc 'url (aref (cdr (assoc 'releases package)) 0)))))
                  (when (string-match-p package-name name)
                    (insert-button name
                                   'action (lambda (x) (browse-url (button-get x 'url)))
                                   'url url)
                    (insert "   ")
                    (insert-button version
                                   'action (lambda (x) (browse-url (button-get x 'url)))
                                   'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-release-url))

                    (insert (format "     (%s %s)"
                                    (propertize "released on" 'face font-lock-string-face)
                                    (format-time-string "%Y-%m-%d" date)))
                    (insert "   (")
                    (insert-button "docs"
                                   'face font-lock-constant-face
                                   'action (lambda (x) (browse-url (button-get x 'url)))
                                   'url (format "%s/%s/%s" alchemist-hexdoc-url name version))
                    (insert ")\n")
                    (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "  ") 1 1 t))))
              packages)
        (goto-char (point-min))
        (alchemist-hex-mode)))
    (pop-to-buffer buffer)))

(defun alchemist-hex-all-dependencies ()
  "Display Hex package dependencies for the current Mix project."
  (interactive)
  (unless (alchemist-project-p)
    (error "No 'mix.exs' file exists."))
  (let* ((mix-content
          (with-temp-buffer
            (insert-file-contents (concat (alchemist-project-root) "mix.exs"))
            (goto-char (point-min))
            (delete-matching-lines "#")
            (buffer-string)))
         (deps-start (with-temp-buffer
                       (insert mix-content)
                       (goto-char (point-min))
                       (search-forward "defp deps do")))
         (deps-end (with-temp-buffer
                     (insert mix-content)
                     (goto-char deps-start)
                     (search-forward "end")))
         (deps (substring mix-content deps-start (- deps-end 4)))
         (deps (replace-regexp-in-string "\\(\\[\\|\\]\\)" "" deps))
         (deps (split-string deps "}\s*,"))
         (deps (-map (lambda (dep)
                       (replace-regexp-in-string "\\(\{\\|\}\\|\n\\|^\s*\\)" "" dep))
                     deps))
         (deps (-sort 'equal deps)))
    (unless (string-match-p "\s*defp? deps do" mix-content)
      (error "No dependency informations available in 'mix.exs'."))
    (let* ((content (with-temp-buffer
                      (goto-char (point-min))
                      (erase-buffer)
                      (-map (lambda (dep)
                              (insert dep)
                              (insert "\n")) deps)
                      (goto-char (point-min))
                      (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" ", ") 1 1 t)
                      (while (search-forward ", " nil t)
                        (replace-match " " nil t))
                      (sort-lines nil (point-min) (point-max))
                      (buffer-string))))
      (alchemist-interact-create-popup alchemist-hex-buffer-name
                                       content
                                       #'(lambda ()
                                           (elixir-mode)
                                           (alchemist-hex-mode))))))

(defun alchemist-hex-info-at-point ()
  "Display Hex package information for the package at point."
  (interactive)
  (alchemist-hex--display-info-for (alchemist-hex--deps-name-at-point)))

(defun alchemist-hex-releases-at-point ()
  "Display Hex package releases for the package at point."
  (interactive)
  (alchemist-hex--display-releases-for (alchemist-hex--deps-name-at-point)))

(defun alchemist-hex-releases (package-name)
  "Display Hex package releases for a certain package."
  (interactive "Mhex releases: \n")
  (alchemist-hex--display-releases-for package-name))

(defun alchemist-hex-info (package-name)
  "Display Hex package info for a certain package."
  (interactive "Mhex info: \n")
  (alchemist-hex--display-info-for package-name))

(define-minor-mode alchemist-hex-mode
  "Minor mode for displaying Hex package manager informations.

\\{alchemist-hex-mode-map}"
  nil
  "Alchemist-Hex"
  alchemist-hex-mode-map
  (setq buffer-read-only t))

(provide 'alchemist-hex)

;;; alchemist-hex.el ends here
