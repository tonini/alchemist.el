;;; alchemist-server-test.el ---

;; Copyright © 2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

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

;;; Code:

(require 'test-helper)

(ert-deftest not-inside-project/start-default-server ()
  (alchemist-server-start)
  (sleep-for 1)
  (should (string= "alchemist-server"
                   (process-name (alchemist-server-process))))
  (should (string= "run"
                   (process-status (process-name (alchemist-server-process))))))

(ert-deftest inside-project/start-project-server ()
  (with-sandbox
   (f-touch "mix.exs")
   (alchemist-server-start)
   (should (string-match-p "alchemist\\.el\\/test\\/sandbox\\/$"
                           (process-name (alchemist-server-process))))
   (should (string= "run"
                    (process-status (process-name (alchemist-server-process)))))))

(ert-deftest inside-project/get-process-name ()
  (with-sandbox
   (f-touch "mix.exs")
   (alchemist-server-start)
   (should (string-match-p "alchemist\\.el\\/test\\/sandbox\\/$"
                           (alchemist-server-process-name)))))

(ert-deftest check-if-process-is-running ()
  (should (not (alchemist-server-process-p)))
  (should (progn
            (alchemist-server-start)
            (alchemist-server-process-p))))

(provide 'alchemist-server-test)

;;; alchemist-server-test.el ends here
