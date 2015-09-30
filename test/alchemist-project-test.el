;;; alchemist-project-tests.el ---

;; Copyright © 2014-2015 Samuel Tonini
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

(ert-deftest test-project-root/no-argument ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root) alchemist-sandbox-path))))

(ert-deftest test-project-root/except-files-exists ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-touch "mix.exs")
   (f-touch ".hex")
   (f-mkdir "path" "to" "lib")
   (should-not (alchemist-project-root))))

(ert-deftest test-project-root/directory-as-argument  ()
  "Should find root directory when directory as argument."
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root "path/to/lib") alchemist-sandbox-path))))

(ert-deftest test-project-root/no-project-root  ()
  "Should return nil when no root."
  (with-sandbox
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root "path/to/lib") nil))))

(ert-deftest test-project-name/no-project-root ()
  "Should return an empty string"
  (with-sandbox
   (should (equal (alchemist-project-name) ""))))

(ert-deftest test-project-name/project-exists ()
  "Should return name of the project"
  (with-sandbox
   (f-touch "mix.exs")
   (should (equal (alchemist-project-name) "sandbox"))))

(ert-deftest test-project-toggle/from-test-to-implementation ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/file.ex")
   (f-touch "test/path/to/file_test.exs")
   (find-file "test/path/to/file_test.exs")

   (alchemist-project-toggle-file-and-tests)
   (should (string-match "lib/path/to/file.ex"
                         (buffer-file-name)))))

(ert-deftest test-project-toggle/from-test-to-implementation/with_web_dir ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "web/path/to/web_file.ex")
   (f-touch "test/path/to/web_file_test.exs")
   (find-file "test/path/to/web_file_test.exs")

   (alchemist-project-toggle-file-and-tests)
   (should (string-match "web/path/to/web_file.ex"
                         (buffer-file-name)))))

(ert-deftest test-project-toggle/from-implementation-to-test ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/other_file.ex")
   (f-touch "test/path/to/other_file_test.exs")

   (find-file "lib/path/to/other_file.ex")
   (alchemist-project-toggle-file-and-tests)
   (should (string-match "test/path/to/other_file_test.exs"
                         (buffer-file-name)))))

(ert-deftest test-project-toggle/from-implementation-to-test/with-web-dir ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "web/path/to/web_other_file.ex")
   (f-touch "test/path/to/web_other_file_test.exs")

   (find-file "web/path/to/web_other_file.ex")
   (alchemist-project-toggle-file-and-tests)
   (should (string-match "test/path/to/web_other_file_test.exs"
                         (buffer-file-name)))))

(ert-deftest test-project-find-files-in-directory ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib")
   (f-mkdir "lib" "path")
   (f-touch "lib/file.ex")
   (f-touch "lib/another.ex")
   (f-touch "lib/path/foo.ex")
   (f-mkdir "web")
   (f-mkdir "web" "controllers")
   (f-mkdir "web" "controllers" "path")
   (f-touch "web/controllers/file.ex")
   (f-touch "web/controllers/another.ex")
   (f-touch "web/controllers/path/foo.ex")
   (should (equal (alchemist-project-files (alchemist-project-root) "lib")
                  '("lib/another.ex" "lib/file.ex" "lib/path/foo.ex")))
   (should (equal (alchemist-project-files (alchemist-project-root) "web/controllers")
                  '("web/controllers/another.ex" "web/controllers/file.ex" "web/controllers/path/foo.ex")))))

(provide 'alchemist-project-tests)
