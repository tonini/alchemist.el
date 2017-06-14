;;; alchemist-project-tests.el ---

;; Copyright © 2014-2017 Samuel Tonini
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

(ert-deftest test-project-root/no-argument ()
  "Should use `default-directory' when no argument."
  (setq alchemist-project-root-path-cache nil)
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root) alchemist-sandbox-path))))

(ert-deftest test-project-root/except-files-exists ()
  "Should use `default-directory' when no argument."
  (setq alchemist-project-root-path-cache nil)
  (with-sandbox
   (f-touch "mix.exs")
   (f-touch ".hex")
   (f-mkdir "path" "to" "lib")
   (should-not (alchemist-project-root))))

(ert-deftest test-project-root/directory-as-argument  ()
  "Should find root directory when directory as argument."
  (setq alchemist-project-root-path-cache nil)
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root "path/to/lib") alchemist-sandbox-path))))

(ert-deftest test-project-root/no-project-root  ()
  "Should return nil when no root."
  (setq alchemist-project-root-path-cache nil)
  (with-sandbox
   (f-mkdir "path" "to" "lib")
   (should (equal (alchemist-project-root "path/to/lib") nil))))

(ert-deftest test-project-name/no-project-root ()
  "Should return an empty string"
  (setq alchemist-project-root-path-cache nil)
  (with-sandbox
   (should (equal (alchemist-project-name) ""))))

(ert-deftest test-project-name/project-exists ()
  (setq alchemist-project-root-path-cache nil)
  "Should return name of the project"
  (with-sandbox
   (f-touch "mix.exs")
   (should (equal (alchemist-project-name) "sandbox"))))

(ert-deftest alchemist-project/file-under-test ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "web" "controllers")
   (f-mkdir "test" "controllers")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/file.ex")
   (f-touch "test/path/to/file_test.exs")
   (f-touch "web/controllers/my_controller.ex")
   (f-touch "test/controllers/my_controller_test.exs")
   (should (equal (file-name-nondirectory
                   (alchemist-project-file-under-test "test/path/to/file_test.exs" "lib"))
                  "file.ex"))
   (should (equal (file-name-nondirectory
                   (alchemist-project-file-under-test "test/controllers/my_controller_test.exs" "web"))
                  "my_controller.ex"))))

(ert-deftest alchemsit-project/switch-from-test-to-file-under-test-1 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/file.ex")
   (f-touch "test/path/to/file_test.exs")
   (find-file "test/path/to/file_test.exs")
   (alchemist-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "file.ex"))))

(ert-deftest alchemsit-project/switch-from-test-to-file-under-test-2 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "controllers")
   (f-mkdir "test" "controllers")
   (f-touch "web/controllers/my_controller.ex")
   (f-touch "test/controllers/my_controller_test.exs")
   (find-file "test/controllers/my_controller_test.exs")
   (alchemist-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "my_controller.ex"))))

(ert-deftest alchemist-project/switch-from-file-under-test-to-test-file-1 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "lib" "path" "to")
   (f-mkdir "test" "path" "to")
   (f-touch "lib/path/to/other_file.ex")
   (f-touch "test/path/to/other_file_test.exs")
   (find-file "lib/path/to/other_file.ex")
   (alchemist-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "other_file_test.exs"))))

(ert-deftest alchemist-project/switch-from-file-under-test-to-test-file-2 ()
  (with-sandbox
   (f-touch "mix.exs")
   (f-mkdir "web" "views")
   (f-mkdir "test" "views")
   (f-touch "web/views/my_view.ex")
   (f-touch "test/views/my_view_test.exs")
   (find-file "web/views/my_view.ex")
   (alchemist-project-toggle-file-and-tests)
   (should (equal (file-name-nondirectory (buffer-file-name))
                  "my_view_test.exs"))))

(ert-deftest alchemist-project/inside-elixir-codebase ()
  (setq alchemist-goto-elixir-source-dir alchemist-sandbox-path)
  (with-sandbox
   (should (alchemist-project-elixir-p))
   (should (equal (alchemist-project-elixir-root) alchemist-sandbox-path))))

(ert-deftest alchemist-project/not-inside-elixir-codebase ()
  (setq alchemist-goto-elixir-source-dir nil)
  (with-sandbox
   (should-not (alchemist-project-elixir-p))
   (should (equal nil (alchemist-project-elixir-root)))))

(provide 'alchemist-project-tests)
