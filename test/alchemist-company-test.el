;;; alchemist-company-test.el ---

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

;;

;;; Code:

(require 'test-helper)

(defun message-current-position ()
  "Test helper function."
  (interactive)
  (message "CURRENT POSITION: %s" (point)))

(defun write-and-complete-in-buffer (string &optional position)
  (with-temp-buffer
    (elixir-mode)
    (company-mode)
    (insert string)
    (if position (goto-char position))
    (company-complete)
    (wait 0.1)
    company-candidates))

(ert-deftest alchemist-company-test/simple-complete ()
  (should (-same-items? (write-and-complete-in-buffer "List.del")
                        '("List.delete" "List.delete_at")))
  (should (-same-items? (write-and-complete-in-buffer "Str")
                        '("Str" "Stream" "String" "StringIO")))
  (should (-same-items? (write-and-complete-in-buffer "En")
                        '("Enum" "Enumerable")))
  (should (-same-items? (write-and-complete-in-buffer "defm")
                        '("defm" "defmacro" "defmacrop" "defmodule")))
  (should (-same-items? (write-and-complete-in-buffer "
defmodule Foo do
  def say_hi, do: true
  def say_bye, do: false

  def bar do
    say_
  end
end" 89) '("say_hi" "say_bye"))))

(ert-deftest alchemist-company-test/module-import-aware ()
  (should (-same-items? (write-and-complete-in-buffer "
defmodule Foo do
  import Mix.Generator

  def bar do
    create_
  end
end
" 67) '("create_file" "create_directory"))))


(ert-deftest alchemist-company-test/module-alias-aware ()
  (should (-same-items? (write-and-complete-in-buffer "
defmodule Foo do
  alias List, as: MyPersonalList

  def bar do
    MyPersonalList.key
  end
end
" 88) '("MyPersonalList.keydelete" "MyPersonalList.keyfind"
        "MyPersonalList.keymember?" "MyPersonalList.keyreplace"
        "MyPersonalList.keysort" "MyPersonalList.keystore"
        "MyPersonalList.keytake")))
  (should (-same-items? (write-and-complete-in-buffer "
defmodule Foo do
  alias :gen_server, as: ErlangGenServer

  def bar do
    ErlangGenServer.system_
  end
end
" 101) '("ErlangGenServer.system_code_change"
         "ErlangGenServer.system_continue"
         "ErlangGenServer.system_get_state"
         "ErlangGenServer.system_replace_state"
         "ErlangGenServer.system_terminate"))))

(ert-deftest alchemist-company-test/module-use-aware ()
  (should (-same-items? (write-and-complete-in-buffer "
defmodule Foo do
  use Behaviour

  def bar do
    defmacro
  end
end
" 61) '("defmacro" "defmacrocallback" "defmacrop"))))

(ert-deftest test-erlang-module-completion ()
  (should (-same-items? (write-and-complete-in-buffer "
:li
" 5) '(":lib" ":lists")))
  (should (-same-items? (write-and-complete-in-buffer "
:file
" 7) '(":file" ":file_io_server" ":file_server" ":file_sorter" ":filelib" ":filename")))
  (should (-same-items? (write-and-complete-in-buffer "
  foo:
" 8) '()))
  (should (-same-items? (write-and-complete-in-buffer "
foo[:bar:]
" 11) '())))

(provide 'alchemist-company-test)

;;; alchemist-company-test.el ends here
