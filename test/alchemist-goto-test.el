;;; alchemist-goto-test.el ---

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

(ert-deftest check-if-an-elixir-source-file ()
  (should (alchemist-goto-elixir-file-p "lib/elixir/lib/list.ex"))
  (should (alchemist-goto-elixir-file-p "lib/elixir/lib/enum.exs"))
  (should-not (alchemist-goto-elixir-file-p "lib/kernel/lib/gen_tcp.erl")))

(ert-deftest check-if-an-erlang-source-file ()
  (should (alchemist-goto-erlang-file-p "lib/elixir/src/list.erl"))
  (should (alchemist-goto-erlang-file-p "lib/elixir/src/elixir.erl"))
  (should-not (alchemist-goto-erlang-file-p "lib/kernel/lib/enum.ex")))

(ert-deftest build-an-elixir-ex-core-file ()
  (set (make-local-variable 'alchemist-goto-elixir-source-dir) "/elixir-source/")
  (should (equal (alchemist-goto--build-elixir-ex-core-file
                  "/private/tmp/elixir-asdASDq/lib/elixir/lib/elixir.ex")
                 "/elixir-source/lib/elixir/lib/elixir.ex")))

(ert-deftest build-an-elixir-erl-core-file ()
  (set (make-local-variable 'alchemist-goto-elixir-source-dir) "/elixir-source/")
  (should (equal (alchemist-goto--build-elixir-erl-core-file
                  "/private/tmp/elixir-asdASDq/lib/elixir/src/elixir.erl")
                 "/elixir-source/lib/elixir/src/elixir.erl")))

(ert-deftest build-an-erlang-core-file ()
  (set (make-local-variable 'alchemist-goto-erlang-source-dir) "/erlang-source/")
  (should (equal (alchemist-goto--build-erlang-core-file
                  "/private/tmp/erlang-vKyIIl/otp-OTP-17.4/lib/edoc/src/edoc_data.erl")
                 "/erlang-source/lib/edoc/src/edoc_data.erl")))

(ert-deftest test-alchemist-goto--extract-symbol ()
  (should (equal (alchemist-goto--extract-symbol "def dgettext(backend, domain, msgid, bindings \\ %{})")
                 "def dgettext(backend, domain, msgid, bindings \\ %{})"))
  (should (equal (alchemist-goto--extract-symbol "def dgettext(backend, domain, msgid, bindings) when is_list(bindings) do")
                 "def dgettext(backend, domain, msgid, bindings) when is_list(bindings)"))
  (should (equal (alchemist-goto--extract-symbol "def dgettext(one) do: :atom")
                 "def dgettext(one)"))
  (should (equal (alchemist-goto--extract-symbol "def dgettext(backend, domain, msgid, bindings) do")
                 "def dgettext(backend, domain, msgid, bindings)"))
  (should (equal (alchemist-goto--extract-symbol "defmodule Test do")
                 "defmodule Test"))
  (should (equal (alchemist-goto--extract-symbol "defmodule Test.Module do")
                 "defmodule Test.Module"))
  (should (equal (alchemist-goto--extract-symbol "def function?(fn) do")
                 "def function?(fn)"))
  (should (equal (alchemist-goto--extract-symbol "defp render! do")
                 "defp render!"))
  (should (equal (alchemist-goto--extract-symbol "defmacro __using__(_) do")
                 "defmacro __using__(_)"))
  (should (equal (alchemist-goto--extract-symbol "def __pubsub_server__, do: @pubsub_server")
                 "def __pubsub_server__")))

(provide 'alchemist-goto-test)

;;; alchemist-goto-test.el ends here
