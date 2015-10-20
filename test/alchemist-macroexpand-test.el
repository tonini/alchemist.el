;;; alchemist-macroexpand-test.el ---

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

;;

;;; Code:

(require 'test-helper)
(require 'alchemist-macroexpand)

(defun capture-macroexpand-popup-content ()
  (with-current-buffer (get-buffer alchemist-macroexpand-buffer-name)
    (buffer-substring-no-properties (point-min) (point-max))))


(ert-deftest alchemist-macroexpand/expand-once-current-line ()
  (with-temp-buffer
    (alchemist-mode)
    (insert "unless false, do: IO.puts 'Please no!'")
    (alchemist-macroexpand-expand-once-current-line)
    (wait 1))
  (should (equal "if(false) do
  nil
else
  IO.puts('Please no!')
end"
                 (capture-macroexpand-popup-content))))

(ert-deftest alchemist-macroexpand/expand-current-line ()
  (with-temp-buffer
    (alchemist-mode)
    (insert "unless false, do: IO.puts 'Please no!'")
    (alchemist-macroexpand-expand-current-line)
    (wait 1))
  (should (equal "case(false) do
  x when x in [false, nil] ->
    IO.puts('Please no!')
  _ ->
    nil
end"
                 (capture-macroexpand-popup-content))))

(ert-deftest alchemist-macroexpand/expand-once-region ()
  (with-temp-buffer
    (alchemist-mode)
    (insert "unless false do
  IO.puts 'Please no!'
end")
    (alchemist-macroexpand-expand-once-region (point-min) (point-max))
    (wait 1))
  (should (equal "if(false) do
  nil
else
  IO.puts('Please no!')
end"
                 (capture-macroexpand-popup-content))))

(ert-deftest alchemist-macroexpand/expand-region ()
  (with-temp-buffer
    (alchemist-mode)
    (insert "unless false do
  IO.puts 'Please no!'
end")
    (alchemist-macroexpand-expand-region (point-min) (point-max))
    (wait 1))
  (should (equal "case(false) do
  x when x in [false, nil] ->
    IO.puts('Please no!')
  _ ->
    nil
end"
                 (capture-macroexpand-popup-content))))

(ert-deftest alchemist-macroexpand/expand-once-print-current-line ()
  (should (equal "unless false, do: IO.puts 'Please no!'
# => if(false) do
# =>   nil
# => else
# =>   IO.puts('Please no!')
# => end" (with-temp-buffer
            (alchemist-mode)
            (insert "unless false, do: IO.puts 'Please no!'")
            (alchemist-macroexpand-expand-once-print-current-line)
            (wait 1)
            (buffer-substring-no-properties (point-min) (point-max))))))


(ert-deftest alchemist-macroexpand/expand-print-current-line ()
  (should (equal "unless false, do: IO.puts 'Please no!'
# => case(false) do
# =>   x when x in [false, nil] ->
# =>     IO.puts('Please no!')
# =>   _ ->
# =>     nil
# => end" (with-temp-buffer
            (alchemist-mode)
            (insert "unless false, do: IO.puts 'Please no!'")
            (alchemist-macroexpand-expand-print-current-line)
            (wait 1)
            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest alchemist-macroexpand/expand-once-print-region ()
  (should (equal "unless false do
  IO.puts 'Please no!'
end
# => if(false) do
# =>   nil
# => else
# =>   IO.puts('Please no!')
# => end" (with-temp-buffer
            (alchemist-mode)
            (insert "unless false do
  IO.puts 'Please no!'
end")
            (alchemist-macroexpand-expand-once-print-region (point-max) (point-min))
            (wait 1)
            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest alchemist-macroexpand/expand-print-region ()
  (should (equal "unless false do
  IO.puts 'Please no!'
end
# => case(false) do
# =>   x when x in [false, nil] ->
# =>     IO.puts('Please no!')
# =>   _ ->
# =>     nil
# => end" (with-temp-buffer
            (alchemist-mode)
            (insert "unless false do
  IO.puts 'Please no!'
end")
            (alchemist-macroexpand-expand-print-region (point-max) (point-min))
            (wait 1)
            (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'alchemist-macroexpand-test)

;;; alchemist-macroexpand-test.el ends here
