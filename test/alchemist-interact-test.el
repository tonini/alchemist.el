;;; alchemist-interact-test.el ---

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

;;

;;; Code:

(ert-deftest alchemist-interact/insert-string-as-comment ()
  (should (equal "
# => sum = fn (a, b) ->
# =>   a + b
# => end
# => sum.(21, 33)"
                 (with-temp-buffer
                   (alchemist-interact-insert-as-comment "sum = fn (a, b) ->
  a + b
end
sum.(21, 33)")
                   (buffer-substring-no-properties (point-min) (point-max)))))
  (should (equal "  # => IO.puts 1 + 1"
                 (with-temp-buffer
                   (alchemist-interact-insert-as-comment "IO.puts 1 + 1")
                   (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'alchemist-interact-test)

;;; alchemist-interact-test.el ends here
