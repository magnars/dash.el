;;; examples-to-tests.el --- Extract dash.el's tests from examples.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: Lots of duplication with examples-to-info.el.

;;; Code:

(require 'ert)

(defun example-to-should (actual sym expected)
  (cond ((eq sym '=>)
         `(should (equal ,actual ,expected)))
        ((eq sym '~>)
         `(should (approx-equal ,actual ,expected)))
        ((eq sym '!!>)
         `(should-error (eval ',actual) :type ',expected))
        (t
         (error "Invalid test case: %S" `(,actual ,sym ,expected)))))


(defmacro defexamples (cmd &rest examples)
  (let ((tests))
    (while examples
      (push (example-to-should (pop examples)
                               (pop examples)
                               (pop examples))
            tests))
    `(ert-deftest ,cmd () ,@(nreverse tests))))

(defun def-example-group (&rest _)) ; ignore

(provide 'examples-to-tests)
;;; examples-to-tests.el ends here
