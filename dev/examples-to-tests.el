(require 'ert)
(require 'dash)
(require 'dash-functional)

(defun example-to-should (example)
  (-let [(actual sym expected) example]
    (cond
     ((eq sym '=>)
      `(should (equal ,actual ,expected)))
     ((eq sym '!!>)
      `(should-error (eval ',actual) :type ',expected)))))

(defmacro defexamples (cmd &rest examples)
  `(ert-deftest ,cmd ()
     ,@(-map 'example-to-should (-partition 3 examples))))

(defun def-example-group (&rest _)) ; ignore

(provide 'examples-to-tests)
