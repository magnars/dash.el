(require 'ert)
(require 'dash)
(require 'dash-functional)

(defun example-to-should (example)
  (let ((actual (car example))
        (expected (nth 2 example)))
    `(should (equal ,actual ,expected))))

(defmacro defexamples (cmd &rest examples)
  `(ert-deftest ,cmd ()
     ,@(-map 'example-to-should (-partition 3 examples))))

(defun def-example-group (&rest _)) ; ignore

(provide 'examples-to-tests)
