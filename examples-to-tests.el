(require 'ert)
(require 'dash)

(defun example-to-should (example)
  (let ((actual (car examples))
        (expected (nth 2 examples)))
    `(should (equal ,actual ,expected))))

(defmacro defexamples (cmd &rest examples)
  `(ert-deftest ,cmd ()
     ,@(-map 'example-to-should (-partition 3 examples))))

(provide 'examples-to-tests)
