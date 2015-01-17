(require 'ert)

(defun example-to-should (actual sym expected)
  (cond ((eq sym '=>)
         `(should (equal ,actual ,expected)))
        ((eq sym '!!>)
         `(should-error (eval ',actual) :type ',expected))
        (t
         (error "invalid test case: %S" `(,actual ,sym ,expected)))))


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
