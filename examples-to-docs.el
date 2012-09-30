(defvar functions '())

(defun example-to-string (example)
  (let ((actual (car example))
        (expected (cadr (cdr example))))
    (format "%s ;; => %s" actual expected)))

(defun examples-to-strings (examples)
  (let (result)
    (while examples
      (setq result (cons (example-to-string examples) result))
      (setq examples (cddr (cdr examples))))
    (nreverse result)))

(defmacro defexamples (cmd &rest examples)
  `(add-to-list 'functions (list
    ',cmd ;; command name
    (cadr (symbol-function ',cmd)) ;; signature
    (car (cddr (symbol-function ',cmd))) ;; docstring
    (examples-to-strings ',examples)))) ;; examples

(defun quote-docstring (docstring)
  (replace-regexp-in-string "`\\([^ ]+\\)'" "`\\1`" docstring))

(defun function-to-md (function)
  (let ((command-name (car function))
        (signature (cadr function))
        (docstring (quote-docstring (cadr (cdr function))))
        (examples (mapconcat 'identity (cadr (cddr function)) "\n")))
    (format "## %s `%s`\n\n%s\n\n```cl\n%s\n```\n" command-name signature docstring examples)))

(defun create-docs-file ()
  (with-temp-file "./docs.md"
    (insert (mapconcat 'function-to-md (nreverse functions) "\n"))))
