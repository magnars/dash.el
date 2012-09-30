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

(defun quote-and-downcase (string)
  (format "`%s`" (downcase string)))

(defun quote-docstring (docstring)
  (let (case-fold-search)
    (setq docstring (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*\\)\\b" 'quote-and-downcase docstring t))
    (setq docstring (replace-regexp-in-string "`\\([^ ]+\\)'" "`\\1`" docstring t)))
  docstring)

(defun function-to-md (function)
  (let ((command-name (car function))
        (signature (cadr function))
        (docstring (quote-docstring (cadr (cdr function))))
        (examples (cadr (cddr function))))
    (format "### %s `%s`\n\n%s\n\n```cl\n%s\n```\n"
            command-name
            signature
            docstring
            (mapconcat 'identity (three-first examples) "\n"))))

(defun function-summary (function)
  (let ((command-name (car function))
        (signature (cadr function)))
    (format "%s %s" command-name signature)))

(defun create-docs-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-prefix.md")
      (goto-char (point-max))
      (newline)
      (insert "## Available functions")
      (newline 2)
      (insert "```cl")
      (newline)
      (insert (mapconcat 'function-summary functions "\n"))
      (newline)
      (insert "```cl")
      (newline 2)
      (insert "There are also anaphoric versions of these
functions where that makes sense, prefixed with two bangs
instead of one.")
      (newline 2)
      (insert "## Documentation and examples")
      (newline 2)
      (insert (mapconcat 'function-to-md functions "\n"))
      (newline)
      (insert-file-contents-literally "./readme-postfix.md"))))

(defun three-first (list)
  (let (first)
    (when (car list)
      (setq first (cons (car list) first))
      (when (cadr list)
        (setq first (cons (cadr list) first))
        (when (car (cddr list))
          (setq first (cons (car (cddr list)) first)))))
    (nreverse first)))
