(require 'dash)

(defvar functions '())

(defun example-to-string (example)
  (let ((actual (car example))
        (expected (nth 2 example)))
    (--> (format "%S ;; => %S" actual expected)
      (replace-regexp-in-string "\\\\\\?" "?" it)
      (replace-regexp-in-string "\n" "\\n" it t t)
      (replace-regexp-in-string "\t" "\\t" it t t)
      (replace-regexp-in-string "\r" "\\r" it t t))))

(defun docs--signature (cmd)
  (if (eq 'macro (car cmd))
      (nth 2 cmd)
    (nth 1 cmd)))

(defun docs--docstring (cmd)
  (if (eq 'macro (car cmd))
      (nth 3 cmd)
    (nth 2 cmd)))

(defmacro defexamples (cmd &rest examples)
  `(add-to-list 'functions (list
                            ',cmd
                            (docs--signature (symbol-function ',cmd))
                            (docs--docstring (symbol-function ',cmd))
                            (-map 'example-to-string (-partition 3 ',examples)))))

(defun quote-and-downcase (string)
  (format "`%s`" (downcase string)))

(defun quote-docstring (docstring)
  (let (case-fold-search)
    (--> docstring
      (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*[0-9]*\\)\\b" 'quote-and-downcase it t)
      (replace-regexp-in-string "`\\([^ ]+\\)'" "`\\1`" it t))))

(defun function-to-md (function)
  (let ((command-name (car function))
        (signature (cadr function))
        (docstring (quote-docstring (nth 2 function)))
        (examples (nth 3 function)))
    (format "### %s `%s`\n\n%s\n\n```cl\n%s\n```\n"
            command-name
            signature
            docstring
            (mapconcat 'identity (-take 3 examples) "\n"))))

(defun docs--chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun github-id (command-name signature)
  (docs--chop-suffix
   "-"
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (format "%S %S" command-name signature))))

(defun function-summary (function)
  (let ((command-name (car function))
        (signature (cadr function)))
    (format "* [%s](#%s) `%s`" command-name (github-id command-name signature) signature)))

(defun simplify-quotes ()
  (goto-char (point-min))
  (while (search-forward "(quote nil)" nil t)
    (replace-match "'()"))
  (goto-char (point-min))
  (while (search-forward "(quote " nil t)
    (forward-char -7)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 7)
      (insert "'"))))

(defun goto-and-remove (s)
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun create-docs-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-template.md")

      (goto-and-remove "[[ function-list ]]")
      (insert (mapconcat 'function-summary functions "\n"))

      (goto-and-remove "[[ function-docs ]]")
      (insert (mapconcat 'function-to-md functions "\n"))

      (simplify-quotes))))
