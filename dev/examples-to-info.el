;;; examples-to-info.el --- Extract dash.el's Info from examples.el

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

;; FIXME: Lots of duplication with examples-to-docs.el.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'help-fns)

(setq text-quoting-style 'grave)

(defvar functions '())

(defun example-to-string (example)
  (let ((actual (car example))
        (expected (nth 2 example)))
    (--> (format "@group\n%S\n    @result{} %S\n@end group" actual expected)
      (replace-regexp-in-string "\\\\\\?" "?" it)
      (replace-regexp-in-string "{\"" "@{\"" it t t)
      (replace-regexp-in-string "}\"" "@}\"" it t t)
      (replace-regexp-in-string " {" " @{" it t t)
      (replace-regexp-in-string "\"{" "\"@{" it t t)
      (replace-regexp-in-string "}," "@{," it t t)
      (replace-regexp-in-string "}@}" "@}@}" it t t))))

(defun docs--signature (function)
  "Given FUNCTION (a symbol), return its argument list.
FUNCTION may reference an elisp function, alias, macro or a subr."
  (let* ((function-value (indirect-function function))
         (is-alias (not (eq function-value (symbol-function function))))
         ;; if FUNCTION isn't an alias, function-symbol is simply FUNCTION
         (function-symbol function))

    (when is-alias
      ;; find the last symbol in the alias chain
      (while (symbolp (symbol-function function-symbol))
        (setq function-symbol (symbol-function function-symbol))))

    (if (subrp function-value)
        ;; read the docstring to find the signature for subrs
        (let* ((docstring-args (car (help-split-fundoc
                                     (documentation function-value)
                                     function-symbol)))
               (fun-with-args (read (downcase docstring-args))))
          (cdr fun-with-args))
      ;; otherwise get the signature directly
      (help-function-arglist function-symbol))))

(defmacro defexamples (cmd &rest examples)
  `(add-to-list 'functions (list
                            ',cmd
                            (docs--signature ',cmd)
                            (documentation ',cmd)
                            (-map 'example-to-string (-partition 3 ',examples)))))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     (add-to-list 'functions ,(concat "### " group))
     (when ,desc
       (add-to-list 'functions ,desc))
     ,@examples))

(defun quote-and-downcase (string)
  (format "@var{%s}" (downcase string)))

(defun unquote-and-link (string)
  (format-link (substring string 1 -1)))

(defun format-link (string-name)
  (-let* ((name (intern string-name))
          ((_ signature _ _) (assoc name functions)))
    (if signature
        (format "@code{%s} (@pxref{%s})" name name)
      (format "@code{%s}" name))))

(defun format-docstring (docstring)
  (let (case-fold-search)
    (--> docstring
      (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*[0-9]*\\)\\b" 'quote-and-downcase it t)
      (replace-regexp-in-string "`\\([^ ]+\\)'" 'unquote-and-link it t)
      (replace-regexp-in-string "{,@}" "{,@@}" it t)
      (replace-regexp-in-string "^  " "    " it))))

(defun function-to-node (function)
  (when (and (stringp function)
             (string-match "^\\(### [[:upper:]][[:alpha:]- ]+\\)$" function))
    (concat (s-replace "### " "* " (match-string 1 function)) "::")))

(defun function-to-info (function)
  (if (stringp function)
      (concat "\n" (s-replace "### " "@node " function) "\n"
              (when (string-match "^### " function)
                (s-replace "### " "@section " function)) "\n")
    (-let [(command-name signature docstring examples) function]
      (format (concat "@anchor{%s}\n"
                      (if (macrop command-name) "@defmac" "@defun")
                      " %s %s\n"
                      "%s\n\n"
                      "@example\n%s\n@end example\n"
                      "@end "
                      (if (macrop command-name) "defmac" "defun") "\n")
              command-name
              command-name
              signature
              (format-docstring docstring)
              (mapconcat 'identity (-take 3 examples) "\n")))))

(defun docs--chop-prefix (prefix s)
  "Remove PREFIX if it is at the start of S."
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

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
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (docs--chop-prefix
                                                   "!"
                                                   (format "%S %S" command-name signature)))))

(defun s-replace (old new s)
  "Replace OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun function-summary (function)
  (if (stringp function)
      (concat "\n" function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) `%s`" command-name (github-id command-name signature) signature))))

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

(defun create-info-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./dash.texi"
      (insert-file-contents-literally "./dash-template.texi")

      (goto-and-remove "@c [[ function-nodes ]]")
      (insert (mapconcat 'function-to-node
                         (-filter (lambda (s)
                                    (when (stringp s)
                                      (string-match "^### " s)))
                                  functions)
                         "\n"))

      (goto-and-remove "@c [[ function-nodes ]]")
      (insert (mapconcat 'function-to-node
                         (-filter (lambda (s)
                                    (when (stringp s)
                                      (string-match "^### " s)))
                                  functions)
                         "\n"))

      (goto-and-remove "@c [[ function-docs ]]")
      (insert (mapconcat 'function-to-info functions "\n"))

      (simplify-quotes))))

;;; examples-to-info.el ends here
