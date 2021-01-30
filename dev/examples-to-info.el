;;; examples-to-info.el --- Extract dash.el's Info from examples.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: Lots of duplication with examples-to-docs.el.

;;; Code:

(require 'dash)

(require 'help-fns)
(require 'lisp-mnt)

(defvar functions ())

(defun example-to-string (example)
  (let ((actual (pop example))
        (err (eq (pop example) '!!>))
        (expected (pop example)))
    (and err (consp expected)
         (setq expected (error-message-string expected)))
    (--> (format "@group\n%S\n    %s %S\n@end group"
                 actual (if err "@error{}" "@result{}") expected)
      (replace-regexp-in-string "\\\\\\?" "?" it t t)
      (replace-regexp-in-string "{\"" "@{\"" it t t)
      (replace-regexp-in-string "}\"" "@}\"" it t t)
      (replace-regexp-in-string " {" " @{" it t t)
      (replace-regexp-in-string "\"{" "\"@{" it t t)
      (replace-regexp-in-string "}," "@}," it t t)
      (replace-regexp-in-string "}@}" "@}@}" it t t)
      (replace-regexp-in-string
       "[^\n[:print:]]"
       (lambda (s) (concat "\\" (text-char-description (string-to-char s))))
       it t t))))

(defun dash--describe (fn)
  "Return the (ARGLIST DOCSTRING) of FN symbol.
Based on `describe-function-1'."
  (with-temp-buffer
    (pcase-let* ((text-quoting-style 'grave)
                 (`(,real-fn ,def ,_alias ,real-def)
                  (help-fns--analyze-function fn))
                 (buf (current-buffer))
                 (doc-raw (documentation fn t))
                 (doc (help-fns--signature fn doc-raw real-def real-fn buf)))
      (goto-char (1+ (point-min)))
      (delete-region (point) (progn (forward-sexp) (1+ (point))))
      (downcase-region (point) (point-max))
      (backward-char)
      (list (read buf) doc))))

(defmacro defexamples (cmd &rest examples)
  `(push (cons ',cmd
               (nconc (dash--describe ',cmd)
                      (list (mapcar #'example-to-string
                                    (-partition 3 ',examples)))))
         functions))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     (push ,(concat "### " group) functions)
     (when ,desc
       (push ,desc functions))
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
      (replace-regexp-in-string "@" "@@" it t t)
      (replace-regexp-in-string "\\<\\([A-Z][A-Z-]*[0-9]*\\)\\>"
                                #'quote-and-downcase it t t)
      (replace-regexp-in-string "`\\([^ ]+\\)'" #'unquote-and-link it t t)
      (replace-regexp-in-string "^  " "    " it t t)
      (replace-regexp-in-string
       "\\.\\.\\.\\($\\)?"
       (lambda (_) (if (match-beginning 1) "@enddots{}" "@dots{}"))
       it t t))))

(defun function-to-node (function)
  (concat (replace-regexp-in-string (rx bos "### ") "* " function t t) "::"))

(defun function-to-info (function)
  (pcase function
    (`(,command-name ,signature ,docstring ,examples)
     (let ((type (if (macrop command-name) "defmac" "defun")))
       (format (concat "\n@anchor{%s}\n"
                       "@" type " %s %s\n"
                       "%s\n\n"
                       "@example\n%s\n@end example\n"
                       "@end " type)
               command-name
               command-name
               signature
               (format-docstring docstring)
               (mapconcat #'identity (-take 3 examples) "\n"))))
    ((rx bos "### ")
     (setq function (substring function (match-end 0)))
     (concat "\n@node " function "\n@section " function))
    (_ (concat "\n" function))))

(defun simplify-quotes ()
  (goto-char (point-min))
  (while (re-search-forward (rx (or "'nil" "(quote nil)")) nil t)
    (replace-match "'()" t t))
  (goto-char (point-min))
  (while (search-forward "(quote " nil t)
    (forward-char -7)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 7)
      (insert "'")))
  (goto-char (point-min))
  (while (search-forward "(function " nil t)
    (forward-char -10)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 10)
      (insert "#'"))))

(defun goto-and-remove (s)
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun create-info-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./dash.texi"
      (insert-file-contents "./dash-template.texi")

      (dolist (pkg '(dash dash-functional))
        (goto-and-remove (format "@c [[ %s-version ]]" pkg))
        (insert (lm-version (format "%s.el" pkg))))

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
