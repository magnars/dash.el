;;; dash-defs.el --- Definitions for Dash examples -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;;; Code:

(require 'dash)
(require 'ert)
;; Added in Emacs 24.4; wrap in `eval-when-compile' when support is dropped.
(require 'subr-x nil t)
(declare-function string-remove-prefix "subr-x" (prefix string))
(declare-function string-remove-suffix "subr-x" (suffix string))

(eval-and-compile
  ;; Preloaded since Emacs 24.4.
  (unless (fboundp 'forward-symbol)
    (autoload 'forward-symbol "thingatpt" nil t)))

(defvar dash--groups ()
  "Alist of grouped examples.

Each element is of the form (NAME . DOC) or (FN . EXAMPLES)
corresponding to the eponymous arguments of `def-example-group'
and `defexamples', respectively.  The only difference is that
EXAMPLES are partitioned into triples (ACTUAL OP EXPECTED), where
EXPECTED should be the result of evaluating ACTUAL, and OP is one
of the following comparison operators:

-  `=>' ACTUAL should be `equal' to EXPECTED.
-  `~>' ACTUAL should be `approx=' to EXPECTED.
- `!!>' ACTUAL should signal the EXPECTED error,
        either an error symbol or an error object.")

(defvar dash--epsilon 1e-15
  "Epsilon used in `approx='.")

(defun approx= (u v)
  "Like `=', but compares floats within `dash--epsilon'.
This allows approximate comparison of floats to work around
differences in implementation between systems.  Used in place of
`equal' when testing actual and expected values with `~>'."
  (or (= u v)
      (< (/ (abs (- u v))
            (max (abs u) (abs v)))
         dash--epsilon)))

(defun dash--example-to-test (example)
  "Return an ERT assertion form based on EXAMPLE."
  (pcase example
    (`(,actual => ,expected) `(should (equal ,actual ,expected)))
    (`(,actual ~> ,expected) `(should (approx= ,actual ,expected)))
    (`(,actual !!> ,(and (pred symbolp) expected))
     `(should-error ,actual :type ',expected))
    (`(,actual !!> ,expected)
     `(should (equal (should-error ,actual) ',expected)))
    (_ (error "Invalid test case: %S" example))))

(defmacro def-example-group (name doc &rest examples)
  "Define a group with NAME and DOC of EXAMPLES of several functions.
See `dash--groups'."
  (declare (indent defun))
  `(progn
     (push (cons ,name ,doc) dash--groups)
     ,@examples))

(defmacro defexamples (fn &rest examples)
  "Define a set of EXAMPLES and corresponding ERT tests for FN.
See `dash--groups'."
  (declare (indent defun))
  (setq examples (-partition 3 examples))
  `(progn
     (push (cons ',fn ',examples) dash--groups)
     (ert-deftest ,fn ()
       ;; Emacs 28.1 complains about an empty `let' body if the test
       ;; body is empty.
       ,@(or (mapcar #'dash--example-to-test examples) '(nil)))))

;; Added in Emacs 25.1.
(defvar text-quoting-style)

(autoload 'help-fns--analyze-function "help-fns")

(defun dash--describe (fn)
  "Return the (ARGS . DOCSTRING) of FN symbol.
ARGS is a propertized string describing the arguments of FN,
such as \"count &optional start step\".
Based on `describe-function-1'."
  ;; Gained last arg in Emacs 25.1.
  (declare-function help-fns--signature "help-fns"
                    (function doc real-def real-function buffer))
  (or (get fn 'dash--describe)
      (with-temp-buffer
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (pcase-let* ((text-quoting-style 'grave)
                     (`(,real-fn ,_def ,_alias ,real-def)
                      (help-fns--analyze-function fn))
                     (doc-raw (documentation fn t))
                     (doc (help-fns--signature
                           fn doc-raw real-def real-fn (current-buffer))))
          ;; Delete everything following args.
          (search-backward ")")
          (delete-region (point) (point-max))
          ;; Delete everything preceding args.
          (goto-char (1+ (point-min)))
          (forward-symbol 1)
          (delete-region (point-min) (1+ (point)))
          (downcase-region (point-min) (point-max))
          ;; Memoize.
          (put fn 'dash--describe (cons (buffer-string) doc))))))

(defun dash--replace-all (old new)
  "Replace occurrences of OLD with NEW in current buffer."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new t t)))

(defun dash--map-prop (prop val fn)
  "Call FN on each region which has text PROP `eq' to VAL.
Search the entire accessible portion of the current buffer.
FN takes the start and end of the region as arguments, and
returns either an updated end position (from which to continue
the search), or nil, defaulting to the original end position."
  (let ((beg (point-min)))
    (while (setq beg (text-property-any beg (point-max) prop val))
      (let ((end (or (next-single-property-change beg prop)
                     (point-max))))
        (setq beg (or (funcall fn beg end) end))))))

(defun dash--quote-md ()
  "Insert Markdown backquotes throughout current buffer.
Specifically, wherever the `dash--quote-md' text property is t."
  (dash--map-prop 'dash--quote-md t
                  (lambda (beg end)
                    (goto-char beg)
                    (let* ((bq "`")
                           ;; Double backquotes needed if backquoted
                           ;; text contains a nested backquote.
                           (nested (search-forward bq end 'move))
                           (pre (if nested "`` " bq))
                           (suf (if nested " ``" bq)))
                      (insert suf)
                      (goto-char beg)
                      (insert pre)
                      (+ end (length pre) (length suf))))))

(defun dash--github-id (fn sig)
  "Return a GitHub Flavored Markdown link ID for FN with SIG."
  (or (get fn 'dash--github-id)
      (let* ((id (string-remove-prefix "!" (format "%s %s" fn sig)))
             (id (replace-regexp-in-string (rx (+ (not (in alnum ?-))))
                                           "-" id t t)))
        ;; Memoize.
        (put fn 'dash--github-id (string-remove-suffix "-" id)))))

(defun dash--github-link (fn &optional full)
  "Return a GitHub Flavored Markdown link to FN.
If FULL is non-nil, use the entire FN signature as the displayed
link label."
  (let ((sig (car (dash--describe fn))))
    (format #("[%s](#%s)" 1 3 (dash--quote-md t))
            (if full (format "(%s %s)" fn sig) fn)
            (dash--github-id fn sig))))

(defun dash--argnames-to-md ()
  "Downcase and mark arg names in current buffer for Markdown."
  (dash--map-prop 'face 'help-argument-name
                  (lambda (beg end)
                    (downcase-region beg end)
                    (put-text-property beg end 'dash--quote-md t))))

(defun dash--metavars-to-md ()
  "Downcase and mark metavariables in current buffer for Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx bow (group (in upper) (* (in upper ?-)) (* num))
                                (| (group ?\() (: (group (? "th")) eow)))
                            nil t)
    (unless (match-beginning 2)
      (let* ((suf (match-string 3))
             (var (downcase (match-string 1)))
             (var (format #("%s%s" 0 2 (dash--quote-md t)) var suf)))
        (replace-match var t t)))))

(defun dash--hyperlinks-to-md ()
  "Convert hyperlinks in current buffer from Elisp to Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx ?` (group (+? nonl)) ?\') nil t)
    (let* ((beg (match-beginning 1))
           (str (match-string 1))
           (sym (intern-soft str)))
      (if (and sym (assq sym dash--groups))
          (replace-match (save-match-data (dash--github-link sym)) t t)
        (delete-char -1)
        (put-text-property beg (point) 'dash--quote-md t)
        (goto-char beg)
        (delete-char -1)
        (forward-char (length str))))))

(defun dash--booleans-to-md ()
  "Mark booleans (nil/t) in current buffer for Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx (| bol (not (in ?\'))) bow
                                (group (| "nil" "t")) eow)
                            nil t)
    (put-text-property (match-beginning 1) (match-end 1) 'dash--quote-md t)))

(defun dash--indent-md-blocks ()
  "Indent example blocks in current buffer for Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx bol "  ") nil t)
    (replace-match "    " t t)))

(defun dash--docstring-to-md (doc)
  "Transcribe DOC to Markdown syntax."
  (with-temp-buffer
    (insert doc)
    (dash--argnames-to-md)
    (dash--metavars-to-md)
    (dash--hyperlinks-to-md)
    (dash--booleans-to-md)
    (dash--indent-md-blocks)
    (buffer-string)))

;; FIXME
(defun dash--docstring-to-texi (doc)
  "Transcribe DOC to Texinfo syntax."
  (with-temp-buffer
    (insert doc)
    ;; Escape literal ?@.
    (dash--replace-all "@" "@@")
    (goto-char (point-min))
    ;; TODO: Use `help-argument-name' like in `dash--argnames-to-md'?
    (while (re-search-forward
            (rx (| (group bow (in "A-Z") (* (in "A-Z" ?-)) (* num) eow)
                   (: ?` (group (+? (not (in ?\s)))) ?\')
                   (group bow (| "nil" "t") eow)
                   (: "..." (? (group eol)))))
            nil t)
      (cond ((match-beginning 1)
             ;; Downcase metavariable reference.
             (downcase-region (match-beginning 1) (match-end 1))
             (replace-match "@var{\\1}" t))
            ((match-beginning 2)
             ;; `quoted' symbol.
             (let ((sym (intern-soft (match-string 2))))
               (replace-match (if (and sym (assq sym dash--groups))
                                  "@code{\\2} (@pxref{\\2})"
                                "@code{\\2}")
                              t)))
            ;; nil/t.
            ((match-beginning 3)
             (unless (= (char-before (match-beginning 3)) ?\')
               (replace-match "@code{\\3}" t)))
            ;; Ellipses.
            ((match-beginning 4) (replace-match "@enddots{}" t t))
            ((replace-match "@dots{}" t t))))
    (buffer-string)))

;; Added in Emacs 26.1.
(defvar print-escape-control-characters)

(defun dash--lisp-to-md (obj)
  "Print Lisp OBJ suitably for Markdown."
  (let ((print-quoted t)
        (print-escape-control-characters t))
    (save-excursion (prin1 obj)))
  (while (re-search-forward (rx (| (group ?\' symbol-start "nil" symbol-end)
                                   (group "\\00") "\\?"))
                            nil 'move)
    (replace-match (cond ((match-beginning 1) "()") ; 'nil -> ().
                         ((match-beginning 2) "\\") ; \00N -> \N.
                         ("?"))                     ; `-any\?' -> `-any?'.
                   t t)))

(defun dash--lisp-to-texi (obj)
  "Print Lisp OBJ suitably for Texinfo."
  (save-excursion (dash--lisp-to-md obj))
  (while (re-search-forward (rx (in "{}")) nil 'move)
    (replace-match "@\\&" t))) ;; { -> @{.

(defun dash--expected (obj err)
  "Prepare OBJ for printing as an expected evaluation result.
ERR non-nil means OBJ is either an error symbol or error object."
  (cond ((and (eq (car-safe obj) 'quote)
              (not (equal obj ''())))
         ;; Unquote expected result.
         (cadr obj))
        ;; Print actual error message.
        (err (error-message-string (-list obj)))
        (obj)))

(defun dash--example-to-md (example)
  "Return a Markdown string documenting EXAMPLE."
  (pcase-let* ((`(,actual ,op ,expected) example)
               (err (eq op '!!>)))
    (setq expected (dash--expected expected err))
    (with-output-to-string
      (with-current-buffer standard-output
        (dash--lisp-to-md actual)
        (insert " ;; ")
        (cond ((memq op '(=> ~>))
               (princ op)
               (insert ?\s)
               (dash--lisp-to-md expected))
              (err (princ expected))
              ((error "Invalid test case: %S" example)))))))

(defun dash--example-to-texi (example)
  "Return a Texinfo string documenting EXAMPLE."
  (pcase-let* ((`(,actual ,op ,expected) example)
               (err (eq op '!!>)))
    (setq expected (dash--expected expected err))
    (with-output-to-string
      (with-current-buffer standard-output
        (insert "@group\n")
        (dash--lisp-to-texi actual)
        (insert "\n    " (if err "@error{}" "@result{}") ?\s)
        (funcall (if err #'princ #'dash--lisp-to-texi) expected)
        (insert "\n@end group")))))

(defun dash--group-to-md (group)
  "Return a Markdown string documenting GROUP."
  (pcase group
    (`(,(and (pred stringp) name) . ,doc)
     (concat "## " name "\n\n" (dash--docstring-to-md doc) "\n"))
    ((and `(,fn . ,examples)
          (let `(,sig . ,doc) (dash--describe fn)))
     (format "#### `(%s %s)`\n\n%s\n\n```el\n%s\n```\n"
             fn sig (dash--docstring-to-md doc)
             (mapconcat #'dash--example-to-md (-take 3 examples) "\n")))))

(defun dash--group-to-texi (group)
  "Return a Texinfo string documenting GROUP."
  ;; Added in Emacs 24.4.
  (declare-function macrop "subr" (object))
  (pcase group
    (`(,(and (pred stringp) name) . ,doc)
     (concat "@node " name "\n@section " name "\n\n"
             (dash--docstring-to-texi doc) "\n"))
    ((and `(,fn . ,examples)
          (let `(,sig . ,doc) (dash--describe fn))
          (let type (if (macrop fn) "defmac" "defun")))
     (format (concat "@anchor{%s}\n"
                     "@%s %s %s\n"
                     "%s\n\n"
                     "@example\n%s\n@end example\n"
                     "@end %s\n")
             fn type fn sig (dash--docstring-to-texi doc)
             (mapconcat #'dash--example-to-texi (-take 3 examples) "\n")
             type))))

(defun dash--summary-to-md (group)
  "Return a Markdown string summarizing GROUP."
  (pcase group
    (`(,(and (pred stringp) name) . ,doc)
     (concat "\n### " name "\n\n" (dash--docstring-to-md doc) "\n"))
    (`(,fn . ,_)
     (format #("* %s (%s)" 5 9 (dash--quote-md t))
             (dash--github-link fn)
             (car (dash--describe fn))))))

(autoload 'lm-version "lisp-mnt")

(defun dash--make-md ()
  "Generate Markdown README."
  (with-temp-file "README.md"
    (insert-file-contents "readme-template.md")
    (dash--replace-all "[[ dash-version ]]" (lm-version "dash.el"))
    (dash--replace-all "[[ function-list ]]"
                       (mapconcat #'dash--summary-to-md dash--groups "\n"))
    (dash--replace-all "[[ function-docs ]]"
                       (mapconcat #'dash--group-to-md dash--groups "\n"))
    (dash--quote-md)))

(defun dash--make-texi ()
  "Generate Texinfo manual."
  (with-temp-file "dash.texi"
    (insert-file-contents "dash-template.texi")
    (dash--replace-all "@c [[ dash-version ]]" (lm-version "dash.el"))
    (dash--replace-all
     "@c [[ function-list ]]"
     (mapconcat (lambda (group) (concat "* " (car group) "::"))
                (--filter (stringp (car it)) dash--groups)
                "\n"))
    (dash--replace-all "@c [[ function-docs ]]"
                       (mapconcat #'dash--group-to-texi dash--groups "\n"))))

(defun dash-make-docs ()
  "Generate Dash Markdown README and Texinfo manual."
  (let ((dash--groups (reverse dash--groups))
        (case-fold-search nil))
    (dash--make-md)
    (dash--make-texi)))

(provide 'dash-defs)

;;; dash-defs.el ends here
