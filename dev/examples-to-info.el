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

(defun dash--print-lisp-as-texi (obj)
  "Print Lisp OBJ suitably for Texinfo."
  (let ((print-quoted t)
        (print-escape-control-characters t))
    (save-excursion (prin1 obj)))
  (while (re-search-forward (rx (| (group ?\' symbol-start "nil" symbol-end)
                                   (group "\\?") (group "\\00") (in "{}")))
                            nil 'move)
    (replace-match (cond ((match-beginning 1) "'()")  ; 'nil -> '().
                         ((match-beginning 2) "?")    ; `-any\?' -> `-any?'.
                         ((match-beginning 3) "\\\\") ; \00N -> \N.
                         ("@\\&"))                    ; { -> @{.
                   t)))

(defun example-to-string (example)
  (pcase-let* ((`(,actual ,err ,expected) example)
               (err (eq err '!!>))
               (case-fold-search nil))
    (and err (consp expected)
         (setq expected (error-message-string expected)))
    (with-output-to-string
      (with-current-buffer standard-output
        (insert "@group\n")
        (dash--print-lisp-as-texi actual)
        (insert "\n    " (if err "@error{}" "@result{}") ?\s)
        (dash--print-lisp-as-texi expected)
        (insert "\n@end group")))))

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
                      (list (-partition 3 ',examples))))
         functions))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     (push ,(propertize group 'dash-group t) functions)
     (when ,desc
       (push ,desc functions))
     ,@examples))

(defun format-docstring (docstring)
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert docstring)
      ;; Escape literal ?@.
      (dash--replace-all "@" "@@")
      (goto-char (point-min))
      (while (re-search-forward
              (rx (| (group bow (in "A-Z") (* (in "A-Z" ?-)) (* num) eow)
                     (: ?` (group (+ (not (in ?\s)))) ?\')
                     (: "..." (? (group eol)))))
              nil t)
        (cond ((match-beginning 1)
               ;; Downcase metavariable reference.
               (downcase-region (match-beginning 1) (match-end 1))
               (replace-match "@var{\\1}" t))
              ((match-beginning 2)
               ;; `quoted' symbol.
               (replace-match (if (assq (intern (match-string 2)) functions)
                                  "@code{\\2} (@pxref{\\2})"
                                "@code{\\2}")
                              t))
              ;; Ellipses.
              ((match-beginning 3) (replace-match "@enddots{}" t t))
              ((replace-match "@dots{}" t t))))
      (buffer-string))))

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
               (mapconcat #'example-to-string (-take 3 examples) "\n"))))
    ((pred (get-text-property 0 'dash-group))
     (concat "\n@node " function "\n@section " function))
    (_ (concat "\n" function))))

(defun dash--replace-all (old new)
  "Replace occurrences of OLD with NEW in current buffer."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new t t)))

(defun create-info-file ()
  (let ((functions (reverse functions)))
    (with-temp-file "dash.texi"
      (insert-file-contents "dash-template.texi")

      (dolist (pkg '(dash dash-functional))
        (dash--replace-all (format "@c [[ %s-version ]]" pkg)
                           (lm-version (format "%s.el" pkg))))

      (dash--replace-all
       "@c [[ function-list ]]"
       (mapconcat (lambda (s) (concat "* " s "::"))
                  (-filter (lambda (s)
                             (and (stringp s)
                                  (get-text-property 0 'dash-group s)))
                           functions)
                  "\n"))

      (dash--replace-all "@c [[ function-docs ]]"
                         (mapconcat #'function-to-info functions "\n")))))

;;; examples-to-info.el ends here
