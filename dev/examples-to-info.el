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
  (save-excursion (prin1 obj))
  (while (re-search-forward (rx (| (group "\\?")
                                   (group (in "{}"))
                                   (not (in ?\n print))))
                            nil 'move)
    (cond ((match-beginning 1)
           ;; Unescape `-any\?' -> `-any?'.
           (delete-region (- (point) 2) (1- (point))))
          ((match-beginning 2)
           ;; Escape braces with @.
           (backward-char)
           (insert ?@)
           (forward-char))
          ((let ((desc (text-char-description (preceding-char))))
             ;; Translate unprintable characters such as ?\^A.
             (replace-match (concat "\\" desc) t t))))))

(defun example-to-string (example)
  (pcase-let* ((`(,actual ,err ,expected) example)
               (err (eq err '!!>)))
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
                      (list (mapcar #'example-to-string
                                    (-partition 3 ',examples)))))
         functions))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     (push ,(concat "### " group) functions)
     (when ,desc
       (push ,desc functions))
     ,@examples))

(defun format-docstring (docstring)
  (let ((case-fold-search nil))
    (with-output-to-string
      (with-current-buffer standard-output
        (insert docstring)
        (goto-char (point-min))
        ;; Escape literal ?@.
        (while (search-forward "@" nil t) (insert ?@))
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
                ((replace-match "@dots{}" t t))))))))

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
