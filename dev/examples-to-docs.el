;;; examples-to-docs.el --- Extract dash.el's doc from examples.el -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.

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

;; FIXME: Lots of duplication with examples-to-info.el.

;;; Code:

(require 'dash)

(require 'help-fns)
(require 'lisp-mnt)

(eval-when-compile
  (require 'subr-x))

(defvar functions ())

(defun dash--print-lisp-as-md (obj)
  "Print Lisp OBJ suitably for Markdown."
  (let ((print-quoted t)
        (print-escape-control-characters t))
    (save-excursion (prin1 obj)))
  (while (re-search-forward
          (rx (| (group ?\' symbol-start "nil" symbol-end) "\\?")) nil 'move)
    ;; 'nil -> (), `-any\?' -> `-any?'.
    (replace-match (if (match-beginning 1) "()" "?") t t)))

(defun example-to-string (example)
  (pcase-let ((`(,actual ,sym ,expected) example)
              (print-quoted t))
    (cond ((eq sym '!!>)
           ;; Print actual error message.
           (setq expected (error-message-string (-list expected))))
          ((and (eq (car-safe expected) 'quote)
                (not (equal expected ''())))
           ;; Unquote expected result.
           (setq expected (cadr expected))))
    (with-output-to-string
      (with-current-buffer standard-output
        (dash--print-lisp-as-md actual)
        (insert " ;; ")
        (cond ((memq sym '(=> ~>))
               (princ sym)
               (insert ?\s)
               (dash--print-lisp-as-md expected))
              ((eq sym '!!>) (princ expected))
              ((error "Invalid test case: %S" example)))))))

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

(defun format-link (name)
  (pcase (assq (intern name) functions)
    (`(,_ ,signature . ,_) (dash--github-link name signature))
    (_ (format "`%s`" name))))

(defun dash--quote-argnames ()
  "Downcase and quote arg names in current buffer for Markdown."
  (let ((beg (point-min)))
    (while (setq beg (text-property-any beg (point-max)
                                        'face 'help-argument-name))
      (goto-char beg)
      (insert ?`)
      (goto-char (or (next-single-property-change (point) 'face)
                     (point-max)))
      (downcase-region (1+ beg) (point))
      (insert ?`)
      (setq beg (point)))))

(defun dash--quote-metavars ()
  "Downcase and quote metavariables in current buffer for Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx bow (group (in upper) (* (in upper ?-)) (* num))
                                (| (group ?\() (: (group (? "th")) eow)))
                            nil t)
    (unless (match-beginning 2)
      (let* ((suf (match-string 3))
             (var (format "`%s`%s" (downcase (match-string 1)) suf)))
        (replace-match var t t)))))

(defun dash--quote-hyperlinks ()
  "Convert hyperlinks in current buffer from Elisp to Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx ?` (+? (not (in " `"))) ?\') nil t)
    (replace-match (format-link (substring (match-string 0) 1 -1)) t t)))

(defun dash--indent-blocks ()
  "Indent example blocks in current buffer for Markdown."
  (goto-char (point-min))
  (while (re-search-forward (rx bol "  ") nil t)
    (replace-match "    " t t)))

(defun dash--format-docstring (docstring)
  (with-temp-buffer
    (let ((case-fold-search nil))
      (insert docstring)
      (dash--quote-argnames)
      (dash--quote-metavars)
      (dash--quote-hyperlinks)
      (dash--indent-blocks)
      (buffer-string))))

(defun function-to-md (function)
  (pcase function
    (`(,command-name ,signature ,docstring ,examples)
     (format "#### %s `%s`\n\n%s\n\n```el\n%s\n```\n"
             command-name
             signature
             (dash--format-docstring docstring)
             (mapconcat #'example-to-string (-take 3 examples) "\n")))
    ((pred (get-text-property 0 'dash-group))
     (concat "## " function "\n"))
    (_ (concat function "\n"))))

(defun dash--github-link (fn signature)
  (--> (string-remove-prefix "!" (format "%s%s" fn signature))
    (replace-regexp-in-string (rx (+ (not (in alnum ?-)))) "-" it t t)
    (format "[`%s`](#%s)" fn (string-remove-suffix "-" it))))

(defun function-summary (function)
  (pcase function
    (`(,fn ,signature . ,_)
     (format "* %s `%s`" (dash--github-link fn signature) signature))
    ((pred (get-text-property 0 'dash-group))
     (concat "\n### " function "\n"))
    (_ (concat function "\n"))))

(defun dash--replace-all (old new)
  "Replace occurrences of OLD with NEW in current buffer."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new t t)))

(defun create-docs-file ()
  (let ((functions (reverse functions)))
    (with-temp-file "README.md"
      (insert-file-contents "readme-template.md")
      (dolist (pkg '(dash dash-functional))
        (dash--replace-all (format "[[ %s-version ]]" pkg)
                           (lm-version (format "%s.el" pkg))))
      (dash--replace-all "[[ function-list ]]"
                         (mapconcat #'function-summary functions "\n"))
      (dash--replace-all "[[ function-docs ]]"
                         (mapconcat #'function-to-md functions "\n")))))

;;; examples-to-docs.el ends here
