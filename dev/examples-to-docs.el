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
(require 'dash-functional)
(require 'lisp-mnt)

(setq text-quoting-style 'grave)

(defvar functions ())

(defun example-to-string (example)
  (-let [(actual sym expected) example]
    (--> (cond
          ((eq sym '=>) (format "=> %S" expected))
          ((eq sym '~>) (format "~> %S" expected))
          ((eq sym '!!>) "Error")
          ((error "Invalid test case: %S" example)))
      (format "%S ;; %s" actual it)
      (replace-regexp-in-string "\\\\\\?" "?" it t t)
      (replace-regexp-in-string "\n" "\\n" it t t)
      (replace-regexp-in-string "\t" "\\t" it t t)
      (replace-regexp-in-string "\r" "\\r" it t t))))

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
  `(push (list ',cmd
               (docs--signature ',cmd)
               (documentation ',cmd)
               (mapcar #'example-to-string (-partition 3 ',examples)))
         functions))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     (push ,(concat "### " group) functions)
     (when ,desc
       (push ,desc functions))
     ,@examples))

(defun quote-and-downcase (string)
  (format "`%s`" (downcase string)))

(defun unquote-and-link (string)
  (format-link (substring string 1 -1)))

(defun format-link (string-name)
  (-let* ((name (intern string-name))
          ((_ signature _ _) (assoc name functions)))
    (if signature
        (format "[`%s`](#%s)" name (github-id name signature))
      (format "`%s`" name))))

(defun format-docstring (docstring)
  (let (case-fold-search)
    (--> docstring
      (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*[0-9]*\\)\\b" 'quote-and-downcase it t)
      (replace-regexp-in-string "`\\([^ ]+\\)'" 'unquote-and-link it t)
      (replace-regexp-in-string "^  " "    " it))))

(defun function-to-md (function)
  (if (stringp function)
      (concat "\n" (s-replace "### " "## " function) "\n")
    (-let [(command-name signature docstring examples) function]
      (format "#### %s `%s`\n\n%s\n\n```el\n%s\n```\n"
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

(defun goto-and-replace-all (s replacement)
  (while (progn (goto-char (point-min)) (search-forward s nil t))
    (delete-char (- (length s)))
    (insert replacement)))

(defun create-docs-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-template.md")

      (goto-and-remove "[[ function-list ]]")
      (insert (mapconcat 'function-summary functions "\n"))

      (goto-and-remove "[[ function-docs ]]")
      (insert (mapconcat 'function-to-md functions "\n"))

      (dolist (pkg '(dash dash-functional))
        (goto-and-replace-all (format "[[ %s-version ]]" pkg)
                              (lm-version (format "%s.el" pkg))))

      (simplify-quotes))))

;;; examples-to-docs.el ends here
