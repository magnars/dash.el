;;; dash-pcase.el --- A pcase pattern following `-let' -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: extensions, lisp
;; Homepage: https://github.com/magnars/dash.el

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

;; A `pcase' pattern that works like `-let'.

;;; Code:

(require 'dash)
(require 'pcase)

(eval-when-compile
  ;; - 24.3 started complaining about unknown `declare' props.
  ;; - 25 introduced `pure' and `side-effect-free'.
  ;; - 30 introduced `important-return-value'.
  (when (boundp 'defun-declarations-alist)
    (dolist (prop '(important-return-value pure side-effect-free))
      (unless (assq prop defun-declarations-alist)
        (push (list prop #'ignore) defun-declarations-alist)))))

(if (fboundp 'gensym)
    (defalias 'pcase-dash--gensym 'gensym)
  (defalias 'pcase-dash--gensym 'dash--match-make-source-symbol))

(defun dash--elem-pattern (elem)
  "Return a pattern for ELEM, which may be further destructured."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond
   ((sequencep elem)
    `(dash ,elem))
   ((eq ?_ (aref (symbol-name elem) 0))
    '_)
   (t
    elem)))

(defun dash--vect-pattern (vect)
  "Return a pattern for VECT matching a vector."
  (declare (important-return-value t)
           (side-effect-free t))
  (cond
   ((= 1 (length vect))
    `(and (pred arrayp)
          (app (pcase--flip aref 0)
               (dash ,(aref vect 0)))))
   ((eq (aref vect 1) '&as)
    `(and (pred arrayp)
          (dash ,(aref vect 0))
          (dash ,(substring vect 2))))
   (t
    (let ((res)
          (tag (pcase-dash--gensym)))
      (catch tag
        (dotimes (idx (length vect))
          (let ((it (aref vect idx)))
            (if (eq it '&rest)
                (progn
                  (push `(app (pcase--flip substring ,idx)
                              (dash ,(aref vect (1+ idx))))
                        res)
                  (throw tag nil))
              (push `(app (pcase--flip aref ,idx)
                          (dash ,it))
                    res)))))
      `(and (pred arrayp)
            ,@(nreverse res))))))

(defun dash--hash-or-null-p (x)
  "Return non-nil if X is a hash table or null."
  (declare (important-return-value t)
           (side-effect-free t))
  (or (null x)
      (hash-table-p x)))

(defun dash--hash-or-null-get (key map)
  "Return value associated with KEY in MAP."
  (declare (important-return-value t)
           (side-effect-free t))
  (when map
    (gethash key map)))

(defun dash--hash-or-list-p (x)
  "Return non-nil if X is a hash table or a list."
  (declare (important-return-value t)
           (side-effect-free t))
  (or (listp x)
      (hash-table-p x)))

(defun dash--hash-or-plist-get (key map)
  "Return value associated with KEY in MAP."
  (declare (important-return-value t)
           (side-effect-free t))
  (if (hash-table-p map)
      (gethash key map)
    (plist-get map key)))

(defun dash--keyvar-pattern (list)
  "Make pattern matching LIST for `&alist', `&plist', `&hash', `&hash?', and `&hash-or-plist'."
  (declare (important-return-value t)
           (side-effect-free t))
  (let ((res)
        (test)
        (getter)
        (type (car list))
        (list (cdr (dash--match-kv-normalize-match-form list))))
    (cond
     ((eq type '&hash)
      (setq getter (lambda (key) `(gethash ,key))
            test #'hash-table-p))
     ((eq type '&hash?)
      (setq getter (lambda (key) `(dash--hash-or-null-get ,key))
            ;; FIXME and TODO: Should this pattern match or not match null?
            test #'dash--hash-or-null-p))
     ((eq type '&hash-or-plist)
      (setq getter (lambda (key) `(dash--hash-or-plist-get ,key))
            test #'dash--hash-or-list-p))
     ((eq type '&plist)
      (setq getter (lambda (key) `(pcase--flip plist-get ,key))
            test #'listp))
     ((eq type '&alist)
      (setq getter (let ((sym (pcase-dash--gensym)))
                     (lambda (key)
                       `(lambda (,sym) (cdr (assoc ,key ,sym)))))
            test #'listp)))
    (while list
      (let ((key (pop list))
            (var (pop list)))
        (push `(app ,(funcall getter key) (dash ,var))
              res)))
    `(and (pred ,test)
          ,@(nreverse res))))

(defun dash--list-pattern (list)
  "Return a pattern for LIST matching a list.

Unlike the vector pattern, the list pattern does not require the
matched expression to be long enough to bind all sub-patterns."
  (declare (important-return-value t)
           (side-effect-free t))
  (let ((first (car list))
        (rest (cdr list)))
    (cond
     ((eq first '&keys)
      `(dash ,(cons '&plist rest)))
     ((null list)
      '_)
     ((null rest)
      `(and (pred listp)
            (app car (dash ,first))))
     ((and (consp rest)
           (eq (car rest) '&as))
      `(and (pred listp)
            (dash ,first)
            (dash ,(cdr rest))))
     (t
      `(and (pred listp)
            (app car-safe (dash ,first))
            (app cdr-safe (dash ,rest)))))))

(if (fboundp 'pcase-defmacro)
    (pcase-defmacro dash (pat)
      "Destructure EXP according to PAT like in `-let'."
      (declare (important-return-value t)
               (side-effect-free t))
      (cond
       ((symbolp pat)
        (dash--elem-pattern pat))
       ((arrayp pat)
        (dash--vect-pattern pat))
       ((memq (car-safe pat) '(&plist &alist &hash &hash? &hash-or-plist))
        (dash--keyvar-pattern pat))
       ((listp pat)
        (dash--list-pattern pat))
       (t (error "Invalid Dash pattern: %s" pat))))
  (warn "`dash-pcase' does not support Emacs versions less than 25.1"))

(provide 'dash-pcase)
;;; dash-pcase.el ends here
