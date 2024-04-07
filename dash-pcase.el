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

(require 'dash)
(require 'pcase)

(defun dash--elem-pattern (elem)
  "Return a pattern for ELEM, which may be further destructured."
  (cond
   ((sequencep elem)
    `(dash ,elem))
   ((eq ?_ (aref (symbol-name elem) 0))
    '_)
   (t
    elem)))

(defun dash--vect-pattern (vect)
  "Return a pattern for VECT matching a vector."
  (cond
   ((= 1 (length vect))
    `(and (pred arrayp)
          (app (pcase--flip aref 0)
               ,(dash--elem-pattern (aref vect 0)))))
   ((eq (aref vect 1) '&as)
    `(and (pred arrayp)
          ,(dash--elem-pattern (aref vect 0))
          ,(dash--vect-pattern (substring vect 2))))
   (t
    (let ((res)
          (tag (gensym)))
      (catch tag
        (dotimes (idx (length vect))
          (let ((it (aref vect idx)))
            (if (eq it '&rest)
                (progn
                  (push `(app (pcase--flip substring ,idx)
                              ,(dash--elem-pattern (aref vect (1+ idx))))
                        res)
                  (throw tag nil))
              (push `(app (pcase--flip aref ,idx)
                          ,(dash--elem-pattern it))
                    res)))))
      `(and (pred arrayp)
            ,@(nreverse res))))))

(defun dash--keyvar-pattern (list)
  "Make pattern matching LIST for `&alist', `&plist', and `&hash'."
  (let ((res)
        (test)
        (getter)
        (type (car list))
        (list (cdr (dash--match-kv-normalize-match-form list))))
    (cond
     ((eq type '&hash)
      (setq getter (lambda (key) `(gethash ,key))
            test #'hash-table-p))
     ((eq type '&plist)
      (setq getter (lambda (key) `(pcase--flip plist-get ,key))
            test #'listp))
     ((eq type '&alist)
      (setq getter (let ((sym (gensym)))
                     (lambda (key)
                       `(lambda (,sym) (cdr (assoc ,key ,sym)))))
            test #'listp)))
    (while list
      (let ((key (pop list))
            (var (pop list)))
        (push `(app ,(funcall getter key) ,(dash--elem-pattern var))
              res)))
    `(and (pred ,test)
          ,@(nreverse res))))

(defun dash--list-pattern (list)
  "Return a pattern for LIST matching a list."
  (let ((first (car list))
        (rest (cdr list)))
    (cond
     ((null list)
      '_)
     ((null rest)
      `(and (pred consp)
            (app car ,first)))
     ((and (consp rest)
           (eq (car rest) '&as))
      `(and (pred consp)
            ,(dash--elem-pattern first)
            ,(dash--list-pattern (cdr rest))))
     ((eq first '&keys)
      (dash--keyvar-pattern (cons '&plist rest)))
     (t
      `(and (pred consp)
            (app car-safe ,(dash--elem-pattern first))
            (app cdr-safe ,(if (consp rest)
                               (dash--list-pattern rest)
                             (dash--elem-pattern rest))))))))

(pcase-defmacro dash (pat)
  "Destructure EXP according to PAT like in `-let'."
  (cond
   ((symbolp pat)
    (dash--elem-pattern pat))
   ((arrayp pat)
    (dash--vect-pattern pat))
   ((memq (car-safe pat) '(&plist &alist &hash))
    (dash--keyvar-pattern pat))
   ((listp pat)
    (dash--list-pattern pat))
   (t (error "Invalid Dash pattern: %s" pat))))

(provide 'dash-pcase)
;;; dash-pcase.el ends here
