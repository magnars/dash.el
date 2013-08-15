;;; dash-functional.el --- Collection of useful combinators for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 2013 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: lisp functions combinators

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

;; Collection of useful combinators for Emacs Lisp
;;
;; See documentation on https://github.com/magnars/dash.el#functions

;;; Code:

(defun -partial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS first and
then additional args."
  (apply 'apply-partially fn args))

(defun -rpartial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with the additional
args first and then ARGS.

Requires Emacs 24 or higher."
  (lambda (&rest args-before) (apply fn (append args-before args))))

(defun -juxt (&rest fns)
  "Takes a list of functions and returns a fn that is the
juxtaposition of those fns. The returned fn takes a variable
number of args, and returns a list containing the result of
applying each fn to the args (left-to-right).

Requires Emacs 24 or higher."
  (lambda (&rest args) (mapcar (lambda (x) (apply x args)) fns)))

(defun -applify (fn)
  "Changes an n-arity function FN to a 1-arity function that
expects a list with n items as arguments"
  (apply-partially 'apply fn))

(defun -on (operator transformer)
  "Return a function of two arguments that first applies
TRANSFORMER to each of them and then applies OPERATOR on the
results (in the same order).

In types: (b -> b -> c) -> (a -> b) -> a -> a -> c"
  (lambda (x y) (funcall operator (funcall transformer x) (funcall transformer y))))

(defun -flip (func)
  "Swap the order of arguments for binary function FUNC.

In types: (a -> b -> c) -> b -> a -> c"
  (lambda (x y) (funcall func y x)))

(defun -const (c)
  "Return a function that returns C ignoring any additional arguments.

In types: a -> b -> a"
  (lambda (&rest args) c))

(defmacro -cut (&rest params)
  "Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See SRFI-26 for detailed description."
  (let* ((i 0)
         (args (mapcar (lambda (x) (setq i (1+ i)) (make-symbol (format "D%d" i)))
                       (-filter (-partial 'eq '<>) params))))
    `(lambda ,args
       ,(--map (if (eq it '<>) (pop args) it) params))))

(defun -not (pred)
  "Take an unary predicates PRED and return an unary predicate
that returns t if PRED returns nil and nil if PRED returns
non-nil."
  (lambda (x) (not (funcall pred x))))

(defun -orfn (&rest preds)
  "Take list of unary predicates PREDS and return an unary
predicate with argument x that returns non-nil if at least one of
the PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-any? (-cut funcall <> x) preds)))

(defun -andfn (&rest preds)
  "Take list of unary predicates PREDS and return an unary
predicate with argument x that returns non-nil if all of the
PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-all? (-cut funcall <> x) preds)))

(provide 'dash-functional)

;;; dash-functional.el ends here
