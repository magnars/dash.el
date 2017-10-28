;;; dash-functional.el --- Collection of useful combinators for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

;; Authors: Matus Goljer <matus.goljer@gmail.com>
;;          Magnar Sveen <magnars@gmail.com>
;; Version: 1.2.0
;; Package-Requires: ((dash "2.0.0") (emacs "24"))
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

(require 'dash)

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
args first and then ARGS."
  (lambda (&rest args-before) (apply fn (append args-before args))))

(defun -juxt (&rest fns)
  "Takes a list of functions and returns a fn that is the
juxtaposition of those fns. The returned fn takes a variable
number of args, and returns a list containing the result of
applying each fn to the args (left-to-right)."
  (lambda (&rest args) (mapcar (lambda (x) (apply x args)) fns)))

(defun -compose (&rest fns)
  "Takes a list of functions and returns a fn that is the
composition of those fns. The returned fn takes a variable
number of arguments, and returns the result of applying
each fn to the result of applying the previous fn to
the arguments (right-to-left)."
  (lambda (&rest args)
    (car (-reduce-r-from (lambda (fn xs) (list (apply fn xs)))
                         args fns))))

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
  (lambda (&rest _) c))

(defmacro -cut (&rest params)
  "Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See SRFI-26 for detailed description."
  (let* ((i 0)
         (args (mapcar (lambda (_) (setq i (1+ i)) (make-symbol (format "D%d" i)))
                       (-filter (-partial 'eq '<>) params))))
    `(lambda ,args
       ,(let ((body (--map (if (eq it '<>) (pop args) it) params)))
          (if (eq (car params) '<>)
              (cons 'funcall body)
            body)))))

(defun -not (pred)
  "Take a unary predicate PRED and return a unary predicate
that returns t if PRED returns nil and nil if PRED returns
non-nil."
  (lambda (x) (not (funcall pred x))))

(defun -orfn (&rest preds)
  "Take list of unary predicates PREDS and return a unary
predicate with argument x that returns non-nil if at least one of
the PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-any? (-cut funcall <> x) preds)))

(defun -andfn (&rest preds)
  "Take list of unary predicates PREDS and return a unary
predicate with argument x that returns non-nil if all of the
PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-all? (-cut funcall <> x) preds)))

(defun -iteratefn (fn n)
  "Return a function FN composed N times with itself.

FN is a unary function.  If you need to use a function of higher
arity, use `-applify' first to turn it into a unary function.

With n = 0, this acts as identity function.

In types: (a -> a) -> Int -> a -> a.

This function satisfies the following law:

  (funcall (-iteratefn fn n) init) = (-last-item (-iterate fn init (1+ n)))."
  (lambda (x) (--dotimes n (setq x (funcall fn x))) x))

(defun -counter (&optional beg end inc)
  "Return a closure that counts from BEG to END, with increment INC.

The closure will return the next value in the counting sequence
each time it is called, and nil after END is reached. BEG
defaults to 0, INC defaults to 1, and if END is nil, the counter
will increment indefinitely.

The closure accepts any number of arguments, which are discarded."
  (let ((inc (or inc 1))
        (n (or beg 0)))
    (lambda (&rest _)
      (when (or (not end) (< n end))
        (prog1 n
          (setq n (+ n inc)))))))

(defvar -fixfn-max-iterations 1000
  "The default maximum number of iterations performed by `-fixfn'
  unless otherwise specified.")

(defun -fixfn (fn &optional equal-test halt-test)
  "Return a function that computes the (least) fixpoint of FN.

FN must be a unary function. The returned lambda takes a single
argument, X, the initial value for the fixpoint iteration. The
iteration halts when either of the following conditions is satisified:

 1. Iteration converges to the fixpoint, with equality being
    tested using EQUAL-TEST. If EQUAL-TEST is not specified,
    `equal' is used. For functions over the floating point
    numbers, it may be necessary to provide an appropriate
    appoximate comparsion test.

 2. HALT-TEST returns a non-nil value. HALT-TEST defaults to a
    simple counter that returns t after `-fixfn-max-iterations',
    to guard against infinite iteration. Otherwise, HALT-TEST
    must be a function that accepts a single argument, the
    current value of X, and returns non-nil as long as iteration
    should continue. In this way, a more sophisticated
    convergence test may be supplied by the caller.

The return value of the lambda is either the fixpoint or, if
iteration halted before converging, a cons with car `halted' and
cdr the final output from HALT-TEST.

In types: (a -> a) -> a -> a."
  (let ((eqfn   (or equal-test 'equal))
    (haltfn (or halt-test
            (-not
              (-counter 0 -fixfn-max-iterations)))))
    (lambda (x)
      (let ((re (funcall fn x))
        (halt? (funcall haltfn x)))
    (while (and (not halt?) (not (funcall eqfn x re)))
      (setq x     re
        re    (funcall fn re)
        halt? (funcall haltfn re)))
    (if halt? (cons 'halted halt?)
      re)))))

(defun -prodfn (&rest fns)
  "Take a list of n functions and return a function that takes a
list of length n, applying i-th function to i-th element of the
input list.  Returns a list of length n.

In types (for n=2): ((a -> b), (c -> d)) -> (a, c) -> (b, d)

This function satisfies the following laws:

  (-compose (-prodfn f g ...) (-prodfn f' g' ...)) = (-prodfn (-compose f f') (-compose g g') ...)
  (-prodfn f g ...) = (-juxt (-compose f (-partial 'nth 0)) (-compose g (-partial 'nth 1)) ...)
  (-compose (-prodfn f g ...) (-juxt f' g' ...)) = (-juxt (-compose f f') (-compose g g') ...)
  (-compose (-partial 'nth n) (-prod f1 f2 ...)) = (-compose fn (-partial 'nth n))"
  (lambda (x) (-zip-with 'funcall fns x)))

(provide 'dash-functional)

;;; dash-functional.el ends here
