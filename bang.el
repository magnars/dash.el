;;; bang.el --- A modern list library for Emacs

;; Copyright (C) 2012 Magnar Sveen, Joel McCracken

;; Authors: Magnar Sveen <magnars@gmail.com>
;;          Joel McCracken
;; Keywords: lists

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

;; The startings of a modern list api for Emacs.

;;; Code:

(defalias '!map 'mapcar)

(defmacro !!map (form list)
  `(!map (lambda (it) ,form) ,list))

(defun !reduce-from (fn initial-value list)
  "Returns the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, returns INITIAL-VALUE and
FN is not called."
  (let ((acc initial-value))
     (while list
       (setq acc (funcall fn acc (car list)))
       (setq list (cdr list)))
     acc))

(defmacro !!reduce-from (form initial-value list)
  "Anaphoric form of `!reduce'. Returns the result of applying
FORM to INITIAL-VALUE and the first item in LIST, then applying
FORM to that result and the 2nd item, etc. If INITIAL-VALUE
contains no items, returns INITIAL-VALUE and FORM is not called."
  `(let ((!--list ,list)
         (!--acc ,initial-value))
     (while !--list
       (let ((it (car !--list))
             (acc !--acc))
         (setq !--acc ,form))
       (setq !--list (cdr !--list)))
     !--acc))

(defun !reduce (fn list)
  "Returns the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce returns the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called."
  (if list
      (!reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro !!reduce (form list)
  "Returns the result of applying FORM to the first 2 items in LIST,
then applying FORM to that result and the 3rd item, etc. If
LIST contains no items, FORM must accept no arguments as
well, and reduce returns the result of calling FORM with no
arguments. If LIST has only 1 item, it is returned and FORM
is not called."
  (if (eval list)
      `(!!reduce-from ,form ,(car (eval list)) ',(cdr (eval list)))
    `(let (acc it) ,form)))

(defun !filter (fn list)
  "Returns a new list of the items in LIST for which FN returns a non-nil value."
  (let ((result '()))
    (while list
      (when (funcall fn (car list))
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))

(defmacro !!filter (form list)
  "Returns a new list of the items in LIST for which FORM returns a non-nil value."
  `(let ((!--list ,list)
         (!--result '()))
     (while !--list
       (let ((it (car !--list)))
         (when ,form
           (setq !--result (cons it !--result))))
       (setq !--list (cdr !--list)))
     (nreverse !--result)))

(defun !remove (fn list)
  "Returns a new list of the items in LIST for which FN returns nil."
  (!!filter (not (funcall fn it)) list))

(defmacro !!remove (form list)
  "Returns a new list of the items in LIST for which FORM returns nil."
  `(!!filter (not ,form) ,list))

(defun !concat (&rest lists)
  "Returns a new list with the concatenation of the elements in
the supplied LISTS."
  (apply 'append (append lists '(nil))))

(defun !mapcat (fn list)
  "Returns the result of applying concat to the result of applying map to FN and LIST.
Thus function FN should return a collection."
  (apply '!concat (!map fn list)))

(defmacro !!mapcat (form list)
  "Returns the result of applying concat to the result of applying map to FORM and LIST.
Thus function FORM should return a collection."
  `(apply '!concat (!!map ,form ,list)))

(defalias '!partial 'apply-partially)

(defun !uniq (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!!filter (not (!contains? !--result it)) list))

(defun !intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!!filter (!contains? list2 it) list))

(defun !difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!!filter (not (!contains? list2 it)) list))

(defun !contains? (list element)
  "Return whether LIST contains ELEMENT.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (cond
   ((null !compare-fn)    (member element list))
   ((eq !compare-fn 'eq)  (memq element list))
   ((eq !compare-fn 'eql) (memql element list))
   (t
    (let ((lst list))
      (while (and lst
                  (not (funcall !compare-fn element (car lst))))
        (setq lst (cdr lst)))
      lst))))

(defvar !compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((!compare-fn =)) (!union numbers1 numbers2 numbers3)")

(provide 'bang)
;;; bang.el ends here
