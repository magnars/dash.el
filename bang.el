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

(defun !map (fn list)
  "Returns a new list consisting of the result of applying FN to the items in LIST."
  (mapcar fn list))

(defmacro !!map (form list)
  "Anaphoric form of `!map'."
  `(mapcar (lambda (it) ,form) ,list))

(defmacro !!reduce-from (form initial-value list)
  "Anaphoric form of `!reduce-from'."
  `(let ((!--list ,list)
         (!--acc ,initial-value))
     (while !--list
       (let ((it (car !--list))
             (acc !--acc))
         (setq !--acc ,form))
       (setq !--list (cdr !--list)))
     !--acc))

(defun !reduce-from (fn initial-value list)
  "Returns the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, returns INITIAL-VALUE and
FN is not called.

In the anaphoric form `!!reduce-from', the accumulated value is
exposed as `acc`."
  (!!reduce-from (funcall fn acc it) initial-value list))

(defmacro !!reduce (form list)
  "Anaphoric form of `!reduce'."
  (if (eval list)
      `(!!reduce-from ,form ,(car (eval list)) ',(cdr (eval list)))
    `(let (acc it) ,form)))

(defun !reduce (fn list)
  "Returns the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce returns the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called.

In the anaphoric form `!!reduce', the accumulated value is
exposed as `acc`."
  (if list
      (!reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro !!filter (form list)
  "Anaphoric form of `!filter'."
  `(let ((!--list ,list)
         (!--result '()))
     (while !--list
       (let ((it (car !--list)))
         (when ,form
           (setq !--result (cons it !--result))))
       (setq !--list (cdr !--list)))
     (nreverse !--result)))

(defun !filter (fn list)
  "Returns a new list of the items in LIST for which FN returns a non-nil value.

Alias: `!select'"
  (!!filter (funcall fn it) list))

(defalias '!select '!filter)
(defalias '!!select '!!filter)

(defmacro !!remove (form list)
  "Anaphoric form of `!remove'."
  `(!!filter (not ,form) ,list))

(defun !remove (fn list)
  "Returns a new list of the items in LIST for which FN returns nil.

Alias: `!reject'"
  (!!remove (funcall fn it) list))

(defalias '!reject '!remove)
(defalias '!!reject '!!remove)

(defmacro !!keep (form list)
  "Anaphoric form of `!keep'."
  `(let ((!--list ,list)
         (!--result '()))
     (while !--list
       (let* ((it (car !--list))
              (mapped ,form))
         (when mapped
           (setq !--result (cons mapped !--result))))
       (setq !--list (cdr !--list)))
     (nreverse !--result)))

(defun !keep (fn list)
  "Returns a new list of the non-nil results of applying FN to the items in LIST."
  (!!keep (funcall fn it) list))

(defun !concat (&rest lists)
  "Returns a new list with the concatenation of the elements in
the supplied LISTS."
  (apply 'append (append lists '(nil))))

(defmacro !!mapcat (form list)
  "Anaphoric form of `!mapcat'."
  `(apply '!concat (!!map ,form ,list)))

(defun !mapcat (fn list)
  "Returns the result of applying concat to the result of applying map to FN and LIST.
Thus function FN should return a collection."
  (!!mapcat (funcall fn it) list))

(defun !partial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS +
additional args."
  (apply 'apply-partially fn args))

(defun !distinct (list)
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
  (not
   (null
    (cond
     ((null !compare-fn)    (member element list))
     ((eq !compare-fn 'eq)  (memq element list))
     ((eq !compare-fn 'eql) (memql element list))
     (t
      (let ((lst list))
        (while (and lst
                    (not (funcall !compare-fn element (car lst))))
          (setq lst (cdr lst)))
        lst))))))

(defmacro !!first (form list)
  "Anaphoric form of `!first'."
  `(let ((!--list ,list)
         (!--needle nil))
     (while (and !--list (not !--needle))
       (let ((it (car !--list)))
         (when ,form (setq !--needle it)))
       (setq !--list (cdr !--list)))
     !--needle))

(defun !first (fn list)
  "Returns the first x in LIST where (FN x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'."
  (!!first (funcall fn it) list))

(defun !--truthy? (val)
  (not (null val)))

(defmacro !!any? (form list)
  "Anaphoric form of `!any?'."
  `(!--truthy? (!!first ,form ,list)))

(defun !any? (fn list)
  "Returns t if (FN x) is non-nil for any x in LIST, else nil.

Alias: `!some?'"
  (!!any? (funcall fn it) list))

(defalias '!some? '!any?)
(defalias '!!some? '!!any?)

(defmacro !!all? (form list)
  "Anaphoric form of `!all?'."
  `(let ((!--list ,list)
         (!--all t))
     (while (and !--all !--list)
       (let ((it (car !--list)))
         (setq !--all ,form))
       (setq !--list (cdr !--list)))
     (!--truthy? !--all)))

(defun !all? (fn list)
  "Returns t if (FN x) is non-nil for all x in LIST, else nil.

Alias: `!every?'"
  (!!all? (funcall fn it) list))

(defalias '!every? '!all?)
(defalias '!!every? '!!all?)

(defmacro !!each (list form)
  "Anaphoric form of `!each'."
  `(let ((!--list ,list))
     (while !--list
       (let ((it (car !--list)))
         ,form)
       (setq !--list (cdr !--list)))))

(defun !each (list fn)
  "Calls FN with every item in LIST. Returns nil, used for side-effects only."
  (!!each list (funcall fn it)))

(defvar !compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((!compare-fn =)) (!union numbers1 numbers2 numbers3)")

(provide 'bang)
;;; bang.el ends here
