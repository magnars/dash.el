;;; dash.el --- A modern list library for Emacs

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.0.1
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

;; A modern list api for Emacs.
;;
;; See documentation on https://github.com/magnars/dash.el#functions

;;; Code:

(defmacro !cons (car cdr)
  "Destructive: Sets CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Sets LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro --each (list form)
  "Anaphoric form of `-each'."
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (while ,l
         (let ((it (car ,l)))
           ,form)
         (!cdr ,l)))))

(defun -each (list fn)
  "Calls FN with every item in LIST. Returns nil, used for side-effects only."
  (--each list (funcall fn it)))

(defmacro --each-while (list pred form)
  "Anaphoric form of `-each-while'."
  (let ((l (make-symbol "list"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,c t))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if ,pred ,form (setq ,c nil)))
         (!cdr ,l)))))

(defun -each-while (list pred fn)
  "Calls FN with every item in LIST while (PRED item) is non-nil.
Returns nil, used for side-effects only."
  (--each-while list (funcall pred it) (funcall fn it)))

(defun -map (fn list)
  "Returns a new list consisting of the result of applying FN to the items in LIST."
  (mapcar fn list))

(defmacro --map (form list)
  "Anaphoric form of `-map'."
  `(mapcar (lambda (it) ,form) ,list))

(defmacro --reduce-from (form initial-value list)
  "Anaphoric form of `-reduce-from'."
  `(let ((acc ,initial-value))
     (--each ,list (setq acc ,form))
     acc))

(defun -reduce-from (fn initial-value list)
  "Returns the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, returns INITIAL-VALUE and
FN is not called.

In the anaphoric form `--reduce-from', the accumulated value is
exposed as `acc`."
  (--reduce-from (funcall fn acc it) initial-value list))

(defmacro --reduce (form list)
  "Anaphoric form of `-reduce'."
  (if (eval list)
      `(--reduce-from ,form ,(car (eval list)) ',(cdr (eval list)))
    `(let (acc it) ,form)))

(defun -reduce (fn list)
  "Returns the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce returns the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called.

In the anaphoric form `--reduce', the accumulated value is
exposed as `acc`."
  (if list
      (-reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro --filter (form list)
  "Anaphoric form of `-filter'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun -filter (fn list)
  "Returns a new list of the items in LIST for which FN returns a non-nil value.

Alias: `-select'"
  (--filter (funcall fn it) list))

(defalias '-select '-filter)
(defalias '--select '--filter)

(defmacro --remove (form list)
  "Anaphoric form of `-remove'."
  `(--filter (not ,form) ,list))

(defun -remove (fn list)
  "Returns a new list of the items in LIST for which FN returns nil.

Alias: `-reject'"
  (--remove (funcall fn it) list))

(defalias '-reject '-remove)
(defalias '--reject '--remove)

(defmacro --keep (form list)
  "Anaphoric form of `-keep'."
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (!cons ,m ,r))))
       (nreverse ,r))))

(defun -keep (fn list)
  "Returns a new list of the non-nil results of applying FN to the items in LIST."
  (--keep (funcall fn it) list))

(defun -flatten (l)
  "Takes a nested list L and returns its contents as a single, flat list."
  (if (listp l)
      (-mapcat '-flatten l)
    (list l)))

(defun -concat (&rest lists)
  "Returns a new list with the concatenation of the elements in the supplied LISTS."
  (apply 'append lists))

(defmacro --mapcat (form list)
  "Anaphoric form of `-mapcat'."
  `(apply 'append (--map ,form ,list)))

(defun -mapcat (fn list)
  "Returns the result of applying concat to the result of applying map to FN and LIST.
Thus function FN should return a collection."
  (--mapcat (funcall fn it) list))

(defmacro --first (form list)
  "Anaphoric form of `-first'."
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n) (when ,form (setq ,n it)))
       ,n)))

(defun -first (fn list)
  "Returns the first x in LIST where (FN x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'."
  (--first (funcall fn it) list))

(defun ---truthy? (val)
  (not (null val)))

(defmacro --any? (form list)
  "Anaphoric form of `-any?'."
  `(---truthy? (--first ,form ,list)))

(defun -any? (fn list)
  "Returns t if (FN x) is non-nil for any x in LIST, else nil.

Alias: `-some?'"
  (--any? (funcall fn it) list))

(defalias '-some? '-any?)
(defalias '--some? '--any?)

(defalias '-any-p '-any?)
(defalias '--any-p '--any?)
(defalias '-some-p '-any?)
(defalias '--some-p '--any?)

(defmacro --all? (form list)
  "Anaphoric form of `-all?'."
  (let ((a (make-symbol "all")))
    `(let ((,a t))
       (--each-while ,list ,a (setq ,a ,form))
       (---truthy? ,a))))

(defun -all? (fn list)
  "Returns t if (FN x) is non-nil for all x in LIST, else nil.

Alias: `-every?'"
  (--all? (funcall fn it) list))

(defalias '-every? '-all?)
(defalias '--every? '--all?)

(defalias '-all-p '-all?)
(defalias '--all-p '--all?)
(defalias '-every-p '-all?)
(defalias '--every-p '--all?)

(defmacro --none? (form list)
  "Anaphoric form `-none?'."
  `(--all? (not ,form) ,list))

(defun -none? (fn list)
  "Returns t if (FN x) is nil for all x in LIST, else nil."
  (--none? (funcall fn it) list))

(defalias '-none-p '-none?)
(defalias '--none-p '--none?)

;; simplify with a --dotimes
(defun -take (n list)
  "Returns a new list of the first N items in LIST, or all items if there are fewer than N."
  (let (result)
    (while (and list (> n 0))
      (!cons (car list) result)
      (!cdr list)
      (setq n (1- n)))
    (nreverse result)))

(defun -drop (n list)
  "Returns the tail of LIST without the first N items."
  (while (and list (> n 0))
    (!cdr list)
    (setq n (1- n)))
  list)

(defmacro --take-while (form list)
  "Anaphoric form of `-take-while'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each-while ,list ,form (!cons it ,r))
       (nreverse ,r))))

(defun -take-while (fn list)
  "Returns a new list of successive items from LIST while (FN item) returns a non-nil value."
  (--take-while (funcall fn it) list))

(defmacro --drop-while (form list)
  "Anaphoric form of `-drop-while'."
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (while (and ,l (let ((it (car ,l))) ,form))
         (!cdr ,l))
       ,l)))

(defun -drop-while (fn list)
  "Returns the tail of LIST starting from the first item for which (FN item) returns nil."
  (--drop-while (funcall fn it) list))

(defun -split-at (n list)
  "Returns a list of ((-take N LIST) (-drop N LIST))"
  (list (-take n list)
        (-drop n list)))

(defmacro --split-with (form list)
  "Anaphoric form of `-split-with'."
  `(list (--take-while ,form ,list)
         (--drop-while ,form ,list)))

(defun -split-with (fn list)
  "Returns a list of ((-take-while FN LIST) (-drop-while FN LIST))"
  (--split-with (funcall fn it) list))

(defun -partition (n list)
  "Returns a new list with the items in LIST grouped into N-sized sublists.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (let ((result nil)
        (sublist nil)
        (len 0))
    (while list
      (!cons (car list) sublist)
      (setq len (1+ len))
      (when (= len n)
        (!cons (nreverse sublist) result)
        (setq sublist nil)
        (setq len 0))
      (!cdr list))
    (nreverse result)))

(defun -partition-all (n list)
  "Returns a new list with the items in LIST grouped into N-sized sublists.
The last group may contain less than N items."
  (let (result)
    (while list
      (!cons (-take n list) result)
      (setq list (-drop n list)))
    (nreverse result)))

(defun -interpose (sep list)
  "Returns a new list of all elements in LIST separated by SEP."
  (let (result)
    (when list
      (!cons (car list) result)
      (!cdr list))
    (while list
      (setq result (cons (car list) (cons sep result)))
      (!cdr list))
    (nreverse result)))

(defun -interleave (&rest lists)
  "Returns a new list of the first item in each list, then the second etc."
  (let (result)
    (while (-none? 'null lists)
      (--each lists (!cons (car it) result))
      (setq lists (-map 'cdr lists)))
    (nreverse result)))

(defmacro --replace-where (pred rep list)
  "Anaphoric form of `-replace-where'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (!cons (if ,pred ,rep it) ,r))
       (nreverse ,r))))

(defun -replace-where (pred rep list)
  "Returns a new list where the elements in LIST that does not match the PRED function
are unchanged, and where the elements in LIST that do match the PRED function are mapped
through the REP function."
  (--replace-where (funcall pred it) (funcall rep it) list))

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
  `(closure (t) (&rest args)
            (apply ',fn (append args ',args))))

(defmacro -> (x &optional form &rest more)
  "Threads the expr through the forms. Inserts X as the second
item in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
second item in second form, etc."
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))

(defmacro ->> (x form &rest more)
  "Threads the expr through the forms. Inserts X as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc."
  (if (null more)
      (if (listp form)
          `(,(car form) ,@(cdr form) ,x)
        (list form x))
    `(->> (->> ,x ,form) ,@more)))

(defmacro --> (x form &rest more)
  "Threads the expr through the forms. Inserts X at the position
signified by the token `it' in the first form. If there are more
forms, inserts the first form at the position signified by `it'
in in second form, etc."
  (if (null more)
      (if (listp form)
          (--replace-where (eq it 'it) x form)
        (list form x))
    `(--> (--> ,x ,form) ,@more)))

(defun -distinct (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (let (result)
    (--each list (when (not (-contains? result it)) (!cons it result)))
    (nreverse result)))

(defun -intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (-contains? list2 it) list))

(defun -difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (not (-contains? list2 it)) list))

(defvar -compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((-compare-fn =)) (-union numbers1 numbers2 numbers3)")

(defun -contains? (list element)
  "Return whether LIST contains ELEMENT.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (not
   (null
    (cond
     ((null -compare-fn)    (member element list))
     ((eq -compare-fn 'eq)  (memq element list))
     ((eq -compare-fn 'eql) (memql element list))
     (t
      (let ((lst list))
        (while (and lst
                    (not (funcall -compare-fn element (car lst))))
          (setq lst (cdr lst)))
        lst))))))

(defalias '-contains-p '-contains?)

(provide 'dash)
;;; dash.el ends here
