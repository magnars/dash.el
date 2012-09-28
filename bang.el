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

(defun !--call-with-it (form-or-fn)
  (if (functionp form-or-fn)
      (list form-or-fn 'it)
    form-or-fn))

(defmacro !filter (form-or-fn list)
  `(let ((!--list ,list)
         (!--result '()))
     (while !--list
       (let ((it (car !--list)))
         (when ,(!--call-with-it form-or-fn)
           (setq !--result (cons it !--result))))
       (setq !--list (cdr !--list)))
     (nreverse !--result)))

(defmacro !map (form-or-fn list)
  (if (functionp form-or-fn)
      `(mapcar #',form-or-fn ,list)
    `(mapcar #'(lambda (it) ,form-or-fn) ,list)))

(defmacro !reduce-from (form-or-fn initial-value list)
  `(let ((!--list ,list)
         (!--acc ,initial-value))
     (while !--list
       (let ((it (car !--list))
             (acc !--acc))
         (setq !--acc ,(if (functionp form-or-fn) (list form-or-fn 'acc 'it) form-or-fn))
         (setq !--list (cdr !--list))))
     !--acc))

(defmacro !reduce (form-or-fn list)
  (if (eval list)
      `(!reduce-from ,form-or-fn ,(car (eval list)) ',(cdr (eval list)))
    (if (functionp form-or-fn)
        (list form-or-fn)
      `(let (acc it) ,form-or-fn))))

(defun !concat (&rest lists)
  (apply 'append (append lists '(nil))))

(defalias '!select '!filter)

(defmacro !reject (form-or-fn list)
  `(!filter (not ,(!--call-with-it form-or-fn)) ,list))

(defmacro !partial (fn &rest args)
  `(apply-partially ',fn ,@args))

(defmacro !mapcat (fn list)
  `(apply '!concat (!map ,fn ,list)))

(defun !uniq (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!filter (not (!contains-p !--result it)) list))

(defun !intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!filter (!contains-p list2 it) list))

(defun !difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!filter (not (!contains-p list2 it)) list))

(defun !contains-p (list element)
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
