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

(eval-when-compile (require 'cl))

(defvar !compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((!compare-fn =)) (!union numbers1 numbers2 numbers3)")

(defun !concat (list)
  (apply 'concatenate 'list list))

(defalias '!map 'mapcar)

(defalias '!filter 'remove-if-not)
(defalias '!select 'remove-if-not)
(defalias '!reject 'remove-if)

(defalias '!partial 'apply-partially)

(defun !mapcat (fn list)
  (!concat (!map fn list)))

(defmacro !--iterate-with-result (&rest forms)
  `(let (result)
     (while list
       (let ((it (car list))) ,@forms)
       (setq list (cdr list)))
     (nreverse result)))

(defun !uniq (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!--iterate-with-result
   (add-to-list 'result it nil !compare-fn)))

(defun !intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!--iterate-with-result
   (when (!contains-p list2 it)
     (setq result (cons it result)))))

(defun !difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (!--iterate-with-result
   (unless (!contains-p list2 it)
     (setq result (cons it result)))))

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

(provide 'bang)
;;; bang.el ends here
