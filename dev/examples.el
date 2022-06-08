;;; examples.el --- Examples/tests for dash.el's API  -*- lexical-binding: t -*-

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

;; Only the first three examples per function are shown in the docs,
;; so make those good.
;;
;; Use the `~>' symbol instead of `=>' to test the expected and actual
;; values with `approx='.

;;; Code:

(require 'dash)
(require 'dash-defs "dev/dash-defs")
(require 'ert)

(eval-when-compile
  ;; TODO: Emacs 24.3 first introduced `setf', so remove this when
  ;; support for earlier versions is dropped.
  (unless (fboundp 'setf)
    (require 'cl))

  ;; TODO: Emacs < 24.4 emitted a bogus warning when byte-compiling
  ;; ERT tests, so remove this when support for those versions is
  ;; dropped.  See  https://bugs.gnu.org/14883.
  (and (< emacs-major-version 25)
       (< emacs-minor-version 4)
       (setq byte-compile-delete-errors t))

  ;; Expander used in destructuring examples below.
  (defun dash-expand:&hash-or-plist (key source)
    "Sample destructuring which works with plists and hash tables."
    `(if (hash-table-p ,source) (gethash ,key ,source)
       (plist-get ,source ,key))))

;; FIXME: These definitions ought to be exported along with the
;; examples, if they are going to be used there.
(defun even? (num) (= 0 (% num 2)))
(defun square (num) (* num num))

(def-example-group "Maps"
  "Functions in this category take a transforming function, which
is then applied sequentially to each or selected elements of the
input list.  The results are collected in order and returned as a
new list."

  (defexamples -map
    (-map (lambda (num) (* num num)) '(1 2 3 4)) => '(1 4 9 16)
    (-map #'1+ '(1 2 3 4)) => '(2 3 4 5)
    (--map (* it it) '(1 2 3 4)) => '(1 4 9 16)
    (--map it ()) => ()
    (-map #'identity ()) => ())

  (defexamples -map-when
    (-map-when 'even? 'square '(1 2 3 4)) => '(1 4 3 16)
    (--map-when (> it 2) (* it it) '(1 2 3 4)) => '(1 2 9 16)
    (--map-when (= it 2) 17 '(1 2 3 4)) => '(1 17 3 4)
    (-map-when (lambda (n) (= n 3)) (-const 0) '(1 2 3 4)) => '(1 2 0 4))

  (defexamples -map-first
    (-map-first 'even? 'square '(1 2 3 4)) => '(1 4 3 4)
    (--map-first (> it 2) (* it it) '(1 2 3 4)) => '(1 2 9 4)
    (--map-first (= it 2) 17 '(1 2 3 2)) => '(1 17 3 2)
    (-map-first 'even? 'square '(1 3 5 7)) => '(1 3 5 7)
    (-map-first 'even? 'square '(2)) => '(4)
    (-map-first 'even? 'square nil) => nil)

  (defexamples -map-last
    (-map-last 'even? 'square '(1 2 3 4)) => '(1 2 3 16)
    (--map-last (> it 2) (* it it) '(1 2 3 4)) => '(1 2 3 16)
    (--map-last (= it 2) 17 '(1 2 3 2)) => '(1 2 3 17)
    ;; the next two tests assert that the input list is not modified #158
    (let ((l '(1 2 3))) (list (--map-last (< it 2) (number-to-string it) l) l)) => '(("1" 2 3) (1 2 3))
    (let ((l '(1 2 3))) (list (--map-last (< it 3) (number-to-string it) l) l)) => '((1 "2" 3) (1 2 3))
    (-map-last 'even? 'square '(1 3 5 7)) => '(1 3 5 7)
    (-map-last 'even? 'square '(2)) => '(4)
    (-map-last 'even? 'square nil) => nil)

  (defexamples -map-indexed
    (-map-indexed (lambda (index item) (- item index)) '(1 2 3 4)) => '(1 1 1 1)
    (--map-indexed (- it it-index) '(1 2 3 4)) => '(1 1 1 1)
    (-map-indexed #'* '(1 2 3 4)) => '(0 2 6 12)
    (-map-indexed #'ignore '(1 2 3 4)) => '(nil nil nil nil)
    (-map-indexed #'ignore '()) => '()
    (--map-indexed t '(1 2 3 4)) => '(t t t t)
    (--map-indexed t '()) => '())

  (defexamples -annotate
    (-annotate #'1+ '(1 2 3)) => '((2 . 1) (3 . 2) (4 . 3))
    (-annotate #'length '((f o o) (bar baz))) => '((3 f o o) (2 bar baz))
    (--annotate (> it 1) '(0 1 2 3)) => '((nil . 0) (nil . 1) (t . 2) (t . 3))
    (--annotate nil ()) => ()
    (--annotate nil '(a)) => '((nil . a))
    (--annotate nil '((a))) => '((nil a))
    (--annotate t ()) => ()
    (--annotate t '(a)) => '((t . a))
    (--annotate t '((a))) => '((t a))
    (--annotate it ()) => ()
    (--annotate it '(a)) => '((a . a))
    (--annotate it '((a))) => '(((a) a))
    (--annotate (list it) ()) => ()
    (--annotate (list it) '(a)) => '(((a) . a))
    (--annotate (list it) '((a))) => '((((a)) a))
    (-annotate #'ignore ()) => ()
    (-annotate #'ignore '(a)) => '((nil . a))
    (-annotate #'ignore '((a))) => '((nil a))
    (-annotate (-andfn) ()) => ()
    (-annotate (-andfn) '(a)) => '((t . a))
    (-annotate (-andfn) '((a))) => '((t a))
    (-annotate #'identity ()) => ()
    (-annotate #'identity '(a)) => '((a . a))
    (-annotate #'identity '((a))) => '(((a) a))
    (-annotate #'list ()) => ()
    (-annotate #'list '(a)) => '(((a) . a))
    (-annotate #'list '((a))) => '((((a)) a)))

  (defexamples -splice
    (-splice #'numberp (lambda (n) (list n n)) '(a 1 b 2)) => '(a 1 1 b 2 2)
    (--splice t (list it it) '(1 2 3 4)) => '(1 1 2 2 3 3 4 4)
    (--splice (eq it :magic) '((magical) (code)) '((foo) :magic (bar)))
    => '((foo) (magical) (code) (bar))
    (--splice nil (list (1+ it)) '()) => '()
    (--splice nil (list (1+ it)) '(1)) => '(1)
    (--splice t (list (1+ it)) '()) => '()
    (--splice t (list (1+ it)) '(1)) => '(2)
    (--splice nil '() '()) => '()
    (--splice nil '() '(1)) => '(1)
    (--splice t '() '()) => '()
    (--splice t '() '(1)) => '()
    (--splice t '() '(1 2)) => '()
    (--splice (= it 1) '() '(1 2)) => '(2)
    (--splice (= it 2) '() '(1 2)) => '(1)
    (--splice (= it 1) '() '(1 2 3)) => '(2 3)
    (--splice (= it 2) '() '(1 2 3)) => '(1 3)
    (--splice (= it 3) '() '(1 2 3)) => '(1 2)
    (-splice #'ignore (lambda (n) (list (1+ n))) '()) => '()
    (-splice #'ignore (lambda (n) (list (1+ n))) '(1)) => '(1)
    (-splice #'identity (lambda (n) (list (1+ n))) '()) => '()
    (-splice #'identity (lambda (n) (list (1+ n))) '(1)) => '(2)
    (-splice #'ignore #'ignore '()) => '()
    (-splice #'ignore #'ignore '(1)) => '(1)
    (-splice #'identity #'ignore '()) => '()
    (-splice #'identity #'ignore '(1)) => '()
    (-splice #'identity #'ignore '(1 2)) => '()
    (-splice (-cut = 1 <>) #'ignore '(1 2)) => '(2)
    (-splice (-cut = 2 <>) #'ignore '(1 2)) => '(1)
    (-splice (-cut = 1 <>) #'ignore '(1 2 3)) => '(2 3)
    (-splice (-cut = 2 <>) #'ignore '(1 2 3)) => '(1 3)
    (-splice (-cut = 3 <>) #'ignore '(1 2 3)) => '(1 2)
    ;; Test for destructive modification.
    (let ((l1 (list 1 2 3))
          (l2 (list 4 5 6)))
      (--splice (= it 2) l2 l1)
      (list l1 l2))
    => '((1 2 3) (4 5 6)))

  (defexamples -splice-list
    (-splice-list 'keywordp '(a b c) '(1 :foo 2)) => '(1 a b c 2)
    (-splice-list 'keywordp nil '(1 :foo 2)) => '(1 2)
    (--splice-list (keywordp it) '(a b c) '(1 :foo 2)) => '(1 a b c 2))

  (defexamples -mapcat
    (-mapcat 'list '(1 2 3)) => '(1 2 3)
    (-mapcat (lambda (item) (list 0 item)) '(1 2 3)) => '(0 1 0 2 0 3)
    (--mapcat (list 0 it) '(1 2 3)) => '(0 1 0 2 0 3))

  (defexamples -copy
    (-copy '(1 2 3)) => '(1 2 3)
    (let ((a '(1 2 3))) (eq a (-copy a))) => nil))

(def-example-group "Sublist selection"
  "Functions returning a sublist of the original list."

  (defexamples -filter
    (-filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) => '(2 4)
    (-filter #'natnump '(-2 -1 0 1 2)) => '(0 1 2)
    (--filter (= 0 (% it 2)) '(1 2 3 4)) => '(2 4)
    (let ((mod 2)) (-filter (lambda (n) (= 0 (% n mod))) '(1 2 3 4))) => '(2 4)
    (let ((mod 2)) (--filter (= 0 (% it mod)) '(1 2 3 4))) => '(2 4)
    (let ((l (list 1 2))) (setcar (-filter #'identity l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (setcar (--filter it l) 0) l) => '(1 2)
    (-filter #'identity '()) => '()
    (-filter #'ignore '()) => '()
    (--filter it '()) => '()
    (--filter nil '()) => '()
    (-filter #'identity '(1)) => '(1)
    (-filter #'ignore '(1)) => '()
    (--filter it '(1)) => '(1)
    (--filter nil '(1)) => '())

  (defexamples -remove
    (-remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) => '(1 3)
    (-remove #'natnump '(-2 -1 0 1 2)) => '(-2 -1)
    (--remove (= 0 (% it 2)) '(1 2 3 4)) => '(1 3)
    (let ((mod 2)) (-remove (lambda (n) (= 0 (% n mod))) '(1 2 3 4))) => '(1 3)
    (let ((mod 2)) (--remove (= 0 (% it mod)) '(1 2 3 4))) => '(1 3)
    (let ((l (list 1 2))) (setcar (-remove #'ignore l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (setcar (--remove nil l) 0) l) => '(1 2)
    (-remove #'identity '()) => '()
    (-remove #'ignore '()) => '()
    (--remove it '()) => '()
    (--remove nil '()) => '()
    (-remove #'identity '(1)) => '()
    (-remove #'ignore '(1)) => '(1)
    (--remove it '(1)) => '()
    (--remove nil '(1)) => '(1))

  (defexamples -remove-first
    (-remove-first #'natnump '(-2 -1 0 1 2)) => '(-2 -1 1 2)
    (-remove-first #'stringp '(1 2 "first" "second")) => '(1 2 "second")
    (--remove-first (> it 3) '(1 2 3 4 5 6)) => '(1 2 3 5 6)
    (-remove-first #'natnump '(2 3 4)) => '(3 4)
    (-remove-first #'natnump '(-3 -2 -1 4)) => '(-3 -2 -1)
    (-remove-first #'natnump '(2)) => '()
    (-remove-first #'natnump '()) => '()
    (-remove-first #'null '(1 3 5 7)) => '(1 3 5 7)
    (let ((l (list 1 2))) (setcar (-remove-first #'identity l) 0) l) => '(1 0)
    (let ((l (list 1 2))) (setcar (-remove-first #'null l) 0) l) => '(1 2))

  (defexamples -remove-last
    (-remove-last #'natnump '(1 3 5 4 7 8 10 -11)) => '(1 3 5 4 7 8 -11)
    (-remove-last #'stringp '(1 2 "last" "second")) => '(1 2 "last")
    (--remove-last (> it 3) '(1 2 3 4 5 6 7 8 9 10)) => '(1 2 3 4 5 6 7 8 9)
    ;; The next two tests assert that the input list is not modified (#158).
    (let ((l (list 1 2))) (setcar (--remove-last (= it 2) l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (setcar (--remove-last (= it 0) l) 0) l) => '(1 2)
    (-remove-last #'identity '()) => '()
    (-remove-last #'identity '(1)) => '()
    (-remove-last #'identity '(nil)) => '(nil)
    (-remove-last #'identity '(1 2)) => '(1)
    (-remove-last #'identity '(1 nil)) => '(nil)
    (--remove-last t '()) => '()
    (--remove-last t '(1)) => '()
    (--remove-last t '(nil)) => '()
    (--remove-last t '(1 2)) => '(1)
    (--remove-last t '(1 2 nil)) => '(1 2)
    (--remove-last it '(1 nil)) => '(nil)
    (-remove-last #'null '()) => '()
    (-remove-last #'null '(1)) => '(1)
    (-remove-last #'null '(nil)) => '()
    (-remove-last #'null '(1 2)) => '(1 2)
    (-remove-last #'null '(nil 1)) => '(1)
    (--remove-last nil '()) => '()
    (--remove-last nil '(1)) => '(1)
    (--remove-last nil '(nil)) => '(nil)
    (--remove-last nil '(1 2)) => '(1 2))

  (defexamples -remove-item
    (-remove-item 3 '(1 2 3 2 3 4 5 3)) => '(1 2 2 4 5)
    (-remove-item 'foo '(foo bar baz foo)) => '(bar baz)
    (-remove-item "bob" '("alice" "bob" "eve" "bob")) => '("alice" "eve")
    (-remove-item nil '()) => '()
    (-remove-item nil '(nil)) => '()
    (let ((l (list 1 2))) (setcar (-remove-item 0 l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (setcar (-remove-item 1 l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (setcar (-remove-item 2 l) 0) l) => '(1 2))

  (defexamples -non-nil
    (-non-nil '(nil 1 nil 2 nil nil 3 4 nil 5 nil)) => '(1 2 3 4 5)
    (-non-nil '((()))) => '((()))
    (-non-nil '()) => '()
    (let ((l (list 1 2))) (setcar (-non-nil l) 0) l) => '(1 2)
    (let ((l (list nil 1))) (setcar (-non-nil l) 0) l) => '(nil 1)
    (let ((l (list 1 nil))) (setcar (-non-nil l) 0) l) => '(1 nil))

  (defexamples -slice
    (-slice '(1 2 3 4 5) 1) => '(2 3 4 5)
    (-slice '(1 2 3 4 5) 0 3) => '(1 2 3)
    (-slice '(1 2 3 4 5 6 7 8 9) 1 -1 2) => '(2 4 6 8)
    (-slice '(1 2 3 4 5) 0 10) => '(1 2 3 4 5) ;; "to > length" should not fill in nils!
    (-slice '(1 2 3 4 5) -3) => '(3 4 5)
    (-slice '(1 2 3 4 5) -3 -1) => '(3 4)
    (-slice '(1 2 3 4 5 6) 0 nil 1) => '(1 2 3 4 5 6)
    (-slice '(1 2 3 4 5 6) 0 nil 2) => '(1 3 5)
    (-slice '(1 2 3 4 5 6) 0 nil 3) => '(1 4)
    (-slice '(1 2 3 4 5 6) 0 nil 10) => '(1)
    (-slice '(1 2 3 4 5 6) 1 4 2) => '(2 4)
    (-slice '(1 2 3 4 5 6) 2 6 3) => '(3 6)
    (-slice '(1 2 3 4 5 6) 2 -1 2) => '(3 5)
    (-slice '(1 2 3 4 5 6) 0 -4 2) => '(1)
    (-slice '(1 2 3 4 5 6) -4 -1 2) => '(3 5)
    (-slice '(1 2 3 4 5 6) -4 5 2) => '(3 5)
    (-slice '(1 2 3 4 5 6) -3 5 1) => '(4 5)
    (-slice '(1 2 3 4 5 6) 1 2 10) => '(2))

  (defexamples -take
    (-take 3 '(1 2 3 4 5)) => '(1 2 3)
    (-take 17 '(1 2 3 4 5)) => '(1 2 3 4 5)
    (-take 0 '(1 2 3 4 5)) => '()
    (-take -1 ()) => ()
    (-take 0 ()) => ()
    (-take 1 ()) => ()
    (-take -1 '(1)) => ()
    (-take 0 '(1)) => ()
    (-take 1 '(1)) => '(1)
    (-take -1 '(1 . 2)) => ()
    (-take 0 '(1 . 2)) => ()
    (-take 1 '(1 . 2)) => '(1)
    (let ((l (list 1 2))) (eq (-take 3 l) l)) => nil)

  (defexamples -take-last
    (-take-last 3 '(1 2 3 4 5)) => '(3 4 5)
    (-take-last 17 '(1 2 3 4 5)) => '(1 2 3 4 5)
    (-take-last 1 '(1 2 3 4 5)) => '(5)
    (-take-last 0 '(1)) => ()
    (-take-last 0 ()) => ()
    (-take-last -1 ()) => ()
    (-take-last -1 '(1)) => ()
    (let ((l (list 1 2))) (setcar (-take-last 1 l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (eq (-take-last 3 l) l)) => nil)

  (defexamples -drop
    (-drop 3 '(1 2 3 4 5)) => '(4 5)
    (-drop 17 '(1 2 3 4 5)) => '()
    (-drop 0 '(1 2 3 4 5)) => '(1 2 3 4 5)
    (-drop -1 ()) => ()
    (-drop 0 ()) => ()
    (-drop 1 ()) => ()
    (-drop -1 '(1)) => '(1)
    (-drop 0 '(1)) => '(1)
    (-drop 1 '(1)) => ()
    (-drop -1 '(1 . 2)) => '(1 . 2)
    (-drop 0 '(1 . 2)) => '(1 . 2)
    (-drop 1 '(1 . 2)) => 2
    (let ((l (list 1 2))) (setcar (-drop 1 l) 0) l) => '(1 0)
    (let ((l (list 1 2))) (eq (-drop 0 l) l)) => t)

  (defexamples -drop-last
    (-drop-last 3 '(1 2 3 4 5)) => '(1 2)
    (-drop-last 17 '(1 2 3 4 5)) => '()
    (-drop-last 0 '(1 2 3 4 5)) => '(1 2 3 4 5)
    (-drop-last 0 ()) => ()
    (-drop-last -1 ()) => ()
    (-drop-last -1 '(1)) => '(1)
    (-drop-last 1 ()) => ()
    (let ((l (list 1 2))) (setcar (-drop-last 1 l) 0) l) => '(1 2)
    (let ((l (list 1 2))) (eq (-drop-last 0 l) l)) => nil)

  (defexamples -take-while
    (-take-while #'even? '(1 2 3 4)) => '()
    (-take-while #'even? '(2 4 5 6)) => '(2 4)
    (--take-while (< it 4) '(1 2 3 4 3 2 1)) => '(1 2 3)
    (--take-while t ()) => ()
    (--take-while nil ()) => ()
    (--take-while nil '(1)) => ()
    (--take-while nil '(1 . 2)) => ()
    (--take-while t '(1)) => '(1)
    (--take-while t '(1 2)) => '(1 2)
    (--take-while (< it-index 0) '(1 . 2)) => ()
    (--take-while (< it-index 1) '(1 . 2)) => '(1)
    (let ((l (list 1 2))) (eq (--take-while t l) l)) => nil)

  (defexamples -drop-while
    (-drop-while #'even? '(1 2 3 4)) => '(1 2 3 4)
    (-drop-while #'even? '(2 4 5 6)) => '(5 6)
    (--drop-while (< it 4) '(1 2 3 4 3 2 1)) => '(4 3 2 1)
    (--drop-while t ()) => ()
    (--drop-while nil ()) => ()
    (--drop-while nil '(1)) => '(1)
    (--drop-while nil '(1 2)) => '(1 2)
    (--drop-while nil '(1 . 2)) => '(1 . 2)
    (--drop-while t '(1)) => ()
    (--drop-while t '(1 2)) => ()
    (--drop-while (< it-index 0) '(1 . 2)) => '(1 . 2)
    (--drop-while (< it-index 1) '(1 . 2)) => 2
    (let ((l (list t 2))) (setcar (-drop-while #'booleanp l) 0) l) => '(t 0)
    (let ((l (list 1 2))) (eq (--drop-while nil l) l)) => t)

  (defexamples -select-by-indices
    (-select-by-indices '(4 10 2 3 6) '("v" "e" "l" "o" "c" "i" "r" "a" "p" "t" "o" "r")) => '("c" "o" "l" "o" "r")
    (-select-by-indices '(2 1 0) '("a" "b" "c")) => '("c" "b" "a")
    (-select-by-indices '(0 1 2 0 1 3 3 1) '("f" "a" "r" "l")) => '("f" "a" "r" "f" "a" "l" "l" "a"))

  (defexamples -select-columns
    (-select-columns '(0 2) '((1 2 3) (a b c) (:a :b :c))) => '((1 3) (a c) (:a :c))
    (-select-columns '(1) '((1 2 3) (a b c) (:a :b :c))) => '((2) (b) (:b))
    (-select-columns nil '((1 2 3) (a b c) (:a :b :c))) => '(nil nil nil))

  (defexamples -select-column
    (-select-column 1 '((1 2 3) (a b c) (:a :b :c))) => '(2 b :b)))

(def-example-group "List to list"
  "Functions returning a modified copy of the input list."

  (defexamples -keep
    (-keep #'cdr '((1 2 3) (4 5) (6))) => '((2 3) (5))
    (-keep (lambda (n) (and (> n 3) (* 10 n))) '(1 2 3 4 5 6)) => '(40 50 60)
    (--keep (and (> it 3) (* 10 it)) '(1 2 3 4 5 6)) => '(40 50 60)
    (-keep #'null '(nil)) => '(t)
    (--keep it '(nil)) => '()
    (--keep t '(nil)) => '(t)
    (--keep t '()) => '()
    (-keep #'identity '()) => '())

  (defexamples -concat
    (-concat '(1)) => '(1)
    (-concat '(1) '(2)) => '(1 2)
    (-concat '(1) '(2 3) '(4)) => '(1 2 3 4)
    (-concat) => nil)

  (defexamples -flatten
    (-flatten '((1))) => '(1)
    (-flatten '((1 (2 3) (((4 (5))))))) => '(1 2 3 4 5)
    (-flatten '(1 2 (3 . 4))) => '(1 2 (3 . 4))
    (-flatten '(nil nil nil)) => nil
    (-flatten '(nil (1) nil)) => '(1)
    (-flatten '(nil (nil) nil)) => nil)

  (defexamples -flatten-n
    (-flatten-n 1 '((1 2) ((3 4) ((5 6))))) => '(1 2 (3 4) ((5 6)))
    (-flatten-n 2 '((1 2) ((3 4) ((5 6))))) => '(1 2 3 4 (5 6))
    (-flatten-n 3 '((1 2) ((3 4) ((5 6))))) => '(1 2 3 4 5 6)
    (-flatten-n 0 '(3 4)) => '(3 4)
    (-flatten-n 0 '((1 2) (3 4))) => '((1 2) (3 4))
    (-flatten-n 0 '(((1 2) (3 4)))) => '(((1 2) (3 4)))
    (-flatten-n 1 '(((1 . 2)) ((3 . 4)))) => '((1 . 2) (3 . 4))
    ;; Test for destructive modification.
    (let ((l (list 1 (list 2) 3))) (ignore (-flatten-n 0 l)) l) => '(1 (2) 3)
    (let ((l (list 1 (list 2) 3))) (ignore (-flatten-n 1 l)) l) => '(1 (2) 3)
    (let ((l (list 1 (list 2) 3))) (ignore (-flatten-n 2 l)) l) => '(1 (2) 3))

  (defexamples -replace
    (-replace 1 "1" '(1 2 3 4 3 2 1)) => '("1" 2 3 4 3 2 "1")
    (-replace "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) => '("a" "nice" "bar" "sentence" "about" "bar")
    (-replace 1 2 nil) => nil)

  (defexamples -replace-first
    (-replace-first 1 "1" '(1 2 3 4 3 2 1)) => '("1" 2 3 4 3 2 1)
    (-replace-first "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) => '("a" "nice" "bar" "sentence" "about" "foo")
    (-replace-first 1 2 nil) => nil)

  (defexamples -replace-last
    (-replace-last 1 "1" '(1 2 3 4 3 2 1)) => '(1 2 3 4 3 2 "1")
    (-replace-last "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) => '("a" "nice" "foo" "sentence" "about" "bar")
    (-replace-last 1 2 nil) => nil)

  (defexamples -insert-at
    (-insert-at 1 'x '(a b c)) => '(a x b c)
    (-insert-at 12 'x '(a b c)) => '(a b c x))

  (defexamples -replace-at
    (-replace-at 0 9 '(0 1 2 3 4 5)) => '(9 1 2 3 4 5)
    (-replace-at 1 9 '(0 1 2 3 4 5)) => '(0 9 2 3 4 5)
    (-replace-at 4 9 '(0 1 2 3 4 5)) => '(0 1 2 3 9 5)
    (-replace-at 5 9 '(0 1 2 3 4 5)) => '(0 1 2 3 4 9))

  (defexamples -update-at
    (-update-at 0 (lambda (x) (+ x 9)) '(0 1 2 3 4 5)) => '(9 1 2 3 4 5)
    (-update-at 1 (lambda (x) (+ x 8)) '(0 1 2 3 4 5)) => '(0 9 2 3 4 5)
    (--update-at 2 (length it) '("foo" "bar" "baz" "quux")) => '("foo" "bar" 3 "quux")
    (--update-at 2 (concat it "zab") '("foo" "bar" "baz" "quux")) => '("foo" "bar" "bazzab" "quux"))

  (defexamples -remove-at
    (-remove-at 0 '("0" "1" "2" "3" "4" "5")) => '("1" "2" "3" "4" "5")
    (-remove-at 1 '("0" "1" "2" "3" "4" "5")) => '("0" "2" "3" "4" "5")
    (-remove-at 2 '("0" "1" "2" "3" "4" "5")) => '("0" "1" "3" "4" "5")
    (-remove-at 3 '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "4" "5")
    (-remove-at 4 '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "3" "5")
    (-remove-at 5 '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "3" "4")
    (-remove-at 5 '((a b) (c d) (e f g) h i ((j) k) l (m))) => '((a b) (c d) (e f g) h i l (m))
    (-remove-at 0 '(((a b) (c d) (e f g) h i ((j) k) l (m)))) => nil)

  (defexamples -remove-at-indices
    (-remove-at-indices '(0) '("0" "1" "2" "3" "4" "5")) => '("1" "2" "3" "4" "5")
    (-remove-at-indices '(0 2 4) '("0" "1" "2" "3" "4" "5")) => '("1" "3" "5")
    (-remove-at-indices '(0 5) '("0" "1" "2" "3" "4" "5")) => '("1" "2" "3" "4")
    (-remove-at-indices '(1 2 3) '("0" "1" "2" "3" "4" "5")) => '("0" "4" "5")
    (-remove-at-indices '(0 1 2 3 4 5) '("0" "1" "2" "3" "4" "5")) => nil
    (-remove-at-indices '(2 0 4) '("0" "1" "2" "3" "4" "5")) => '("1" "3" "5")
    (-remove-at-indices '(5 0) '("0" "1" "2" "3" "4" "5")) => '("1" "2" "3" "4")
    (-remove-at-indices '(1 3 2) '("0" "1" "2" "3" "4" "5")) => '("0" "4" "5")
    (-remove-at-indices '(0 3 4 2 5 1) '("0" "1" "2" "3" "4" "5")) => nil
    (-remove-at-indices '(1) '("0" "1" "2" "3" "4" "5")) => '("0" "2" "3" "4" "5")
    (-remove-at-indices '(2) '("0" "1" "2" "3" "4" "5")) => '("0" "1" "3" "4" "5")
    (-remove-at-indices '(3) '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "4" "5")
    (-remove-at-indices '(4) '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "3" "5")
    (-remove-at-indices '(5) '("0" "1" "2" "3" "4" "5")) => '("0" "1" "2" "3" "4")
    (-remove-at-indices '(1 2 4) '((a b) (c d) (e f g) h i ((j) k) l (m))) => '((a b) h ((j) k) l (m))
    (-remove-at-indices '(5) '((a b) (c d) (e f g) h i ((j) k) l (m))) => '((a b) (c d) (e f g) h i l (m))
    (-remove-at-indices '(0) '(((a b) (c d) (e f g) h i ((j) k) l (m)))) => nil
    (-remove-at-indices '(2 3) '((0) (1) (2) (3) (4) (5) (6))) => '((0) (1) (4) (5) (6))))

(def-example-group "Reductions"
  "Functions reducing lists to a single value (which may also be a list)."

  (defexamples -reduce-from
    (-reduce-from #'- 10 '(1 2 3)) => 4
    (-reduce-from #'list 10 '(1 2 3)) => '(((10 1) 2) 3)
    (--reduce-from (concat acc " " it) "START" '("a" "b" "c")) => "START a b c"
    (--reduce-from (- acc it) 10 '(1 2 3)) => 4
    (--reduce-from (- acc it) 10 '(1)) => 9
    (--reduce-from (- acc it) 10 ()) => 10
    (-reduce-from #'- 7 '(1)) => 6
    (-reduce-from #'- 7 ()) => 7
    (--reduce-from (list acc it-index) nil '(1 2 3)) => '(((nil 0) 1) 2)
    (--reduce-from t nil '(1 2 3)) => t)

  (defexamples -reduce-r-from
    (-reduce-r-from #'- 10 '(1 2 3)) => -8
    (-reduce-r-from #'list 10 '(1 2 3)) => '(1 (2 (3 10)))
    (--reduce-r-from (concat it " " acc) "END" '("a" "b" "c")) => "a b c END"
    (--reduce-r-from (- it acc) 10 '(1 2 3)) => -8
    (--reduce-r-from (- it acc) 10 '(1)) => -9
    (--reduce-r-from (- it acc) 10 ()) => 10
    (-reduce-r-from #'- 7 '(1)) => -6
    (-reduce-r-from #'- 7 ()) => 7
    (--reduce-r-from (list acc it-index) nil '(1 2 3)) => '(((nil 2) 1) 0)
    (--reduce-r-from t nil '(1 2 3)) => t)

  (defexamples -reduce
    (-reduce #'- '(1 2 3 4)) => -8
    (-reduce #'list '(1 2 3 4)) => '(((1 2) 3) 4)
    (--reduce (format "%s-%d" acc it) '(1 2 3)) => "1-2-3"
    (-reduce #'- ()) => 0
    (-reduce #'- '(1)) => 1
    (--reduce (- acc it) '(1)) => 1
    (--reduce (list acc it) ()) => '(nil nil)
    (--reduce t '(1 2)) => t
    (-reduce #'vector ()) => []
    (-reduce #'vector '(1)) => 1
    (-reduce #'vector '(1 2)) => [1 2])

  (defexamples -reduce-r
    (-reduce-r #'- '(1 2 3 4)) => -2
    (-reduce-r #'list '(1 2 3 4)) => '(1 (2 (3 4)))
    (--reduce-r (format "%s-%d" acc it) '(1 2 3)) => "3-2-1"
    (-reduce-r #'+ ()) => 0
    (-reduce-r #'- '(1)) => 1
    (--reduce (- it acc) '(1)) => 1
    (--reduce-r (list it acc) ()) => '(nil nil)
    (--reduce-r t '(1 2)) => t
    (-reduce-r #'vector ()) => []
    (-reduce-r #'vector '(1)) => 1
    (-reduce-r #'vector '(1 2)) => [1 2])

  (defexamples -reductions-from
    (-reductions-from #'max 0 '(2 1 4 3)) => '(0 2 2 4 4)
    (-reductions-from #'* 1 '(1 2 3 4)) => '(1 1 2 6 24)
    (--reductions-from (format "(FN %s %d)" acc it) "INIT" '(1 2 3))
    => '("INIT" "(FN INIT 1)" "(FN (FN INIT 1) 2)" "(FN (FN (FN INIT 1) 2) 3)")
    (-reductions-from #'- 10 '(1)) => '(10 9)
    (-reductions-from #'- 10 ()) => '(10)
    (--reductions-from (- acc it) 10 '(1)) => '(10 9)
    (--reductions-from (- acc it) 10 ()) => '(10)
    (--reductions-from t 10 '(1 2 3)) => '(10 t t t)
    (--reductions-from (list acc it-index) nil '(1 2 3))
    => '(nil (nil 0) ((nil 0) 1) (((nil 0) 1) 2)))

  (defexamples -reductions-r-from
    (-reductions-r-from #'max 0 '(2 1 4 3)) => '(4 4 4 3 0)
    (-reductions-r-from #'* 1 '(1 2 3 4)) => '(24 24 12 4 1)
    (--reductions-r-from (format "(FN %d %s)" it acc) "INIT" '(1 2 3))
    => '("(FN 1 (FN 2 (FN 3 INIT)))" "(FN 2 (FN 3 INIT))" "(FN 3 INIT)" "INIT")
    (-reductions-r-from #'- 10 '(1)) => '(-9 10)
    (-reductions-r-from #'- 10 ()) => '(10)
    (--reductions-r-from (- acc it) 10 '(1)) => '(9 10)
    (--reductions-r-from (- acc it) 10 ()) => '(10)
    (--reductions-r-from t 10 '(1 2 3)) => '(t t t 10)
    (--reductions-r-from (list acc it-index) nil '(1 2 3))
    => '((((nil 2) 1) 0) ((nil 2) 1) (nil 2) nil))

  (defexamples -reductions
    (-reductions #'+ '(1 2 3 4)) => '(1 3 6 10)
    (-reductions #'* '(1 2 3 4)) => '(1 2 6 24)
    (--reductions (format "(FN %s %d)" acc it) '(1 2 3))
    => '(1 "(FN 1 2)" "(FN (FN 1 2) 3)")
    (-reductions #'- '(1)) => '(1)
    (-reductions #'- ()) => '(0)
    (-reductions #'vector ()) => '([])
    (-reductions #'vector '(1)) => '(1)
    (-reductions #'vector '(1 2)) => '(1 [1 2])
    (--reductions t '(1 2 3)) => '(1 t t)
    (--reductions (list it acc) ()) => '((nil nil))
    (--reductions (list it acc) '(1)) => '(1))

  (defexamples -reductions-r
    (-reductions-r #'+ '(1 2 3 4)) => '(10 9 7 4)
    (-reductions-r #'* '(1 2 3 4)) => '(24 24 12 4)
    (--reductions-r (format "(FN %d %s)" it acc) '(1 2 3))
    => '("(FN 1 (FN 2 3))" "(FN 2 3)" 3)
    (-reductions-r #'- '(1)) => '(1)
    (-reductions-r #'- ()) => '(0)
    (-reductions-r #'vector ()) => '([])
    (-reductions-r #'vector '(1)) => '(1)
    (-reductions-r #'vector '(1 2)) => '([1 2] 2)
    (--reductions-r t '(1 2 3)) => '(t t 3)
    (--reductions-r (list it acc) ()) => '((nil nil))
    (--reductions-r (list it acc) '(1)) => '(1))

  (defexamples -count
    (-count 'even? '(1 2 3 4 5)) => 2
    (--count (< it 4) '(1 2 3 4)) => 3)

  (defexamples -sum
    (-sum '()) => 0
    (-sum '(1)) => 1
    (-sum '(1 2 3 4)) => 10)

  (defexamples -running-sum
    (-running-sum '(1 2 3 4)) => '(1 3 6 10)
    (-running-sum '(1)) => '(1)
    (-running-sum '()) !!> (wrong-type-argument consp ()))

  (defexamples -product
    (-product '()) => 1
    (-product '(1)) => 1
    (-product '(1 2 3 4)) => 24)

  (defexamples -running-product
    (-running-product '(1 2 3 4)) => '(1 2 6 24)
    (-running-product '(1)) => '(1)
    (-running-product '()) !!> (wrong-type-argument consp ()))

  (defexamples -inits
    (-inits '(1 2 3 4)) => '(nil (1) (1 2) (1 2 3) (1 2 3 4))
    (-inits nil) => '(nil)
    (-inits '(1)) => '(nil (1)))

  (defexamples -tails
    (-tails '(1 2 3 4)) => '((1 2 3 4) (2 3 4) (3 4) (4) nil)
    (-tails nil) => '(nil)
    (-tails '(1)) => '((1) nil))

  (defexamples -common-prefix
    (-common-prefix '(1)) => '(1)
    (-common-prefix '(1 2) '(3 4) '(1 2)) => '()
    (-common-prefix '(1 2) '(1 2 3) '(1 2 3 4)) => '(1 2)
    (-common-prefix () '(1 2) '(1 2)) => ()
    (-common-prefix '(1 2) '(1 2) ()) => ()
    (-common-prefix '(1) '(1)) => '(1)
    (-common-prefix '(())) => '(())
    (-common-prefix () ()) => ()
    (-common-prefix ()) => ()
    (-common-prefix) => ())

  (defexamples -common-suffix
    (-common-suffix '(1)) => '(1)
    (-common-suffix '(1 2) '(3 4) '(1 2)) => '()
    (-common-suffix '(1 2 3 4) '(2 3 4) '(3 4)) => '(3 4)
    (-common-suffix () '(1 2) '(1 2)) => ()
    (-common-suffix '(1 2) '(1 2) ()) => ()
    (-common-suffix '(1) '(1)) => '(1)
    (-common-suffix '(())) => '(())
    (-common-suffix () ()) => ()
    (-common-suffix ()) => ()
    (-common-suffix) => ())

  (defexamples -min
    (-min '(0)) => 0
    (-min '(3 2 1)) => 1
    (-min '(1 2 3)) => 1)

  (defexamples -min-by
    (-min-by '> '(4 3 6 1)) => 1
    (--min-by (> (car it) (car other)) '((1 2 3) (2) (3 2))) => '(1 2 3)
    (--min-by (> (length it) (length other)) '((1 2 3) (2) (3 2))) => '(2))

  (defexamples -max
    (-max '(0)) => 0
    (-max '(3 2 1)) => 3
    (-max '(1 2 3)) => 3)

  (defexamples -max-by
    (-max-by '> '(4 3 6 1)) => 6
    (--max-by (> (car it) (car other)) '((1 2 3) (2) (3 2))) => '(3 2)
    (--max-by (> (length it) (length other)) '((1 2 3) (2) (3 2))) => '(1 2 3))

  (defexamples -frequencies
    (-frequencies '()) => '()
    (-frequencies '(1 2 3 1 2 1)) => '((1 . 3) (2 . 2) (3 . 1))
    (let ((-compare-fn #'string=)) (-frequencies '(a "a"))) => '((a . 2))
    (let ((-compare-fn #'string=)) (-frequencies '("a" a))) => '(("a" . 2))
    (-frequencies '(1)) => '((1 . 1))
    (-frequencies '(1 1)) => '((1 . 2))
    (-frequencies '(2 1 1)) => '((2 . 1) (1 . 2))
    (let ((-compare-fn #'eq)
          (a (string ?a)))
      (-frequencies `(,a ,(string ?a) ,a)))
    => '(("a" . 2) ("a" . 1))
    (let ((-compare-fn #'eq)
          (a (string ?a)))
      (-frequencies `(,(string ?a) ,a ,a)))
    => '(("a" . 1) ("a" . 2))))

(def-example-group "Unfolding"
  "Operations dual to reductions, building lists from a seed
value rather than consuming a list to produce a single value."

  (defexamples -iterate
    (-iterate #'1+ 1 10) => '(1 2 3 4 5 6 7 8 9 10)
    (-iterate (lambda (x) (+ x x)) 2 5) => '(2 4 8 16 32)
    (--iterate (* it it) 2 5) => '(2 4 16 256 65536)
    (-iterate #'1+ 1 0) => ()
    (-iterate #'1+ 1 -1) => ()
    (-iterate #'ignore 1 1) => '(1)
    (-iterate #'ignore 1 3) => '(1 nil nil)
    (--iterate nil nil 0) => ()
    (--iterate nil nil 1) => '(nil)
    (--iterate nil nil 2) => '(nil nil)
    (--iterate (setq it -1) 1 3) => '(1 -1 -1)
    (let (l) (--iterate (push 1 l) (push 0 l) -1) l) => ()
    (let (l) (--iterate (push 1 l) (push 0 l) 0) l) => ()
    (let (l) (--iterate (push 1 l) (push 0 l) 1) l) => '(0)
    (let (l) (--iterate (push 1 l) (push 0 l) 2) l) => '(1 0))

  (defexamples -unfold
    (-unfold (lambda (x) (unless (= x 0) (cons x (1- x)))) 10) => '(10 9 8 7 6 5 4 3 2 1)
    (--unfold (when it (cons it (cdr it))) '(1 2 3 4)) => '((1 2 3 4) (2 3 4) (3 4) (4))
    (--unfold (when it (cons it (butlast it))) '(1 2 3 4)) => '((1 2 3 4) (1 2 3) (1 2) (1)))

  (defexamples -repeat
    (-repeat 3 :a) => '(:a :a :a)
    (-repeat 1 :a) => '(:a)
    (-repeat 0 :a) => '()
    (-repeat -1 :a) => ()
    (-repeat -1 ()) => ()
    (-repeat 0 ()) => ()
    (-repeat 1 ()) => '(())
    (-repeat 2 ()) => '(() ()))

  (defexamples -cycle
    (-take 5 (-cycle '(1 2 3))) => '(1 2 3 1 2)
    (-take 7 (-cycle '(1 "and" 3))) => '(1 "and" 3 1 "and" 3 1)
    (-zip (-cycle '(1 2 3)) '(1 2)) => '((1 . 1) (2 . 2))
    (-zip-with #'cons (-cycle '(1 2 3)) '(1 2)) => '((1 . 1) (2 . 2))
    (-map (-partial #'-take 5) (-split-at 5 (-cycle '(1 2 3)))) => '((1 2 3 1 2) (3 1 2 3 1))
    (let ((l (list 1))) (eq l (-cycle l))) => nil))

(def-example-group "Predicates"
  "Reductions of one or more lists to a boolean value."

  (defexamples -some
    (-some #'stringp '(1 "2" 3)) => t
    (--some (string-match-p "x" it) '("foo" "axe" "xor")) => 1
    (--some (= it-index 3) '(0 1 2)) => nil
    (-some (lambda (s) (string-match-p "x" s)) '("foo" "bar" "baz")) => nil
    (--some (member 'foo it) '((foo bar) (baz))) => '(foo bar)
    (--some (plist-get it :bar) '((:foo 1 :bar 2) (:baz 3))) => 2
    (-some #'null '(1 2 3)) => nil
    (-some #'null '(1)) => nil
    (-some #'null '()) => nil
    (--some (not it) '(1 2 3)) => nil
    (--some (not it) '(1)) => nil
    (--some (not it) '()) => nil
    (-some #'identity '(1 2 3)) => 1
    (-some #'identity '(1)) => 1
    (-some #'identity '()) => nil
    (--some it '(1 2 3)) => 1
    (--some it '(1)) => 1
    (--some it '()) => nil)

  (defexamples -every
    (-every #'numberp '(1 2 3)) => t
    (--every (string-match-p "x" it) '("axe" "xor")) => 0
    (--every (= it it-index) '(0 1 3)) => nil
    (-every #'ignore '()) => t
    (-every #'ignore '(0)) => nil
    (-every #'ignore '(0 1)) => nil
    (--every nil '()) => t
    (--every nil '(0)) => nil
    (--every nil '(0 1)) => nil
    (-every #'identity '()) => t
    (-every #'identity '(0)) => 0
    (-every #'identity '(0 1)) => 1
    (--every it '()) => t
    (--every it '(1)) => 1
    (--every it '(1 2)) => 2
    (--every it-index '()) => t
    (--every it-index '(1)) => 0
    (--every it-index '(1 2)) => 1
    (let ((r 'r)) (-every (lambda (x) (setq r x)) '()) r) => 'r
    (let ((r 'r)) (-every (lambda (x) (setq r x)) '(nil 1)) r) => nil
    (let (r) (-every (lambda (x) (setq r x)) '(0 1)) r) => 1
    (let (i) (--every (ignore (setq i it-index)) '()) i) => nil
    (let (i) (--every (ignore (setq i it-index)) '(a)) i) => 0
    (let (i) (--every (ignore (setq i it-index)) '(a b)) i) => 0)

  (defexamples -any?
    (-any? #'numberp '(nil 0 t)) => t
    (-any? #'numberp '(nil t t)) => nil
    (-any? #'null '(1 3 5)) => nil
    (-any? #'null '(1 3 ())) => t
    (-any? #'identity '()) => nil
    (-any? #'identity '(0)) => t
    (-any? #'identity '(nil)) => nil
    (--any? (= 0 (% it 2)) '(1 2 3)) => t
    (--any? (= it it-index) '()) => nil
    (--any? (= it it-index) '(0)) => t
    (--any? (= it it-index) '(1)) => nil
    (--any? (= it it-index) '(1 1)) => t
    (--any? (= it it-index) '(1 2)) => nil)

  (defexamples -all?
    (-all? #'numberp '(1 2 3)) => t
    (-all? #'numberp '(2 t 6)) => nil
    (--all? (= 0 (% it 2)) '(2 4 6)) => t
    (-all? #'identity '()) => t
    (-all? #'identity '(0)) => t
    (-all? #'identity '(0 1)) => t
    (-all? #'identity '(nil)) => nil
    (--all? (= it it-index) '()) => t
    (--all? (= it it-index) '(0)) => t
    (--all? (= it it-index) '(1)) => nil
    (--all? (= it it-index) '(1 1)) => nil
    (--all? (= it it-index) '(0 1)) => t)

  (defexamples -none?
    (-none? 'even? '(1 2 3)) => nil
    (-none? 'even? '(1 3 5)) => t
    (--none? (= 0 (% it 2)) '(1 2 3)) => nil)

  (defexamples -only-some?
    (-only-some? 'even? '(1 2 3)) => t
    (-only-some? 'even? '(1 3 5)) => nil
    (-only-some? 'even? '(2 4 6)) => nil
    (--only-some? (> it 2) '(1 2 3)) => t)

  (defexamples -contains?
    (-contains? '(1 2 3) 1) => '(1 2 3)
    (-contains? '(1 2 3) 2) => '(2 3)
    (-contains? '(1 2 3) 4) => '()
    (-contains? '() 1) => '()
    (-contains? '() '()) => '()
    (-contains? `(,(string ?a)) "a") => '("a")
    (-contains? '(a a) 'a) => '(a a)
    (-contains? '(b b a a) 'a) => '(a a)
    (-contains? '(a a b b) 'a) => '(a a b b)
    (let ((-compare-fn #'eq)) (-contains? `(,(string ?a)) "a")) => '()
    (let ((-compare-fn #'string=)) (-contains? '(a) 'b)) => '()
    (let ((-compare-fn #'string=)) (-contains? '(a) "a")) => '(a)
    (let ((-compare-fn #'string=)) (-contains? '("a") 'a)) => '("a")
    (let ((-compare-fn #'string=)) (-contains? '(a "a") 'a)) => '(a "a")
    (let ((-compare-fn #'string=)) (-contains? '("a" a) 'a)) => '("a" a))

  (defexamples -is-prefix?
    (-is-prefix? '(1 2 3) '(1 2 3 4 5)) => t
    (-is-prefix? '(1 2 3 4 5) '(1 2 3)) => nil
    (-is-prefix? '(1 3) '(1 2 3 4 5)) => nil
    (-is-prefix? '(1 2 3) '(1 2 4 5)) => nil
    (-is-prefix? '(1 2 3) '(1 2)) => nil
    (-is-prefix? '(1 2) '(1 2)) => t
    (-is-prefix? '(1) '(1 2)) => t
    (-is-prefix? '(1) '(1)) => t
    (-is-prefix? '() '(1)) => t
    (-is-prefix? '() '()) => t
    (-is-prefix? '() '(nil)) => t
    (-is-prefix? '(nil) '(nil)) => t
    (-is-prefix? '(nil) '()) => nil
    (-is-prefix? '(2 3) '(1 2 3)) => nil
    (let* ((p (list 1 2)) (l p) (c (copy-sequence p)))
      (and (-is-prefix? p l) (equal p c) (equal l c)))
    => t)

  (defexamples -is-suffix?
    (-is-suffix? '(3 4 5) '(1 2 3 4 5)) => t
    (-is-suffix? '(1 2 3 4 5) '(3 4 5)) => nil
    (-is-suffix? '(3 5) '(1 2 3 4 5)) => nil
    (-is-suffix? '(3 4 5) '(1 2 3 5)) => nil
    (-is-suffix? '(1 2 3) '(2 3)) => nil
    (-is-suffix? '(1 2) '(1 2)) => t
    (-is-suffix? '(2) '(1 2)) => t
    (-is-suffix? '(1) '(1)) => t
    (-is-suffix? '() '(1)) => t
    (-is-suffix? '() '()) => t
    (-is-suffix? '() '(nil)) => t
    (-is-suffix? '(nil) '(nil)) => t
    (-is-suffix? '(nil) '()) => nil
    (-is-suffix? '(1 2) '(1 2 3)) => nil
    (-is-suffix? '(1 2) '(1 2 1 2)) => t
    (-is-suffix? '(1 2) '(1 3 1 2)) => t
    (let* ((s (list 1 2)) (l s) (c (copy-sequence s)))
      (and (-is-suffix? s l) (equal s c) (equal l c)))
    => t)

  (defexamples -is-infix?
    (-is-infix? '(1 2 3) '(1 2 3 4 5)) => t
    (-is-infix? '(2 3 4) '(1 2 3 4 5)) => t
    (-is-infix? '(3 4 5) '(1 2 3 4 5)) => t
    (-is-infix? '(2 3 4) '(1 2 4 5)) => nil
    (-is-infix? '(2 4) '(1 2 3 4 5)) => nil)

  (defexamples -cons-pair?
    (-cons-pair? '(1 . 2)) => t
    (-cons-pair? '(1 2)) => nil
    (-cons-pair? '(1)) => nil
    (-cons-pair? ()) => nil
    (-cons-pair? "") => nil
    (-cons-pair? '(1 2 . 3)) => nil
    (-cons-pair? '(() . "")) => t))

(def-example-group "Partitioning"
  "Functions partitioning the input list into a list of lists."

  (defexamples -split-at
    (-split-at 3 '(1 2 3 4 5)) => '((1 2 3) (4 5))
    (-split-at 17 '(1 2 3 4 5)) => '((1 2 3 4 5) ())
    (-split-at 0 '(1 2 3 4 5)) => '(() (1 2 3 4 5))
    (-split-at -1 ()) => '(() ())
    (-split-at 0 ()) => '(() ())
    (-split-at 1 ()) => '(() ())
    (-split-at -1 '(1)) => '(() (1))
    (-split-at 0 '(1)) => '(() (1))
    (-split-at 1 '(1)) => '((1) ())
    (-split-at 2 '(1)) => '((1) ())
    (-split-at -1 '(1 2)) => '(() (1 2))
    (-split-at 1 '(1 2)) => '((1) (2))
    (-split-at 2 '(1 2)) => '((1 2) ())
    (-split-at 3 '(1 2)) => '((1 2) ())
    (let* ((l (list 1 2)) (s (-split-at 1 l))) (eq (car s) l)) => nil
    (let* ((l (list 1 2)) (s (-split-at 1 l))) (eq (cadr s) (cdr l))) => t)

  (defexamples -split-with
    (-split-with 'even? '(1 2 3 4)) => '(() (1 2 3 4))
    (-split-with 'even? '(2 4 5 6)) => '((2 4) (5 6))
    (--split-with (< it 4) '(1 2 3 4 3 2 1)) => '((1 2 3) (4 3 2 1)))

  (defexamples -split-on
    (-split-on '| '(Nil | Leaf a | Node [Tree a])) => '((Nil) (Leaf a) (Node [Tree a]))
    (-split-on :endgroup '("a" "b" :endgroup "c" :endgroup "d" "e")) => '(("a" "b") ("c") ("d" "e"))
    (-split-on :endgroup '("a" "b" :endgroup :endgroup "d" "e")) => '(("a" "b") ("d" "e"))
    (-split-on :endgroup '("a" "b" :endgroup "c" :endgroup)) => '(("a" "b") ("c"))
    (-split-on :endgroup '("a" "b" :endgroup :endgroup :endgroup "d" "e")) => '(("a" "b") ("d" "e"))
    (-split-on :endgroup '(:endgroup "c" :endgroup "d" "e")) => '(("c") ("d" "e"))
    (-split-on '| '(Nil | | Node [Tree a])) => '((Nil) (Node [Tree a])))

  (defexamples -split-when
    (-split-when 'even? '(1 2 3 4 5 6)) => '((1) (3) (5))
    (-split-when 'even? '(1 2 3 4 6 8 9)) => '((1) (3) (9))
    (--split-when (memq it '(&optional &rest)) '(a b &optional c d &rest args)) => '((a b) (c d) (args))
    (-split-when 'even? '(1 2 3 5 6)) => '((1) (3 5))
    (-split-when 'even? '(1 2 3 5)) => '((1) (3 5))
    (-split-when 'even? '(1 3 4 5 6)) => '((1 3) (5))
    (-split-when 'even? '(1 2 3 4 5 6 8 10)) => '((1) (3) (5))
    (-split-when 'even? '(1 2 3 5 7 6)) => '((1) (3 5 7)))

  (defexamples -separate
    (-separate (lambda (num) (= 0 (% num 2))) '(1 2 3 4 5 6 7)) => '((2 4 6) (1 3 5 7))
    (--separate (< it 5) '(3 7 5 9 3 2 1 4 6)) => '((3 3 2 1 4) (7 5 9 6))
    (-separate 'cdr '((1 2) (1) (1 2 3) (4))) => '(((1 2) (1 2 3)) ((1) (4))))

  (defexamples -partition
    (-partition 2 '(1 2 3 4 5 6)) => '((1 2) (3 4) (5 6))
    (-partition 2 '(1 2 3 4 5 6 7)) => '((1 2) (3 4) (5 6))
    (-partition 3 '(1 2 3 4 5 6 7)) => '((1 2 3) (4 5 6)))

  (defexamples -partition-all
    (-partition-all 2 '(1 2 3 4 5 6)) => '((1 2) (3 4) (5 6))
    (-partition-all 2 '(1 2 3 4 5 6 7)) => '((1 2) (3 4) (5 6) (7))
    (-partition-all 3 '(1 2 3 4 5 6 7)) => '((1 2 3) (4 5 6) (7)))

  (defexamples -partition-in-steps
    (-partition-in-steps 2 1 '(1 2 3 4)) => '((1 2) (2 3) (3 4))
    (-partition-in-steps 3 2 '(1 2 3 4)) => '((1 2 3))
    (-partition-in-steps 3 2 '(1 2 3 4 5)) => '((1 2 3) (3 4 5))
    (-partition-in-steps 2 1 '(1)) => '()
    (-partition-in-steps 2 0 '(1)) !!> wrong-type-argument
    (-partition-in-steps 2 -1 '(1)) !!> wrong-type-argument)

  (defexamples -partition-all-in-steps
    (-partition-all-in-steps 2 1 '(1 2 3 4)) => '((1 2) (2 3) (3 4) (4))
    (-partition-all-in-steps 3 2 '(1 2 3 4)) => '((1 2 3) (3 4))
    (-partition-all-in-steps 3 2 '(1 2 3 4 5)) => '((1 2 3) (3 4 5) (5))
    (-partition-all-in-steps 4 2 '(0 1 2 3 4 5 6)) => '((0 1 2 3)
                                                        (2 3 4 5)
                                                        (4 5 6)
                                                        (6))
    (-partition-all-in-steps 2 1 '(1)) => '((1))
    (-partition-all-in-steps 2 0 '(1)) !!> wrong-type-argument
    (-partition-all-in-steps 2 -1 '(1)) !!> wrong-type-argument)

  (defexamples -partition-by
    (-partition-by 'even? '()) => '()
    (-partition-by 'even? '(1 1 2 2 2 3 4 6 8)) => '((1 1) (2 2 2) (3) (4 6 8))
    (--partition-by (< it 3) '(1 2 3 4 3 2 1)) => '((1 2) (3 4 3) (2 1)))

  (defexamples -partition-by-header
    (--partition-by-header (= it 1) '(1 2 3 1 2 1 2 3 4)) => '((1 2 3) (1 2) (1 2 3 4))
    (--partition-by-header (> it 0) '(1 2 0 1 0 1 2 3 0)) => '((1 2 0) (1 0) (1 2 3 0))
    (-partition-by-header 'even? '(2 1 1 1 4 1 3 5 6 6 1)) => '((2 1 1 1) (4 1 3 5) (6 6 1)))

  (defexamples -partition-after-pred
    (-partition-after-pred #'booleanp '()) => '()
    (-partition-after-pred #'booleanp '(t t)) => '((t) (t))
    (-partition-after-pred #'booleanp '(0 0 t t 0 t)) => '((0 0 t) (t) (0 t))
    (-partition-after-pred #'booleanp '(t)) => '((t))
    (-partition-after-pred #'booleanp '(0 t)) => '((0 t))
    (--partition-after-pred (= (% it 2) 0) '()) => '()
    (--partition-after-pred (= (mod it 2) 1) '()) => '()
    (--partition-after-pred (= (% it 2) 0) '(0)) => '((0))
    (--partition-after-pred (= (mod it 2) 1) '(0)) => '((0))
    (--partition-after-pred (= (% it 2) 0) '(0 1)) => '((0) (1))
    (--partition-after-pred (= (mod it 2) 1) '(0 1)) => '((0 1))
    (--partition-after-pred (= (% it 2) 0) '(0 1 2)) => '((0) (1 2))
    (--partition-after-pred (= (mod it 2) 1) '(0 1 2)) => '((0 1) (2))
    (--partition-after-pred (= (% it 2) 0) '(0 1 2 3)) => '((0) (1 2) (3))
    (--partition-after-pred (= (mod it 2) 1) '(0 1 2 3)) => '((0 1) (2 3))
    (--partition-after-pred t '()) => ()
    (--partition-after-pred t '(0)) => '((0))
    (--partition-after-pred t '(0 1)) => '((0) (1))
    (--partition-after-pred t '(0 1 2)) => '((0) (1) (2))
    (--partition-after-pred nil '()) => '()
    (--partition-after-pred nil '(0)) => '((0))
    (--partition-after-pred nil '(0 1)) => '((0 1))
    (--partition-after-pred nil '(0 1 2)) => '((0 1 2)))

  (defexamples -partition-before-pred
    (-partition-before-pred #'booleanp '()) => '()
    (-partition-before-pred #'booleanp '(0 t)) => '((0) (t))
    (-partition-before-pred #'booleanp '(0 0 t 0 t t)) => '((0 0) (t 0) (t) (t))
    (-partition-before-pred #'booleanp '(t)) => '((t))
    (-partition-before-pred #'booleanp '(t t)) => '((t) (t))
    (-partition-before-pred #'booleanp '(0 t 0)) => '((0) (t 0)))

  (defexamples -partition-before-item
    (-partition-before-item 3 '()) => '()
    (-partition-before-item 3 '(1)) => '((1))
    (-partition-before-item 3 '(3)) => '((3))
    (-partition-before-item 3 '(1 3)) => '((1) (3))
    (-partition-before-item 3 '(1 3 4)) => '((1) (3 4))
    (-partition-before-item 3 '(1 2 3 2 3 3 4)) => '((1 2) (3 2) (3) (3 4)))

  (defexamples -partition-after-item
    (-partition-after-item 3 '()) => '()
    (-partition-after-item 3 '(1)) => '((1))
    (-partition-after-item 3 '(3)) => '((3))
    (-partition-after-item 3 '(3 1)) => '((3) (1))
    (-partition-after-item 3 '(3 1 3)) => '((3) (1 3))
    (-partition-after-item 3 '(3 2 3 3 4 5 3 2)) => '((3) (2 3) (3) (4 5 3) (2)))

  (defexamples -group-by
    (-group-by 'even? '()) => '()
    (-group-by 'even? '(1 1 2 2 2 3 4 6 8)) => '((nil . (1 1 3)) (t . (2 2 2 4 6 8)))
    (--group-by (car (split-string it "/")) '("a/b" "c/d" "a/e")) => '(("a" . ("a/b" "a/e")) ("c" . ("c/d")))))

(def-example-group "Indexing"
  "Functions retrieving or sorting based on list indices and
related predicates."

  (defexamples -elem-index
    (-elem-index 2 '(6 7 8 3 4)) => nil
    (-elem-index "bar" '("foo" "bar" "baz")) => 1
    (-elem-index '(1 2) '((3) (5 6) (1 2) nil)) => 2
    (-elem-index nil ()) => nil
    (-elem-index nil '(t)) => nil
    (-elem-index nil '(nil)) => 0
    (-elem-index nil '(nil t)) => 0
    (-elem-index nil '(t nil)) => 1
    (-elem-index t ()) => nil
    (-elem-index t '(nil)) => nil
    (-elem-index t '(t)) => 0
    (-elem-index t '(t nil)) => 0
    (-elem-index t '(nil t)) => 1)

  (defexamples -elem-indices
    (-elem-indices 2 '(6 7 8 3 4 1)) => '()
    (-elem-indices "bar" '("foo" "bar" "baz")) => '(1)
    (-elem-indices '(1 2) '((3) (1 2) (5 6) (1 2) nil)) => '(1 3)
    (-elem-indices nil ()) => ()
    (-elem-indices nil '(t)) => ()
    (-elem-indices nil '(nil)) => '(0)
    (-elem-indices nil '(nil t)) => '(0)
    (-elem-indices nil '(t nil)) => '(1)
    (-elem-indices nil '(t t)) => ()
    (-elem-indices nil '(nil nil)) => '(0 1)
    (-elem-indices t ()) => ()
    (-elem-indices t '(t)) => '(0)
    (-elem-indices t '(nil)) => ()
    (-elem-indices t '(nil t)) => '(1)
    (-elem-indices t '(t nil)) => '(0)
    (-elem-indices t '(t t)) => '(0 1)
    (-elem-indices t '(nil nil)) => ())

  (defexamples -find-index
    (-find-index #'numberp '(a b c)) => nil
    (-find-index #'natnump '(1 0 -1)) => 0
    (--find-index (> it 5) '(2 4 1 6 3 3 5 8)) => 3
    (-find-index (-cut string< "baz" <>) '("bar" "foo" "baz")) => 1
    (--find-index nil ()) => nil
    (--find-index nil '(5)) => nil
    (--find-index nil '(5 6 7)) => nil
    (--find-index t ()) => nil
    (--find-index t '(5)) => 0
    (--find-index t '(5 . 6)) => 0
    (--find-index t '(5 6 7)) => 0
    (let (x) (--find-index (setq x it) ()) x) => nil
    (let (x) (--find-index (setq x it) '(5)) x) => 5
    (let (x) (--find-index (setq x it) '(5 6 7)) x) => 5
    (let (x) (--find-index (ignore (setq x it)) ()) x) => nil
    (let (x) (--find-index (ignore (setq x it)) '(5)) x) => 5
    (let (x) (--find-index (ignore (setq x it)) '(5 6 7)) x) => 7)

  (defexamples -find-last-index
    (-find-last-index #'numberp '(a b c)) => nil
    (--find-last-index (> it 5) '(2 7 1 6 3 8 5 2)) => 5
    (-find-last-index (-partial #'string< 'a) '(c b a)) => 1
    (--find-last-index nil ()) => nil
    (--find-last-index nil '(t)) => nil
    (--find-last-index nil '(nil)) => nil
    (--find-last-index nil '(nil nil)) => nil
    (--find-last-index nil '(nil t)) => nil
    (--find-last-index nil '(t nil)) => nil
    (--find-last-index nil '(t t)) => nil
    (--find-last-index t ()) => nil
    (--find-last-index t '(t)) => 0
    (--find-last-index t '(nil)) => 0
    (--find-last-index t '(nil nil)) => 1
    (--find-last-index t '(nil t)) => 1
    (--find-last-index t '(t nil)) => 1
    (--find-last-index t '(t t)) => 1
    (--find-last-index it ()) => nil
    (--find-last-index it '(t)) => 0
    (--find-last-index it '(nil)) => nil
    (--find-last-index it '(nil nil)) => nil
    (--find-last-index it '(nil t)) => 1
    (--find-last-index it '(t nil)) => 0
    (--find-last-index it '(t t)) => 1)

  (defexamples -find-indices
    (-find-indices #'numberp '(a b c)) => '()
    (-find-indices #'numberp '(8 1 d 2 b c a 3)) => '(0 1 3 7)
    (--find-indices (> it 5) '(2 4 1 6 3 3 5 8)) => '(3 7)
    (--find-indices (string< "baz" it) '("bar" "foo" "baz")) => '(1)
    (--find-indices nil ()) => ()
    (--find-indices nil '(1)) => ()
    (--find-indices nil '(nil)) => ()
    (--find-indices t ()) => ()
    (--find-indices t '(1)) => '(0)
    (--find-indices t '(nil)) => '(0)
    (--find-indices t '(1 2)) => '(0 1)
    (--find-indices t '(nil nil)) => '(0 1)
    (--find-indices it ()) => ()
    (--find-indices it '(1)) => '(0)
    (--find-indices it '(nil)) => ()
    (--find-indices it '(1 2)) => '(0 1)
    (--find-indices it '(nil nil)) => ()
    (-find-indices #'ignore ()) => ()
    (-find-indices #'ignore '(1)) => ()
    (-find-indices #'ignore '(nil)) => ()
    (-find-indices (-andfn) ()) => ()
    (-find-indices (-andfn) '(1)) => '(0)
    (-find-indices (-andfn) '(nil)) => '(0)
    (-find-indices (-andfn) '(1 2)) => '(0 1)
    (-find-indices (-andfn) '(nil nil)) => '(0 1)
    (-find-indices #'identity ()) => ()
    (-find-indices #'identity '(1)) => '(0)
    (-find-indices #'identity '(nil)) => ()
    (-find-indices #'identity '(1 2)) => '(0 1)
    (-find-indices #'identity '(nil nil)) => ())

  (defexamples -grade-up
    (-grade-up #'< '(3 1 4 2 1 3 3)) => '(1 4 3 0 5 6 2)
    (let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-up #'< l) l)) => '(1 1 2 3 3 3 4))

  (defexamples -grade-down
    (-grade-down #'< '(3 1 4 2 1 3 3)) => '(2 0 5 6 3 1 4)
    (let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-down #'< l) l)) => '(4 3 3 3 2 1 1)))

(def-example-group "Set operations"
  "Operations pretending lists are sets."

  (defexamples -union
    (-union '(1 2 3) '(3 4 5))  => '(1 2 3 4 5)
    (-union '(1 2 2 4) '())  => '(1 2 4)
    (-union '(1 1 2 2) '(4 4 3 2 1))  => '(1 2 4 3)
    (-union '() '()) => '()
    (-union '() '(a)) => '(a)
    (-union '() '(a a)) => '(a)
    (-union '() '(a a b)) => '(a b)
    (-union '() '(a b a)) => '(a b)
    (-union '() '(b a a)) => '(b a)
    (-union '(a) '()) => '(a)
    (-union '(a a) '()) => '(a)
    (-union '(a a b) '()) => '(a b)
    (-union '(a b a) '()) => '(a b)
    (-union '(b a a) '()) => '(b a)
    (let ((dash--short-list-length 0)) (-union '() '(a))) => '(a)
    (let ((dash--short-list-length 0)) (-union '() '(a a))) => '(a)
    (let ((dash--short-list-length 0)) (-union '() '(a a b))) => '(a b)
    (let ((dash--short-list-length 0)) (-union '() '(a b a))) => '(a b)
    (let ((dash--short-list-length 0)) (-union '() '(b a a))) => '(b a)
    (let ((dash--short-list-length 0)) (-union '(a) '())) => '(a)
    (let ((dash--short-list-length 0)) (-union '(a a) '())) => '(a)
    (let ((dash--short-list-length 0)) (-union '(a a b) '())) => '(a b)
    (let ((dash--short-list-length 0)) (-union '(a b a) '())) => '(a b)
    (let ((dash--short-list-length 0)) (-union '(b a a) '())) => '(b a)
    (let ((dash--short-list-length 0)) (-union '(a a b c c) '(e e d c b)))
    => '(a b c e d)
    (let ((-compare-fn #'string=)) (-union '(a "b") '("a" b))) => '(a "b")
    (let ((-compare-fn #'string=)) (-union '("a" b) '(a "b"))) => '("a" b))

  (defexamples -difference
    (-difference '() '()) => '()
    (-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
    (-difference '(1 2 3 4) '(3 4 5 6)) => '(1 2)
    (-difference '() '(a)) => '()
    (-difference '(a) '()) => '(a)
    (-difference '(a) '(a)) => '()
    (-difference '(a a) '()) => '(a)
    (-difference '(a a) '(a)) => '()
    (-difference '(a a) '(a a)) => '()
    (-difference '(a a) '(b)) => '(a)
    (-difference '(a b c c d a) '(c c b)) => '(a d)
    (let ((dash--short-list-length 0)) (-difference '(a) '(a))) => '()
    (let ((dash--short-list-length 0)) (-difference '(a a) '(a))) => '()
    (let ((dash--short-list-length 0)) (-difference '(a a) '(a a))) => '()
    (let ((dash--short-list-length 0)) (-difference '(a a) '(b))) => '(a)
    (let ((dash--short-list-length 0)) (-difference '(a b c c d a) '(c c b)))
    => '(a d)
    (let ((-compare-fn #'string=)) (-difference '(a) '("a"))) => '()
    (let ((-compare-fn #'string=)) (-difference '("a") '(a))) => '()
    (let ((-compare-fn #'string=)) (-difference '(a "a") '(a))) => '()
    (let ((-compare-fn #'string=)) (-difference '(a "a") '(b))) => '(a)
    (let ((-compare-fn #'string=)) (-difference '("a") '(a a))) => '())

  (defexamples -intersection
    (-intersection '() '()) => '()
    (-intersection '(1 2 3) '(4 5 6)) => '()
    (-intersection '(1 2 2 3) '(4 3 3 2)) => '(2 3)
    (-intersection '() '(a)) => '()
    (-intersection '(a) '()) => '()
    (-intersection '(a) '(a)) => '(a)
    (-intersection '(a a b) '(b a)) => '(a b)
    (-intersection '(a b) '(b a a)) => '(a b)
    (let ((dash--short-list-length 0)) (-intersection '(a) '(b))) => '()
    (let ((dash--short-list-length 0)) (-intersection '(a) '(a))) => '(a)
    (let ((dash--short-list-length 0)) (-intersection '(a a b) '(b b a)))
    => '(a b)
    (let ((dash--short-list-length 0)) (-intersection '(a a b) '(b a)))
    => '(a b)
    (let ((dash--short-list-length 0)) (-intersection '(a b) '(b a a)))
    => '(a b)
    (let ((-compare-fn #'string=)) (-intersection '(a) '("a")) => '(a))
    (let ((-compare-fn #'string=)) (-intersection '("a") '(a)) => '("a")))

  (defexamples -powerset
    (-powerset '()) => '(())
    (-powerset '(x y)) => '((x y) (x) (y) ())
    (-powerset '(x y z)) => '((x y z) (x y) (x z) (x) (y z) (y) (z) ())
    (let ((p (-powerset '()))) (setcar p t) (-powerset '())) => '(()))

  (defexamples -permutations
    (-permutations '()) => '(())
    (-permutations '(a a b)) => '((a a b) (a b a) (b a a))
    (-permutations '(a b c))
    => '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
    (-permutations '(1)) => '((1))
    (-permutations '(a)) => '((a))
    (-permutations '(())) => '((()))
    (-permutations '(1 1)) => '((1 1))
    (-permutations '(1 2)) => '((1 2) (2 1))
    (-permutations '(2 1)) => '((2 1) (1 2))
    (-permutations '(1 a)) => '((1 a) (a 1))
    (-permutations '(a 1)) => '((a 1) (1 a))
    (-permutations '(a a)) => '((a a))
    (-permutations '(a b)) => '((a b) (b a))
    (-permutations '(b a)) => '((b a) (a b))
    (-permutations '(1 1 1)) => '((1 1 1))
    (-permutations '(1 1 2)) => '((1 1 2) (1 2 1) (2 1 1))
    (-permutations '(1 2 1)) => '((1 1 2) (1 2 1) (2 1 1))
    (-permutations '(2 1 1)) => '((2 1 1) (1 2 1) (1 1 2))
    (-permutations '(1 1 a)) => '((1 1 a) (1 a 1) (a 1 1))
    (-permutations '(1 a 1)) => '((1 1 a) (1 a 1) (a 1 1))
    (-permutations '(a 1 1)) => '((a 1 1) (1 a 1) (1 1 a))
    (-permutations '(a a 1)) => '((a a 1) (a 1 a) (1 a a))
    (-permutations '(a 1 a)) => '((a a 1) (a 1 a) (1 a a))
    (-permutations '(1 a a)) => '((1 a a) (a 1 a) (a a 1))
    (-permutations '(1 2 3))
    => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
    (-permutations '(3 2 1))
    => '((3 2 1) (3 1 2) (2 3 1) (2 1 3) (1 3 2) (1 2 3))
    (-permutations '(1 2 a))
    => '((1 2 a) (1 a 2) (2 1 a) (2 a 1) (a 1 2) (a 2 1))
    (-permutations '(1 a 2))
    => '((1 a 2) (1 2 a) (a 1 2) (a 2 1) (2 1 a) (2 a 1))
    (-permutations '(a 1 2))
    => '((a 1 2) (a 2 1) (1 a 2) (1 2 a) (2 a 1) (2 1 a))
    (-permutations '(a b 1))
    => '((a b 1) (a 1 b) (b a 1) (b 1 a) (1 a b) (1 b a))
    (-permutations '(a 1 b))
    => '((a 1 b) (a b 1) (1 a b) (1 b a) (b a 1) (b 1 a))
    (-permutations '(1 a b))
    => '((1 a b) (1 b a) (a 1 b) (a b 1) (b 1 a) (b a 1))
    (-permutations '(a a a)) => '((a a a))
    (-permutations '(a b a)) => '((a a b) (a b a) (b a a))
    (-permutations '(b a a)) => '((b a a) (a b a) (a a b))
    (-permutations '(c b a))
    => '((c b a) (c a b) (b c a) (b a c) (a c b) (a b c))
    (let ((-compare-fn #'string=)) (-permutations '(a "a"))) => '((a a))
    (let ((-compare-fn #'string=)) (-permutations '("a" a))) => '(("a" "a"))
    (let ((-compare-fn #'string=)) (-permutations '(a "a" b)))
    => '((a a b) (a b a) (b a a))
    (let ((-compare-fn #'string=)) (-permutations '(a b "a")))
    => '((a a b) (a b a) (b a a))
    (let ((-compare-fn #'string=)) (-permutations '(b a "a")))
    => '((b a a) (a b a) (a a b))
    (let ((-compare-fn #'string=)) (-permutations '("a" a b)))
    => '(("a" "a" b) ("a" b "a") (b "a" "a"))
    (let ((-compare-fn #'string=)) (-permutations '("a" b a)))
    => '(("a" "a" b) ("a" b "a") (b "a" "a"))
    (let ((-compare-fn #'string=)) (-permutations '(b "a" a)))
    => '((b "a" "a") ("a" b "a") ("a" "a" b)))

  (defexamples -distinct
    (-distinct '()) => '()
    (-distinct '(1 1 2 3 3)) => '(1 2 3)
    (-distinct '(t t t)) => '(t)
    (-distinct '(nil nil nil)) => '(nil)
    (-uniq '((1) (2) (1) (1))) => '((1) (2))
    (let ((-compare-fn #'eq)) (-uniq '((1) (2) (1) (1)))) => '((1) (2) (1) (1))
    (let ((-compare-fn #'eq)) (-uniq '(:a :b :a :a))) => '(:a :b)
    (let ((-compare-fn #'eql)) (-uniq '(2.1 3.1 2.1 2.1))) => '(2.1 3.1)
    (let ((-compare-fn #'string=))
      (-uniq '(dash "dash" "ash" "cash" "bash")))
    => '(dash "ash" "cash" "bash")
    (let ((-compare-fn #'string=)) (-uniq '(a))) => '(a)
    (let ((-compare-fn #'string=)) (-uniq '(a a))) => '(a)
    (let ((-compare-fn #'string=)) (-uniq '(a b))) => '(a b)
    (let ((-compare-fn #'string=)) (-uniq '(b a))) => '(b a)
    (let ((-compare-fn #'string=)) (-uniq '(a "a"))) => '(a)
    (let ((-compare-fn #'string=)) (-uniq '("a" a))) => '("a")
    (let ((dash--short-list-length 0)) (-uniq '(a))) => '(a)
    (let ((dash--short-list-length 0)) (-uniq '(a b))) => '(a b)
    (let ((dash--short-list-length 0)) (-uniq '(b a))) => '(b a)
    (let ((dash--short-list-length 0)) (-uniq '(a a))) => '(a)
    (let ((dash--short-list-length 0)) (-uniq '(a a b))) => '(a b)
    (let ((dash--short-list-length 0)) (-uniq '(a b a))) => '(a b)
    (let ((dash--short-list-length 0)) (-uniq '(b a a))) => '(b a)
    (let ((dash--short-list-length 0)
          (-compare-fn #'eq))
      (-uniq (list (string ?a) (string ?a))))
    => '("a" "a")
    (let ((dash--short-list-length 0)
          (-compare-fn #'eq)
          (a (string ?a)))
      (-uniq (list a a)))
    => '("a"))

  (defexamples -same-items?
    (-same-items? '(1 2 3) '(1 2 3)) => t
    (-same-items? '(1 1 2 3) '(3 3 2 1)) => t
    (-same-items? '(1 2 3) '(1 2 3 4)) => nil
    (-same-items? '((a . 1) (b . 2)) '((a . 1) (b . 2))) => t
    (-same-items? '() '()) => t
    (-same-items? '() '(a)) => nil
    (-same-items? '(a) '()) => nil
    (-same-items? '(a) '(a)) => t
    (-same-items? '(a) '(b)) => nil
    (-same-items? '(a) '(a a)) => t
    (-same-items? '(b) '(a a)) => nil
    (-same-items? '(a) '(a b)) => nil
    (-same-items? '(a a) '(a)) => t
    (-same-items? '(a a) '(b)) => nil
    (-same-items? '(a a) '(a b)) => nil
    (-same-items? '(a b) '(a)) => nil
    (-same-items? '(a b) '(a a)) => nil
    (-same-items? '(a a) '(a a)) => t
    (-same-items? '(a a b) '(b b a a)) => t
    (-same-items? '(b b a a) '(a a b)) => t
    (let ((dash--short-list-length 0)) (-same-items? '(a) '(a))) => t
    (let ((dash--short-list-length 0)) (-same-items? '(a) '(b))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a) '(a a))) => t
    (let ((dash--short-list-length 0)) (-same-items? '(b) '(a a))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a) '(a b))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a a) '(a))) => t
    (let ((dash--short-list-length 0)) (-same-items? '(a a) '(b))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a a) '(a b))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a b) '(a))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a b) '(a a))) => nil
    (let ((dash--short-list-length 0)) (-same-items? '(a a) '(a a))) => t
    (let ((dash--short-list-length 0)) (-same-items? '(a a b) '(b b a a))) => t
    (let ((dash--short-list-length 0)) (-same-items? '(b b a a) '(a a b))) => t))

(def-example-group "Other list operations"
  "Other list functions not fit to be classified elsewhere."

  (defexamples -rotate
    (-rotate 3 '(1 2 3 4 5 6 7)) => '(5 6 7 1 2 3 4)
    (-rotate -3 '(1 2 3 4 5 6 7)) => '(4 5 6 7 1 2 3)
    (-rotate 16 '(1 2 3 4 5 6 7)) => '(6 7 1 2 3 4 5)
    (-rotate -16 '(1 2 3 4 5 6 7)) => '(3 4 5 6 7 1 2)
    (let* ((l (list 1 2)) (r (-rotate -1 l))) (setcdr l 0) r) => '(2 1)
    (let* ((l (list 1 2)) (r (-rotate 0 l))) (setcdr l 0) r) => '(1 2)
    (let* ((l (list 1 2)) (r (-rotate 1 l))) (setcdr l 0) r) => '(2 1)
    (-rotate -1 '()) => '()
    (-rotate 0 '()) => '()
    (-rotate 1 '()) => '()
    (-rotate -2 '(1)) => '(1)
    (-rotate -1 '(1)) => '(1)
    (-rotate 0 '(1)) => '(1)
    (-rotate 1 '(1)) => '(1)
    (-rotate 2 '(1)) => '(1)
    (-rotate -4 '(1 2)) => '(1 2)
    (-rotate -3 '(1 2)) => '(2 1)
    (-rotate -2 '(1 2)) => '(1 2)
    (-rotate -1 '(1 2)) => '(2 1)
    (-rotate 0 '(1 2)) => '(1 2)
    (-rotate 1 '(1 2)) => '(2 1)
    (-rotate 2 '(1 2)) => '(1 2)
    (-rotate 3 '(1 2)) => '(2 1)
    (-rotate 4 '(1 2)) => '(1 2)
    (-rotate -6 '(1 2 3)) => '(1 2 3)
    (-rotate -5 '(1 2 3)) => '(3 1 2)
    (-rotate -4 '(1 2 3)) => '(2 3 1)
    (-rotate -3 '(1 2 3)) => '(1 2 3)
    (-rotate -2 '(1 2 3)) => '(3 1 2)
    (-rotate -1 '(1 2 3)) => '(2 3 1)
    (-rotate 0 '(1 2 3)) => '(1 2 3)
    (-rotate 1 '(1 2 3)) => '(3 1 2)
    (-rotate 2 '(1 2 3)) => '(2 3 1)
    (-rotate 3 '(1 2 3)) => '(1 2 3)
    (-rotate 4 '(1 2 3)) => '(3 1 2)
    (-rotate 5 '(1 2 3)) => '(2 3 1)
    (-rotate 6 '(1 2 3)) => '(1 2 3))

  (defexamples -cons*
    (-cons* 1 2) => '(1 . 2)
    (-cons* 1 2 3) => '(1 2 . 3)
    (-cons* 1) => 1
    (-cons* 1 2 3 4) => '(1 2 3 . 4)
    (apply #'-cons* (number-sequence 1 10)) => '(1 2 3 4 5 6 7 8 9 . 10)
    (-cons*) => ()
    (-cons* ()) => ()
    (-cons* 1 ()) => '(1)
    (-cons* 1 '(2)) => '(1 2)
    ;; Assert that &rest conses a fresh list in case that ever changes.
    (let ((l (list 1 2))) (apply #'-cons* l) l) => '(1 2))

  (defexamples -snoc
    (-snoc '(1 2 3) 4) => '(1 2 3 4)
    (-snoc '(1 2 3) 4 5 6) => '(1 2 3 4 5 6)
    (-snoc '(1 2 3) '(4 5 6)) => '(1 2 3 (4 5 6)))

  (defexamples -interpose
    (-interpose "-" '()) => '()
    (-interpose "-" '("a")) => '("a")
    (-interpose "-" '("a" "b" "c")) => '("a" "-" "b" "-" "c"))

  (defexamples -interleave
    (-interleave '(1 2) '("a" "b")) => '(1 "a" 2 "b")
    (-interleave '(1 2) '("a" "b") '("A" "B")) => '(1 "a" "A" 2 "b" "B")
    (-interleave '(1 2 3) '("a" "b")) => '(1 "a" 2 "b")
    (-interleave '(1 2 3) '("a" "b" "c" "d")) => '(1 "a" 2 "b" 3 "c")
    (-interleave) => nil)

  (defexamples -iota
    (-iota 6) => '(0 1 2 3 4 5)
    (-iota 4 2.5 -2) => '(2.5 0.5 -1.5 -3.5)
    (-iota -1) !!> (wrong-type-argument natnump -1)
    (-iota 0) => ()
    (-iota 0 nil 0) => ()
    (-iota 1 nil 0) => '(0)
    (-iota 1) => '(0)
    (-iota 1 nil -1) => '(0))

  (defexamples -zip-with
    (-zip-with '+ '(1 2 3) '(4 5 6)) => '(5 7 9)
    (-zip-with 'cons '(1 2 3) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (--zip-with (concat it " and " other) '("Batman" "Jekyll") '("Robin" "Hyde")) => '("Batman and Robin" "Jekyll and Hyde"))

  (defexamples -zip
    (-zip '(1 2 3) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2 3) '(4 5 6 7)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2) '(3 4 5) '(6)) => '((1 3 6))
    (-zip '(1 2 3 4) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2 3) '(4 5 6) '(7 8 9)) => '((1 4 7) (2 5 8) (3 6 9))
    (-zip) => nil)

  (defexamples -zip-lists
    (-zip-lists '(1 2 3) '(4 5 6)) => '((1 4) (2 5) (3 6))
    (-zip-lists '(1 2 3) '(4 5 6 7)) => '((1 4) (2 5) (3 6))
    (-zip-lists '(1 2) '(3 4 5) '(6)) => '((1 3 6))
    (-zip-lists '(1 2 3 4) '(4 5 6)) => '((1 4) (2 5) (3 6))
    (-zip-lists '(1 2 3) '(4 5 6) '(7 8 9)) => '((1 4 7) (2 5 8) (3 6 9))
    (-zip-lists) => nil)

  (defexamples -zip-fill
    (-zip-fill 0 '(1 2 3 4 5) '(6 7 8 9)) => '((1 . 6) (2 . 7) (3 . 8) (4 . 9) (5 . 0)))

  (defexamples -unzip
    (-unzip (-zip '(1 2 3) '(a b c) '("e" "f" "g"))) => '((1 2 3) (a b c) ("e" "f" "g"))
    (-unzip '((1 2) (3 4) (5 6) (7 8) (9 10))) => '((1 3 5 7 9) (2 4 6 8 10))
    (-unzip '((1 2) (3 4))) => '((1 . 3) (2 . 4)))

  (defexamples -pad
    (-pad 0 '()) => '(())
    (-pad 0 '(1 2) '(3 4)) => '((1 2) (3 4))
    (-pad 0 '(1 2) '(3 4 5 6) '(7 8 9)) => '((1 2 0 0) (3 4 5 6) (7 8 9 0))
    (-pad 0) => ()
    (-pad 0 () ()) => '(() ())
    (-pad 0 '(1)) => '((1))
    (-pad 0 '(1) '(1)) => '((1) (1))
    (-pad 0 '(1 2 3) '(4 5)) => '((1 2 3) (4 5 0))
    (-pad nil ()) => '(())
    (-pad nil () ()) => '(() ())
    (-pad nil '(nil nil) '(nil) '(nil nil nil nil nil))
    => '((nil nil nil nil nil) (nil nil nil nil nil) (nil nil nil nil nil)))

  (defexamples -table
    (-table '* '(1 2 3) '(1 2 3)) => '((1 2 3) (2 4 6) (3 6 9))
    (-table (lambda (a b) (-sum (-zip-with '* a b))) '((1 2) (3 4)) '((1 3) (2 4))) => '((7 15) (10 22))
    (apply '-table 'list (-repeat 3 '(1 2))) => '((((1 1 1) (2 1 1)) ((1 2 1) (2 2 1))) (((1 1 2) (2 1 2)) ((1 2 2) (2 2 2)))))

  (defexamples -table-flat
    (-table-flat 'list '(1 2 3) '(a b c)) => '((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))
    (-table-flat '* '(1 2 3) '(1 2 3)) => '(1 2 3 2 4 6 3 6 9)
    (apply '-table-flat 'list (-repeat 3 '(1 2))) => '((1 1 1) (2 1 1) (1 2 1) (2 2 1) (1 1 2) (2 1 2) (1 2 2) (2 2 2))
    (-table-flat '+ '(2)) => '(2)
    (-table-flat '- '(2 4)) => '(-2 -4)

    ;; flatten law tests
    (-flatten-n 1 (-table 'list '(1 2 3) '(a b c))) => '((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))
    (-flatten-n 1 (-table '* '(1 2 3) '(1 2 3))) => '(1 2 3 2 4 6 3 6 9)
    (-flatten-n 2 (apply '-table 'list (-repeat 3 '(1 2)))) => '((1 1 1) (2 1 1) (1 2 1) (2 2 1) (1 1 2) (2 1 2) (1 2 2) (2 2 2)))

  (defexamples -first
    (-first #'natnump '(-1 0 1)) => 0
    (-first #'null '(1 2 3)) => nil
    (--first (> it 2) '(1 2 3)) => 3
    (let ((c 0)) (--first (setq c (1+ c)) '(nil nil nil)) c) => 1
    (--first nil '(1 2 3)) => nil
    (--first nil '(1)) => nil
    (--first nil '()) => nil
    (-first #'ignore '(1 2 3)) => nil
    (-first #'ignore '(1)) => nil
    (-first #'ignore '()) => nil
    (--first (not it) '(1 2 nil)) => nil
    (--first (not it) '(nil 1 2)) => nil
    (--first (not it) '(nil)) => nil
    (--first (not it) '()) => nil
    (-first #'null '(1 2 nil)) => nil
    (-first #'null '(nil 1 2)) => nil
    (-first #'null '(nil)) => nil
    (-first #'null '()) => nil
    (--first t '(1 2 3)) => 1
    (--first t '(1)) => 1
    (--first t '()) => nil
    (-first #'identity '(1 2 3)) => 1
    (-first #'identity '(1)) => 1
    (-first #'identity '()) => nil)

  (defexamples -last
    (-last 'even? '(1 2 3 4 5 6 3 3 3)) => 6
    (-last 'even? '(1 3 7 5 9)) => nil
    (--last (> (length it) 3) '("a" "looong" "word" "and" "short" "one")) => "short")

  (defexamples -first-item
    (-first-item '()) => '()
    (-first-item '(1 2 3 4 5)) => 1
    (let ((list (list 1 2 3))) (setf (-first-item list) 5) list) => '(5 2 3)
    (-first-item 1) !!> wrong-type-argument)

  (defexamples -second-item
    (-second-item '()) => '()
    (-second-item '(1 2 3 4 5)) => 2
    (let ((list (list 1 2))) (setf (-second-item list) 5) list) => '(1 5)
    (-second-item '(1)) => '()
    (-second-item 1) !!> wrong-type-argument)

  (defexamples -third-item
    (-third-item '()) => '()
    (-third-item '(1 2)) => '()
    (-third-item '(1 2 3 4 5)) => 3
    (-third-item 1) !!> wrong-type-argument)

  (defexamples -fourth-item
    (-fourth-item '()) => '()
    (-fourth-item '(1 2 3)) => '()
    (-fourth-item '(1 2 3 4 5)) => 4
    (-fourth-item 1) !!> wrong-type-argument)

  (defexamples -fifth-item
    (-fifth-item '()) => '()
    (-fifth-item '(1 2 3 4)) => '()
    (-fifth-item '(1 2 3 4 5)) => 5
    (-fifth-item 1) !!> wrong-type-argument)

  (defexamples -last-item
    (-last-item '()) => '()
    (-last-item '(1 2 3 4 5)) => 5
    (let ((list (list 1 2 3))) (setf (-last-item list) 5) list) => '(1 2 5)
    (-last-item '(1)) => 1
    (-last-item 1) !!> wrong-type-argument)

  (defexamples -butlast
    (-butlast '(1 2 3)) => '(1 2)
    (-butlast '(1 2)) => '(1)
    (-butlast '(1)) => nil
    (-butlast nil) => nil)

  (defexamples -sort
    (-sort '< '(3 1 2)) => '(1 2 3)
    (-sort '> '(3 1 2)) => '(3 2 1)
    (--sort (< it other) '(3 1 2)) => '(1 2 3)
    (let ((l '(3 1 2))) (-sort '> l) l) => '(3 1 2))

  (defexamples -list
    (-list 1) => '(1)
    (-list '()) => '()
    (-list '(1 2 3)) => '(1 2 3)
    (with-no-warnings (-list 1 2 3)) => '(1 2 3)
    (let ((l (list 1 2))) (setcar (-list l) 3) l) => '(3 2)
    (let ((l (list 1 2))) (setcar (apply #'-list l) 3) l)
    => '(1 2)
    (-list '((1) (2))) => '((1) (2))
    (with-no-warnings (-list)) => ()
    (with-no-warnings (-list () 1)) => ()
    (with-no-warnings (-list () ())) => ()
    (with-no-warnings (-list 1 ())) => '(1 ())
    (with-no-warnings (-list 1 '(2))) => '(1 (2))
    (-list '(())) => '(())
    (-list '(() 1)) => '(() 1))

  (defexamples -fix
    (-fix (lambda (l) (-non-nil (--mapcat (-split-at (/ (length it) 2) it) l))) '((1 2 3))) => '((1) (2) (3))
    (let ((l '((starwars scifi)
               (jedi starwars warrior))))
      (--fix (-uniq (--mapcat (cons it (cdr (assq it l))) it)) '(jedi book)))
    => '(jedi starwars warrior scifi book)))

(def-example-group "Tree operations"
  "Functions pretending lists are trees."

  (defexamples -tree-seq
    (-tree-seq 'listp 'identity '(1 (2 3) 4 (5 (6 7)))) => '((1 (2 3) 4 (5 (6 7))) 1 (2 3) 2 3 4 (5 (6 7)) 5 (6 7) 6 7)
    (-tree-seq 'listp 'reverse '(1 (2 3) 4 (5 (6 7)))) => '((1 (2 3) 4 (5 (6 7))) (5 (6 7)) (6 7) 7 6 5 4 (2 3) 3 2 1)
    (--tree-seq (vectorp it) (append it nil) [1 [2 3] 4 [5 [6 7]]]) => '([1 [2 3] 4 [5 [6 7]]] 1 [2 3] 2 3 4 [5 [6 7]] 5 [6 7] 6 7))

  (defexamples -tree-map
    (-tree-map '1+ '(1 (2 3) (4 (5 6) 7))) => '(2 (3 4) (5 (6 7) 8))
    (-tree-map '(lambda (x) (cons x (expt 2 x))) '(1 (2 3) 4)) => '((1 . 2) ((2 . 4) (3 . 8)) (4 . 16))
    (--tree-map (length it) '("<body>" ("<p>" "text" "</p>") "</body>")) => '(6 (3 4 4) 7)
    (--tree-map 1 '(1 2 (3 4) (5 6))) => '(1 1 (1 1) (1 1))
    (--tree-map (cdr it) '((1 . 2) (3 . 4) (5 . 6))) => '(2 4 6))

  (defexamples -tree-map-nodes
    (-tree-map-nodes 'vectorp (lambda (x) (-sum (append x nil))) '(1 [2 3] 4 (5 [6 7] 8))) => '(1 5 4 (5 13 8))
    (-tree-map-nodes 'keywordp (lambda (x) (symbol-name x)) '(1 :foo 4 ((5 6 :bar) :baz 8))) => '(1 ":foo" 4 ((5 6 ":bar") ":baz" 8))
    (--tree-map-nodes
     (eq (car-safe it) 'add-mode)
     (-concat it (list :mode 'emacs-lisp-mode))
     '(with-mode emacs-lisp-mode (foo bar) (add-mode a b) (baz (add-mode c d)))) => '(with-mode emacs-lisp-mode (foo bar) (add-mode a b :mode emacs-lisp-mode) (baz (add-mode c d :mode emacs-lisp-mode))))

  (defexamples -tree-reduce
    (-tree-reduce '+ '(1 (2 3) (4 5))) => 15
    (-tree-reduce 'concat '("strings" (" on" " various") ((" levels")))) => "strings on various levels"
    (--tree-reduce (cond
                    ((stringp it) (concat it " " acc))
                    (t (let ((sn (symbol-name it))) (concat "<" sn ">" acc "</" sn ">"))))
                   '(body (p "some words") (div "more" (b "bold") "words"))) => "<body><p>some words</p> <div>more <b>bold</b> words</div></body>")

  (defexamples -tree-reduce-from
    (-tree-reduce-from '+ 1 '(1 (1 1) ((1)))) => 8
    (--tree-reduce-from (-concat acc (list it)) nil '(1 (2 3 (4 5)) (6 7))) => '((7 6) ((5 4) 3 2) 1))

  (defexamples -tree-mapreduce
    (-tree-mapreduce 'list 'append '(1 (2 (3 4) (5 6)) (7 (8 9)))) => '(1 2 3 4 5 6 7 8 9)
    (--tree-mapreduce 1 (+ it acc) '(1 (2 (4 9) (2 1)) (7 (4 3)))) => 9
    (--tree-mapreduce 0 (max acc (1+ it)) '(1 (2 (4 9) (2 1)) (7 (4 3)))) => 3
    (--tree-mapreduce (-value-to-list it)
                      (-concat it acc)
                      '((1 . 2) (3 . 4) (5 (6 7) 8)))
    => '(1 2 3 4 5 6 7 8)
    (--tree-mapreduce (if (-cons-pair? it) (cdr it) it)
                      (concat it " " acc)
                      '("foo" (bar . "bar") ((baz . "baz")) "quux" (qwop . "qwop")))
    => "foo bar baz quux qwop"
    (--tree-mapreduce (if (-cons-pair? it) (list (cdr it)) nil)
                      (append it acc)
                      '((elisp-mode (foo (bar . booze)) (baz . qux)) (c-mode (foo . bla) (bum . bam))))
    => '(booze qux bla bam))

  (defexamples -tree-mapreduce-from
    (-tree-mapreduce-from 'identity '* 1 '(1 (2 (3 4) (5 6)) (7 (8 9)))) => 362880
    (--tree-mapreduce-from (+ it it) (cons it acc) nil '(1 (2 (4 9) (2 1)) (7 (4 3)))) => '(2 (4 (8 18) (4 2)) (14 (8 6)))
    (concat "{" (--tree-mapreduce-from
                 (cond
                  ((-cons-pair? it)
                   (concat (symbol-name (car it)) " -> " (symbol-name (cdr it))))
                  (t (concat (symbol-name it) " : {")))
                 (concat it (unless (or (equal acc "}")
                                        (equal (substring it (1- (length it))) "{"))
                              ", ") acc)
                 "}"
                 '((elisp-mode (foo (bar . booze)) (baz . qux)) (c-mode (foo . bla) (bum . bam)))))
    => "{elisp-mode : {foo : {bar -> booze}, baz -> qux}, c-mode : {foo -> bla, bum -> bam}}")

  (defexamples -clone
    (let* ((a '(1 2 3)) (b (-clone a))) (nreverse a) b) => '(1 2 3)))

(def-example-group "Threading macros"
  "Macros that conditionally combine sequential forms for brevity
or readability."

  (defexamples ->
    (-> '(2 3 5)) => '(2 3 5)
    (-> '(2 3 5) (append '(8 13))) => '(2 3 5 8 13)
    (-> '(2 3 5) (append '(8 13)) (-slice 1 -1)) => '(3 5 8)
    (-> 5 square) => 25
    (-> 5 (+ 3) square) => 64)

  (defexamples ->>
    (->> '(1 2 3) (-map 'square)) => '(1 4 9)
    (->> '(1 2 3) (-map 'square) (-remove 'even?)) => '(1 9)
    (->> '(1 2 3) (-map 'square) (-reduce '+)) => 14
    (->> 5 (- 8)) => 3
    (->> 5 (- 3) square) => 4)

  (defexamples -->
    (--> "def" (concat "abc" it "ghi")) => "abcdefghi"
    (--> "def" (concat "abc" it "ghi") (upcase it)) => "ABCDEFGHI"
    (--> "def" (concat "abc" it "ghi") upcase) => "ABCDEFGHI"
    (--> "def" upcase) => "DEF"
    (--> 3 (car (list it))) => 3

    (--> '(1 2 3 4) (--map (1+ it) it)) => '(2 3 4 5)
    (--map (--> it (1+ it)) '(1 2 3 4)) => '(2 3 4 5)

    (--filter (--> it (equal 0 (mod it 2))) '(1 2 3 4)) => '(2 4)
    (--> '(1 2 3 4) (--filter (equal 0 (mod it 2)) it)) => '(2 4)

    (--annotate (--> it (< 1 it)) '(0 1 2 3)) => '((nil . 0)
                                                   (nil . 1)
                                                   (t . 2)
                                                   (t . 3))

    (--> '(0 1 2 3) (--annotate (< 1 it) it)) => '((nil . 0)
                                                   (nil . 1)
                                                   (t . 2)
                                                   (t . 3)))

  (defexamples -as->
    (-as-> 3 my-var (1+ my-var) (list my-var) (mapcar (lambda (ele) (* 2 ele)) my-var)) => '(8)
    (-as-> 3 my-var 1+) => 4
    (-as-> 3 my-var) => 3
    (-as-> "def" string (concat "abc" string "ghi")) => "abcdefghi"
    (-as-> "def" string (concat "abc" string "ghi") upcase) => "ABCDEFGHI"
    (-as-> "def" string (concat "abc" string "ghi") (upcase string)) => "ABCDEFGHI")

  (defexamples -some->
    (-some-> '(2 3 5)) => '(2 3 5)
    (-some-> 5 square) => 25
    (-some-> 5 even? square) => nil
    (-some-> nil square) => nil)

  (defexamples -some->>
    (-some->> '(1 2 3) (-map 'square)) => '(1 4 9)
    (-some->> '(1 3 5) (-last 'even?) (+ 100)) => nil
    (-some->> '(2 4 6) (-last 'even?) (+ 100)) => 106
    (-some->> '("A" "B" :c) (-filter 'stringp) (-reduce 'concat)) => "AB"
    (-some->> '(:a :b :c) (-filter 'stringp) (-reduce 'concat)) => nil)

  (defexamples -some-->
    (-some--> "def" (concat "abc" it "ghi")) => "abcdefghi"
    (-some--> nil (concat "abc" it "ghi")) => nil
    (-some--> '(0 1) (-remove #'natnump it) (append it it) (-map #'1+ it))
    => '()
    (-some--> '(0 1) (-filter #'natnump it) (append it it) (-map #'1+ it))
    => '(1 2 1 2)
    ;; FIXME: Is there a better way to have this compile without warnings?
    (eval '(-some--> 1 nil) t) !!> (void-function nil)
    (-some--> nil) => nil
    (-some--> t) => t)

  (defexamples -doto
    (-doto (list 1 2 3) pop pop) => '(3)
    (-doto (cons 1 2) (setcar 3) (setcdr 4)) => '(3 . 4)
    (gethash 'k (--doto (make-hash-table) (puthash 'k 'v it))) => 'v
    (-doto (cons 1 2)) => '(1 . 2)))

(def-example-group "Binding"
  "Macros that combine `let' and `let*' with destructuring and flow control."

  (defexamples -when-let
    (-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) => 5
    (-when-let ((&plist :foo foo) (list :foo "foo")) foo) => "foo"
    (-when-let ((&plist :foo foo) (list :bar "bar")) foo) => nil
    (--when-let (member :b '(:a :b :c)) (cons :d it)) => '(:d :b :c)
    ;; Check negative condition irrespective of compiler optimizations.
    (--when-let (stringp ()) (cons it :a)) => nil
    (--when-let (stringp (list ())) (cons it :a)) => nil)

  (defexamples -when-let*
    (-when-let* ((x 5) (y 3) (z (+ y 4))) (+ x y z)) => 15
    (-when-let* ((x 5) (y nil) (z 7)) (+ x y z)) => nil)

  (defexamples -if-let
    (-if-let (match-index (string-match "d" "abc")) (+ match-index 3) 7) => 7
    (--if-let (even? 4) it nil) => t)

  (defexamples -if-let*
    (-if-let* ((x 5) (y 3) (z 7)) (+ x y z) "foo") => 15
    (-if-let* ((x 5) (y nil) (z 7)) (+ x y z) "foo") => "foo"
    (-if-let* (((_ _ x) '(nil nil 7))) x) => 7)

  (defexamples -let
    (-let (([a (b c) d] [1 (2 3) 4])) (list a b c d)) => '(1 2 3 4)
    (-let [(a b c . d) (list 1 2 3 4 5 6)] (list a b c d)) => '(1 2 3 (4 5 6))
    (-let [(&plist :foo foo :bar bar) (list :baz 3 :foo 1 :qux 4 :bar 2)] (list foo bar)) => '(1 2)
    (let ((a (list 1 2 3))
          (b (list 'a 'b 'c)))
      (-let (((a . b) a)
             ((c . d) b))
        (list a b c d))) => '(1 (2 3) a (b c))
    (-let ((a "foo") (b "bar")) (list a b)) => '("foo" "bar")
    (-let [foo (list 1 2 3)] foo) => '(1 2 3)
    (-let [(&plist :foo foo :bar bar) (list :foo 1 :bar 2)] (list foo bar)) => '(1 2)
    (-let [(&plist :foo (a b) :bar c) (list :foo (list 1 2) :bar 3)] (list a b c)) => '(1 2 3)
    ;; nil value in plist means subsequent cons matches are nil, because
    ;; (car nil) => nil
    (-let [(&plist :foo (a b)) (list :bar 1)] (list a b)) => '(nil nil)
    (-let [(&plist :foo (&plist :baz baz) :bar bar)
           (list :foo (list 1 2 :baz 2 :bar 4) :bar 3)]
      (list baz bar)) => '(2 3)
    (-let [(_ (&plist :level level :title title))
           (list 'paragraph (list :title "foo" :level 2))]
      (list level title)) => '(2 "foo")
    (-let [(&alist :foo (&plist 'face face 'invisible inv) :bar bar)
           (list (cons :bar 2) (cons :foo (list 'face 'foo-face 'invisible t)))]
      (list bar face inv)) => '(2 foo-face t)
    (-let [(a (b c) d) (list 1 (list 2 3) 4 5 6)] (list a b c d)) => '(1 2 3 4)
    (-let [[a _ c] [1 2 3 4]] (list a c)) => '(1 3)
    (-let [[_ _ _ a] (vector 1 2 3 4)] a) => 4
    (-let [[a _ _ _ b] (vector 1 2 3 4 5)] (list a b)) => '(1 5)
    (-let [[a (b c) d] [1 (2 3) 4]] (list a b c d)) => '(1 2 3 4)
    (-let [[a b c] (string ?f ?o ?b ?a ?r)] (list a b c)) => '(?f ?o ?b)
    (-let [[a b c] "abcdef"] (list a b c)) => '(?a ?b ?c)
    (-let [[a (b [c]) d] [1 (2 [3 4]) 5 6]] (list a b c d)) => '(1 2 3 5)
    (-let [(a b c d) (list 1 2 3 4 5 6)] (list a b c d)) => '(1 2 3 4)
    (-let [([a b]) (list (vector 1 2 3))] (list a b)) => '(1 2)
    ;; d is bound to nil. I don't think we want to error in such a case.
    ;; After all (car nil) => nil
    (-let [(a b c d) (list 1 2 3)] (list a b c d)) => '(1 2 3 nil)
    (-let [[a b c] [1 2 3 4]] (list a b c)) => '(1 2 3)
    (-let [[a] [1 2 3 4]] a) => 1
    (-let [[a b &rest c] "abcdef"] (list a b c)) => '(?a ?b "cdef")
    (-let [[a b &rest c] [1 2 3 4 5 6]] (list a b c)) => '(1 2 [3 4 5 6])
    (-let [[a b &rest [c d]] [1 2 3 4 5 6]] (list a b c d)) => '(1 2 3 4)
    ;; here we error, because "vectors" are rigid, immutable structures,
    ;; so we should know how many elements there are
    (-let [[a b c d] [1 2 3]] (+ a b c d)) !!> args-out-of-range
    (-let [(a . (b . c)) (cons 1 (cons 2 3))] (list a b c)) => '(1 2 3)
    (-let [(_ _ . [a b]) (cons 1 (cons 2 (vector 3 4)))] (list a b)) => '(3 4)
    (-let [(_ _ . (a b)) (cons 1 (cons 2 (list 3 4)))] (list a b)) => '(3 4)
    (-let [([a b] _ _ c) (list (vector 1 2) 3 4 5)] (list a b c)) => '(1 2 5)
    ;; final cdr optimization
    (-let [(((a))) (list (list (list 1 2) 3) 4)] a) => 1
    (-let [(((a b) c) d) (list (list (list 1 2) 3) 4)] (list a b c d)) => '(1 2 3 4)
    (-let [(((a b) . c) . d) (list (list (list 1 2) 3) 4)] (list a b c d)) => '(1 2 (3) (4))
    (-let [(((a b) c)) (list (list (list 1 2) 3) 4)] (list a b c)) => '(1 2 3)
    (-let [(((a b) . c)) (list (list (list 1 2) 3) 4)] (list a b c)) => '(1 2 (3))
    ;; cdr-skip optimization
    (-let [(_ (_ (_ a))) (list 1 (list 2 (list 3 4)))] a) => 4
    (-let [(_ (a)) (list 1 (list 2))] a) => 2
    (-let [(_ _ _ a) (list 1 2 3 4 5)] a) => 4
    (-let [(_ _ _ (a b)) (list 1 2 3 (list 4 5))] (list a b)) => '(4 5)
    (-let [(_ a _ b) (list 1 2 3 4 5)] (list a b)) => '(2 4)
    (-let [(_ a _ b _ c) (list 1 2 3 4 5 6)] (list a b c)) => '(2 4 6)
    (-let [(_ a _ b _ _ _ c) (list 1 2 3 4 5 6 7 8)] (list a b c)) => '(2 4 8)
    (-let [(_ a _ _ _ b _ c) (list 1 2 3 4 5 6 7 8)] (list a b c)) => '(2 6 8)
    (-let [(_ _ _ a _ _ _ b _ _ _ c) (list 1 2 3 4 5 6 7 8 9 10 11 12)] (list a b c)) => '(4 8 12)
    (-let [(_ (a b) _ c) (list 1 (list 2 3) 4 5)] (list a b c)) => '(2 3 5)
    (-let [(_ (a b) _ . c) (list 1 (list 2 3) 4 5)] (list a b c)) => '(2 3 (5))
    (-let [(_ (a b) _ (c d)) (list 1 (list 2 3) 4 (list 5 6))] (list a b c d)) => '(2 3 5 6)
    (-let [(_ (a b) _ _ _ (c d)) (list 1 (list 2 3) 4 5 6 (list 7 8))] (list a b c d)) => '(2 3 7 8)
    (-let [(_ (a b) _ . (c d)) (list 1 (list 2 3) 4 5 6)] (list a b c d)) => '(2 3 5 6)
    (-let [(_ (a b) _ _ _ [c d]) (list 1 (list 2 3) 4 5 6 (vector 7 8))] (list a b c d)) => '(2 3 7 8)
    (-let [(_ [a b] _ _ _ [c d]) (list 1 (vector 2 3) 4 5 6 (vector 7 8))] (list a b c d)) => '(2 3 7 8)
    (-let [(_ _ _ . a) (list 1 2 3 4 5)] a) => '(4 5)
    (-let [(_ a _ _) (list 1 2 3 4 5)] a) => 2
    (-let [(_ . b) (cons 1 2)] b) => 2
    (-let [([a b c d] . e) (cons (vector 1 2 3 4) 5)] (list a b c d e)) => '(1 2 3 4 5)
    (-let [([a b c d] _ . e) (cons (vector 1 2 3 4) (cons 5 6))] (list a b c d e)) => '(1 2 3 4 6)
    ;; late-binding optimization
    (-let [(((a))) (list (list (list 1 2) 3) 4)] a) => 1
    (-let [(((&plist :foo a :bar b))) (list (list (list :bar 1 :foo 2) 3) 4)] (list a b)) => '(2 1)
    (-let [(((a b) c) d) (list (list (list 1 2) 3) 4)] (list a b c d)) => '(1 2 3 4)
    (-let [(((a b) c) . d) (list (list (list 1 2) 3) 4)] (list a b c d)) => '(1 2 3 (4))
    (-let [(((a b) c)) (list (list (list 1 2) 3) 4)] (list a b c)) => '(1 2 3)
    (-let [(a b c d) (list 1 2 3 4)] (list a b c d)) => '(1 2 3 4)
    (-let [(a) (list 1 2 3 4)] (list a)) => '(1)
    (-let [(_ a) (list 1 2 3 4)] (list a)) => '(2)
    (-let [(_ _ a) (list 1 2 3 4)] (list a)) => '(3)
    (-let [(_ _ . a) (list 1 2 3 4)] a) => '(3 4)
    (-let [(_ _ [a b]) (list 1 2 (vector 3 4))] (list a b)) => '(3 4)
    (-let [(a _ _ b) (list 1 2 3 4 5 6 7 8)] (list a b)) => '(1 4)
    (-let [(_ _ a _ _ b) (list 1 2 3 4 5 6 7 8)] (list a b)) => '(3 6)
    (-let [(_ _ a _ _ . b) (list 1 2 3 4 5 6 7 8)] (cons a b)) => '(3 6 7 8)
    (-let [(_ a _ b) (list 1 2 3 4)] (list a b)) => '(2 4)
    (-let [(a b c (d e)) (list 1 2 3 (list 4 5))] (list a b c d e)) => '(1 2 3 4 5)
    (-let [(_ _ (_ _ (_ _ a))) (list 1 2 (list 3 4 (list 5 6 7)))] a) => 7
    (-let [(_ (_ (_ a))) (list 1 (list 2 (list 3 4)))] a) => 4
    (-let [(_ _ . (&plist :foo a :bar b)) (list 1 2 :bar 2 :foo 1)] (list a b)) => '(1 2)
    ;; &keys support
    (-let [(_ _ &keys :foo a :bar b) (list 1 2 :bar 4 :foo 3)] (list a b)) => '(3 4)
    (-let [(a _ &keys :foo b :bar c) (list 1 2 :bar 4 :foo 3)] (list a b c)) => '(1 3 4)
    (-let [(a _ _ _ &keys :foo b :bar c) (list 1 2 3 4 :bar 6 :foo 5)] (list a b c)) => '(1 5 6)
    (-let [(a b &keys :foo c :bar d) (list 1 2 :bar 4 :foo 3)] (list a b c d)) => '(1 2 3 4)
    (-let [(a b &keys) (list 1 2 :bar 4 :foo 3)] (list a b)) => '(1 2)
    (-let [(&keys :foo a :bar b) (list 1 2 :bar 4 :foo 3)] (list a b)) => '(3 4)
    (-let [(a b (c _ _ &keys :foo [d _ (&alist :bar (e &keys :baz f) :qux (&plist :fux g))] :mux h) i)
           (list 1 2 (list 3 'skip 'skip :foo (vector 4 'skip (list (cons :bar (list 5 :baz 6)) (cons :qux (list :fux 7)))) :mux 8) 9)]
      (list a b c d e f g h i)) => '(1 2 3 4 5 6 7 8 9)
    ;; single-binding optimization for vectors and kv
    (-let [[_ [_ [_ a]]] (vector 1 (vector 2 (vector 3 4)))] a) => 4
    (-let [[a _ _ _] (vector 1 2 3 4)] a) => 1
    (-let [[_ _ _ a] (vector 1 2 3 4)] a) => 4
    (-let [[_ _ a _] (vector 1 2 3 4)] a) => 3
    (-let [[a [_ [_ b]]] (vector 1 (vector 2 (vector 3 4)))] (list a b)) => '(1 4)
    (-let [[(a _ b)] (vector (list 1 2 3 4))] (list a b)) => '(1 3)
    (-let [(&plist 'a a) (list 'a 1 'b 2)] a) => 1
    (-let [(&plist 'a [a b]) (list 'a [1 2] 'b 3)] (list a b)) => '(1 2)
    (-let [(&plist 'a [a b] 'c c) (list 'a [1 2] 'c 3)] (list a b c)) => '(1 2 3)
    ;; mixing dot and &alist
    (-let (((x y &alist 'a a 'c c) (list 1 2 '(a . b) '(e . f) '(g . h) '(c . d)))) (list x y a c)) => '(1 2 b d)
    (-let (((_ _ &alist 'a a 'c c) (list 1 2 '(a . b) '(e . f) '(g . h) '(c . d)))) (list a c)) => '(b d)
    (-let (((x y . (&alist 'a a 'c c)) (list 1 2 '(a . b) '(e . f) '(g . h) '(c . d)))) (list x y a c)) => '(1 2 b d)
    (-let (((_ _ . (&alist 'a a 'c c)) (list 1 2 '(a . b) '(e . f) '(g . h) '(c . d)))) (list a c)) => '(b d)
    (-let (((x y (&alist 'a a 'c c)) (list 1 2 '((a . b) (e . f) (g . h) (c . d))))) (list x y a c)) => '(1 2 b d)
    (-let (((_ _ . ((&alist 'a a 'c c))) (list 1 2 '((a . b) (e . f) (g . h) (c . d))))) (list a c)) => '(b d)
    ;; test bindings with no explicit val
    (-let (a) a) => nil
    (-let ((a)) a) => nil
    (-let (a b) (list a b)) => '(nil nil)
    (-let ((a) (b)) (list a b)) => '(nil nil)
    ;; auto-derived match forms for kv destructuring
    ;;; test that we normalize all the supported kv stores
    (-let (((&plist :foo :bar) (list :foo 1 :bar 2))) (list foo bar)) => '(1 2)
    (-let (((&alist :foo :bar) (list (cons :foo 1) (cons :bar 2)))) (list foo bar)) => '(1 2)
    (let ((hash (make-hash-table)))
      (puthash :foo 1 hash)
      (puthash :bar 2 hash)
      (-let (((&hash :foo :bar) hash)) (list foo bar))) => '(1 2)
    (-let (((&hash :foo (&hash? :bar)) (make-hash-table))) bar) => nil
    ;; Ensure `hash?' expander evaluates its arg only once
    (let* ((ht (make-hash-table :test #'equal))
           (fn (lambda (ht) (push 3 (gethash 'a ht)) ht)))
      (puthash 'a nil ht)
      (-let (((&hash? 'a) (funcall fn ht)))
        a)) => '(3)
    (-let (((_ &keys :foo :bar) (list 'ignored :foo 1 :bar 2))) (list foo bar)) => '(1 2)
    ;;; go over all the variations of match-form derivation
    (-let (((&plist :foo foo :bar) (list :foo 1 :bar 2))) (list foo bar)) => '(1 2)
    (-let (((&plist :foo foo :bar bar) (list :foo 1 :bar 2))) (list foo bar)) => '(1 2)
    (-let (((&plist :foo x :bar y) (list :foo 1 :bar 2))) (list x y)) => '(1 2)
    (-let (((&plist :foo (x) :bar [y]) (list :foo (list 1) :bar (vector 2)))) (list x y)) => '(1 2)
    (-let (((&plist 'foo 'bar) (list 'foo 1 'bar 2))) (list foo bar)) => '(1 2)
    (-let (((&plist 'foo foo 'bar) (list 'foo 1 'bar 2))) (list foo bar)) => '(1 2)
    (-let (((&plist 'foo foo 'bar bar) (list 'foo 1 'bar 2))) (list foo bar)) => '(1 2)
    (-let (((&plist 'foo x 'bar y) (list 'foo 1 'bar 2))) (list x y)) => '(1 2)
    (-let (((&alist "foo" "bar") (list (cons "foo" 1) (cons "bar" 2)))) (list foo bar)) => '(1 2)
    (-let (((&alist "foo" x "bar") (list (cons "foo" 1) (cons "bar" 2)))) (list x bar)) => '(1 2)
    (-let (((&alist "foo" x "bar" y) (list (cons "foo" 1) (cons "bar" 2)))) (list x y)) => '(1 2)
    (-let (((&alist :a 'b "c") (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    (-let (((&alist 'b :a "c") (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    (-let (((&alist 'b "c" :a) (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    (-let (((&alist "c" 'b :a) (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    (-let (((&alist "c" :a 'b) (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    (-let (((&alist :a "c" 'b) (list (cons :a 1) (cons 'b 2) (cons "c" 3)))) (list a b c)) => '(1 2 3)
    ;; FIXME: Byte-compiler chokes on these in Emacs < 26.
    (eval '(-let (((&plist 'foo 1) (list 'foo 'bar))) (list foo)) t) !!> error
    (eval '(-let (((&plist foo :bar) (list :foo :bar))) (list foo)) t) !!> error
    ;; test the &as form
    (-let (((items &as first . rest) (list 1 2 3))) (list first rest items)) => '(1 (2 3) (1 2 3))
    (-let [(all &as [vect &as a b] bar) (list [1 2] 3)] (list a b bar vect all)) => '(1 2 3 [1 2] ([1 2] 3))
    (-let [(all &as (list &as a b) bar) (list (list 1 2) 3)] (list a b bar list all)) => '(1 2 3 (1 2) ((1 2) 3))
    (-let [(x &as [a b]) (list (vector 1 2 3))] (list a b x)) => '(1 2 ([1 2 3]))
    (-let [(result &as [_ a] [_ b]) (list [1 2] [3 4])] (list a b result)) => '(2 4 ([1 2] [3 4]))
    (-let [(result &as [fst &as _ a] [snd &as _ b]) (list [1 2] [3 4])] (list a b fst snd result)) => '(2 4 [1 2] [3 4] ([1 2] [3 4]))
    (-let [[x &as a b &rest r] (vector 1 2 3)] (list a b r x)) => '(1 2 [3] [1 2 3])
    (-let [[x &as a] (vector 1 2 3)] (list a x)) => '(1 [1 2 3])
    (-let [[x &as _ _ a] (vector 1 2 3)] (list a x)) => '(3 [1 2 3])
    (-let [[x &as _ _ a] (vector 1 2 (list 3 4))] (list a x)) => '((3 4) [1 2 (3 4)])
    (-let [[x &as _ _ (a b)] (vector 1 2 (list 3 4))] (list a b x)) => '(3 4 [1 2 (3 4)])
    (-let [(b &as beg . end) (cons 1 2)] (list beg end b)) => '(1 2 (1 . 2))
    (-let [(plist &as &plist :a a :b b) (list :a 1 :b 2)] (list a b plist)) => '(1 2 (:a 1 :b 2))
    (-let [(alist &as &alist :a a :b b) (list (cons :a 1) (cons :b 2))] (list a b alist)) => '(1 2 ((:a . 1) (:b . 2)))
    (-let [(list &as _ _ _ a _ _ _ b _ _ _ c) (list 1 2 3 4 5 6 7 8 9 10 11 12)] (list a b c list)) => '(4 8 12 (1 2 3 4 5 6 7 8 9 10 11 12))
    (-let (((x &as a b) (list 1 2))
           ((y &as c d) (list 3 4)))
      (list a b c d x y))
    => '(1 2 3 4 (1 2) (3 4))
    (-let (((&hash-or-plist :key)
            (--doto (make-hash-table)
              (puthash :key "value" it))))
      key)
    => "value"
    (-let (((&hash-or-plist :key) '(:key "value")))
      key)
    => "value")

  (defexamples -let*
    (-let* (((a . b) (cons 1 2))
            ((c . d) (cons 3 4)))
      (list a b c d)) => '(1 2 3 4)
    (-let* (((a . b) (cons 1 (cons 2 3)))
            ((c . d) b))
      (list a b c d)) => '(1 (2 . 3) 2 3)
    (-let* (((&alist "foo" foo "bar" bar) (list (cons "foo" 1) (cons "bar" (list 'a 'b 'c))))
            ((a b c) bar))
      (list foo a b c bar)) => '(1 a b c (a b c))
    (let ((a (list 1 2 3))
          (b (list 'a 'b 'c)))
      (ignore b)
      (-let* (((a . b) a)
              ((c . d) b)) ;; b here comes from above binding
        (list a b c d)))
    => '(1 (2 3) 2 (3))
    (-let* ((a "foo") (b a)) (list a b)) => '("foo" "foo")
    ;; test bindings with no explicit val
    (-let* (a) a) => nil
    (-let* ((a)) a) => nil
    (-let* (a b) (list a b)) => '(nil nil)
    (-let* ((a) (b)) (list a b)) => '(nil nil))

  (defexamples -lambda
    (-map (-lambda ((x y)) (+ x y)) '((1 2) (3 4) (5 6))) => '(3 7 11)
    (-map (-lambda ([x y]) (+ x y)) '([1 2] [3 4] [5 6])) => '(3 7 11)
    (funcall (-lambda ((_ . a) (_ . b)) (-concat a b)) '(1 2 3) '(4 5 6)) => '(2 3 5 6)
    (-map (-lambda ((&plist :a a :b b)) (+ a b)) '((:a 1 :b 2) (:a 3 :b 4) (:a 5 :b 6))) => '(3 7 11)
    (-map (-lambda (x) (let ((k (car x)) (v (cadr x))) (+ k v))) '((1 2) (3 4) (5 6))) => '(3 7 11)
    (funcall (-lambda ((a) (b)) (+ a b)) '(1 2 3) '(4 5 6)) => 5
    ;; FIXME: Byte-compiler chokes on this in Emacs < 26.
    (eval '(-lambda a t) t) !!> wrong-type-argument
    (funcall (-lambda (a b) (+ a b)) 1 2) => 3
    (funcall (-lambda (a (b c)) (+ a b c)) 1 (list 2 3)) => 6
    (funcall (-lambda () 1)) => 1
    (let* ((x 0) (f (-lambda () (setq x (1+ x))))) (--dotimes 3 (funcall f)) x)
    => 3)

  (defexamples -setq
    (let (a) (-setq a 1) a) => 1
    (let (a b) (-setq (a b) (list 1 2)) (list a b)) => '(1 2)
    (let (c) (-setq (&plist :c c) (list :c "c")) c) => "c"
    (let (a b) (-setq a 1 b 2) (list a b)) => '(1 2)
    (let (a b) (-setq (&plist :a a) '(:a (:b 1)) (&plist :b b) a) (cons b a))
    => '(1 :b 1)
    (let (a b x y z)
      (ignore a b x y z)
      (-setq (a b (&plist 'x x 'y y)) '(1 2 (x 3 y 4)) z x))
    => 3
    ;; FIXME: Byte-compiler chokes on this in Emacs < 26.
    (eval '(let (a) (-setq a)) t) !!> wrong-number-of-arguments))

(def-example-group "Side effects"
  "Functions iterating over lists for side effect only."

  (defexamples -each
    (let (l) (-each '(1 2 3) (lambda (x) (push x l))) l) => '(3 2 1)
    (let (l) (--each '(1 2 3) (push it l)) l) => '(3 2 1)
    (-each '(1 2 3) #'identity) => nil
    (--each '(1 2 3) it) => nil
    (--each '(1 2 3) nil) => nil
    (let (l) (-each () (lambda (x) (push x l))) l) => ()
    (let (l) (--each () (push it l)) l) => ()
    (let (l) (--each '(1 2 3) (push it l) (setq it-index -1)) l) => '(3 2 1))

  (defexamples -each-while
    (let (l) (-each-while '(2 4 5 6) #'even? (lambda (x) (push x l))) l) => '(4 2)
    (let (l) (--each-while '(1 2 3 4) (< it 3) (push it l)) l) => '(2 1)
    (let ((s 0)) (--each-while '(1 3 4 5) (< it 5) (setq s (+ s it))) s) => 8
    (let (s) (-each-while () (lambda (_) t) (lambda (_) (setq s t))) s) => nil
    (let (s) (--each-while () t (setq s t)) s) => nil
    (let (s) (--each-while '(1) t (setq s it)) s) => 1
    (let (s) (--each-while '(1) nil (setq s it)) s) => nil
    (let (s) (--each-while '(1) (setq it t) (setq s it)) s) => 1
    (let (s) (--each-while '(1 . 2) nil (setq s it)) s) => nil
    (let (s) (--each-while '(1 . 2) (< it-index 0) (setq s it)) s) => nil
    (let (s) (--each-while '(1 . 2) (< it-index 1) (setq s it)) s) => 1
    (--each-while '(1) t t) => nil)

  (defexamples -each-indexed
    (let (l) (-each-indexed '(a b c) (lambda (i x) (push (list x i) l))) l) => '((c 2) (b 1) (a 0))
    (let (l) (--each-indexed '(a b c) (push (list it it-index) l)) l) => '((c 2) (b 1) (a 0))
    (let (l) (--each-indexed '() (push it l)) l) => '()
    (let (l) (-each-indexed () (lambda (_ x) (push x l))) l) => ())

  (defexamples -each-r
    (let (l) (-each-r '(1 2 3) (lambda (x) (push x l))) l) => '(1 2 3)
    (let (l) (--each-r '(1 2 3) (push it l)) l) => '(1 2 3)
    (-each-r '(1 2 3) #'identity) => nil
    (--each-r '(1 2 3) it) => nil
    (--each-r '(1 2 3) nil) => nil
    (let (l) (--each-r '(1 2 3) (push it l) (setq it-index -1)) l) => '(1 2 3)
    (let (l) (-each-r () (lambda (x) (push x l))) l) => ()
    (let (l) (--each-r () (push it l)) l) => ())

  (defexamples -each-r-while
    (let (l) (-each-r-while '(2 4 5 6) #'even? (lambda (x) (push x l))) l) => '(6)
    (let (l) (--each-r-while '(1 2 3 4) (>= it 3) (push it l)) l) => '(3 4)
    (let ((s 0)) (--each-r-while '(1 2 3 5) (> it 1) (setq s (+ s it))) s) => 10
    (let (s) (-each-r-while () (lambda (_) t) (lambda (_) (setq s t))) s) => nil
    (let (s) (--each-r-while () t (setq s t)) s) => nil
    (let (s) (--each-r-while '(1) t (setq s it)) s) => 1
    (let (s) (--each-r-while '(1) nil (setq s it)) s) => nil
    (let (s) (--each-r-while '(1) (setq it t) (setq s it)) s) => 1
    (--each-r-while '(1) t t) => nil)

  (defexamples -dotimes
    (let (s) (-dotimes 3 (lambda (n) (push n s))) s) => '(2 1 0)
    (let (s) (-dotimes 0 (lambda (n) (push n s))) s) => '()
    (let (s) (--dotimes 5 (push it s)) s) => '(4 3 2 1 0)
    (let (s) (--dotimes 0 (push it s)) s) => ()
    (let (s) (--dotimes 3 (push it s) (setq it -1)) s) => '(2 1 0)
    (--dotimes 3 t) => nil))

(def-example-group "Destructive operations"
  "Macros that modify variables holding lists."

  (defexamples !cons
    (let (l) (!cons 5 l) l) => '(5)
    (let ((l '(3))) (!cons 5 l) l) => '(5 3))

  (defexamples !cdr
    (let ((l '(3))) (!cdr l) l) => '()
    (let ((l '(3 5))) (!cdr l) l) => '(5)))

(def-example-group "Function combinators"
  "Functions that manipulate and compose other functions."

  (defexamples -partial
    (funcall (-partial #'+ 5)) => 5
    (funcall (-partial #'- 5) 3) => 2
    (funcall (-partial #'+ 5 2) 3) => 10
    (funcall (-partial #'+)) => 0
    (funcall (-partial #'+) 5) => 5
    (apply (-partial #'+ 5) 10 '(1 2)) => 18)

  (defexamples -rpartial
    (funcall (-rpartial #'- 5)) => -5
    (funcall (-rpartial #'- 5) 8) => 3
    (funcall (-rpartial #'- 5 2) 10) => 3
    (funcall (-rpartial #'-)) => 0
    (apply (-rpartial #'- 1) 2 '(20 3)) => -22)

  (defexamples -juxt
    (funcall (-juxt) 1 2) => '()
    (funcall (-juxt #'+ #'- #'* #'/) 7 5) => '(12 2 35 1)
    (mapcar (-juxt #'number-to-string #'1+) '(1 2)) => '(("1" 2) ("2" 3))
    (funcall (-juxt #'+ #'-)) => '(0 0)
    (funcall (-juxt)) => '())

  (defexamples -compose
    (funcall (-compose #'- #'1+ #'+) 1 2 3) => -7
    (funcall (-compose #'identity #'1+) 3) => 4
    (mapcar (-compose #'not #'stringp) '(nil "")) => '(t nil)
    (funcall (-compose #'1+ #'identity) 3) => 4
    (mapcar (lambda (fn)
              (list (funcall fn 0) (funcall fn 1)))
            (list (-compose (-compose #'natnump #'1+) #'lognot)
                  (-compose #'natnump (-compose #'1+ #'lognot))
                  (-compose #'natnump #'1+ #'lognot)))
    => '((t nil) (t nil) (t nil))
    (funcall (-compose)) => nil
    (funcall (-compose) nil) => nil
    (funcall (-compose) nil 1) => nil
    (funcall (-compose) 1) => 1
    (funcall (-compose) 1 2) => 1
    (-compose #'+) => #'+)

  (defexamples -applify
    (funcall (-applify #'+) ()) => 0
    (mapcar (-applify #'+) '((1 1 1) (1 2 3) (5 5 5))) => '(3 6 15)
    (funcall (-applify #'<) '(3 6)) => t
    (apply (-applify #'+) '(())) => 0
    (apply (-applify #'+) '((1 2))) => 3
    (funcall (-applify #'+)) !!> wrong-number-of-arguments
    (mapcar (-applify (lambda (a b) `(,a (,b)))) '((1 1) (1 2) (5 5)))
    => '((1 (1)) (1 (2)) (5 (5))))

  (defexamples -on
    (-sort (-on #'< #'length) '((1 2 3) (1) (1 2))) => '((1) (1 2) (1 2 3))
    (funcall (-on #'min #'string-to-number) "22" "2" "1" "12") => 1
    (-min-by (-on #'> #'length) '((1 2 3) (4) (1 2))) => '(4)
    (-min-by (-on #'string< #'number-to-string) '(2 100 22)) => 22
    (-max-by (-on #'> #'car) '((2 2 3) (3) (1 2))) => '(3)
    (-sort (-on #'string< #'number-to-string) '(12 1 2 22)) => '(1 12 2 22)
    (funcall (-on #'+ #'1+) 1 2) => 5
    (funcall (-on #'+ #'identity) 1 2) => 3
    (funcall (-on #'* #'length) '(1 2 3) '(4 5)) => 6
    (funcall (-on (-on #'+ #'length) #'cdr) '(1 2 3) '(4 5)) => 3
    (funcall (-on #'+ (lambda (x) (length (cdr x)))) '(1 2 3) '(4 5)) => 3
    (-sort (-on #'< #'car) '((3 2 5) (2) (1 2))) => '((1 2) (2) (3 2 5))
    (-sort (-on #'< (lambda (x) (length x))) '((1 2 3) (1) (1 2)))
    => '((1) (1 2) (1 2 3))
    (-sort (-on (-on #'< #'car) #'cdr) '((0 3) (2 1) (4 2 8)))
    => '((2 1) (4 2 8) (0 3))
    (-sort (-on #'< #'cadr) '((0 3) (2 1) (4 2 8))) => '((2 1) (4 2 8) (0 3))
    (funcall (-on #'not #'not) nil) => nil
    (funcall (-on #'+ #'1+) 1 10 100 1000) => 1115
    (funcall (-on #'+ #'1+) 1 10 100) => 114
    (funcall (-on #'+ #'1+) 1 10) => 13
    (funcall (-on #'+ #'1+) 1) => 2
    (funcall (-on #'+ #'1+)) => 0
    (funcall (-on #'1+ #'1+) 0) => 2
    (funcall (-on #'+ #'*)) => 0
    (funcall (-on #'* #'+)) => 1)

  (defexamples -flip
    (-sort (-flip #'<) '(4 3 6 1)) => '(6 4 3 1)
    (funcall (-flip #'-) 3 2 1 10) => 4
    (funcall (-flip #'1+) 1) => 2
    (funcall (-flip #'<) 2 1) => t
    (funcall (-flip #'list) 1 2 3) => '(3 2 1)
    (funcall (-flip #'list) 1 2) => '(2 1)
    (funcall (-flip #'list) 1) => '(1)
    (funcall (-flip #'list)) => '()
    ;; Assert that &rest conses a fresh list in case that ever changes.
    (let ((a (list 1 2 3 4))) (apply (-flip #'-) a) a) => '(1 2 3 4))

  (defexamples -rotate-args
    (funcall (-rotate-args -1 #'list) 1 2 3 4) => '(2 3 4 1)
    (funcall (-rotate-args 1 #'-) 1 10 100) => 89
    (funcall (-rotate-args 2 #'list) 3 4 5 1 2) => '(1 2 3 4 5)
    (funcall (-rotate-args -2 #'list) 1 2 3 4) => '(3 4 1 2)
    (funcall (-rotate-args 0 #'list) 1 2 3 4) => '(1 2 3 4)
    (funcall (-rotate-args 1 #'list) 1 2 3 4) => '(4 1 2 3)
    (funcall (-rotate-args 2 #'list) 1 2 3 4) => '(3 4 1 2)
    (funcall (-rotate-args -2 #'list) 1 2 3) => '(3 1 2)
    (funcall (-rotate-args -1 #'list) 1 2 3) => '(2 3 1)
    (funcall (-rotate-args 0 #'list) 1 2 3) => '(1 2 3)
    (funcall (-rotate-args 1 #'list) 1 2 3) => '(3 1 2)
    (funcall (-rotate-args 2 #'list) 1 2 3) => '(2 3 1)
    (funcall (-rotate-args -2 #'list) 1 2) => '(1 2)
    (funcall (-rotate-args -1 #'list) 1 2) => '(2 1)
    (funcall (-rotate-args 0 #'list) 1 2) => '(1 2)
    (funcall (-rotate-args 1 #'list) 1 2) => '(2 1)
    (funcall (-rotate-args 2 #'list) 1 2) => '(1 2)
    (funcall (-rotate-args -2 #'list) 1) => '(1)
    (funcall (-rotate-args -1 #'list) 1) => '(1)
    (funcall (-rotate-args 0 #'list) 1) => '(1)
    (funcall (-rotate-args 1 #'list) 1) => '(1)
    (funcall (-rotate-args 2 #'list) 1) => '(1)
    (funcall (-rotate-args -2 #'list)) => '()
    (funcall (-rotate-args -1 #'list)) => '()
    (funcall (-rotate-args 0 #'list)) => '()
    (funcall (-rotate-args 1 #'list)) => '()
    (funcall (-rotate-args 2 #'list)) => '()
    (let ((a (list 1 2 3))) (apply (-rotate-args 2 #'-) a) a) => '(1 2 3))

  (defexamples -const
    (funcall (-const 2) 1 3 "foo") => 2
    (mapcar (-const 1) '("a" "b" "c" "d")) => '(1 1 1 1)
    (-sum (mapcar (-const 1) '("a" "b" "c" "d"))) => 4
    (funcall (-const t)) => t
    (funcall (-const nil)) => nil
    (funcall (-const t) nil) => t
    (funcall (-const nil) nil) => nil)

  (defexamples -cut
    (funcall (-cut list 1 <> 3 <> 5) 2 4) => '(1 2 3 4 5)
    (-map (-cut funcall <> 5) `(1+ 1- ,(lambda (x) (/ 1.0 x)))) => '(6 4 0.2)
    (-map (-cut <> 1 2 3) '(list vector string)) => '((1 2 3) [1 2 3] "")
    (-filter (-cut < <> 5) '(1 3 5 7 9)) => '(1 3))

  (defexamples -not
    (funcall (-not #'numberp) "5") => t
    (-sort (-not #'<) '(5 2 1 0 6)) => '(6 5 2 1 0)
    (-filter (-not (-partial #'< 4)) '(1 2 3 4 5 6 7 8)) => '(1 2 3 4)
    ;; Variadic `<' was introduced in Emacs 24.4.
    (funcall (-not (lambda (a b c) (and (< a b) (< b c)))) 1 2 3) => nil
    (funcall (-not (lambda (a b c) (and (< a b) (< b c)))) 3 2 1) => t
    (funcall (-not #'<) 1 2) => nil
    (funcall (-not #'<) 2 1) => t
    (funcall (-not #'+) 1) => nil
    (funcall (-not #'+)) => nil)

  (defexamples -orfn
    (-filter (-orfn #'natnump #'booleanp) '(1 nil "a" -4 b c t)) => '(1 nil t)
    (funcall (-orfn #'symbolp (-cut string-match-p "x" <>)) "axe") => 1
    (funcall (-orfn #'= #'+) 1 1) => t
    (funcall (-orfn #'+ #'null)) => 0
    (funcall (-orfn #'+ #'null) 1) => 1
    (funcall (-orfn #'+ #'null) 1 2) => 3
    (funcall (-orfn #'+ #'null) 1 2 3) => 6
    (funcall (-orfn #'ignore #'+)) => 0
    (funcall (-orfn #'ignore #'+) 1) => 1
    (funcall (-orfn #'ignore #'+) 1 2) => 3
    (funcall (-orfn #'ignore #'+) 1 2 3) => 6
    (-filter (-orfn #'symbolp) '(a b 1 nil t 2)) => '(a b nil t)
    (-filter (-orfn #'null) '(a b 1 nil t 2)) => '(nil)
    (-filter (-orfn) '(nil t)) => '()
    (-orfn #'null) => #'null
    (-orfn) => #'ignore)

  (defexamples -andfn
    (-filter (-andfn #'numberp (-cut < <> 5)) '(a 1 b 6 c 2)) => '(1 2)
    (mapcar (-andfn #'numberp #'1+) '(a 1 b 6)) => '(nil 2 nil 7)
    (funcall (-andfn #'= #'+) 1 1) => 2
    (funcall (-andfn #'ignore #'+)) => nil
    (funcall (-andfn #'ignore #'+) 1) => nil
    (funcall (-andfn #'ignore #'+) 1 2) => nil
    (funcall (-andfn #'+ #'ignore)) => nil
    (funcall (-andfn #'+ #'ignore) 1) => nil
    (funcall (-andfn #'+ #'ignore) 1 2) => nil
    (funcall (-andfn #'+ #'list)) => '()
    (funcall (-andfn #'+ #'list) 1) => '(1)
    (funcall (-andfn #'+ #'list) 1 2) => '(1 2)
    (funcall (-andfn #'list #'+)) => nil
    (funcall (-andfn #'list #'+) 1) => 1
    (funcall (-andfn #'list #'+) 1 2) => 3
    (funcall (-andfn #'* #'+)) => 0
    (funcall (-andfn #'+ #'*)) => 1
    (-andfn #'null) => #'null
    (funcall (-andfn)) => t
    (funcall (-andfn) nil) => t
    (funcall (-andfn) t) => t)

  (defexamples -iteratefn
    (funcall (-iteratefn (lambda (x) (* x x)) 3) 2) => 256
    (funcall (-iteratefn '1+ 3) 1) => 4
    (funcall (-iteratefn 'cdr 3) '(1 2 3 4 5)) => '(4 5)
    (let ((init '(1 2 3 4 5))
          (fn 'cdr))
      (and (equal (funcall (-iteratefn fn 0) init)
                  (-last-item (-iterate fn init (1+ 0))))
           (equal (funcall (-iteratefn fn 3) init)
                  (-last-item (-iterate fn init (1+ 3))))
           (equal (funcall (-iteratefn fn 5) init)
                  (-last-item (-iterate fn init (1+ 5)))))) => t)

  (defexamples -fixfn
    ;; Solve cos(x) = x (may not converge without fuzzy comparison).
    (funcall (-fixfn #'cos #'approx=) 0.7) ~> 0.7390851332151607
    ;; Solve x^4 - x - 10 = 0 (converges using `equal' comparison).
    (funcall (-fixfn (lambda (x) (expt (+ x 10) 0.25))) 2.0)
    => 1.8555845286409378
    ;; The sin function has a fixpoint at zero, but it converges too
    ;; slowly and is halted.
    (funcall (-fixfn #'sin #'approx=) 0.1) => '(halted . t))

  (defexamples -prodfn
    (funcall (-prodfn '1+ '1- 'number-to-string) '(1 2 3)) => '(2 1 "3")
    (-map (-prodfn '1+ '1-) '((1 2) (3 4) (5 6) (7 8))) => '((2 1) (4 3) (6 5) (8 7))
    (apply '+ (funcall (-prodfn 'length 'string-to-number) '((1 2 3) "15"))) => 18
    (let ((f '1+)
          (g '1-)
          (ff 'string-to-number)
          (gg 'length)
          (input '(1 2))
          (input2 "foo")
          (input3 '("10" '(1 2 3))))
      (and (equal (funcall (-prodfn f g) input)
                  (funcall (-juxt (-compose f (-partial 'nth 0)) (-compose g (-partial 'nth 1))) input))
           (equal (funcall (-compose (-prodfn f g) (-juxt ff gg)) input2)
                  (funcall (-juxt (-compose f ff) (-compose g gg)) input2))
           (equal (funcall (-compose (-partial 'nth 0) (-prodfn f g)) input)
                  (funcall (-compose f (-partial 'nth 0)) input))
           (equal (funcall (-compose (-partial 'nth 1) (-prodfn f g)) input)
                  (funcall (-compose g (-partial 'nth 1)) input))
           (equal (funcall (-compose (-prodfn f g) (-prodfn ff gg)) input3)
                  (funcall (-prodfn (-compose f ff) (-compose g gg)) input3)))) => t))

(ert-deftest dash--member-fn ()
  "Test `dash--member-fn'."
  (dolist (cmp '(nil equal))
    (let ((-compare-fn cmp))
      (should (eq (dash--member-fn) #'member))))
  (let ((-compare-fn #'eq))
    (should (eq (dash--member-fn) #'memq)))
  (let ((-compare-fn #'eql))
    (should (eq (dash--member-fn) #'memql)))
  (let* ((-compare-fn #'string=)
         (member (dash--member-fn)))
    (should-not (memq member '(member memq memql)))
    (should-not (funcall member "foo" ()))
    (should-not (funcall member "foo" '(bar)))
    (should (equal (funcall member "foo" '(foo bar)) '(foo bar)))
    (should (equal (funcall member "foo" '(bar foo)) '(foo)))))

(ert-deftest dash--assoc-fn ()
  "Test `dash--assoc-fn'."
  (dolist (cmp '(nil equal))
    (let ((-compare-fn cmp))
      (should (eq (dash--assoc-fn) #'assoc))))
  (let ((-compare-fn #'eq))
    (should (eq (dash--assoc-fn) #'assq)))
  (let* ((-compare-fn #'string=)
         (assoc (dash--assoc-fn)))
    (should-not (memq assoc '(assoc assq)))
    (should-not (funcall assoc 'foo ()))
    (should-not (funcall assoc 'foo '(foo)))
    (should-not (funcall assoc 'foo '((bar))))
    (should-not (funcall assoc 'bar '((foo) bar)))
    (should (equal (funcall assoc 'foo '((foo))) '(foo)))
    (should (equal (funcall assoc 'bar '((foo) (bar))) '(bar)))
    (should (equal (funcall assoc 'foo '((foo 1) (foo 2))) '(foo 1)))))

(ert-deftest dash--hash-test-fn ()
  "Test `dash--hash-test-fn'."
  (let ((-compare-fn nil))
    (should (eq (dash--hash-test-fn) #'equal)))
  (dolist (cmp '(equal eq eql))
    (let ((-compare-fn cmp))
      (should (eq (dash--hash-test-fn) cmp))))
  (let ((-compare-fn #'string=))
    (should-not (dash--hash-test-fn))))

(ert-deftest dash--size+ ()
  "Test `dash--size+'."
  (dotimes (a 3)
    (dotimes (b 3)
      (should (= (dash--size+ a b) (+ a b)))))
  (should (= (dash--size+ (- most-positive-fixnum 10) 5)
             (- most-positive-fixnum 5)))
  (should (= (dash--size+ (1- most-positive-fixnum) 0)
             (1- most-positive-fixnum)))
  (dotimes (i 2)
    (should (= (dash--size+ (1- most-positive-fixnum) (1+ i))
               most-positive-fixnum)))
  (dotimes (i 3)
    (should (= (dash--size+ most-positive-fixnum i)
               most-positive-fixnum))))

(ert-deftest dash--numbers<= ()
  "Test `dash--numbers<='."
  (should (dash--numbers<= ()))
  (should (dash--numbers<= '(0)))
  (should (dash--numbers<= '(0 0)))
  (should (dash--numbers<= '(0 1)))
  (should (dash--numbers<= '(0 0 0)))
  (should (dash--numbers<= '(0 0 1)))
  (should (dash--numbers<= '(0 1 1)))
  (should-not (dash--numbers<= '(a)))
  (should-not (dash--numbers<= '(0 a)))
  (should-not (dash--numbers<= '(a 0)))
  (should-not (dash--numbers<= '(0 0 a)))
  (should-not (dash--numbers<= '(0 a 0)))
  (should-not (dash--numbers<= '(1 0)))
  (should-not (dash--numbers<= '(1 0 0)))
  (should-not (dash--numbers<= '(1 1 0))))

(ert-deftest dash--next-lex-perm ()
  "Test `dash--next-lex-perm'."
  (dolist (vecs '(([0])
                  ([0 0])
                  ([0 1] . [1 0])
                  ([0 0 0])
                  ([0 0 1] . [0 1 0])
                  ([0 1 0] . [1 0 0])
                  ([0 1 1] . [1 0 1])
                  ([1 0 0])
                  ([1 0 1] . [1 1 0])
                  ([1 1 0])
                  ([1 1 1])
                  ([0 1 2] . [0 2 1])
                  ([0 2 1] . [1 0 2])
                  ([1 0 2] . [1 2 0])
                  ([1 2 0] . [2 0 1])
                  ([2 0 1] . [2 1 0])
                  ([2 1 0])))
    (let* ((prev (copy-sequence (car vecs)))
           (copy (copy-sequence prev))
           (next (cdr vecs)))
      (should (equal (dash--next-lex-perm prev (length prev)) next))
      ;; Vector should either be updated in place, or left alone.
      (should (equal prev (or next copy))))))

(ert-deftest dash--lex-perms ()
  "Test `dash--lex-perms'."
  (dolist (perms '(([0] (0))
                   ([0 0] (0 0))
                   ([0 1] (0 1) (1 0))
                   ([1 0] (1 0))))
    (should (equal (dash--lex-perms (copy-sequence (car perms)))
                   (cdr perms))))
  (should (equal (dash--lex-perms (vector 0 1) (vector 2 3))
                 '((2 3) (3 2))))
  (should (equal (dash--lex-perms (vector 0 1 2) (vector 5 4 3))
                 '((5 4 3)
                   (5 3 4)
                   (4 5 3)
                   (4 3 5)
                   (3 5 4)
                   (3 4 5)))))

;;; examples.el ends here
