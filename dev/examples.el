;;; examples.el --- Examples/tests for dash.el's API  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

;; Only the first three examples per function are shown in the docs,
;; so make those good.

;;; Code:

(require 'dash)

(defun even? (num) (= 0 (% num 2)))
(defun square (num) (* num num))
(defun three-letters () '("A" "B" "C"))

;; Allow approximate comparison of floating-point results, to work
;; around differences in implementation between systems. Use the `~>'
;; symbol instead of `=>' to test the expected and actual values with
;; `approx-equal'
(defvar dash--epsilon 1e-15)
(defun approx-equal (u v)
  (or (= u v)
      (< (/ (abs (- u v))
        (max (abs u) (abs v)))
     dash--epsilon)))

(def-example-group "Maps"
  "Functions in this category take a transforming function, which
is then applied sequentially to each or selected elements of the
input list.  The results are collected in order and returned as
new list."

  (defexamples -map
    (-map (lambda (num) (* num num)) '(1 2 3 4)) => '(1 4 9 16)
    (-map 'square '(1 2 3 4)) => '(1 4 9 16)
    (--map (* it it) '(1 2 3 4)) => '(1 4 9 16)
    (--map (concat it it) (three-letters)) => '("AA" "BB" "CC"))

  (defexamples -map-when
    (-map-when 'even? 'square '(1 2 3 4)) => '(1 4 3 16)
    (--map-when (> it 2) (* it it) '(1 2 3 4)) => '(1 2 9 16)
    (--map-when (= it 2) 17 '(1 2 3 4)) => '(1 17 3 4)
    (-map-when (lambda (n) (= n 3)) (lambda (n) 0) '(1 2 3 4)) => '(1 2 0 4))

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
    (--map-indexed (- it it-index) '(1 2 3 4)) => '(1 1 1 1))

  (defexamples -annotate
    (-annotate '1+ '(1 2 3)) => '((2 . 1) (3 . 2) (4 . 3))
    (-annotate 'length '(("h" "e" "l" "l" "o") ("hello" "world"))) => '((5 . ("h" "e" "l" "l" "o")) (2 . ("hello" "world")))
    (--annotate (< 1 it) '(0 1 2 3)) => '((nil . 0) (nil . 1) (t . 2) (t . 3)))

  (defexamples -splice
    (-splice 'even? (lambda (x) (list x x)) '(1 2 3 4)) => '(1 2 2 3 4 4)
    (--splice 't (list it it) '(1 2 3 4)) => '(1 1 2 2 3 3 4 4)
    (--splice (equal it :magic) '((list of) (magical) (code)) '((foo) (bar) :magic (baz))) => '((foo) (bar) (list of) (magical) (code) (baz)))

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
    (-filter 'even? '(1 2 3 4)) => '(2 4)
    (--filter (= 0 (% it 2)) '(1 2 3 4)) => '(2 4))

  (defexamples -remove
    (-remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) => '(1 3)
    (-remove 'even? '(1 2 3 4)) => '(1 3)
    (--remove (= 0 (% it 2)) '(1 2 3 4)) => '(1 3)
    (let ((mod 2)) (-remove (lambda (num) (= 0 (% num mod))) '(1 2 3 4))) => '(1 3)
    (let ((mod 2)) (--remove (= 0 (% it mod)) '(1 2 3 4))) => '(1 3))

  (defexamples -remove-first
    (-remove-first 'even? '(1 3 5 4 7 8 10)) => '(1 3 5 7 8 10)
    (-remove-first 'stringp '(1 2 "first" "second" "third")) => '(1 2 "second" "third")
    (--remove-first (> it 3) '(1 2 3 4 5 6 7 8 9 10)) => '(1 2 3 5 6 7 8 9 10)
    (-remove-first 'even? '(2 3 4)) => '(3 4)
    (-remove-first 'even? '(3 5 7 4)) => '(3 5 7)
    (-remove-first 'even? '(2)) => nil
    (-remove-first 'even? '(1 3 5 7)) => '(1 3 5 7))

  (defexamples -remove-last
    (-remove-last 'even? '(1 3 5 4 7 8 10 11)) => '(1 3 5 4 7 8 11)
    (-remove-last 'stringp '(1 2 "last" "second" "third")) => '(1 2 "last" "second")
    (--remove-last (> it 3) '(1 2 3 4 5 6 7 8 9 10)) => '(1 2 3 4 5 6 7 8 9)
    ;; the next two tests assert that the input list is not modified #158
    (let ((l '(1 2 3))) (list (--remove-last (< it 2) l) l)) => '((2 3) (1 2 3))
    (let ((l '(1 2 3))) (list (--remove-last (< it 4) l) l)) => '((1 2) (1 2 3)))

  (defexamples -remove-item
    (-remove-item 3 '(1 2 3 2 3 4 5 3)) => '(1 2 2 4 5)
    (-remove-item 'foo '(foo bar baz foo)) => '(bar baz)
    (-remove-item "bob" '("alice" "bob" "eve" "bob" "dave")) => '("alice" "eve" "dave"))

  (defexamples -non-nil
    (-non-nil '(1 nil 2 nil nil 3 4 nil 5 nil)) => '(1 2 3 4 5))

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
    (-take 17 '(1 2 3 4 5)) => '(1 2 3 4 5))

  (defexamples -take-last
    (-take-last 3 '(1 2 3 4 5)) => '(3 4 5)
    (-take-last 17 '(1 2 3 4 5)) => '(1 2 3 4 5)
    (-take-last 1 '(1 2 3 4 5)) => '(5)
    (let ((l '(1 2 3 4 5)))
      (setcar (-take-last 2 l) 1)
      l) => '(1 2 3 4 5))

  (defexamples -drop
    (-drop 3 '(1 2 3 4 5)) => '(4 5)
    (-drop 17 '(1 2 3 4 5)) => '())

  (defexamples -drop-last
    (-drop-last 3 '(1 2 3 4 5)) => '(1 2)
    (-drop-last 17 '(1 2 3 4 5)) => '())

  (defexamples -take-while
    (-take-while 'even? '(1 2 3 4)) => '()
    (-take-while 'even? '(2 4 5 6)) => '(2 4)
    (--take-while (< it 4) '(1 2 3 4 3 2 1)) => '(1 2 3))

  (defexamples -drop-while
    (-drop-while 'even? '(1 2 3 4)) => '(1 2 3 4)
    (-drop-while 'even? '(2 4 5 6)) => '(5 6)
    (--drop-while (< it 4) '(1 2 3 4 3 2 1)) => '(4 3 2 1))

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
    (-keep 'cdr '((1 2 3) (4 5) (6))) => '((2 3) (5))
    (-keep (lambda (num) (when (> num 3) (* 10 num))) '(1 2 3 4 5 6)) => '(40 50 60)
    (--keep (when (> it 3) (* 10 it)) '(1 2 3 4 5 6)) => '(40 50 60))

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
    (-flatten-n 0 '(((1 2) (3 4)))) => '(((1 2) (3 4))))

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
  "Functions reducing lists into single value."

  (defexamples -reduce-from
    (-reduce-from '- 10 '(1 2 3)) => 4
    (-reduce-from (lambda (memo item)
                    (concat "(" memo " - " (int-to-string item) ")")) "10" '(1 2 3)) => "(((10 - 1) - 2) - 3)"
                    (--reduce-from (concat acc " " it) "START" '("a" "b" "c")) => "START a b c"
                    (-reduce-from '+ 7 '()) => 7
                    (-reduce-from '+ 7 '(1)) => 8)

  (defexamples -reduce-r-from
    (-reduce-r-from '- 10 '(1 2 3)) => -8
    (-reduce-r-from (lambda (item memo)
                      (concat "(" (int-to-string item) " - " memo ")")) "10" '(1 2 3)) => "(1 - (2 - (3 - 10)))"
                      (--reduce-r-from (concat it " " acc) "END" '("a" "b" "c")) => "a b c END"
                      (-reduce-r-from '+ 7 '()) => 7
                      (-reduce-r-from '+ 7 '(1)) => 8)

  (defexamples -reduce
    (-reduce '- '(1 2 3 4)) => -8
    (-reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) => "1-2-3"
    (--reduce (format "%s-%s" acc it) '(1 2 3)) => "1-2-3"
    (-reduce '+ '()) => 0
    (-reduce '+ '(1)) => 1
    (--reduce (format "%s-%s" acc it) '()) => "nil-nil")

  (defexamples -reduce-r
    (-reduce-r '- '(1 2 3 4)) => -2
    (-reduce-r (lambda (item memo) (format "%s-%s" memo item)) '(1 2 3)) => "3-2-1"
    (--reduce-r (format "%s-%s" acc it) '(1 2 3)) => "3-2-1"
    (-reduce-r '+ '()) => 0
    (-reduce-r '+ '(1)) => 1
    (--reduce-r (format "%s-%s" it acc) '()) => "nil-nil")

  (defexamples -count
    (-count 'even? '(1 2 3 4 5)) => 2
    (--count (< it 4) '(1 2 3 4)) => 3)

  (defexamples -sum
    (-sum '()) => 0
    (-sum '(1)) => 1
    (-sum '(1 2 3 4)) => 10)

  (defexamples -product
    (-product '()) => 1
    (-product '(1)) => 1
    (-product '(1 2 3 4)) => 24)

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
    (--max-by (> (length it) (length other)) '((1 2 3) (2) (3 2))) => '(1 2 3)))

(def-example-group "Unfolding"
  "Operations dual to reductions, building lists from seed value rather than consuming a list to produce a single value."

  (defexamples -iterate
    (-iterate '1+ 1 10) => '(1 2 3 4 5 6 7 8 9 10)
    (-iterate (lambda (x) (+ x x)) 2 5) => '(2 4 8 16 32)
    (--iterate (* it it) 2 5) => '(2 4 16 256 65536))

  (defexamples -unfold
    (-unfold (lambda (x) (unless (= x 0) (cons x (1- x)))) 10) => '(10 9 8 7 6 5 4 3 2 1)
    (--unfold (when it (cons it (cdr it))) '(1 2 3 4)) => '((1 2 3 4) (2 3 4) (3 4) (4))
    (--unfold (when it (cons it (butlast it))) '(1 2 3 4)) => '((1 2 3 4) (1 2 3) (1 2) (1))))

(def-example-group "Predicates" nil
  (defexamples -any?
    (-any? 'even? '(1 2 3)) => t
    (-any? 'even? '(1 3 5)) => nil
    (-any? 'null '(1 3 5)) => nil
    (-any? 'null '(1 3 ())) => t
    (--any? (= 0 (% it 2)) '(1 2 3)) => t)

  (defexamples -all?
    (-all? 'even? '(1 2 3)) => nil
    (-all? 'even? '(2 4 6)) => t
    (--all? (= 0 (% it 2)) '(2 4 6)) => t)

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
    (-contains? '(1 2 3) 1) => t
    (-contains? '(1 2 3) 2) => t
    (-contains? '(1 2 3) 4) => nil
    (-contains? '() 1) => nil
    (-contains? '() '()) => nil)

  (defexamples -same-items?
    (-same-items? '(1 2 3) '(1 2 3)) => t
    (-same-items? '(1 2 3) '(3 2 1)) => t
    (-same-items? '(1 2 3) '(1 2 3 4)) => nil
    (-same-items? '((a . 1) (b . 2)) '((a . 1) (b . 2))) => t
    (-same-items? '(1 2 3) '(2 3 1)) => t)

  (defexamples -is-prefix?
    (-is-prefix? '(1 2 3) '(1 2 3 4 5)) => t
    (-is-prefix? '(1 2 3 4 5) '(1 2 3)) => nil
    (-is-prefix? '(1 3) '(1 2 3 4 5)) => nil
    (-is-prefix? '(1 2 3) '(1 2 4 5)) => nil)

  (defexamples -is-suffix?
    (-is-suffix? '(3 4 5) '(1 2 3 4 5)) => t
    (-is-suffix? '(1 2 3 4 5) '(3 4 5)) => nil
    (-is-suffix? '(3 5) '(1 2 3 4 5)) => nil
    (-is-suffix? '(3 4 5) '(1 2 3 5)) => nil
    (let ((l '(1 2 3)))
      (list (-is-suffix? '(3) l)
            l)) => '(t (1 2 3)))

  (defexamples -is-infix?
    (-is-infix? '(1 2 3) '(1 2 3 4 5)) => t
    (-is-infix? '(2 3 4) '(1 2 3 4 5)) => t
    (-is-infix? '(3 4 5) '(1 2 3 4 5)) => t
    (-is-infix? '(2 3 4) '(1 2 4 5)) => nil
    (-is-infix? '(2 4) '(1 2 3 4 5)) => nil))

(def-example-group "Partitioning"
  "Functions partitioning the input list into a list of lists."

  (defexamples -split-at
    (-split-at 3 '(1 2 3 4 5)) => '((1 2 3) (4 5))
    (-split-at 17 '(1 2 3 4 5)) => '((1 2 3 4 5) nil))

  (defexamples -split-with
    (-split-with 'even? '(1 2 3 4)) => '(() (1 2 3 4))
    (-split-with 'even? '(2 4 5 6)) => '((2 4) (5 6))
    (--split-with (< it 4) '(1 2 3 4 3 2 1)) => '((1 2 3) (4 3 2 1)))

  (defexamples -split-on
    (-split-on '| '(Nil | Leaf a | Node [Tree a])) => '((Nil) (Leaf a) (Node [Tree a]))
    (-split-on ':endgroup '("a" "b" :endgroup "c" :endgroup "d" "e")) => '(("a" "b") ("c") ("d" "e"))
    (-split-on ':endgroup '("a" "b" :endgroup :endgroup "d" "e")) => '(("a" "b") ("d" "e"))
    (-split-on ':endgroup '("a" "b" :endgroup "c" :endgroup)) => '(("a" "b") ("c"))
    (-split-on ':endgroup '("a" "b" :endgroup :endgroup :endgroup "d" "e")) => '(("a" "b") ("d" "e"))
    (-split-on ':endgroup '(:endgroup "c" :endgroup "d" "e")) => '(("c") ("d" "e"))
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
    (-partition-in-steps 2 1 '(1)) => '())

  (defexamples -partition-all-in-steps
    (-partition-all-in-steps 2 1 '(1 2 3 4)) => '((1 2) (2 3) (3 4) (4))
    (-partition-all-in-steps 3 2 '(1 2 3 4)) => '((1 2 3) (3 4))
    (-partition-all-in-steps 3 2 '(1 2 3 4 5)) => '((1 2 3) (3 4 5) (5))
    (-partition-all-in-steps 2 1 '(1)) => '((1)))

  (defexamples -partition-by
    (-partition-by 'even? '()) => '()
    (-partition-by 'even? '(1 1 2 2 2 3 4 6 8)) => '((1 1) (2 2 2) (3) (4 6 8))
    (--partition-by (< it 3) '(1 2 3 4 3 2 1)) => '((1 2) (3 4 3) (2 1)))

  (defexamples -partition-by-header
    (--partition-by-header (= it 1) '(1 2 3 1 2 1 2 3 4)) => '((1 2 3) (1 2) (1 2 3 4))
    (--partition-by-header (> it 0) '(1 2 0 1 0 1 2 3 0)) => '((1 2 0) (1 0) (1 2 3 0))
    (-partition-by-header 'even? '(2 1 1 1 4 1 3 5 6 6 1)) => '((2 1 1 1) (4 1 3 5) (6 6 1)))

  (defexamples -partition-after-pred
    (-partition-after-pred #'oddp '()) => '()
    (-partition-after-pred #'oddp '(1)) => '((1))
    (-partition-after-pred #'oddp '(0 1)) => '((0 1))
    (-partition-after-pred #'oddp '(1 1)) => '((1) (1))
    (-partition-after-pred #'oddp '(0 0 0 1 0 1 1 0 1)) => '((0 0 0 1) (0 1) (1) (0 1)))

  (defexamples -partition-before-pred
    (-partition-before-pred #'oddp '()) => '()
    (-partition-before-pred #'oddp '(1)) => '((1))
    (-partition-before-pred #'oddp '(0 1)) => '((0) (1))
    (-partition-before-pred #'oddp '(1 1)) => '((1) (1))
    (-partition-before-pred #'oddp '(0 1 0)) => '((0) (1 0))
    (-partition-before-pred #'oddp '(0 0 0 1 0 1 1 0 1)) => '((0 0 0) (1 0) (1) (1 0) (1)))

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
  "Return indices of elements based on predicates, sort elements by indices etc."

  (defexamples -elem-index
    (-elem-index 2 '(6 7 8 2 3 4)) => 3
    (-elem-index "bar" '("foo" "bar" "baz")) => 1
    (-elem-index '(1 2) '((3) (5 6) (1 2) nil)) => 2)

  (defexamples -elem-indices
    (-elem-indices 2 '(6 7 8 2 3 4 2 1)) => '(3 6)
    (-elem-indices "bar" '("foo" "bar" "baz")) => '(1)
    (-elem-indices '(1 2) '((3) (1 2) (5 6) (1 2) nil)) => '(1 3))

  (defexamples -find-index
    (-find-index 'even? '(2 4 1 6 3 3 5 8)) => 0
    (--find-index (< 5 it) '(2 4 1 6 3 3 5 8)) => 3
    (-find-index (-partial 'string-lessp "baz") '("bar" "foo" "baz")) => 1)

  (defexamples -find-last-index
    (-find-last-index 'even? '(2 4 1 6 3 3 5 8)) => 7
    (--find-last-index (< 5 it) '(2 7 1 6 3 8 5 2)) => 5
    (-find-last-index (-partial 'string-lessp "baz") '("q" "foo" "baz")) => 1)

  (defexamples -find-indices
    (-find-indices 'even? '(2 4 1 6 3 3 5 8)) => '(0 1 3 7)
    (--find-indices (< 5 it) '(2 4 1 6 3 3 5 8)) => '(3 7)
    (-find-indices (-partial 'string-lessp "baz") '("bar" "foo" "baz")) => '(1))

  (defexamples -grade-up
    (-grade-up '< '(3 1 4 2 1 3 3)) => '(1 4 3 0 5 6 2)
    (let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-up '< l) l)) => '(1 1 2 3 3 3 4))

  (defexamples -grade-down
    (-grade-down '< '(3 1 4 2 1 3 3)) => '(2 0 5 6 3 1 4)
    (let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-down '< l) l)) => '(4 3 3 3 2 1 1)))

(def-example-group "Set operations"
  "Operations pretending lists are sets."

  (defexamples -union
    (-union '(1 2 3) '(3 4 5))  => '(1 2 3 4 5)
    (-union '(1 2 3 4) '())  => '(1 2 3 4)
    (-union '(1 1 2 2) '(3 2 1))  => '(1 1 2 2 3))

  (defexamples -difference
    (-difference '() '()) => '()
    (-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
    (-difference '(1 2 3 4) '(3 4 5 6)) => '(1 2))

  (defexamples -intersection
    (-intersection '() '()) => '()
    (-intersection '(1 2 3) '(4 5 6)) => '()
    (-intersection '(1 2 3 4) '(3 4 5 6)) => '(3 4))

  (defexamples -powerset
    (-powerset '()) => '(nil)
    (-powerset '(x y z)) => '((x y z) (x y) (x z) (x) (y z) (y) (z) nil))

  (defexamples -permutations
    (-permutations '()) => '(nil)
    (-permutations '(1 2)) => '((1 2) (2 1))
    (-permutations '(a b c)) => '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a)))

  (defexamples -distinct
    (-distinct '()) => '()
    (-distinct '(1 2 2 4)) => '(1 2 4)))

(def-example-group "Other list operations"
  "Other list functions not fit to be classified elsewhere."

  (defexamples -rotate
    (-rotate 3 '(1 2 3 4 5 6 7)) => '(5 6 7 1 2 3 4)
    (-rotate -3 '(1 2 3 4 5 6 7)) => '(4 5 6 7 1 2 3))

  (defexamples -repeat
    (-repeat 3 :a) => '(:a :a :a)
    (-repeat 1 :a) => '(:a)
    (-repeat 0 :a) => nil
    (-repeat -1 :a) => nil)

  (defexamples -cons*
    (-cons* 1 2) => '(1 . 2)
    (-cons* 1 2 3) => '(1 2 . 3)
    (-cons* 1) => 1
    (-cons* 1 2 3 4) => '(1 2 3 . 4)
    (apply '-cons* (number-sequence 1 10)) => '(1 2 3 4 5 6 7 8 9 . 10))

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
    (-interleave '(1 2 3) '("a" "b" "c" "d")) => '(1 "a" 2 "b" 3 "c"))

  (defexamples -zip-with
    (-zip-with '+ '(1 2 3) '(4 5 6)) => '(5 7 9)
    (-zip-with 'cons '(1 2 3) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (--zip-with (concat it " and " other) '("Batman" "Jekyll") '("Robin" "Hyde")) => '("Batman and Robin" "Jekyll and Hyde"))

  (defexamples -zip
    (-zip '(1 2 3) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2 3) '(4 5 6 7)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2 3 4) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6))
    (-zip '(1 2 3) '(4 5 6) '(7 8 9)) => '((1 4 7) (2 5 8) (3 6 9))
    (-zip '(1 2) '(3 4 5) '(6)) => '((1 3 6)))

  (defexamples -zip-fill
    (-zip-fill 0 '(1 2 3 4 5) '(6 7 8 9)) => '((1 . 6) (2 . 7) (3 . 8) (4 . 9) (5 . 0)))

  (defexamples -unzip
    (-unzip (-zip '(1 2 3) '(a b c) '("e" "f" "g"))) => '((1 2 3) (a b c) ("e" "f" "g"))
    (-unzip '((1 2) (3 4) (5 6) (7 8) (9 10))) => '((1 3 5 7 9) (2 4 6 8 10)))

  (defexamples -cycle
    (-take 5 (-cycle '(1 2 3))) => '(1 2 3 1 2)
    (-take 7 (-cycle '(1 "and" 3))) => '(1 "and" 3 1 "and" 3 1)
    (-zip (-cycle '(1 2 3)) '(1 2)) => '((1 . 1) (2 . 2))
    (-zip-with 'cons (-cycle '(1 2 3)) '(1 2)) => '((1 . 1) (2 . 2))
    (-map (-partial '-take 5) (-split-at 5 (-cycle '(1 2 3)))) => '((1 2 3 1 2) (3 1 2 3 1)))

  (defexamples -pad
    (-pad 0 '()) => '(())
    (-pad 0 '(1)) => '((1))
    (-pad 0 '(1 2 3) '(4 5)) => '((1 2 3) (4 5 0))
    (-pad nil '(1 2 3) '(4 5) '(6 7 8 9 10)) => '((1 2 3 nil nil) (4 5 nil nil nil) (6 7 8 9 10))
    (-pad 0 '(1 2) '(3 4)) => '((1 2) (3 4)))

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
    (-first 'even? '(1 2 3)) => 2
    (-first 'even? '(1 3 5)) => nil
    (-first 'null '(1 3 5)) => nil
    (-first 'null '(1 3 ())) => nil
    (--first (> it 2) '(1 2 3)) => 3)

  (defexamples -some
    (-some 'even? '(1 2 3)) => t
    (-some 'null '(1 2 3)) => nil
    (-some 'null '(1 2 ())) => t
    (--some (member 'foo it) '((foo bar) (baz))) => '(foo bar)
    (--some (plist-get it :bar) '((:foo 1 :bar 2) (:baz 3))) => 2)

  (defexamples -last
    (-last 'even? '(1 2 3 4 5 6 3 3 3)) => 6
    (-last 'even? '(1 3 7 5 9)) => nil
    (--last (> (length it) 3) '("a" "looong" "word" "and" "short" "one")) => "short")

  (defexamples -first-item
    (-first-item '(1 2 3)) => 1
    (-first-item nil) => nil
    (let ((list (list 1 2 3))) (setf (-first-item list) 5) list) => '(5 2 3))

  (defexamples -last-item
    (-last-item '(1 2 3)) => 3
    (-last-item nil) => nil
    (let ((list (list 1 2 3))) (setf (-last-item list) 5) list) => '(1 2 5))

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
    (-list 1 2 3) => '(1 2 3)
    (-list '(1 2 3)) => '(1 2 3)
    (-list '((1) (2))) => '((1) (2)))

  (defexamples -fix
    (-fix (lambda (l) (-non-nil (--mapcat (-split-at (/ (length it) 2) it) l))) '((1 2 3 4 5 6))) => '((1) (2) (3) (4) (5) (6))
    (let ((data '(("starwars" "scifi")
                  ("jedi" "starwars" "warrior"))))
      (--fix (-uniq (--mapcat (cons it (cdr (assoc it data))) it)) '("jedi" "book"))) => '("jedi" "starwars" "warrior" "scifi" "book")))

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
                      '((elips-mode (foo (bar . booze)) (baz . qux)) (c-mode (foo . bla) (bum . bam))))
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
                 '((elips-mode (foo (bar . booze)) (baz . qux)) (c-mode (foo . bla) (bum . bam)))))
    => "{elips-mode : {foo : {bar -> booze}, baz -> qux}, c-mode : {foo -> bla, bum -> bam}}")

  (defexamples -clone
    (let* ((a '(1 2 3)) (b (-clone a))) (nreverse a) b) => '(1 2 3)))

(def-example-group "Threading macros" nil
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
    (-some--> '(1 3 5) (-filter 'even? it) (append it it) (-map 'square it)) => nil
    (-some--> '(2 4 6) (-filter 'even? it) (append it it) (-map 'square it)) => '(4 16 36 4 16 36)))

(def-example-group "Binding"
  "Convenient versions of `let` and `let*` constructs combined with flow control."

  (defexamples -when-let
    (-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) => 5
    (-when-let ((&plist :foo foo) (list :foo "foo")) foo) => "foo"
    (-when-let ((&plist :foo foo) (list :bar "bar")) foo) => nil
    (--when-let (member :b '(:a :b :c)) (cons :d it)) => '(:d :b :c)
    (--when-let (even? 3) (cat it :a)) => nil)

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
    (-let [[a b c d] [1 2 3]] t) !!> args-out-of-range
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
      (list a b c d x y)) => '(1 2 3 4 (1 2) (3 4)))

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
      (-let* (((a . b) a)
              ((c . d) b)) ;; b here comes from above binding
        (list a b c d))) => '(1 (2 3) 2 (3))
    (-let* ((a "foo") (b a)) (list a b)) => '("foo" "foo"))

  (defexamples -lambda
    (-map (-lambda ((x y)) (+ x y)) '((1 2) (3 4) (5 6))) => '(3 7 11)
    (-map (-lambda ([x y]) (+ x y)) '([1 2] [3 4] [5 6])) => '(3 7 11)
    (funcall (-lambda ((_ . a) (_ . b)) (-concat a b)) '(1 2 3) '(4 5 6)) => '(2 3 5 6)
    (-map (-lambda ((&plist :a a :b b)) (+ a b)) '((:a 1 :b 2) (:a 3 :b 4) (:a 5 :b 6))) => '(3 7 11)
    (-map (-lambda (x) (let ((k (car x)) (v (cadr x))) (+ k v))) '((1 2) (3 4) (5 6))) => '(3 7 11)
    (funcall (-lambda ((a) (b)) (+ a b)) '(1 2 3) '(4 5 6)) => 5
    (-lambda a t) !!> wrong-type-argument
    (funcall (-lambda (a b) (+ a b)) 1 2) => 3
    (funcall (-lambda (a (b c)) (+ a b c)) 1 (list 2 3)) => 6))

(def-example-group "Side-effects"
  "Functions iterating over lists for side-effect only."

  (defexamples -each
    (let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s))))) => nil
    (let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s)))) s) => '(3 2 1)
    (let (s) (--each '(1 2 3) (setq s (cons it s))) s) => '(3 2 1)
    (let (s) (--each (reverse (three-letters)) (setq s (cons it s))) s) => '("A" "B" "C"))

  (defexamples -each-while
    (let (s) (-each-while '(2 4 5 6) 'even? (lambda (item) (!cons item s))) s) => '(4 2)
    (let (s) (--each-while '(1 2 3 4) (< it 3) (!cons it s)) s) => '(2 1))

  (defexamples -each-indexed
    (let (s) (-each-indexed '(a b c) (lambda (index item) (setq s (cons (list item index) s)))) s) => '((c 2) (b 1) (a 0))
    (let (s) (--each-indexed '(a b c) (setq s (cons (list it it-index) s))) s) => '((c 2) (b 1) (a 0)))

  (defexamples -dotimes
    (let (s) (-dotimes 3 (lambda (n) (!cons n s))) s) => '(2 1 0)
    (let (s) (--dotimes 5 (!cons it s)) s) => '(4 3 2 1 0))

  (defexamples -doto
    (-doto '(1 2 3) (!cdr) (!cdr)) => '(3)
    (-doto '(1 . 2) (setcar 3) (setcdr 4)) => '(3 . 4)))

(def-example-group "Destructive operations" nil
  (defexamples !cons
    (let (l) (!cons 5 l) l) => '(5)
    (let ((l '(3))) (!cons 5 l) l) => '(5 3))

  (defexamples !cdr
    (let ((l '(3))) (!cdr l) l) => '()
    (let ((l '(3 5))) (!cdr l) l) => '(5)))

(def-example-group "Function combinators"
  "These combinators require Emacs 24 for its lexical scope. So they are offered in a separate package: `dash-functional`."

  (defexamples -partial
    (funcall (-partial '- 5) 3) => 2
    (funcall (-partial '+ 5 2) 3) => 10)

  (unless (version< emacs-version "24")
    (defexamples -rpartial
      (funcall (-rpartial '- 5) 8) => 3
      (funcall (-rpartial '- 5 2) 10) => 3)

    (defexamples -juxt
      (funcall (-juxt '+ '-) 3 5) => '(8 -2)
      (-map (-juxt 'identity 'square) '(1 2 3)) => '((1 1) (2 4) (3 9)))

    (defexamples -compose
      (funcall (-compose 'square '+) 2 3) => (square (+ 2 3))
      (funcall (-compose 'identity 'square) 3) => (square 3)
      (funcall (-compose 'square 'identity) 3) => (square 3)
      (funcall (-compose (-compose 'not 'even?) 'square) 3) => (funcall (-compose 'not (-compose 'even? 'square)) 3)))

  (defexamples -applify
    (-map (-applify '+) '((1 1 1) (1 2 3) (5 5 5))) => '(3 6 15)
    (-map (-applify (lambda (a b c) `(,a (,b (,c))))) '((1 1 1) (1 2 3) (5 5 5))) => '((1 (1 (1))) (1 (2 (3))) (5 (5 (5))))
    (funcall (-applify '<) '(3 6)) => t)

  (unless (version< emacs-version "24")
    (defexamples -on
      (-sort (-on '< 'length) '((1 2 3) (1) (1 2))) => '((1) (1 2) (1 2 3))
      (-min-by (-on '> 'length) '((1 2 3) (4) (1 2))) => '(4)
      (-min-by (-on 'string-lessp 'int-to-string) '(2 100 22)) => 22
      (-max-by (-on '> 'car) '((2 2 3) (3) (1 2))) => '(3)
      (-sort (-on 'string-lessp 'int-to-string) '(10 12 1 2 22)) => '(1 10 12 2 22)
      (funcall (-on '+ '1+) 1 2) => 5
      (funcall (-on '+ 'identity) 1 2) => 3
      (funcall (-on '* 'length) '(1 2 3) '(4 5)) => 6
      (funcall (-on (-on '+ 'length) 'cdr) '(1 2 3) '(4 5)) => 3
      (funcall (-on '+ (lambda (x) (length (cdr x)))) '(1 2 3) '(4 5)) => 3
      (-sort (-on '< 'car) '((3 2 5) (2) (1 2))) => '((1 2) (2) (3 2 5))
      (-sort (-on '< (lambda (x) (length x))) '((1 2 3) (1) (1 2))) => '((1) (1 2) (1 2 3))
      (-sort (-on (-on '< 'car) 'cdr) '((0 3) (2 1) (4 2 8))) => '((2 1) (4 2 8) (0 3))
      (-sort (-on '< 'cadr) '((0 3) (2 1) (4 2 8))) => '((2 1) (4 2 8) (0 3)))

    (defexamples -flip
      (funcall (-flip '<) 2 1) => t
      (funcall (-flip '-) 3 8) => 5
      (-sort (-flip '<) '(4 3 6 1)) => '(6 4 3 1))

    (defexamples -const
      (funcall (-const 2) 1 3 "foo") => 2
      (-map (-const 1) '("a" "b" "c" "d")) => '(1 1 1 1)
      (-sum (-map (-const 1) '("a" "b" "c" "d"))) => 4)

    (defexamples -cut
      (funcall (-cut list 1 <> 3 <> 5) 2 4) => '(1 2 3 4 5)
      (-map (-cut funcall <> 5) '(1+ 1- (lambda (x) (/ 1.0 x)))) => '(6 4 0.2)
      (-map (-cut <> 1 2 3) (list 'list 'vector 'string)) => '((1 2 3) [1 2 3] "")
      (-filter (-cut < <> 5) '(1 3 5 7 9)) => '(1 3))

    (defexamples -not
      (funcall (-not 'even?) 5) => t
      (-filter (-not (-partial '< 4)) '(1 2 3 4 5 6 7 8)) => '(1 2 3 4))

    (defexamples -orfn
      (-filter (-orfn 'even? (-partial (-flip '<) 5)) '(1 2 3 4 5 6 7 8 9 10)) => '(1 2 3 4 6 8 10)
      (funcall (-orfn 'stringp 'even?) "foo") => t)

    (defexamples -andfn
      (funcall (-andfn (-cut < <> 10) 'even?) 6) => t
      (funcall (-andfn (-cut < <> 10) 'even?) 12) => nil
      (-filter (-andfn (-not 'even?) (-cut >= 5 <>)) '(1 2 3 4 5 6 7 8 9 10)) => '(1 3 5))

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
      ;; Find solution to cos(x) = x (may not converge without fuzzy comparison)
      (funcall (-fixfn 'cos 'approx-equal) 0.7) ~> 0.7390851332151607
      ;; Find solution to x^4 - x - 10 = 0 (converges using 'equal comparison)
      (funcall (-fixfn (lambda (x) (expt (+ x 10) 0.25))) 2.0) => 1.8555845286409378
      ;; The sin function has a fixpoint at zero, but it converges too slowly and is halted
      (funcall (-fixfn 'sin 'approx-equal) 0.1) => '(halted . t))

    (defexamples -prodfn
      (funcall (-prodfn '1+ '1- 'int-to-string) '(1 2 3)) => '(2 1 "3")
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
                    (funcall (-prodfn (-compose f ff) (-compose g gg)) input3)))) => t)))

;; Local Variables:
;; eval: (font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => \\| !!> \\| ~>" (0 'font-lock-keyword-face)) ("(defexamples[[:blank:]]+\\(.*\\)" (1 'font-lock-function-name-face))))
;; End:

;;; examples.el ends here
