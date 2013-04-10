;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\| => " (0 'font-lock-keyword-face)))); -*-

;; Only the first three examples per function are shown in the docs,
;; so make those good.

(require 'dash)

(defun even? (num) (= 0 (% num 2)))
(defun square (num) (* num num))
(defun three-letters () '("A" "B" "C"))

(defexamples -map
  (-map (lambda (num) (* num num)) '(1 2 3 4)) => '(1 4 9 16)
  (-map 'square '(1 2 3 4)) => '(1 4 9 16)
  (--map (* it it) '(1 2 3 4)) => '(1 4 9 16)
  (--map (concat it it) (three-letters)) => '("AA" "BB" "CC"))

(defexamples -reduce-from
  (-reduce-from '+ 7 '(1 2)) => 10
  (-reduce-from (lambda (memo item) (+ memo item)) 7 '(1 2)) => 10
  (--reduce-from (+ acc it) 7 '(1 2 3)) => 13
  (-reduce-from '+ 7 '()) => 7
  (-reduce-from '+ 7 '(1)) => 8)

(defexamples -reduce
  (-reduce '+ '(1 2)) => 3
  (-reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) => "1-2-3"
  (--reduce (format "%s-%s" acc it) '(1 2 3)) => "1-2-3"
  (-reduce '+ '()) => 0
  (-reduce '+ '(1)) => 1
  (--reduce (format "%s-%s" acc it) '()) => "nil-nil")

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

(defexamples -keep
  (-keep 'cdr '((1 2 3) (4 5) (6))) => '((2 3) (5))
  (-keep (lambda (num) (when (> num 3) (* 10 num))) '(1 2 3 4 5 6)) => '(40 50 60)
  (--keep (when (> it 3) (* 10 it)) '(1 2 3 4 5 6)) => '(40 50 60))

(defexamples -map-when
  (-map-when 'even? 'square '(1 2 3 4)) => '(1 4 3 16)
  (--map-when (> it 2) (* it it) '(1 2 3 4)) => '(1 2 9 16)
  (--map-when (= it 2) 17 '(1 2 3 4)) => '(1 17 3 4)
  (-map-when (lambda (n) (= n 3)) (lambda (n) 0) '(1 2 3 4)) => '(1 2 0 4))

(defexamples -map-indexed
  (-map-indexed (lambda (index item) (- item index)) '(1 2 3 4)) => '(1 1 1 1)
  (--map-indexed (- it it-index) '(1 2 3 4)) => '(1 1 1 1))

(defexamples -flatten
  (-flatten '((1))) => '(1)
  (-flatten '((1 (2 3) (((4 (5))))))) => '(1 2 3 4 5)
  (-flatten '(1 2 (3 . 4))) => '(1 2 (3 . 4)))

(defexamples -concat
  (-concat '(1)) => '(1)
  (-concat '(1) '(2)) => '(1 2)
  (-concat '(1) '(2 3) '(4)) => '(1 2 3 4)
  (-concat) => nil)

(defexamples -mapcat
  (-mapcat 'list '(1 2 3)) => '(1 2 3)
  (-mapcat (lambda (item) (list 0 item)) '(1 2 3)) => '(0 1 0 2 0 3)
  (--mapcat (list 0 it) '(1 2 3)) => '(0 1 0 2 0 3))

(defexamples -cons*
  (-cons* 1 2) => '(1 . 2)
  (-cons* 1 2 3) => '(1 2 . 3)
  (-cons* 1) => 1)

(defexamples -count
  (-count 'even? '(1 2 3 4 5)) => 2
  (--count (< it 4) '(1 2 3 4)) => 3)

(defexamples -any?
  (-any? 'even? '(1 2 3)) => t
  (-any? 'even? '(1 3 5)) => nil
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

(defexamples -each
  (let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s))))) => nil
  (let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s)))) s) => '(3 2 1)
  (let (s) (--each '(1 2 3) (setq s (cons it s))) s) => '(3 2 1)
  (let (s) (--each (reverse (three-letters)) (setq s (cons it s))) s) => '("A" "B" "C"))

(defexamples -each-while
  (let (s) (-each-while '(2 4 5 6) 'even? (lambda (item) (!cons item s))) s) => '(4 2)
  (let (s) (--each-while '(1 2 3 4) (< it 3) (!cons it s)) s) => '(2 1))

(defexamples -dotimes
  (let (s) (-dotimes 3 (lambda (n) (!cons n s))) s) => '(2 1 0)
  (let (s) (--dotimes 5 (!cons it s)) s) => '(4 3 2 1 0))

(defexamples -repeat
  (-repeat 3 :a) => '(:a :a :a)
  (-repeat 1 :a) => '(:a)
  (-repeat 0 :a) => nil
  (-repeat -1 :a) => nil)

(defexamples -slice
  (-slice '(1 2 3 4 5) 1) => '(2 3 4 5)
  (-slice '(1 2 3 4 5) 0 3) => '(1 2 3)
  (-slice '(1 2 3 4 5) 1 -1) => '(2 3 4))

(defexamples -take
  (-take 3 '(1 2 3 4 5)) => '(1 2 3)
  (-take 17 '(1 2 3 4 5)) => '(1 2 3 4 5))

(defexamples -drop
  (-drop 3 '(1 2 3 4 5)) => '(4 5)
  (-drop 17 '(1 2 3 4 5)) => '())

(defexamples -take-while
  (-take-while 'even? '(1 2 3 4)) => '()
  (-take-while 'even? '(2 4 5 6)) => '(2 4)
  (--take-while (< it 4) '(1 2 3 4 3 2 1)) => '(1 2 3))

(defexamples -drop-while
  (-drop-while 'even? '(1 2 3 4)) => '(1 2 3 4)
  (-drop-while 'even? '(2 4 5 6)) => '(5 6)
  (--drop-while (< it 4) '(1 2 3 4 3 2 1)) => '(4 3 2 1))

(defexamples -split-at
  (-split-at 3 '(1 2 3 4 5)) => '((1 2 3) (4 5))
  (-split-at 17 '(1 2 3 4 5)) => '((1 2 3 4 5) nil))

(defexamples -split-with
  (-split-with 'even? '(1 2 3 4)) => '(() (1 2 3 4))
  (-split-with 'even? '(2 4 5 6)) => '((2 4) (5 6))
  (--split-with (< it 4) '(1 2 3 4 3 2 1)) => '((1 2 3) (4 3 2 1)))

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

(defexamples -partition-by
  (-partition-by 'even? '()) => '()
  (-partition-by 'even? '(1 1 2 2 2 3 4 6 8)) => '((1 1) (2 2 2) (3) (4 6 8))
  (--partition-by (< it 3) '(1 2 3 4 3 2 1)) => '((1 2) (3 4 3) (2 1)))

(defexamples -partition-by-header
  (--partition-by-header (= it 1) '(1 2 3 1 2 1 2 3 4)) => '((1 2 3) (1 2) (1 2 3 4))
  (--partition-by-header (> it 0) '(1 2 0 1 0 1 2 3 0)) => '((1 2 0) (1 0) (1 2 3 0))
  (-partition-by-header 'even? '(2 1 1 1 4 1 3 5 6 6 1)) => '((2 1 1 1) (4 1 3 5) (6 6 1)))

(defexamples -group-by
  (-group-by 'even? '()) => '()
  (-group-by 'even? '(1 1 2 2 2 3 4 6 8)) => '((nil . (1 1 3)) (t . (2 2 2 4 6 8)))
  (--group-by (car (split-string it "/")) '("a/b" "c/d" "a/e")) => '(("a" . ("a/b" "a/e")) ("c" . ("c/d"))))

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
  (-zip '(1 2 3 4) '(4 5 6)) => '((1 . 4) (2 . 5) (3 . 6)))

(defexamples -first
  (-first 'even? '(1 2 3)) => 2
  (-first 'even? '(1 3 5)) => nil
  (--first (> it 2) '(1 2 3)) => 3)

(defexamples -last
  (-last 'even? '(1 2 3 4 5 6 3 3 3)) => 6
  (-last 'even? '(1 3 7 5 9)) => nil
  (--last (> (length it) 3) '("a" "looong" "word" "and" "short" "one")) => "short")

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

(defexamples -distinct
  (-distinct '()) => '()
  (-distinct '(1 2 2 4)) => '(1 2 4))

(defexamples -contains?
  (-contains? '(1 2 3) 1) => t
  (-contains? '(1 2 3) 2) => t
  (-contains? '(1 2 3) 4) => nil
  (-contains? '() 1) => nil
  (-contains? '() '()) => nil)

(defexamples -partial
  (funcall (-partial '- 5) 3) => 2
  (funcall (-partial '+ 5 2) 3) => 10)

(unless (version< emacs-version "24")
  (defexamples -rpartial
    (funcall (-rpartial '- 5) 8) => 3
    (funcall (-rpartial '- 5 2) 10) => 3))

(defexamples -applify
  (-map (-applify '+) '((1 1 1) (1 2 3) (5 5 5))) => '(3 6 15)
  (-map (-applify (lambda (a b c) `(,a (,b (,c))))) '((1 1 1) (1 2 3) (5 5 5))) => '((1 (1 (1))) (1 (2 (3))) (5 (5 (5)))))

(defexamples ->
  (-> "Abc") => "Abc"
  (-> "Abc" (concat "def")) => "Abcdef"
  (-> "Abc" (concat "def") (concat "ghi")) => "Abcdefghi"
  (-> 5 square) => 25
  (-> 5 (+ 3) square) => 64)

(defexamples ->>
  (->> "Abc" (concat "def")) => "defAbc"
  (->> "Abc" (concat "def") (concat "ghi")) => "ghidefAbc"
  (->> 5 (- 8)) => 3
  (->> 5 (- 3) square) => 4)

(defexamples -->
  (--> "def" (concat "abc" it "ghi")) => "abcdefghi"
  (--> "def" (concat "abc" it "ghi") (upcase it)) => "ABCDEFGHI"
  (--> "def" (concat "abc" it "ghi") upcase) => "ABCDEFGHI")

(defexamples -when-let
  (-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) => 5
  (--when-let (member :b '(:a :b :c)) (cons :d it)) => '(:d :b :c)
  (--when-let (even? 3) (cat it :a)) => nil)

(defexamples -if-let
  (-if-let (match-index (string-match "d" "abc")) (+ match-index 3) 7) => 7
  (--if-let (even? 4) it nil) => t)

(defexamples !cons
  (let (l) (!cons 5 l) l) => '(5)
  (let ((l '(3))) (!cons 5 l) l) => '(5 3))

(defexamples !cdr
  (let ((l '(3))) (!cdr l) l) => '()
  (let ((l '(3 5))) (!cdr l) l) => '(5))
