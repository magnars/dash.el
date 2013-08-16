# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el [![Build Status](https://secure.travis-ci.org/magnars/dash.el.png)](http://travis-ci.org/magnars/dash.el)

A modern list api for Emacs. No 'cl required.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

    M-x package-install dash

Or you can just dump `dash.el` in your load path somewhere.

## Functions

* [-map](#-map-fn-list) `(fn list)`
* [-reduce-from](#-reduce-from-fn-initial-value-list) `(fn initial-value list)`
* [-reduce-r-from](#-reduce-r-from-fn-initial-value-list) `(fn initial-value list)`
* [-reduce](#-reduce-fn-list) `(fn list)`
* [-reduce-r](#-reduce-r-fn-list) `(fn list)`
* [-filter](#-filter-pred-list) `(pred list)`
* [-remove](#-remove-pred-list) `(pred list)`
* [-keep](#-keep-fn-list) `(fn list)`
* [-map-when](#-map-when-pred-rep-list) `(pred rep list)`
* [-map-indexed](#-map-indexed-fn-list) `(fn list)`
* [-flatten](#-flatten-l) `(l)`
* [-concat](#-concat-rest-lists) `(&rest lists)`
* [-mapcat](#-mapcat-fn-list) `(fn list)`
* [-cons*](#-cons-rest-args) `(&rest args)`
* [-count](#-count-pred-list) `(pred list)`
* [-sum](#-sum-list) `(list)`
* [-product](#-product-list) `(list)`
* [-min](#-min-list) `(list)`
* [-min-by](#-min-by-comparator-list) `(comparator list)`
* [-max](#-max-list) `(list)`
* [-max-by](#-max-by-comparator-list) `(comparator list)`
* [-any?](#-any-pred-list) `(pred list)`
* [-all?](#-all-pred-list) `(pred list)`
* [-none?](#-none-pred-list) `(pred list)`
* [-only-some?](#-only-some-pred-list) `(pred list)`
* [-each](#-each-list-fn) `(list fn)`
* [-each-while](#-each-while-list-pred-fn) `(list pred fn)`
* [-dotimes](#-dotimes-num-fn) `(num fn)`
* [-repeat](#-repeat-n-x) `(n x)`
* [-slice](#-slice-list-from-optional-to) `(list from &optional to)`
* [-take](#-take-n-list) `(n list)`
* [-drop](#-drop-n-list) `(n list)`
* [-take-while](#-take-while-pred-list) `(pred list)`
* [-drop-while](#-drop-while-pred-list) `(pred list)`
* [-split-at](#-split-at-n-list) `(n list)`
* [-rotate](#-rotate-n-list) `(n list)`
* [-insert-at](#-insert-at-n-x-list) `(n x list)`
* [-split-with](#-split-with-pred-list) `(pred list)`
* [-separate](#-separate-pred-list) `(pred list)`
* [-partition](#-partition-n-list) `(n list)`
* [-partition-all-in-steps](#-partition-all-in-steps-n-step-list) `(n step list)`
* [-partition-in-steps](#-partition-in-steps-n-step-list) `(n step list)`
* [-partition-all](#-partition-all-n-list) `(n list)`
* [-partition-by](#-partition-by-fn-list) `(fn list)`
* [-partition-by-header](#-partition-by-header-fn-list) `(fn list)`
* [-group-by](#-group-by-fn-list) `(fn list)`
* [-interpose](#-interpose-sep-list) `(sep list)`
* [-interleave](#-interleave-rest-lists) `(&rest lists)`
* [-zip-with](#-zip-with-fn-list1-list2) `(fn list1 list2)`
* [-zip](#-zip-list1-list2) `(list1 list2)`
* [-first](#-first-pred-list) `(pred list)`
* [-last](#-last-pred-list) `(pred list)`
* [-first-item](#-first-item-list) `(list)`
* [-last-item](#-last-item-list) `(list)`
* [-union](#-union-list-list2) `(list list2)`
* [-difference](#-difference-list-list2) `(list list2)`
* [-intersection](#-intersection-list-list2) `(list list2)`
* [-distinct](#-distinct-list) `(list)`
* [-contains?](#-contains-list-element) `(list element)`
* [-sort](#-sort-predicate-list) `(predicate list)`
* [->](#--x-optional-form-rest-more) `(x &optional form &rest more)`
* [->>](#--x-form-rest-more) `(x form &rest more)`
* [-->](#---x-form-rest-more) `(x form &rest more)`
* [-when-let](#-when-let-var-val-rest-body) `(var-val &rest body)`
* [-when-let*](#-when-let-vars-vals-rest-body) `(vars-vals &rest body)`
* [-if-let](#-if-let-var-val-then-optional-else) `(var-val then &optional else)`
* [-if-let*](#-if-let-vars-vals-then-optional-else) `(vars-vals then &optional else)`
* [-partial](#-partial-fn-rest-args) `(fn &rest args)`
* [-rpartial](#-rpartial-fn-rest-args) `(fn &rest args)`
* [-juxt](#-juxt-rest-fns) `(&rest fns)`
* [-applify](#-applify-fn) `(fn)`
* [-on](#-on-operator-transformer) `(operator transformer)`
* [-flip](#-flip-func) `(func)`
* [-const](#-const-c) `(c)`
* [-cut](#-cut-rest-params) `(&rest params)`
* [-not](#-not-pred) `(pred)`
* [-orfn](#-orfn-rest-preds) `(&rest preds)`
* [-andfn](#-andfn-rest-preds) `(&rest preds)`
* [!cons](#-cons-car-cdr) `(car cdr)`
* [!cdr](#-cdr-list) `(list)`

There are also anaphoric versions of these functions where that makes sense,
prefixed with two dashes instead of one.

## Anaphoric functions

While `-map` takes a function to map over the list, you can also use
the anaphoric form with double dashes - which will then be executed
with `it` exposed as the list item. Here's an example:

```cl
(-map (lambda (n) (* n n)) '(1 2 3 4)) ;; normal version

(--map (* it it) '(1 2 3 4)) ;; anaphoric version
```

of course the original can also be written like

```cl
(defun square (n) (* n n))

(-map 'square '(1 2 3 4))
```

which demonstrates the usefulness of both versions.

## Documentation and examples

### -map `(fn list)`

Returns a new list consisting of the result of applying `fn` to the items in `list`.

```cl
(-map (lambda (num) (* num num)) '(1 2 3 4)) ;; => '(1 4 9 16)
(-map 'square '(1 2 3 4)) ;; => '(1 4 9 16)
(--map (* it it) '(1 2 3 4)) ;; => '(1 4 9 16)
```

### -reduce-from `(fn initial-value list)`

Returns the result of applying `fn` to `initial-value` and the
first item in `list`, then applying `fn` to that result and the 2nd
item, etc. If `list` contains no items, returns `initial-value` and
`fn` is not called.

In the anaphoric form `--reduce-from`, the accumulated value is
exposed as `acc`.

```cl
(-reduce-from '- 10 '(1 2 3)) ;; => 4
(-reduce-from (lambda (memo item) (concat "(" memo " - " (int-to-string item) ")")) "10" '(1 2 3)) ;; => "(((10 - 1) - 2) - 3)"
(--reduce-from (concat acc " " it) "START" '("a" "b" "c")) ;; => "START a b c"
```

### -reduce-r-from `(fn initial-value list)`

Replace conses with `fn`, nil with `initial-value` and evaluate
the resulting expression. If `list` is empty, `initial-value` is
returned and `fn` is not called.

Note: this function works the same as `-reduce-from` but the
operation associates from right instead of from left.

```cl
(-reduce-r-from '- 10 '(1 2 3)) ;; => -8
(-reduce-r-from (lambda (item memo) (concat "(" (int-to-string item) " - " memo ")")) "10" '(1 2 3)) ;; => "(1 - (2 - (3 - 10)))"
(--reduce-r-from (concat it " " acc) "END" '("a" "b" "c")) ;; => "a b c END"
```

### -reduce `(fn list)`

Returns the result of applying `fn` to the first 2 items in `list`,
then applying `fn` to that result and the 3rd item, etc. If `list`
contains no items, `fn` must accept no arguments as well, and
reduce returns the result of calling `fn` with no arguments. If
`list` has only 1 item, it is returned and `fn` is not called.

In the anaphoric form `--reduce`, the accumulated value is
exposed as `acc`.

```cl
(-reduce '- '(1 2 3 4)) ;; => -8
(-reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) ;; => "1-2-3"
(--reduce (format "%s-%s" acc it) '(1 2 3)) ;; => "1-2-3"
```

### -reduce-r `(fn list)`

Replace conses with `fn` and evaluate the resulting expression.
The final nil is ignored. If `list` contains no items, `fn` must
accept no arguments as well, and reduce returns the result of
calling `fn` with no arguments. If `list` has only 1 item, it is
returned and `fn` is not called.

The first argument of `fn` is the new item, the second is the
accumulated value.

Note: this function works the same as `-reduce` but the operation
associates from right instead of from left.

```cl
(-reduce-r '- '(1 2 3 4)) ;; => -2
(-reduce-r (lambda (item memo) (format "%s-%s" memo item)) '(1 2 3)) ;; => "3-2-1"
(--reduce-r (format "%s-%s" acc it) '(1 2 3)) ;; => "3-2-1"
```

### -filter `(pred list)`

Returns a new list of the items in `list` for which `pred` returns a non-nil value.

Alias: `-select`

```cl
(-filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => '(2 4)
(-filter 'even? '(1 2 3 4)) ;; => '(2 4)
(--filter (= 0 (% it 2)) '(1 2 3 4)) ;; => '(2 4)
```

### -remove `(pred list)`

Returns a new list of the items in `list` for which `pred` returns nil.

Alias: `-reject`

```cl
(-remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => '(1 3)
(-remove 'even? '(1 2 3 4)) ;; => '(1 3)
(--remove (= 0 (% it 2)) '(1 2 3 4)) ;; => '(1 3)
```

### -keep `(fn list)`

Returns a new list of the non-nil results of applying `fn` to the items in `list`.

```cl
(-keep 'cdr '((1 2 3) (4 5) (6))) ;; => '((2 3) (5))
(-keep (lambda (num) (when (> num 3) (* 10 num))) '(1 2 3 4 5 6)) ;; => '(40 50 60)
(--keep (when (> it 3) (* 10 it)) '(1 2 3 4 5 6)) ;; => '(40 50 60)
```

### -map-when `(pred rep list)`

Returns a new list where the elements in `list` that does not match the `pred` function
are unchanged, and where the elements in `list` that do match the `pred` function are mapped
through the `rep` function.

```cl
(-map-when 'even? 'square '(1 2 3 4)) ;; => '(1 4 3 16)
(--map-when (> it 2) (* it it) '(1 2 3 4)) ;; => '(1 2 9 16)
(--map-when (= it 2) 17 '(1 2 3 4)) ;; => '(1 17 3 4)
```

### -map-indexed `(fn list)`

Returns a new list consisting of the result of (`fn` index item) for each item in `list`.

In the anaphoric form `--map-indexed`, the index is exposed as `it-index`.

```cl
(-map-indexed (lambda (index item) (- item index)) '(1 2 3 4)) ;; => '(1 1 1 1)
(--map-indexed (- it it-index) '(1 2 3 4)) ;; => '(1 1 1 1)
```

### -flatten `(l)`

Takes a nested list `l` and returns its contents as a single, flat list.

```cl
(-flatten '((1))) ;; => '(1)
(-flatten '((1 (2 3) (((4 (5))))))) ;; => '(1 2 3 4 5)
(-flatten '(1 2 (3 . 4))) ;; => '(1 2 (3 . 4))
```

### -concat `(&rest lists)`

Returns a new list with the concatenation of the elements in the supplied `lists`.

```cl
(-concat '(1)) ;; => '(1)
(-concat '(1) '(2)) ;; => '(1 2)
(-concat '(1) '(2 3) '(4)) ;; => '(1 2 3 4)
```

### -mapcat `(fn list)`

Returns the concatenation of the result of mapping `fn` over `list`.
Thus function `fn` should return a list.

```cl
(-mapcat 'list '(1 2 3)) ;; => '(1 2 3)
(-mapcat (lambda (item) (list 0 item)) '(1 2 3)) ;; => '(0 1 0 2 0 3)
(--mapcat (list 0 it) '(1 2 3)) ;; => '(0 1 0 2 0 3)
```

### -cons* `(&rest args)`

Makes a new list from the elements of `args`.

The last 2 members of `args` are used as the final cons of the
result so if the final member of `args` is not a list the result is
a dotted list.

```cl
(-cons* 1 2) ;; => '(1 . 2)
(-cons* 1 2 3) ;; => '(1 2 . 3)
(-cons* 1) ;; => 1
```

### -count `(pred list)`

Counts the number of items in `list` where (`pred` item) is non-nil.

```cl
(-count 'even? '(1 2 3 4 5)) ;; => 2
(--count (< it 4) '(1 2 3 4)) ;; => 3
```

### -sum `(list)`

Return the sum of `list`.

```cl
(-sum '()) ;; => 0
(-sum '(1)) ;; => 1
(-sum '(1 2 3)) ;; => 6
```

### -product `(list)`

Return the product of `list`.

```cl
(-product '()) ;; => 1
(-product '(1)) ;; => 1
(-product '(1 2 3)) ;; => 6
```

### -min `(list)`

Return the smallest value from `list` of numbers or markers.

```cl
(-min '(0)) ;; => 0
(-min '(3 2 1)) ;; => 1
(-min '(1 2 3)) ;; => 1
```

### -min-by `(comparator list)`

Take a comparison function `comparator` and a `list` and return
the least element of the list by the comparison function.

See also combinator `-on` which can transform the values before
comparing them.

```cl
(-min-by '> '(4 3 6 1)) ;; => 1
(-min-by (-on '> 'length) '((1 2 3) (1) (1 2))) ;; => '(1)
(-min-by (-on 'string-lessp 'int-to-string) '(2 100 22)) ;; => 22
```

### -max `(list)`

Return the largest value from `list` of numbers or markers.

```cl
(-max '(0)) ;; => 0
(-max '(3 2 1)) ;; => 3
(-max '(1 2 3)) ;; => 3
```

### -max-by `(comparator list)`

Take a comparison function `comparator` and a `list` and return
the greatest element of the list by the comparison function.

See also combinator `-on` which can transform the values before
comparing them.

```cl
(-max-by '> '(4 3 6 1)) ;; => 6
(-max-by (-on '> 'car) '((2 2 3) (3) (1 2))) ;; => '(3)
(-max-by (-on '> 'string-to-int) '("1" "2" "3")) ;; => "3"
```

### -any? `(pred list)`

Returns t if (`pred` x) is non-nil for any x in `list`, else nil.

Alias: `-some?`

```cl
(-any? 'even? '(1 2 3)) ;; => t
(-any? 'even? '(1 3 5)) ;; => nil
(--any? (= 0 (% it 2)) '(1 2 3)) ;; => t
```

### -all? `(pred list)`

Returns t if (`pred` x) is non-nil for all x in `list`, else nil.

Alias: `-every?`

```cl
(-all? 'even? '(1 2 3)) ;; => nil
(-all? 'even? '(2 4 6)) ;; => t
(--all? (= 0 (% it 2)) '(2 4 6)) ;; => t
```

### -none? `(pred list)`

Returns t if (`pred` x) is nil for all x in `list`, else nil.

```cl
(-none? 'even? '(1 2 3)) ;; => nil
(-none? 'even? '(1 3 5)) ;; => t
(--none? (= 0 (% it 2)) '(1 2 3)) ;; => nil
```

### -only-some? `(pred list)`

Returns `t` if there is a mix of items in `list` that matches and does not match `pred`.
Returns `nil` both if all items match the predicate, and if none of the items match the predicate.

```cl
(-only-some? 'even? '(1 2 3)) ;; => t
(-only-some? 'even? '(1 3 5)) ;; => nil
(-only-some? 'even? '(2 4 6)) ;; => nil
```

### -each `(list fn)`

Calls `fn` with every item in `list`. Returns nil, used for side-effects only.

```cl
(let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s))))) ;; => nil
(let (s) (-each '(1 2 3) (lambda (item) (setq s (cons item s)))) s) ;; => '(3 2 1)
(let (s) (--each '(1 2 3) (setq s (cons it s))) s) ;; => '(3 2 1)
```

### -each-while `(list pred fn)`

Calls `fn` with every item in `list` while (`pred` item) is non-nil.
Returns nil, used for side-effects only.

```cl
(let (s) (-each-while '(2 4 5 6) 'even? (lambda (item) (!cons item s))) s) ;; => '(4 2)
(let (s) (--each-while '(1 2 3 4) (< it 3) (!cons it s)) s) ;; => '(2 1)
```

### -dotimes `(num fn)`

Repeatedly calls `fn` (presumably for side-effects) passing in integers from 0 through n-1.

```cl
(let (s) (-dotimes 3 (lambda (n) (!cons n s))) s) ;; => '(2 1 0)
(let (s) (--dotimes 5 (!cons it s)) s) ;; => '(4 3 2 1 0)
```

### -repeat `(n x)`

Return a list with `x` repeated `n` times.
Returns nil if `n` is less than 1.

```cl
(-repeat 3 :a) ;; => '(:a :a :a)
(-repeat 1 :a) ;; => '(:a)
(-repeat 0 :a) ;; => nil
```

### -slice `(list from &optional to)`

Return copy of `list`, starting from index `from` to index `to`.
`from` or `to` may be negative.

```cl
(-slice '(1 2 3 4 5) 1) ;; => '(2 3 4 5)
(-slice '(1 2 3 4 5) 0 3) ;; => '(1 2 3)
(-slice '(1 2 3 4 5) 1 -1) ;; => '(2 3 4)
```

### -take `(n list)`

Returns a new list of the first `n` items in `list`, or all items if there are fewer than `n`.

```cl
(-take 3 '(1 2 3 4 5)) ;; => '(1 2 3)
(-take 17 '(1 2 3 4 5)) ;; => '(1 2 3 4 5)
```

### -drop `(n list)`

Returns the tail of `list` without the first `n` items.

```cl
(-drop 3 '(1 2 3 4 5)) ;; => '(4 5)
(-drop 17 '(1 2 3 4 5)) ;; => '()
```

### -take-while `(pred list)`

Returns a new list of successive items from `list` while (`pred` item) returns a non-nil value.

```cl
(-take-while 'even? '(1 2 3 4)) ;; => '()
(-take-while 'even? '(2 4 5 6)) ;; => '(2 4)
(--take-while (< it 4) '(1 2 3 4 3 2 1)) ;; => '(1 2 3)
```

### -drop-while `(pred list)`

Returns the tail of `list` starting from the first item for which (`pred` item) returns nil.

```cl
(-drop-while 'even? '(1 2 3 4)) ;; => '(1 2 3 4)
(-drop-while 'even? '(2 4 5 6)) ;; => '(5 6)
(--drop-while (< it 4) '(1 2 3 4 3 2 1)) ;; => '(4 3 2 1)
```

### -split-at `(n list)`

Returns a list of ((-take `n` `list`) (-drop `n` `list`)), in no more than one pass through the list.

```cl
(-split-at 3 '(1 2 3 4 5)) ;; => '((1 2 3) (4 5))
(-split-at 17 '(1 2 3 4 5)) ;; => '((1 2 3 4 5) nil)
```

### -rotate `(n list)`

Rotate `list` `n` places to the right.  With `n` negative, rotate to the left.
The time complexity is `o`(n).

```cl
(-rotate 3 '(1 2 3 4 5 6 7)) ;; => '(5 6 7 1 2 3 4)
(-rotate -3 '(1 2 3 4 5 6 7)) ;; => '(4 5 6 7 1 2 3)
```

### -insert-at `(n x list)`

Returns a list with `x` inserted into `list` at position `n`.

```cl
(-insert-at 1 'x '(a b c)) ;; => '(a x b c)
(-insert-at 12 'x '(a b c)) ;; => '(a b c x)
```

### -split-with `(pred list)`

Returns a list of ((-take-while `pred` `list`) (-drop-while `pred` `list`)), in no more than one pass through the list.

```cl
(-split-with 'even? '(1 2 3 4)) ;; => '(nil (1 2 3 4))
(-split-with 'even? '(2 4 5 6)) ;; => '((2 4) (5 6))
(--split-with (< it 4) '(1 2 3 4 3 2 1)) ;; => '((1 2 3) (4 3 2 1))
```

### -separate `(pred list)`

Returns a list of ((-filter `pred` `list`) (-remove `pred` `list`)), in one pass through the list.

```cl
(-separate (lambda (num) (= 0 (% num 2))) '(1 2 3 4 5 6 7)) ;; => '((2 4 6) (1 3 5 7))
(--separate (< it 5) '(3 7 5 9 3 2 1 4 6)) ;; => '((3 3 2 1 4) (7 5 9 6))
(-separate 'cdr '((1 2) (1) (1 2 3) (4))) ;; => '(((1 2) (1 2 3)) ((1) (4)))
```

### -partition `(n list)`

Returns a new list with the items in `list` grouped into `n-`sized sublists.
If there are not enough items to make the last group `n-`sized,
those items are discarded.

```cl
(-partition 2 '(1 2 3 4 5 6)) ;; => '((1 2) (3 4) (5 6))
(-partition 2 '(1 2 3 4 5 6 7)) ;; => '((1 2) (3 4) (5 6))
(-partition 3 '(1 2 3 4 5 6 7)) ;; => '((1 2 3) (4 5 6))
```

### -partition-all-in-steps `(n step list)`

Returns a new list with the items in `list` grouped into `n-`sized sublists at offsets `step` apart.
The last groups may contain less than `n` items.

```cl
(-partition-all-in-steps 2 1 '(1 2 3 4)) ;; => '((1 2) (2 3) (3 4) (4))
(-partition-all-in-steps 3 2 '(1 2 3 4)) ;; => '((1 2 3) (3 4))
(-partition-all-in-steps 3 2 '(1 2 3 4 5)) ;; => '((1 2 3) (3 4 5) (5))
```

### -partition-in-steps `(n step list)`

Returns a new list with the items in `list` grouped into `n-`sized sublists at offsets `step` apart.
If there are not enough items to make the last group `n-`sized,
those items are discarded.

```cl
(-partition-in-steps 2 1 '(1 2 3 4)) ;; => '((1 2) (2 3) (3 4))
(-partition-in-steps 3 2 '(1 2 3 4)) ;; => '((1 2 3))
(-partition-in-steps 3 2 '(1 2 3 4 5)) ;; => '((1 2 3) (3 4 5))
```

### -partition-all `(n list)`

Returns a new list with the items in `list` grouped into `n-`sized sublists.
The last group may contain less than `n` items.

```cl
(-partition-all 2 '(1 2 3 4 5 6)) ;; => '((1 2) (3 4) (5 6))
(-partition-all 2 '(1 2 3 4 5 6 7)) ;; => '((1 2) (3 4) (5 6) (7))
(-partition-all 3 '(1 2 3 4 5 6 7)) ;; => '((1 2 3) (4 5 6) (7))
```

### -partition-by `(fn list)`

Applies `fn` to each item in `list`, splitting it each time `fn` returns a new value.

```cl
(-partition-by 'even? '()) ;; => '()
(-partition-by 'even? '(1 1 2 2 2 3 4 6 8)) ;; => '((1 1) (2 2 2) (3) (4 6 8))
(--partition-by (< it 3) '(1 2 3 4 3 2 1)) ;; => '((1 2) (3 4 3) (2 1))
```

### -partition-by-header `(fn list)`

Applies `fn` to the first item in `list`. That is the header
  value. Applies `fn` to each item in `list`, splitting it each time
  `fn` returns the header value, but only after seeing at least one
  other value (the body).

```cl
(--partition-by-header (= it 1) '(1 2 3 1 2 1 2 3 4)) ;; => '((1 2 3) (1 2) (1 2 3 4))
(--partition-by-header (> it 0) '(1 2 0 1 0 1 2 3 0)) ;; => '((1 2 0) (1 0) (1 2 3 0))
(-partition-by-header 'even? '(2 1 1 1 4 1 3 5 6 6 1)) ;; => '((2 1 1 1) (4 1 3 5) (6 6 1))
```

### -group-by `(fn list)`

Separate `list` into an alist whose keys are `fn` applied to the
elements of `list`.  Keys are compared by `equal`.

```cl
(-group-by 'even? '()) ;; => '()
(-group-by 'even? '(1 1 2 2 2 3 4 6 8)) ;; => '((nil 1 1 3) (t 2 2 2 4 6 8))
(--group-by (car (split-string it "/")) '("a/b" "c/d" "a/e")) ;; => '(("a" "a/b" "a/e") ("c" "c/d"))
```

### -interpose `(sep list)`

Returns a new list of all elements in `list` separated by `sep`.

```cl
(-interpose "-" '()) ;; => '()
(-interpose "-" '("a")) ;; => '("a")
(-interpose "-" '("a" "b" "c")) ;; => '("a" "-" "b" "-" "c")
```

### -interleave `(&rest lists)`

Returns a new list of the first item in each list, then the second etc.

```cl
(-interleave '(1 2) '("a" "b")) ;; => '(1 "a" 2 "b")
(-interleave '(1 2) '("a" "b") '("A" "B")) ;; => '(1 "a" "A" 2 "b" "B")
(-interleave '(1 2 3) '("a" "b")) ;; => '(1 "a" 2 "b")
```

### -zip-with `(fn list1 list2)`

Zip the two lists `list1` and `list2` using a function `fn`.  This
function is applied pairwise taking as first argument element of
`list1` and as second argument element of `list2` at corresponding
position.

The anaphoric form `--zip-with` binds the elements from `list1` as `it`,
and the elements from `list2` as `other`.

```cl
(-zip-with '+ '(1 2 3) '(4 5 6)) ;; => '(5 7 9)
(-zip-with 'cons '(1 2 3) '(4 5 6)) ;; => '((1 . 4) (2 . 5) (3 . 6))
(--zip-with (concat it " and " other) '("Batman" "Jekyll") '("Robin" "Hyde")) ;; => '("Batman and Robin" "Jekyll and Hyde")
```

### -zip `(list1 list2)`

Zip the two lists together.  Return the list where elements
are cons pairs with car being element from `list1` and cdr being
element from `list2`.  The length of the returned list is the
length of the shorter one.

```cl
(-zip '(1 2 3) '(4 5 6)) ;; => '((1 . 4) (2 . 5) (3 . 6))
(-zip '(1 2 3) '(4 5 6 7)) ;; => '((1 . 4) (2 . 5) (3 . 6))
(-zip '(1 2 3 4) '(4 5 6)) ;; => '((1 . 4) (2 . 5) (3 . 6))
```

### -first `(pred list)`

Returns the first x in `list` where (`pred` x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car`.

```cl
(-first 'even? '(1 2 3)) ;; => 2
(-first 'even? '(1 3 5)) ;; => nil
(--first (> it 2) '(1 2 3)) ;; => 3
```

### -last `(pred list)`

Return the last x in `list` where (`pred` x) is non-nil, else nil.

```cl
(-last 'even? '(1 2 3 4 5 6 3 3 3)) ;; => 6
(-last 'even? '(1 3 7 5 9)) ;; => nil
(--last (> (length it) 3) '("a" "looong" "word" "and" "short" "one")) ;; => "short"
```

### -first-item `(list)`

Returns the first item of `list`, or nil on an empty list.

```cl
(-first-item '(1 2 3)) ;; => 1
(-first-item nil) ;; => nil
```

### -last-item `(list)`

Returns the first item of `list`, or nil on an empty list.

```cl
(-last-item '(1 2 3)) ;; => 3
(-last-item nil) ;; => nil
```

### -union `(list list2)`

Return a new list containing the elements of `list1` and elements of `list2` that are not in `list1`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```cl
(-union '(1 2 3) '(3 4 5)) ;; => '(1 2 3 4 5)
(-union '(1 2 3 4) '()) ;; => '(1 2 3 4)
(-union '(1 1 2 2) '(3 2 1)) ;; => '(1 1 2 2 3)
```

### -difference `(list list2)`

Return a new list with only the members of `list` that are not in `list2`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```cl
(-difference '() '()) ;; => '()
(-difference '(1 2 3) '(4 5 6)) ;; => '(1 2 3)
(-difference '(1 2 3 4) '(3 4 5 6)) ;; => '(1 2)
```

### -intersection `(list list2)`

Return a new list containing only the elements that are members of both `list` and `list2`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```cl
(-intersection '() '()) ;; => '()
(-intersection '(1 2 3) '(4 5 6)) ;; => '()
(-intersection '(1 2 3 4) '(3 4 5 6)) ;; => '(3 4)
```

### -distinct `(list)`

Return a new list with all duplicates removed.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

Alias: `-uniq`

```cl
(-distinct '()) ;; => '()
(-distinct '(1 2 2 4)) ;; => '(1 2 4)
```

### -contains? `(list element)`

Return whether `list` contains `element`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```cl
(-contains? '(1 2 3) 1) ;; => t
(-contains? '(1 2 3) 2) ;; => t
(-contains? '(1 2 3) 4) ;; => nil
```

### -sort `(predicate list)`

Sort `list`, stably, comparing elements using `predicate`.
Returns the sorted list.  `list` is `not` modified by side effects.
`predicate` is called with two elements of `list`, and should return non-nil
if the first element should sort before the second.

```cl
(-sort '< '(3 1 2)) ;; => '(1 2 3)
(-sort '> '(3 1 2)) ;; => '(3 2 1)
(--sort (< it other) '(3 1 2)) ;; => '(1 2 3)
```

### -> `(x &optional form &rest more)`

Threads the expr through the forms. Inserts `x` as the second
item in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
second item in second form, etc.

```cl
(-> "Abc") ;; => "Abc"
(-> "Abc" (concat "def")) ;; => "Abcdef"
(-> "Abc" (concat "def") (concat "ghi")) ;; => "Abcdefghi"
```

### ->> `(x form &rest more)`

Threads the expr through the forms. Inserts `x` as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc.

```cl
(->> "Abc" (concat "def")) ;; => "defAbc"
(->> "Abc" (concat "def") (concat "ghi")) ;; => "ghidefAbc"
(->> 5 (- 8)) ;; => 3
```

### --> `(x form &rest more)`

Threads the expr through the forms. Inserts `x` at the position
signified by the token `it` in the first form. If there are more
forms, inserts the first form at the position signified by `it`
in in second form, etc.

```cl
(--> "def" (concat "abc" it "ghi")) ;; => "abcdefghi"
(--> "def" (concat "abc" it "ghi") (upcase it)) ;; => "ABCDEFGHI"
(--> "def" (concat "abc" it "ghi") upcase) ;; => "ABCDEFGHI"
```

### -when-let `(var-val &rest body)`

If `val` evaluates to non-nil, bind it to `var` and execute body.
`var-val` should be a (`var` `val`) pair.

```cl
(-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) ;; => 5
(--when-let (member :b '(:a :b :c)) (cons :d it)) ;; => '(:d :b :c)
(--when-let (even? 3) (cat it :a)) ;; => nil
```

### -when-let* `(vars-vals &rest body)`

If all `vals` evaluate to true, bind them to their corresponding
  `vars` and execute body. `vars-vals` should be a list of (`var` `val`)
  pairs (corresponding to bindings of `let*`).

```cl
(-when-let* ((x 5) (y 3) (z (+ y 4))) (+ x y z)) ;; => 15
(-when-let* ((x 5) (y nil) (z 7)) (+ x y z)) ;; => nil
```

### -if-let `(var-val then &optional else)`

If `val` evaluates to non-nil, bind it to `var` and do `then`,
otherwise do `else`. `var-val` should be a (`var` `val`) pair.

```cl
(-if-let (match-index (string-match "d" "abc")) (+ match-index 3) 7) ;; => 7
(--if-let (even? 4) it nil) ;; => t
```

### -if-let* `(vars-vals then &optional else)`

If all `vals` evaluate to true, bind them to their corresponding
  `vars` and do `then`, otherwise do `else`. `vars-vals` should be a list
  of (`var` `val`) pairs (corresponding to the bindings of `let*`).

```cl
(-if-let* ((x 5) (y 3) (z 7)) (+ x y z) "foo") ;; => 15
(-if-let* ((x 5) (y nil) (z 7)) (+ x y z) "foo") ;; => "foo"
```

### -partial `(fn &rest args)`

Takes a function `fn` and fewer than the normal arguments to `fn`,
and returns a fn that takes a variable number of additional `args`.
When called, the returned function calls `fn` with `args` first and
then additional args.

```cl
(funcall (-partial '- 5) 3) ;; => 2
(funcall (-partial '+ 5 2) 3) ;; => 10
```

### -rpartial `(fn &rest args)`

Takes a function `fn` and fewer than the normal arguments to `fn`,
and returns a fn that takes a variable number of additional `args`.
When called, the returned function calls `fn` with the additional
args first and then `args`.

Requires Emacs 24 or higher.

```cl
(funcall (-rpartial '- 5) 8) ;; => 3
(funcall (-rpartial '- 5 2) 10) ;; => 3
```

### -juxt `(&rest fns)`

Takes a list of functions and returns a fn that is the
juxtaposition of those fns. The returned fn takes a variable
number of args, and returns a list containing the result of
applying each fn to the args (left-to-right).

Requires Emacs 24 or higher.

```cl
(funcall (-juxt '+ '-) 3 5) ;; => '(8 -2)
(-map (-juxt 'identity 'square) '(1 2 3)) ;; => '((1 1) (2 4) (3 9))
```

### -applify `(fn)`

Changes an n-arity function `fn` to a 1-arity function that
expects a list with n items as arguments

```cl
(-map (-applify '+) '((1 1 1) (1 2 3) (5 5 5))) ;; => '(3 6 15)
(-map (-applify (lambda (a b c) (\` ((\, a) ((\, b) ((\, c))))))) '((1 1 1) (1 2 3) (5 5 5))) ;; => '((1 (1 (1))) (1 (2 (3))) (5 (5 (5))))
(funcall (-applify '<) '(3 6)) ;; => t
```

### -on `(operator transformer)`

Return a function of two arguments that first applies
`transformer` to each of them and then applies `operator` on the
results (in the same order).

In types: (b -> b -> c) -> (a -> b) -> a -> a -> c

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(-sort (-on '< 'length) '((1 2 3) (1) (1 2))) ;; => '((1) (1 2) (1 2 3))
(-sort (-on 'string-lessp 'int-to-string) '(10 12 1 2 22)) ;; => '(1 10 12 2 22)
(funcall (-on '+ '1+) 1 2) ;; => 5
```

### -flip `(func)`

Swap the order of arguments for binary function `func`.

In types: (a -> b -> c) -> b -> a -> c

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(funcall (-flip '<) 2 1) ;; => t
(funcall (-flip '-) 3 8) ;; => 5
(-sort (-flip '<) '(4 3 6 1)) ;; => '(6 4 3 1)
```

### -const `(c)`

Return a function that returns `c` ignoring any additional arguments.

In types: a -> b -> a

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(funcall (-const 2) 1 3 "foo") ;; => 2
(-map (-const 1) '("a" "b" "c" "d")) ;; => '(1 1 1 1)
(-sum (-map (-const 1) '("a" "b" "c" "d"))) ;; => 4
```

### -cut `(&rest params)`

Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See `srfi-26` for detailed description.

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(funcall (-cut list 1 <> 3 <> 5) 2 4) ;; => '(1 2 3 4 5)
(-map (-cut funcall <> 5) '(1+ 1- (lambda (x) (/ 1.0 x)))) ;; => '(6 4 0.2)
(-filter (-cut < <> 5) '(1 3 5 7 9)) ;; => '(1 3)
```

### -not `(pred)`

Take an unary predicates `pred` and return an unary predicate
that returns t if `pred` returns nil and nil if `pred` returns
non-nil.

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(funcall (-not 'even?) 5) ;; => t
(-filter (-not (-partial '< 4)) '(1 2 3 4 5 6 7 8)) ;; => '(1 2 3 4)
```

### -orfn `(&rest preds)`

Take list of unary predicates `preds` and return an unary
predicate with argument x that returns non-nil if at least one of
the `preds` returns non-nil on x.

In types: [a -> Bool] -> a -> Bool

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(-filter (-orfn 'even? (-partial (-flip '<) 5)) '(1 2 3 4 5 6 7 8 9 10)) ;; => '(1 2 3 4 6 8 10)
(funcall (-orfn 'stringp 'even?) "foo") ;; => t
```

### -andfn `(&rest preds)`

Take list of unary predicates `preds` and return an unary
predicate with argument x that returns non-nil if all of the
`preds` returns non-nil on x.

In types: [a -> Bool] -> a -> Bool

Available by `(require 'dash-functional)`. Requires Emacs 24 or higher.

```cl
(funcall (-andfn (-cut < <> 10) 'even?) 6) ;; => t
(funcall (-andfn (-cut < <> 10) 'even?) 12) ;; => nil
(-filter (-andfn (-not 'even?) (-cut >= 5 <>)) '(1 2 3 4 5 6 7 8 9 10)) ;; => '(1 3 5)
```

### !cons `(car cdr)`

Destructive: Sets `cdr` to the cons of `car` and `cdr`.

```cl
(let (l) (!cons 5 l) l) ;; => '(5)
(let ((l '(3))) (!cons 5 l) l) ;; => '(5 3)
```

### !cdr `(list)`

Destructive: Sets `list` to the cdr of `list`.

```cl
(let ((l '(3))) (!cdr l) l) ;; => '()
(let ((l '(3 5))) (!cdr l) l) ;; => '(5)
```


## Contribute

Yes, please do. Pure functions in the list manipulation realm only,
please. There's a suite of tests in `dev/examples.el`, so remember to add
tests for your function, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/dash.el

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## Changelist

### From 1.8.0 to master

The stated scope of dash is increasing. It now includes more
functional style functions, like combinators and threading macros.
These have been creeping in anyway, since they're so darn useful. Time
to make it official. :)

- Split out `dash-functional.el` (Matus Goljer)
- Add `-andfn`, `-orfn`, `-not`, `-cut`, `-const`, `-flip` and `-on`. (Matus Goljer)

### From 1.7.0 to 1.8.0

- Add `-first-item` and `-last-item` (Wilfred Hughes)

### From 1.6.0 to 1.7.0

- Add `-rotate` (Matus Goljer)

### From 1.5.0 to 1.6.0

- Add `-min`, `-max`, `-min-by` and `-max-by` (Johan Andersson)

### From 1.4.0 to 1.5.0

- Add `-sum` and `-product` (Johan Andersson)

### From 1.3.0 to 1.4.0

- Add `-sort`
- Add `-reduce-r` (Matus Goljer)
- Add `-reduce-r-from` (Matus Goljer)

### From 1.2.0 to 1.3.0

- Add `-partition-in-steps`
- Add `-partition-all-in-steps`

### From 1.1.0 to 1.2.0

- Add `-last` (Matus Goljer)
- Add `-insert-at` (Emanuel Evans)
- Add `-when-let` and `-if-let` (Emanuel Evans)
- Add `-when-let*` and `-if-let*` (Emanuel Evans)
- Some bugfixes

## Contributors

 - [Matus Goljer](https://github.com/Fuco1) contributed `-union`, `-separate`, `-zip`, `-zip-with`, `-reduce-r`, `-reduce-r-from` and `-rotate`.
 - [Takafumi Arakaki](https://github.com/tkf) contributed `-group-by`.
 - [tali713](https://github.com/tali713) is the author of `-applify`.
 - [Víctor M. Valenzuela](https://github.com/vemv) contributed `-repeat`.
 - [Nic Ferrier](https://github.com/nicferrier) contributed `-cons*`.
 - [Wilfred Hughes](https://github.com/Wilfred) contributed `-slice`, `-first-item` and `-last-item`.
 - [Emanuel Evans](https://github.com/shosti) contributed `-if-let`, `-when-let` and `-insert-at`.
 - [Johan Andersson](https://github.com/rejeep) contributed `-sum`, `-product`, `-min`, `-max`, `-min-by` and `-max-by`.

Thanks!

## License

Copyright (C) 2012-2013 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: lists

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
