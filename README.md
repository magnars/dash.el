[![CI](https://github.com/magnars/dash.el/actions/workflows/test.yml/badge.svg)](https://github.com/magnars/dash.el/actions/workflows/test.yml)
[![GNU ELPA](https://elpa.gnu.org/packages/dash.svg)](https://elpa.gnu.org/packages/dash.html)
[![GNU-devel ELPA](https://elpa.gnu.org/devel/dash.svg)](https://elpa.gnu.org/devel/dash.html)
[![MELPA Stable](https://stable.melpa.org/packages/dash-badge.svg)](https://stable.melpa.org/#/dash)
[![MELPA](https://melpa.org/packages/dash-badge.svg)](https://melpa.org/#/dash)

# <img align="right" src="rainbow-dash.png"> dash.el

A modern list API for Emacs.  No
[`'cl`](https://gnu.org/software/emacs/manual/html_node/cl/) required.

See the end of the file for license conditions.

## Contents

* [Change log](#change-log)
  * [Upcoming breaking change!](#upcoming-breaking-change)
* [Installation](#installation)
* [Functions](#functions)
* [Contribute](#contribute)
* [Contributors](#contributors)
* [License](#license)

## Change log

See the [`NEWS.md`](NEWS.md) file.

### Upcoming breaking change!

- For backward compatibility reasons, `-zip` when called with two
  lists returns a list of cons cells, rather than a list of proper
  lists.  This is a clunky API, and may be changed in a future release
  to always return a list of proper lists, as `-zip-lists` currently
  does.

  **N.B.:** Do not rely on the current behavior of `-zip` for two
  lists.  Instead, use `-zip-pair` for a list of cons cells, and
  `-zip-lists` for a list of proper lists.

## Installation

Dash is available on [GNU ELPA](https://elpa.gnu.org/), [GNU-devel
ELPA](https://elpa.gnu.org/devel/), and [MELPA](https://melpa.org/),
and can be installed with the standard command `package-install`:

    M-x package-install RET dash RET

See [`(info "(emacs) Package
Installation")`](https://gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html).

Alternatively, you can just dump `dash.el` in your `load-path`
somewhere.  See [`(info "(emacs) Lisp
Libraries")`](https://gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html).

### Using in a package

Add something like this to the library's headers:

    ;; Package-Requires: ((dash "2.19.1"))

See [`(info "(elisp) Library
Headers")`](https://gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html).

### Fontification of special variables

Font lock of special Dash variables (`it`, `acc`, etc.) in Emacs Lisp
buffers can optionally be enabled with the autoloaded minor mode
`dash-fontify-mode`.  In older Emacs versions which do not dynamically
detect macros, the minor mode also fontifies Dash macro calls.

To automatically enable the minor mode in all Emacs Lisp buffers, just
call its autoloaded global counterpart `global-dash-fontify-mode`,
either interactively or from your `user-init-file`:

```el
(global-dash-fontify-mode)
```

### Info symbol lookup

While editing Elisp files, you can use `C-h S` (`info-lookup-symbol`)
to look up Elisp symbols in the relevant Info manuals (see [`(emacs)
Info
Lookup`](https://gnu.org/software/emacs/manual/html_node/emacs/Info-Lookup.html)).
To enable the same for Dash symbols, use the command
`dash-register-info-lookup`.  It can be called directly when needed,
or automatically from your `user-init-file`.  For example:

```el
(with-eval-after-load 'info-look
  (dash-register-info-lookup))
```

## Functions

All functions and constructs in the library use a dash (`-`) prefix.

The library also provides anaphoric macro versions of functions where
that makes sense.  The names of these macros are prefixed with two
dashes (`--`) instead of one.

While `-map` applies a function to each element of a list, its
anaphoric counterpart `--map` evaluates a form with the local variable
`it` temporarily bound to the current list element instead.  For
example:

```el
(-map (lambda (n) (* n n)) '(1 2 3 4)) ; Normal version.
(--map (* it it) '(1 2 3 4))           ; Anaphoric version.
```

The normal version can of course also be written as follows:

```el
(defun my-square (n)
  "Return N multiplied by itself."
  (* n n))

(-map #'my-square '(1 2 3 4))
```

This demonstrates the utility of both versions.

### Maps

Functions in this category take a transforming function, which
is then applied sequentially to each or selected elements of the
input list.  The results are collected in order and returned as a
new list.

* [`-map`](#-map-fn-list) `(fn list)`
* [`-map-when`](#-map-when-pred-rep-list) `(pred rep list)`
* [`-map-first`](#-map-first-pred-rep-list) `(pred rep list)`
* [`-map-last`](#-map-last-pred-rep-list) `(pred rep list)`
* [`-map-indexed`](#-map-indexed-fn-list) `(fn list)`
* [`-annotate`](#-annotate-fn-list) `(fn list)`
* [`-splice`](#-splice-pred-fun-list) `(pred fun list)`
* [`-splice-list`](#-splice-list-pred-new-list-list) `(pred new-list list)`
* [`-mapcat`](#-mapcat-fn-list) `(fn list)`
* [`-copy`](#-copy-list) `(list)`

### Sublist selection

Functions returning a sublist of the original list.

* [`-filter`](#-filter-pred-list) `(pred list)`
* [`-remove`](#-remove-pred-list) `(pred list)`
* [`-remove-first`](#-remove-first-pred-list) `(pred list)`
* [`-remove-last`](#-remove-last-pred-list) `(pred list)`
* [`-remove-item`](#-remove-item-item-list) `(item list)`
* [`-non-nil`](#-non-nil-list) `(list)`
* [`-slice`](#-slice-list-from-optional-to-step) `(list from &optional to step)`
* [`-take`](#-take-n-list) `(n list)`
* [`-take-last`](#-take-last-n-list) `(n list)`
* [`-drop`](#-drop-n-list) `(n list)`
* [`-drop-last`](#-drop-last-n-list) `(n list)`
* [`-take-while`](#-take-while-pred-list) `(pred list)`
* [`-drop-while`](#-drop-while-pred-list) `(pred list)`
* [`-select-by-indices`](#-select-by-indices-indices-list) `(indices list)`
* [`-select-columns`](#-select-columns-columns-table) `(columns table)`
* [`-select-column`](#-select-column-column-table) `(column table)`

### List to list

Functions returning a modified copy of the input list.

* [`-keep`](#-keep-fn-list) `(fn list)`
* [`-concat`](#-concat-rest-lists) `(&rest lists)`
* [`-flatten`](#-flatten-l) `(l)`
* [`-flatten-n`](#-flatten-n-num-list) `(num list)`
* [`-replace`](#-replace-old-new-list) `(old new list)`
* [`-replace-first`](#-replace-first-old-new-list) `(old new list)`
* [`-replace-last`](#-replace-last-old-new-list) `(old new list)`
* [`-insert-at`](#-insert-at-n-x-list) `(n x list)`
* [`-replace-at`](#-replace-at-n-x-list) `(n x list)`
* [`-update-at`](#-update-at-n-func-list) `(n func list)`
* [`-remove-at`](#-remove-at-n-list) `(n list)`
* [`-remove-at-indices`](#-remove-at-indices-indices-list) `(indices list)`

### Reductions

Functions reducing lists to a single value (which may also be a list).

* [`-reduce-from`](#-reduce-from-fn-init-list) `(fn init list)`
* [`-reduce-r-from`](#-reduce-r-from-fn-init-list) `(fn init list)`
* [`-reduce`](#-reduce-fn-list) `(fn list)`
* [`-reduce-r`](#-reduce-r-fn-list) `(fn list)`
* [`-reductions-from`](#-reductions-from-fn-init-list) `(fn init list)`
* [`-reductions-r-from`](#-reductions-r-from-fn-init-list) `(fn init list)`
* [`-reductions`](#-reductions-fn-list) `(fn list)`
* [`-reductions-r`](#-reductions-r-fn-list) `(fn list)`
* [`-count`](#-count-pred-list) `(pred list)`
* [`-sum`](#-sum-list) `(list)`
* [`-running-sum`](#-running-sum-list) `(list)`
* [`-product`](#-product-list) `(list)`
* [`-running-product`](#-running-product-list) `(list)`
* [`-inits`](#-inits-list) `(list)`
* [`-tails`](#-tails-list) `(list)`
* [`-common-prefix`](#-common-prefix-rest-lists) `(&rest lists)`
* [`-common-suffix`](#-common-suffix-rest-lists) `(&rest lists)`
* [`-min`](#-min-list) `(list)`
* [`-min-by`](#-min-by-comparator-list) `(comparator list)`
* [`-max`](#-max-list) `(list)`
* [`-max-by`](#-max-by-comparator-list) `(comparator list)`

### Unfolding

Operations dual to reductions, building lists from a seed
value rather than consuming a list to produce a single value.

* [`-iterate`](#-iterate-fun-init-n) `(fun init n)`
* [`-unfold`](#-unfold-fun-seed) `(fun seed)`

### Predicates

Reductions of one or more lists to a boolean value.

* [`-some`](#-some-pred-list) `(pred list)`
* [`-every`](#-every-pred-list) `(pred list)`
* [`-any?`](#-any-pred-list) `(pred list)`
* [`-all?`](#-all-pred-list) `(pred list)`
* [`-none?`](#-none-pred-list) `(pred list)`
* [`-only-some?`](#-only-some-pred-list) `(pred list)`
* [`-contains?`](#-contains-list-element) `(list element)`
* [`-same-items?`](#-same-items-list-list2) `(list list2)`
* [`-is-prefix?`](#-is-prefix-prefix-list) `(prefix list)`
* [`-is-suffix?`](#-is-suffix-suffix-list) `(suffix list)`
* [`-is-infix?`](#-is-infix-infix-list) `(infix list)`
* [`-cons-pair?`](#-cons-pair-obj) `(obj)`

### Partitioning

Functions partitioning the input list into a list of lists.

* [`-split-at`](#-split-at-n-list) `(n list)`
* [`-split-with`](#-split-with-pred-list) `(pred list)`
* [`-split-on`](#-split-on-item-list) `(item list)`
* [`-split-when`](#-split-when-fn-list) `(fn list)`
* [`-separate`](#-separate-pred-list) `(pred list)`
* [`-partition`](#-partition-n-list) `(n list)`
* [`-partition-all`](#-partition-all-n-list) `(n list)`
* [`-partition-in-steps`](#-partition-in-steps-n-step-list) `(n step list)`
* [`-partition-all-in-steps`](#-partition-all-in-steps-n-step-list) `(n step list)`
* [`-partition-by`](#-partition-by-fn-list) `(fn list)`
* [`-partition-by-header`](#-partition-by-header-fn-list) `(fn list)`
* [`-partition-after-pred`](#-partition-after-pred-pred-list) `(pred list)`
* [`-partition-before-pred`](#-partition-before-pred-pred-list) `(pred list)`
* [`-partition-before-item`](#-partition-before-item-item-list) `(item list)`
* [`-partition-after-item`](#-partition-after-item-item-list) `(item list)`
* [`-group-by`](#-group-by-fn-list) `(fn list)`

### Indexing

Functions retrieving or sorting based on list indices and
related predicates.

* [`-elem-index`](#-elem-index-elem-list) `(elem list)`
* [`-elem-indices`](#-elem-indices-elem-list) `(elem list)`
* [`-find-index`](#-find-index-pred-list) `(pred list)`
* [`-find-last-index`](#-find-last-index-pred-list) `(pred list)`
* [`-find-indices`](#-find-indices-pred-list) `(pred list)`
* [`-grade-up`](#-grade-up-comparator-list) `(comparator list)`
* [`-grade-down`](#-grade-down-comparator-list) `(comparator list)`

### Set operations

Operations pretending lists are sets.

* [`-union`](#-union-list-list2) `(list list2)`
* [`-difference`](#-difference-list-list2) `(list list2)`
* [`-intersection`](#-intersection-list-list2) `(list list2)`
* [`-powerset`](#-powerset-list) `(list)`
* [`-permutations`](#-permutations-list) `(list)`
* [`-distinct`](#-distinct-list) `(list)`

### Other list operations

Other list functions not fit to be classified elsewhere.

* [`-rotate`](#-rotate-n-list) `(n list)`
* [`-repeat`](#-repeat-n-x) `(n x)`
* [`-cons*`](#-cons-rest-args) `(&rest args)`
* [`-snoc`](#-snoc-list-elem-rest-elements) `(list elem &rest elements)`
* [`-interpose`](#-interpose-sep-list) `(sep list)`
* [`-interleave`](#-interleave-rest-lists) `(&rest lists)`
* [`-iota`](#-iota-count-optional-start-step) `(count &optional start step)`
* [`-zip-with`](#-zip-with-fn-list1-list2) `(fn list1 list2)`
* [`-zip`](#-zip-rest-lists) `(&rest lists)`
* [`-zip-lists`](#-zip-lists-rest-lists) `(&rest lists)`
* [`-zip-fill`](#-zip-fill-fill-value-rest-lists) `(fill-value &rest lists)`
* [`-unzip`](#-unzip-lists) `(lists)`
* [`-cycle`](#-cycle-list) `(list)`
* [`-pad`](#-pad-fill-value-rest-lists) `(fill-value &rest lists)`
* [`-table`](#-table-fn-rest-lists) `(fn &rest lists)`
* [`-table-flat`](#-table-flat-fn-rest-lists) `(fn &rest lists)`
* [`-first`](#-first-pred-list) `(pred list)`
* [`-last`](#-last-pred-list) `(pred list)`
* [`-first-item`](#-first-item-list) `(list)`
* [`-second-item`](#-second-item-list) `(list)`
* [`-third-item`](#-third-item-list) `(list)`
* [`-fourth-item`](#-fourth-item-list) `(list)`
* [`-fifth-item`](#-fifth-item-list) `(list)`
* [`-last-item`](#-last-item-list) `(list)`
* [`-butlast`](#-butlast-list) `(list)`
* [`-sort`](#-sort-comparator-list) `(comparator list)`
* [`-list`](#-list-arg) `(arg)`
* [`-fix`](#-fix-fn-list) `(fn list)`

### Tree operations

Functions pretending lists are trees.

* [`-tree-seq`](#-tree-seq-branch-children-tree) `(branch children tree)`
* [`-tree-map`](#-tree-map-fn-tree) `(fn tree)`
* [`-tree-map-nodes`](#-tree-map-nodes-pred-fun-tree) `(pred fun tree)`
* [`-tree-reduce`](#-tree-reduce-fn-tree) `(fn tree)`
* [`-tree-reduce-from`](#-tree-reduce-from-fn-init-value-tree) `(fn init-value tree)`
* [`-tree-mapreduce`](#-tree-mapreduce-fn-folder-tree) `(fn folder tree)`
* [`-tree-mapreduce-from`](#-tree-mapreduce-from-fn-folder-init-value-tree) `(fn folder init-value tree)`
* [`-clone`](#-clone-list) `(list)`

### Threading macros

Macros that conditionally combine sequential forms for brevity
or readability.

* [`->`](#--x-optional-form-rest-more) `(x &optional form &rest more)`
* [`->>`](#--x-optional-form-rest-more) `(x &optional form &rest more)`
* [`-->`](#---x-rest-forms) `(x &rest forms)`
* [`-as->`](#-as--value-variable-rest-forms) `(value variable &rest forms)`
* [`-some->`](#-some--x-optional-form-rest-more) `(x &optional form &rest more)`
* [`-some->>`](#-some--x-optional-form-rest-more) `(x &optional form &rest more)`
* [`-some-->`](#-some---expr-rest-forms) `(expr &rest forms)`
* [`-doto`](#-doto-init-rest-forms) `(init &rest forms)`

### Binding

Macros that combine `let` and `let*` with destructuring and flow control.

* [`-when-let`](#-when-let-var-val-rest-body) `((var val) &rest body)`
* [`-when-let*`](#-when-let-vars-vals-rest-body) `(vars-vals &rest body)`
* [`-if-let`](#-if-let-var-val-then-rest-else) `((var val) then &rest else)`
* [`-if-let*`](#-if-let-vars-vals-then-rest-else) `(vars-vals then &rest else)`
* [`-let`](#-let-varlist-rest-body) `(varlist &rest body)`
* [`-let*`](#-let-varlist-rest-body) `(varlist &rest body)`
* [`-lambda`](#-lambda-match-form-rest-body) `(match-form &rest body)`
* [`-setq`](#-setq-match-form-val) `([match-form val] ...)`

### Side effects

Functions iterating over lists for side effect only.

* [`-each`](#-each-list-fn) `(list fn)`
* [`-each-while`](#-each-while-list-pred-fn) `(list pred fn)`
* [`-each-indexed`](#-each-indexed-list-fn) `(list fn)`
* [`-each-r`](#-each-r-list-fn) `(list fn)`
* [`-each-r-while`](#-each-r-while-list-pred-fn) `(list pred fn)`
* [`-dotimes`](#-dotimes-num-fn) `(num fn)`

### Destructive operations

Macros that modify variables holding lists.

* [`!cons`](#cons-car-cdr) `(car cdr)`
* [`!cdr`](#cdr-list) `(list)`

### Function combinators

Functions that manipulate and compose other functions.

* [`-partial`](#-partial-fun-rest-args) `(fun &rest args)`
* [`-rpartial`](#-rpartial-fn-rest-args) `(fn &rest args)`
* [`-juxt`](#-juxt-rest-fns) `(&rest fns)`
* [`-compose`](#-compose-rest-fns) `(&rest fns)`
* [`-applify`](#-applify-fn) `(fn)`
* [`-on`](#-on-op-trans) `(op trans)`
* [`-flip`](#-flip-fn) `(fn)`
* [`-rotate-args`](#-rotate-args-n-fn) `(n fn)`
* [`-const`](#-const-c) `(c)`
* [`-cut`](#-cut-rest-params) `(&rest params)`
* [`-not`](#-not-pred) `(pred)`
* [`-orfn`](#-orfn-rest-preds) `(&rest preds)`
* [`-andfn`](#-andfn-rest-preds) `(&rest preds)`
* [`-iteratefn`](#-iteratefn-fn-n) `(fn n)`
* [`-fixfn`](#-fixfn-fn-optional-equal-test-halt-test) `(fn &optional equal-test halt-test)`
* [`-prodfn`](#-prodfn-rest-fns) `(&rest fns)`

## Maps

Functions in this category take a transforming function, which
is then applied sequentially to each or selected elements of the
input list.  The results are collected in order and returned as a
new list.

#### -map `(fn list)`

Apply `fn` to each item in `list` and return the list of results.

This function's anaphoric counterpart is `--map`.

```el
(-map (lambda (num) (* num num)) '(1 2 3 4)) ;; => (1 4 9 16)
(-map #'1+ '(1 2 3 4)) ;; => (2 3 4 5)
(--map (* it it) '(1 2 3 4)) ;; => (1 4 9 16)
```

#### -map-when `(pred rep list)`

Return a new list where the elements in `list` that do not match the `pred` function
are unchanged, and where the elements in `list` that do match the `pred` function are mapped
through the `rep` function.

Alias: `-replace-where`

See also: [`-update-at`](#-update-at-n-func-list)

```el
(-map-when 'even? 'square '(1 2 3 4)) ;; => (1 4 3 16)
(--map-when (> it 2) (* it it) '(1 2 3 4)) ;; => (1 2 9 16)
(--map-when (= it 2) 17 '(1 2 3 4)) ;; => (1 17 3 4)
```

#### -map-first `(pred rep list)`

Replace first item in `list` satisfying `pred` with result of `rep` called on this item.

See also: [`-map-when`](#-map-when-pred-rep-list), [`-replace-first`](#-replace-first-old-new-list)

```el
(-map-first 'even? 'square '(1 2 3 4)) ;; => (1 4 3 4)
(--map-first (> it 2) (* it it) '(1 2 3 4)) ;; => (1 2 9 4)
(--map-first (= it 2) 17 '(1 2 3 2)) ;; => (1 17 3 2)
```

#### -map-last `(pred rep list)`

Replace last item in `list` satisfying `pred` with result of `rep` called on this item.

See also: [`-map-when`](#-map-when-pred-rep-list), [`-replace-last`](#-replace-last-old-new-list)

```el
(-map-last 'even? 'square '(1 2 3 4)) ;; => (1 2 3 16)
(--map-last (> it 2) (* it it) '(1 2 3 4)) ;; => (1 2 3 16)
(--map-last (= it 2) 17 '(1 2 3 2)) ;; => (1 2 3 17)
```

#### -map-indexed `(fn list)`

Apply `fn` to each index and item in `list` and return the list of results.
This is like [`-map`](#-map-fn-list), but `fn` takes two arguments: the index of the
current element within `list`, and the element itself.

This function's anaphoric counterpart is `--map-indexed`.

For a side-effecting variant, see also [`-each-indexed`](#-each-indexed-list-fn).

```el
(-map-indexed (lambda (index item) (- item index)) '(1 2 3 4)) ;; => (1 1 1 1)
(--map-indexed (- it it-index) '(1 2 3 4)) ;; => (1 1 1 1)
(-map-indexed #'* '(1 2 3 4)) ;; => (0 2 6 12)
```

#### -annotate `(fn list)`

Return a list of cons cells where each cell is `fn` applied to each
element of `list` paired with the unmodified element of `list`.

```el
(-annotate '1+ '(1 2 3)) ;; => ((2 . 1) (3 . 2) (4 . 3))
(-annotate 'length '(("h" "e" "l" "l" "o") ("hello" "world"))) ;; => ((5 "h" "e" "l" "l" "o") (2 "hello" "world"))
(--annotate (< 1 it) '(0 1 2 3)) ;; => ((nil . 0) (nil . 1) (t . 2) (t . 3))
```

#### -splice `(pred fun list)`

Splice lists generated by `fun` in place of elements matching `pred` in `list`.

`fun` takes the element matching `pred` as input.

This function can be used as replacement for `,@` in case you
need to splice several lists at marked positions (for example
with keywords).

See also: [`-splice-list`](#-splice-list-pred-new-list-list), [`-insert-at`](#-insert-at-n-x-list)

```el
(-splice 'even? (lambda (x) (list x x)) '(1 2 3 4)) ;; => (1 2 2 3 4 4)
(--splice 't (list it it) '(1 2 3 4)) ;; => (1 1 2 2 3 3 4 4)
(--splice (equal it :magic) '((list of) (magical) (code)) '((foo) (bar) :magic (baz))) ;; => ((foo) (bar) (list of) (magical) (code) (baz))
```

#### -splice-list `(pred new-list list)`

Splice `new-list` in place of elements matching `pred` in `list`.

See also: [`-splice`](#-splice-pred-fun-list), [`-insert-at`](#-insert-at-n-x-list)

```el
(-splice-list 'keywordp '(a b c) '(1 :foo 2)) ;; => (1 a b c 2)
(-splice-list 'keywordp nil '(1 :foo 2)) ;; => (1 2)
(--splice-list (keywordp it) '(a b c) '(1 :foo 2)) ;; => (1 a b c 2)
```

#### -mapcat `(fn list)`

Return the concatenation of the result of mapping `fn` over `list`.
Thus function `fn` should return a list.

```el
(-mapcat 'list '(1 2 3)) ;; => (1 2 3)
(-mapcat (lambda (item) (list 0 item)) '(1 2 3)) ;; => (0 1 0 2 0 3)
(--mapcat (list 0 it) '(1 2 3)) ;; => (0 1 0 2 0 3)
```

#### -copy `(list)`

Create a shallow copy of `list`.

```el
(-copy '(1 2 3)) ;; => (1 2 3)
(let ((a '(1 2 3))) (eq a (-copy a))) ;; => nil
```

## Sublist selection

Functions returning a sublist of the original list.

#### -filter `(pred list)`

Return a new list of the items in `list` for which `pred` returns non-nil.

Alias: `-select`.

This function's anaphoric counterpart is `--filter`.

For similar operations, see also [`-keep`](#-keep-fn-list) and [`-remove`](#-remove-pred-list).

```el
(-filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => (2 4)
(-filter #'natnump '(-2 -1 0 1 2)) ;; => (0 1 2)
(--filter (= 0 (% it 2)) '(1 2 3 4)) ;; => (2 4)
```

#### -remove `(pred list)`

Return a new list of the items in `list` for which `pred` returns nil.

Alias: `-reject`.

This function's anaphoric counterpart is `--remove`.

For similar operations, see also [`-keep`](#-keep-fn-list) and [`-filter`](#-filter-pred-list).

```el
(-remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => (1 3)
(-remove #'natnump '(-2 -1 0 1 2)) ;; => (-2 -1)
(--remove (= 0 (% it 2)) '(1 2 3 4)) ;; => (1 3)
```

#### -remove-first `(pred list)`

Remove the first item from `list` for which `pred` returns non-nil.
This is a non-destructive operation, but only the front of `list`
leading up to the removed item is a copy; the rest is `list`'s
original tail.  If no item is removed, then the result is a
complete copy.

Alias: `-reject-first`.

This function's anaphoric counterpart is `--remove-first`.

See also [`-map-first`](#-map-first-pred-rep-list), [`-remove-item`](#-remove-item-item-list), and [`-remove-last`](#-remove-last-pred-list).

```el
(-remove-first #'natnump '(-2 -1 0 1 2)) ;; => (-2 -1 1 2)
(-remove-first #'stringp '(1 2 "first" "second")) ;; => (1 2 "second")
(--remove-first (> it 3) '(1 2 3 4 5 6)) ;; => (1 2 3 5 6)
```

#### -remove-last `(pred list)`

Remove the last item from `list` for which `pred` returns non-nil.
The result is a copy of `list` regardless of whether an element is
removed.

Alias: `-reject-last`.

This function's anaphoric counterpart is `--remove-last`.

See also [`-map-last`](#-map-last-pred-rep-list), [`-remove-item`](#-remove-item-item-list), and [`-remove-first`](#-remove-first-pred-list).

```el
(-remove-last #'natnump '(1 3 5 4 7 8 10 -11)) ;; => (1 3 5 4 7 8 -11)
(-remove-last #'stringp '(1 2 "last" "second")) ;; => (1 2 "last")
(--remove-last (> it 3) '(1 2 3 4 5 6 7 8 9 10)) ;; => (1 2 3 4 5 6 7 8 9)
```

#### -remove-item `(item list)`

Return a copy of `list` with all occurrences of `item` removed.
The comparison is done with `equal`.

```el
(-remove-item 3 '(1 2 3 2 3 4 5 3)) ;; => (1 2 2 4 5)
(-remove-item 'foo '(foo bar baz foo)) ;; => (bar baz)
(-remove-item "bob" '("alice" "bob" "eve" "bob")) ;; => ("alice" "eve")
```

#### -non-nil `(list)`

Return a copy of `list` with all nil items removed.

```el
(-non-nil '(nil 1 nil 2 nil nil 3 4 nil 5 nil)) ;; => (1 2 3 4 5)
(-non-nil '((nil))) ;; => ((nil))
(-non-nil ()) ;; => ()
```

#### -slice `(list from &optional to step)`

Return copy of `list`, starting from index `from` to index `to`.

`from` or `to` may be negative.  These values are then interpreted
modulo the length of the list.

If `step` is a number, only each `step`th item in the resulting
section is returned.  Defaults to 1.

```el
(-slice '(1 2 3 4 5) 1) ;; => (2 3 4 5)
(-slice '(1 2 3 4 5) 0 3) ;; => (1 2 3)
(-slice '(1 2 3 4 5 6 7 8 9) 1 -1 2) ;; => (2 4 6 8)
```

#### -take `(n list)`

Return a copy of the first `n` items in `list`.
Return a copy of `list` if it contains `n` items or fewer.
Return nil if `n` is zero or less.

See also: [`-take-last`](#-take-last-n-list).

```el
(-take 3 '(1 2 3 4 5)) ;; => (1 2 3)
(-take 17 '(1 2 3 4 5)) ;; => (1 2 3 4 5)
(-take 0 '(1 2 3 4 5)) ;; => ()
```

#### -take-last `(n list)`

Return a copy of the last `n` items of `list` in order.
Return a copy of `list` if it contains `n` items or fewer.
Return nil if `n` is zero or less.

See also: [`-take`](#-take-n-list).

```el
(-take-last 3 '(1 2 3 4 5)) ;; => (3 4 5)
(-take-last 17 '(1 2 3 4 5)) ;; => (1 2 3 4 5)
(-take-last 1 '(1 2 3 4 5)) ;; => (5)
```

#### -drop `(n list)`

Return the tail (not a copy) of `list` without the first `n` items.
Return nil if `list` contains `n` items or fewer.
Return `list` if `n` is zero or less.

For another variant, see also [`-drop-last`](#-drop-last-n-list).

```el
(-drop 3 '(1 2 3 4 5)) ;; => (4 5)
(-drop 17 '(1 2 3 4 5)) ;; => ()
(-drop 0 '(1 2 3 4 5)) ;; => (1 2 3 4 5)
```

#### -drop-last `(n list)`

Return a copy of `list` without its last `n` items.
Return a copy of `list` if `n` is zero or less.
Return nil if `list` contains `n` items or fewer.

See also: [`-drop`](#-drop-n-list).

```el
(-drop-last 3 '(1 2 3 4 5)) ;; => (1 2)
(-drop-last 17 '(1 2 3 4 5)) ;; => ()
(-drop-last 0 '(1 2 3 4 5)) ;; => (1 2 3 4 5)
```

#### -take-while `(pred list)`

Take successive items from `list` for which `pred` returns non-nil.
`pred` is a function of one argument.  Return a new list of the
successive elements from the start of `list` for which `pred` returns
non-nil.

This function's anaphoric counterpart is `--take-while`.

For another variant, see also [`-drop-while`](#-drop-while-pred-list).

```el
(-take-while #'even? '(1 2 3 4)) ;; => ()
(-take-while #'even? '(2 4 5 6)) ;; => (2 4)
(--take-while (< it 4) '(1 2 3 4 3 2 1)) ;; => (1 2 3)
```

#### -drop-while `(pred list)`

Drop successive items from `list` for which `pred` returns non-nil.
`pred` is a function of one argument.  Return the tail (not a copy)
of `list` starting from its first element for which `pred` returns
nil.

This function's anaphoric counterpart is `--drop-while`.

For another variant, see also [`-take-while`](#-take-while-pred-list).

```el
(-drop-while #'even? '(1 2 3 4)) ;; => (1 2 3 4)
(-drop-while #'even? '(2 4 5 6)) ;; => (5 6)
(--drop-while (< it 4) '(1 2 3 4 3 2 1)) ;; => (4 3 2 1)
```

#### -select-by-indices `(indices list)`

Return a list whose elements are elements from `list` selected
as `(nth i list)` for all i from `indices`.

```el
(-select-by-indices '(4 10 2 3 6) '("v" "e" "l" "o" "c" "i" "r" "a" "p" "t" "o" "r")) ;; => ("c" "o" "l" "o" "r")
(-select-by-indices '(2 1 0) '("a" "b" "c")) ;; => ("c" "b" "a")
(-select-by-indices '(0 1 2 0 1 3 3 1) '("f" "a" "r" "l")) ;; => ("f" "a" "r" "f" "a" "l" "l" "a")
```

#### -select-columns `(columns table)`

Select `columns` from `table`.

`table` is a list of lists where each element represents one row.
It is assumed each row has the same length.

Each row is transformed such that only the specified `columns` are
selected.

See also: [`-select-column`](#-select-column-column-table), [`-select-by-indices`](#-select-by-indices-indices-list)

```el
(-select-columns '(0 2) '((1 2 3) (a b c) (:a :b :c))) ;; => ((1 3) (a c) (:a :c))
(-select-columns '(1) '((1 2 3) (a b c) (:a :b :c))) ;; => ((2) (b) (:b))
(-select-columns nil '((1 2 3) (a b c) (:a :b :c))) ;; => (nil nil nil)
```

#### -select-column `(column table)`

Select `column` from `table`.

`table` is a list of lists where each element represents one row.
It is assumed each row has the same length.

The single selected column is returned as a list.

See also: [`-select-columns`](#-select-columns-columns-table), [`-select-by-indices`](#-select-by-indices-indices-list)

```el
(-select-column 1 '((1 2 3) (a b c) (:a :b :c))) ;; => (2 b :b)
```

## List to list

Functions returning a modified copy of the input list.

#### -keep `(fn list)`

Return a new list of the non-nil results of applying `fn` to each item in `list`.
Like [`-filter`](#-filter-pred-list), but returns the non-nil results of `fn` instead of
the corresponding elements of `list`.

Its anaphoric counterpart is `--keep`.

```el
(-keep #'cdr '((1 2 3) (4 5) (6))) ;; => ((2 3) (5))
(-keep (lambda (n) (and (> n 3) (* 10 n))) '(1 2 3 4 5 6)) ;; => (40 50 60)
(--keep (and (> it 3) (* 10 it)) '(1 2 3 4 5 6)) ;; => (40 50 60)
```

#### -concat `(&rest lists)`

Return a new list with the concatenation of the elements in the supplied `lists`.

```el
(-concat '(1)) ;; => (1)
(-concat '(1) '(2)) ;; => (1 2)
(-concat '(1) '(2 3) '(4)) ;; => (1 2 3 4)
```

#### -flatten `(l)`

Take a nested list `l` and return its contents as a single, flat list.

Note that because `nil` represents a list of zero elements (an
empty list), any mention of nil in `l` will disappear after
flattening.  If you need to preserve nils, consider [`-flatten-n`](#-flatten-n-num-list)
or map them to some unique symbol and then map them back.

Conses of two atoms are considered "terminals", that is, they
aren't flattened further.

See also: [`-flatten-n`](#-flatten-n-num-list)

```el
(-flatten '((1))) ;; => (1)
(-flatten '((1 (2 3) (((4 (5))))))) ;; => (1 2 3 4 5)
(-flatten '(1 2 (3 . 4))) ;; => (1 2 (3 . 4))
```

#### -flatten-n `(num list)`

Flatten `num` levels of a nested `list`.

See also: [`-flatten`](#-flatten-l)

```el
(-flatten-n 1 '((1 2) ((3 4) ((5 6))))) ;; => (1 2 (3 4) ((5 6)))
(-flatten-n 2 '((1 2) ((3 4) ((5 6))))) ;; => (1 2 3 4 (5 6))
(-flatten-n 3 '((1 2) ((3 4) ((5 6))))) ;; => (1 2 3 4 5 6)
```

#### -replace `(old new list)`

Replace all `old` items in `list` with `new`.

Elements are compared using `equal`.

See also: [`-replace-at`](#-replace-at-n-x-list)

```el
(-replace 1 "1" '(1 2 3 4 3 2 1)) ;; => ("1" 2 3 4 3 2 "1")
(-replace "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) ;; => ("a" "nice" "bar" "sentence" "about" "bar")
(-replace 1 2 nil) ;; => nil
```

#### -replace-first `(old new list)`

Replace the first occurrence of `old` with `new` in `list`.

Elements are compared using `equal`.

See also: [`-map-first`](#-map-first-pred-rep-list)

```el
(-replace-first 1 "1" '(1 2 3 4 3 2 1)) ;; => ("1" 2 3 4 3 2 1)
(-replace-first "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) ;; => ("a" "nice" "bar" "sentence" "about" "foo")
(-replace-first 1 2 nil) ;; => nil
```

#### -replace-last `(old new list)`

Replace the last occurrence of `old` with `new` in `list`.

Elements are compared using `equal`.

See also: [`-map-last`](#-map-last-pred-rep-list)

```el
(-replace-last 1 "1" '(1 2 3 4 3 2 1)) ;; => (1 2 3 4 3 2 "1")
(-replace-last "foo" "bar" '("a" "nice" "foo" "sentence" "about" "foo")) ;; => ("a" "nice" "foo" "sentence" "about" "bar")
(-replace-last 1 2 nil) ;; => nil
```

#### -insert-at `(n x list)`

Return a list with `x` inserted into `list` at position `n`.

See also: [`-splice`](#-splice-pred-fun-list), [`-splice-list`](#-splice-list-pred-new-list-list)

```el
(-insert-at 1 'x '(a b c)) ;; => (a x b c)
(-insert-at 12 'x '(a b c)) ;; => (a b c x)
```

#### -replace-at `(n x list)`

Return a list with element at `n`th position in `list` replaced with `x`.

See also: [`-replace`](#-replace-old-new-list)

```el
(-replace-at 0 9 '(0 1 2 3 4 5)) ;; => (9 1 2 3 4 5)
(-replace-at 1 9 '(0 1 2 3 4 5)) ;; => (0 9 2 3 4 5)
(-replace-at 4 9 '(0 1 2 3 4 5)) ;; => (0 1 2 3 9 5)
```

#### -update-at `(n func list)`

Return a list with element at `n`th position in `list` replaced with `(func (nth n list))`.

See also: [`-map-when`](#-map-when-pred-rep-list)

```el
(-update-at 0 (lambda (x) (+ x 9)) '(0 1 2 3 4 5)) ;; => (9 1 2 3 4 5)
(-update-at 1 (lambda (x) (+ x 8)) '(0 1 2 3 4 5)) ;; => (0 9 2 3 4 5)
(--update-at 2 (length it) '("foo" "bar" "baz" "quux")) ;; => ("foo" "bar" 3 "quux")
```

#### -remove-at `(n list)`

Return a list with element at `n`th position in `list` removed.

See also: [`-remove-at-indices`](#-remove-at-indices-indices-list), [`-remove`](#-remove-pred-list)

```el
(-remove-at 0 '("0" "1" "2" "3" "4" "5")) ;; => ("1" "2" "3" "4" "5")
(-remove-at 1 '("0" "1" "2" "3" "4" "5")) ;; => ("0" "2" "3" "4" "5")
(-remove-at 2 '("0" "1" "2" "3" "4" "5")) ;; => ("0" "1" "3" "4" "5")
```

#### -remove-at-indices `(indices list)`

Return a list whose elements are elements from `list` without
elements selected as `(nth i list)` for all i
from `indices`.

See also: [`-remove-at`](#-remove-at-n-list), [`-remove`](#-remove-pred-list)

```el
(-remove-at-indices '(0) '("0" "1" "2" "3" "4" "5")) ;; => ("1" "2" "3" "4" "5")
(-remove-at-indices '(0 2 4) '("0" "1" "2" "3" "4" "5")) ;; => ("1" "3" "5")
(-remove-at-indices '(0 5) '("0" "1" "2" "3" "4" "5")) ;; => ("1" "2" "3" "4")
```

## Reductions

Functions reducing lists to a single value (which may also be a list).

#### -reduce-from `(fn init list)`

Reduce the function `fn` across `list`, starting with `init`.
Return the result of applying `fn` to `init` and the first element of
`list`, then applying `fn` to that result and the second element,
etc.  If `list` is empty, return `init` without calling `fn`.

This function's anaphoric counterpart is `--reduce-from`.

For other folds, see also [`-reduce`](#-reduce-fn-list) and [`-reduce-r`](#-reduce-r-fn-list).

```el
(-reduce-from #'- 10 '(1 2 3)) ;; => 4
(-reduce-from #'list 10 '(1 2 3)) ;; => (((10 1) 2) 3)
(--reduce-from (concat acc " " it) "START" '("a" "b" "c")) ;; => "START a b c"
```

#### -reduce-r-from `(fn init list)`

Reduce the function `fn` across `list` in reverse, starting with `init`.
Return the result of applying `fn` to the last element of `list` and
`init`, then applying `fn` to the second-to-last element and the
previous result of `fn`, etc.  That is, the first argument of `fn` is
the current element, and its second argument the accumulated
value.  If `list` is empty, return `init` without calling `fn`.

This function is like [`-reduce-from`](#-reduce-from-fn-init-list) but the operation associates
from the right rather than left.  In other words, it starts from
the end of `list` and flips the arguments to `fn`.  Conceptually, it
is like replacing the conses in `list` with applications of `fn`, and
its last link with `init`, and evaluating the resulting expression.

This function's anaphoric counterpart is `--reduce-r-from`.

For other folds, see also [`-reduce-r`](#-reduce-r-fn-list) and [`-reduce`](#-reduce-fn-list).

```el
(-reduce-r-from #'- 10 '(1 2 3)) ;; => -8
(-reduce-r-from #'list 10 '(1 2 3)) ;; => (1 (2 (3 10)))
(--reduce-r-from (concat it " " acc) "END" '("a" "b" "c")) ;; => "a b c END"
```

#### -reduce `(fn list)`

Reduce the function `fn` across `list`.
Return the result of applying `fn` to the first two elements of
`list`, then applying `fn` to that result and the third element, etc.
If `list` contains a single element, return it without calling `fn`.
If `list` is empty, return the result of calling `fn` with no
arguments.

This function's anaphoric counterpart is `--reduce`.

For other folds, see also [`-reduce-from`](#-reduce-from-fn-init-list) and [`-reduce-r`](#-reduce-r-fn-list).

```el
(-reduce #'- '(1 2 3 4)) ;; => -8
(-reduce #'list '(1 2 3 4)) ;; => (((1 2) 3) 4)
(--reduce (format "%s-%d" acc it) '(1 2 3)) ;; => "1-2-3"
```

#### -reduce-r `(fn list)`

Reduce the function `fn` across `list` in reverse.
Return the result of applying `fn` to the last two elements of
`list`, then applying `fn` to the third-to-last element and the
previous result of `fn`, etc.  That is, the first argument of `fn` is
the current element, and its second argument the accumulated
value.  If `list` contains a single element, return it without
calling `fn`.  If `list` is empty, return the result of calling `fn`
with no arguments.

This function is like [`-reduce`](#-reduce-fn-list) but the operation associates from
the right rather than left.  In other words, it starts from the
end of `list` and flips the arguments to `fn`.  Conceptually, it is
like replacing the conses in `list` with applications of `fn`,
ignoring its last link, and evaluating the resulting expression.

This function's anaphoric counterpart is `--reduce-r`.

For other folds, see also [`-reduce-r-from`](#-reduce-r-from-fn-init-list) and [`-reduce`](#-reduce-fn-list).

```el
(-reduce-r #'- '(1 2 3 4)) ;; => -2
(-reduce-r #'list '(1 2 3 4)) ;; => (1 (2 (3 4)))
(--reduce-r (format "%s-%d" acc it) '(1 2 3)) ;; => "3-2-1"
```

#### -reductions-from `(fn init list)`

Return a list of `fn`'s intermediate reductions across `list`.
That is, a list of the intermediate values of the accumulator
when [`-reduce-from`](#-reduce-from-fn-init-list) (which see) is called with the same
arguments.

This function's anaphoric counterpart is `--reductions-from`.

For other folds, see also [`-reductions`](#-reductions-fn-list) and [`-reductions-r`](#-reductions-r-fn-list).

```el
(-reductions-from #'max 0 '(2 1 4 3)) ;; => (0 2 2 4 4)
(-reductions-from #'* 1 '(1 2 3 4)) ;; => (1 1 2 6 24)
(--reductions-from (format "(FN %s %d)" acc it) "INIT" '(1 2 3)) ;; => ("INIT" "(FN INIT 1)" "(FN (FN INIT 1) 2)" "(FN (FN (FN INIT 1) 2) 3)")
```

#### -reductions-r-from `(fn init list)`

Return a list of `fn`'s intermediate reductions across reversed `list`.
That is, a list of the intermediate values of the accumulator
when [`-reduce-r-from`](#-reduce-r-from-fn-init-list) (which see) is called with the same
arguments.

This function's anaphoric counterpart is `--reductions-r-from`.

For other folds, see also [`-reductions`](#-reductions-fn-list) and [`-reductions-r`](#-reductions-r-fn-list).

```el
(-reductions-r-from #'max 0 '(2 1 4 3)) ;; => (4 4 4 3 0)
(-reductions-r-from #'* 1 '(1 2 3 4)) ;; => (24 24 12 4 1)
(--reductions-r-from (format "(FN %d %s)" it acc) "INIT" '(1 2 3)) ;; => ("(FN 1 (FN 2 (FN 3 INIT)))" "(FN 2 (FN 3 INIT))" "(FN 3 INIT)" "INIT")
```

#### -reductions `(fn list)`

Return a list of `fn`'s intermediate reductions across `list`.
That is, a list of the intermediate values of the accumulator
when [`-reduce`](#-reduce-fn-list) (which see) is called with the same arguments.

This function's anaphoric counterpart is `--reductions`.

For other folds, see also [`-reductions`](#-reductions-fn-list) and [`-reductions-r`](#-reductions-r-fn-list).

```el
(-reductions #'+ '(1 2 3 4)) ;; => (1 3 6 10)
(-reductions #'* '(1 2 3 4)) ;; => (1 2 6 24)
(--reductions (format "(FN %s %d)" acc it) '(1 2 3)) ;; => (1 "(FN 1 2)" "(FN (FN 1 2) 3)")
```

#### -reductions-r `(fn list)`

Return a list of `fn`'s intermediate reductions across reversed `list`.
That is, a list of the intermediate values of the accumulator
when [`-reduce-r`](#-reduce-r-fn-list) (which see) is called with the same arguments.

This function's anaphoric counterpart is `--reductions-r`.

For other folds, see also [`-reductions-r-from`](#-reductions-r-from-fn-init-list) and
[`-reductions`](#-reductions-fn-list).

```el
(-reductions-r #'+ '(1 2 3 4)) ;; => (10 9 7 4)
(-reductions-r #'* '(1 2 3 4)) ;; => (24 24 12 4)
(--reductions-r (format "(FN %d %s)" it acc) '(1 2 3)) ;; => ("(FN 1 (FN 2 3))" "(FN 2 3)" 3)
```

#### -count `(pred list)`

Counts the number of items in `list` where (`pred` item) is non-nil.

```el
(-count 'even? '(1 2 3 4 5)) ;; => 2
(--count (< it 4) '(1 2 3 4)) ;; => 3
```

#### -sum `(list)`

Return the sum of `list`.

```el
(-sum ()) ;; => 0
(-sum '(1)) ;; => 1
(-sum '(1 2 3 4)) ;; => 10
```

#### -running-sum `(list)`

Return a list with running sums of items in `list`.
`list` must be non-empty.

```el
(-running-sum '(1 2 3 4)) ;; => (1 3 6 10)
(-running-sum '(1)) ;; => (1)
(-running-sum ()) ;; Wrong type argument: consp, nil
```

#### -product `(list)`

Return the product of `list`.

```el
(-product ()) ;; => 1
(-product '(1)) ;; => 1
(-product '(1 2 3 4)) ;; => 24
```

#### -running-product `(list)`

Return a list with running products of items in `list`.
`list` must be non-empty.

```el
(-running-product '(1 2 3 4)) ;; => (1 2 6 24)
(-running-product '(1)) ;; => (1)
(-running-product ()) ;; Wrong type argument: consp, nil
```

#### -inits `(list)`

Return all prefixes of `list`.

```el
(-inits '(1 2 3 4)) ;; => (nil (1) (1 2) (1 2 3) (1 2 3 4))
(-inits nil) ;; => (nil)
(-inits '(1)) ;; => (nil (1))
```

#### -tails `(list)`

Return all suffixes of `list`

```el
(-tails '(1 2 3 4)) ;; => ((1 2 3 4) (2 3 4) (3 4) (4) nil)
(-tails nil) ;; => (nil)
(-tails '(1)) ;; => ((1) nil)
```

#### -common-prefix `(&rest lists)`

Return the longest common prefix of `lists`.

```el
(-common-prefix '(1)) ;; => (1)
(-common-prefix '(1 2) '(3 4) '(1 2)) ;; => ()
(-common-prefix '(1 2) '(1 2 3) '(1 2 3 4)) ;; => (1 2)
```

#### -common-suffix `(&rest lists)`

Return the longest common suffix of `lists`.

```el
(-common-suffix '(1)) ;; => (1)
(-common-suffix '(1 2) '(3 4) '(1 2)) ;; => ()
(-common-suffix '(1 2 3 4) '(2 3 4) '(3 4)) ;; => (3 4)
```

#### -min `(list)`

Return the smallest value from `list` of numbers or markers.

```el
(-min '(0)) ;; => 0
(-min '(3 2 1)) ;; => 1
(-min '(1 2 3)) ;; => 1
```

#### -min-by `(comparator list)`

Take a comparison function `comparator` and a `list` and return
the least element of the list by the comparison function.

See also combinator [`-on`](#-on-op-trans) which can transform the values before
comparing them.

```el
(-min-by '> '(4 3 6 1)) ;; => 1
(--min-by (> (car it) (car other)) '((1 2 3) (2) (3 2))) ;; => (1 2 3)
(--min-by (> (length it) (length other)) '((1 2 3) (2) (3 2))) ;; => (2)
```

#### -max `(list)`

Return the largest value from `list` of numbers or markers.

```el
(-max '(0)) ;; => 0
(-max '(3 2 1)) ;; => 3
(-max '(1 2 3)) ;; => 3
```

#### -max-by `(comparator list)`

Take a comparison function `comparator` and a `list` and return
the greatest element of the list by the comparison function.

See also combinator [`-on`](#-on-op-trans) which can transform the values before
comparing them.

```el
(-max-by '> '(4 3 6 1)) ;; => 6
(--max-by (> (car it) (car other)) '((1 2 3) (2) (3 2))) ;; => (3 2)
(--max-by (> (length it) (length other)) '((1 2 3) (2) (3 2))) ;; => (1 2 3)
```

## Unfolding

Operations dual to reductions, building lists from a seed
value rather than consuming a list to produce a single value.

#### -iterate `(fun init n)`

Return a list of iterated applications of `fun` to `init`.

This means a list of the form:

    (`init` (`fun` `init`) (`fun` (`fun` `init`)) ...)

`n` is the length of the returned list.

```el
(-iterate #'1+ 1 10) ;; => (1 2 3 4 5 6 7 8 9 10)
(-iterate (lambda (x) (+ x x)) 2 5) ;; => (2 4 8 16 32)
(--iterate (* it it) 2 5) ;; => (2 4 16 256 65536)
```

#### -unfold `(fun seed)`

Build a list from `seed` using `fun`.

This is "dual" operation to [`-reduce-r`](#-reduce-r-fn-list): while -reduce-r
consumes a list to produce a single value, [`-unfold`](#-unfold-fun-seed) takes a
seed value and builds a (potentially infinite!) list.

`fun` should return `nil` to stop the generating process, or a
cons (`a` . `b`), where `a` will be prepended to the result and `b` is
the new seed.

```el
(-unfold (lambda (x) (unless (= x 0) (cons x (1- x)))) 10) ;; => (10 9 8 7 6 5 4 3 2 1)
(--unfold (when it (cons it (cdr it))) '(1 2 3 4)) ;; => ((1 2 3 4) (2 3 4) (3 4) (4))
(--unfold (when it (cons it (butlast it))) '(1 2 3 4)) ;; => ((1 2 3 4) (1 2 3) (1 2) (1))
```

## Predicates

Reductions of one or more lists to a boolean value.

#### -some `(pred list)`

Return (`pred` x) for the first `list` item where (`pred` x) is non-nil, else nil.

Alias: `-any`.

This function's anaphoric counterpart is `--some`.

```el
(-some #'stringp '(1 "2" 3)) ;; => t
(--some (string-match-p "x" it) '("foo" "axe" "xor")) ;; => 1
(--some (= it-index 3) '(0 1 2)) ;; => nil
```

#### -every `(pred list)`

Return non-nil if `pred` returns non-nil for all items in `list`.
If so, return the last such result of `pred`.  Otherwise, once an
item is reached for which `pred` returns nil, return nil without
calling `pred` on any further `list` elements.

This function is like `-every-p`, but on success returns the last
non-nil result of `pred` instead of just t.

This function's anaphoric counterpart is `--every`.

```el
(-every #'numberp '(1 2 3)) ;; => t
(--every (string-match-p "x" it) '("axe" "xor")) ;; => 0
(--every (= it it-index) '(0 1 3)) ;; => nil
```

#### -any? `(pred list)`

Return t if (`pred` x) is non-nil for any x in `list`, else nil.

Alias: `-any-p`, `-some?`, `-some-p`

```el
(-any? #'numberp '(nil 0 t)) ;; => t
(-any? #'numberp '(nil t t)) ;; => nil
(-any? #'null '(1 3 5)) ;; => nil
```

#### -all? `(pred list)`

Return t if (`pred` `x`) is non-nil for all `x` in `list`, else nil.
In the latter case, stop after the first `x` for which (`pred` `x`) is
nil, without calling `pred` on any subsequent elements of `list`.

The similar function [`-every`](#-every-pred-list) is more widely useful, since it
returns the last non-nil result of `pred` instead of just t on
success.

Alias: `-all-p`, `-every-p`, `-every?`.

This function's anaphoric counterpart is `--all?`.

```el
(-all? #'numberp '(1 2 3)) ;; => t
(-all? #'numberp '(2 t 6)) ;; => nil
(--all? (= 0 (% it 2)) '(2 4 6)) ;; => t
```

#### -none? `(pred list)`

Return t if (`pred` x) is nil for all x in `list`, else nil.

Alias: `-none-p`

```el
(-none? 'even? '(1 2 3)) ;; => nil
(-none? 'even? '(1 3 5)) ;; => t
(--none? (= 0 (% it 2)) '(1 2 3)) ;; => nil
```

#### -only-some? `(pred list)`

Return `t` if at least one item of `list` matches `pred` and at least one item of `list` does not match `pred`.
Return `nil` both if all items match the predicate or if none of the items match the predicate.

Alias: `-only-some-p`

```el
(-only-some? 'even? '(1 2 3)) ;; => t
(-only-some? 'even? '(1 3 5)) ;; => nil
(-only-some? 'even? '(2 4 6)) ;; => nil
```

#### -contains? `(list element)`

Return non-nil if `list` contains `element`.

The test for equality is done with `equal`, or with `-compare-fn`
if that's non-nil.

Alias: `-contains-p`

```el
(-contains? '(1 2 3) 1) ;; => t
(-contains? '(1 2 3) 2) ;; => t
(-contains? '(1 2 3) 4) ;; => nil
```

#### -same-items? `(list list2)`

Return true if `list` and `list2` has the same items.

The order of the elements in the lists does not matter.

Alias: `-same-items-p`

```el
(-same-items? '(1 2 3) '(1 2 3)) ;; => t
(-same-items? '(1 2 3) '(3 2 1)) ;; => t
(-same-items? '(1 2 3) '(1 2 3 4)) ;; => nil
```

#### -is-prefix? `(prefix list)`

Return non-nil if `prefix` is a prefix of `list`.

Alias: `-is-prefix-p`.

```el
(-is-prefix? '(1 2 3) '(1 2 3 4 5)) ;; => t
(-is-prefix? '(1 2 3 4 5) '(1 2 3)) ;; => nil
(-is-prefix? '(1 3) '(1 2 3 4 5)) ;; => nil
```

#### -is-suffix? `(suffix list)`

Return non-nil if `suffix` is a suffix of `list`.

Alias: `-is-suffix-p`.

```el
(-is-suffix? '(3 4 5) '(1 2 3 4 5)) ;; => t
(-is-suffix? '(1 2 3 4 5) '(3 4 5)) ;; => nil
(-is-suffix? '(3 5) '(1 2 3 4 5)) ;; => nil
```

#### -is-infix? `(infix list)`

Return non-nil if `infix` is infix of `list`.

This operation runs in O(n^2) time

Alias: `-is-infix-p`

```el
(-is-infix? '(1 2 3) '(1 2 3 4 5)) ;; => t
(-is-infix? '(2 3 4) '(1 2 3 4 5)) ;; => t
(-is-infix? '(3 4 5) '(1 2 3 4 5)) ;; => t
```

#### -cons-pair? `(obj)`

Return non-nil if `obj` is a true cons pair.
That is, a cons (`a` . `b`) where `b` is not a list.

Alias: `-cons-pair-p`.

```el
(-cons-pair? '(1 . 2)) ;; => t
(-cons-pair? '(1 2)) ;; => nil
(-cons-pair? '(1)) ;; => nil
```

## Partitioning

Functions partitioning the input list into a list of lists.

#### -split-at `(n list)`

Split `list` into two sublists after the `n`th element.
The result is a list of two elements (`take` `drop`) where `take` is a
new list of the first `n` elements of `list`, and `drop` is the
remaining elements of `list` (not a copy).  `take` and `drop` are like
the results of [`-take`](#-take-n-list) and [`-drop`](#-drop-n-list), respectively, but the split
is done in a single list traversal.

```el
(-split-at 3 '(1 2 3 4 5)) ;; => ((1 2 3) (4 5))
(-split-at 17 '(1 2 3 4 5)) ;; => ((1 2 3 4 5) nil)
(-split-at 0 '(1 2 3 4 5)) ;; => (nil (1 2 3 4 5))
```

#### -split-with `(pred list)`

Return a list of ((-take-while `pred` `list`) (-drop-while `pred` `list`)), in no more than one pass through the list.

```el
(-split-with 'even? '(1 2 3 4)) ;; => (nil (1 2 3 4))
(-split-with 'even? '(2 4 5 6)) ;; => ((2 4) (5 6))
(--split-with (< it 4) '(1 2 3 4 3 2 1)) ;; => ((1 2 3) (4 3 2 1))
```

#### -split-on `(item list)`

Split the `list` each time `item` is found.

Unlike [`-partition-by`](#-partition-by-fn-list), the `item` is discarded from the results.
Empty lists are also removed from the result.

Comparison is done by `equal`.

See also [`-split-when`](#-split-when-fn-list)

```el
(-split-on '| '(Nil | Leaf a | Node [Tree a])) ;; => ((Nil) (Leaf a) (Node [Tree a]))
(-split-on :endgroup '("a" "b" :endgroup "c" :endgroup "d" "e")) ;; => (("a" "b") ("c") ("d" "e"))
(-split-on :endgroup '("a" "b" :endgroup :endgroup "d" "e")) ;; => (("a" "b") ("d" "e"))
```

#### -split-when `(fn list)`

Split the `list` on each element where `fn` returns non-nil.

Unlike [`-partition-by`](#-partition-by-fn-list), the "matched" element is discarded from
the results.  Empty lists are also removed from the result.

This function can be thought of as a generalization of
`split-string`.

```el
(-split-when 'even? '(1 2 3 4 5 6)) ;; => ((1) (3) (5))
(-split-when 'even? '(1 2 3 4 6 8 9)) ;; => ((1) (3) (9))
(--split-when (memq it '(&optional &rest)) '(a b &optional c d &rest args)) ;; => ((a b) (c d) (args))
```

#### -separate `(pred list)`

Return a list of ((-filter `pred` `list`) (-remove `pred` `list`)), in one pass through the list.

```el
(-separate (lambda (num) (= 0 (% num 2))) '(1 2 3 4 5 6 7)) ;; => ((2 4 6) (1 3 5 7))
(--separate (< it 5) '(3 7 5 9 3 2 1 4 6)) ;; => ((3 3 2 1 4) (7 5 9 6))
(-separate 'cdr '((1 2) (1) (1 2 3) (4))) ;; => (((1 2) (1 2 3)) ((1) (4)))
```

#### -partition `(n list)`

Return a new list with the items in `list` grouped into `n`-sized sublists.
If there are not enough items to make the last group `n`-sized,
those items are discarded.

```el
(-partition 2 '(1 2 3 4 5 6)) ;; => ((1 2) (3 4) (5 6))
(-partition 2 '(1 2 3 4 5 6 7)) ;; => ((1 2) (3 4) (5 6))
(-partition 3 '(1 2 3 4 5 6 7)) ;; => ((1 2 3) (4 5 6))
```

#### -partition-all `(n list)`

Return a new list with the items in `list` grouped into `n`-sized sublists.
The last group may contain less than `n` items.

```el
(-partition-all 2 '(1 2 3 4 5 6)) ;; => ((1 2) (3 4) (5 6))
(-partition-all 2 '(1 2 3 4 5 6 7)) ;; => ((1 2) (3 4) (5 6) (7))
(-partition-all 3 '(1 2 3 4 5 6 7)) ;; => ((1 2 3) (4 5 6) (7))
```

#### -partition-in-steps `(n step list)`

Return a new list with the items in `list` grouped into `n`-sized sublists at offsets `step` apart.
If there are not enough items to make the last group `n`-sized,
those items are discarded.

```el
(-partition-in-steps 2 1 '(1 2 3 4)) ;; => ((1 2) (2 3) (3 4))
(-partition-in-steps 3 2 '(1 2 3 4)) ;; => ((1 2 3))
(-partition-in-steps 3 2 '(1 2 3 4 5)) ;; => ((1 2 3) (3 4 5))
```

#### -partition-all-in-steps `(n step list)`

Return a new list with the items in `list` grouped into `n`-sized sublists at offsets `step` apart.
The last groups may contain less than `n` items.

```el
(-partition-all-in-steps 2 1 '(1 2 3 4)) ;; => ((1 2) (2 3) (3 4) (4))
(-partition-all-in-steps 3 2 '(1 2 3 4)) ;; => ((1 2 3) (3 4))
(-partition-all-in-steps 3 2 '(1 2 3 4 5)) ;; => ((1 2 3) (3 4 5) (5))
```

#### -partition-by `(fn list)`

Apply `fn` to each item in `list`, splitting it each time `fn` returns a new value.

```el
(-partition-by 'even? ()) ;; => ()
(-partition-by 'even? '(1 1 2 2 2 3 4 6 8)) ;; => ((1 1) (2 2 2) (3) (4 6 8))
(--partition-by (< it 3) '(1 2 3 4 3 2 1)) ;; => ((1 2) (3 4 3) (2 1))
```

#### -partition-by-header `(fn list)`

Apply `fn` to the first item in `list`. That is the header
value. Apply `fn` to each item in `list`, splitting it each time `fn`
returns the header value, but only after seeing at least one
other value (the body).

```el
(--partition-by-header (= it 1) '(1 2 3 1 2 1 2 3 4)) ;; => ((1 2 3) (1 2) (1 2 3 4))
(--partition-by-header (> it 0) '(1 2 0 1 0 1 2 3 0)) ;; => ((1 2 0) (1 0) (1 2 3 0))
(-partition-by-header 'even? '(2 1 1 1 4 1 3 5 6 6 1)) ;; => ((2 1 1 1) (4 1 3 5) (6 6 1))
```

#### -partition-after-pred `(pred list)`

Partition `list` after each element for which `pred` returns non-nil.

This function's anaphoric counterpart is `--partition-after-pred`.

```el
(-partition-after-pred #'booleanp ()) ;; => ()
(-partition-after-pred #'booleanp '(t t)) ;; => ((t) (t))
(-partition-after-pred #'booleanp '(0 0 t t 0 t)) ;; => ((0 0 t) (t) (0 t))
```

#### -partition-before-pred `(pred list)`

Partition directly before each time `pred` is true on an element of `list`.

```el
(-partition-before-pred #'booleanp ()) ;; => ()
(-partition-before-pred #'booleanp '(0 t)) ;; => ((0) (t))
(-partition-before-pred #'booleanp '(0 0 t 0 t t)) ;; => ((0 0) (t 0) (t) (t))
```

#### -partition-before-item `(item list)`

Partition directly before each time `item` appears in `list`.

```el
(-partition-before-item 3 ()) ;; => ()
(-partition-before-item 3 '(1)) ;; => ((1))
(-partition-before-item 3 '(3)) ;; => ((3))
```

#### -partition-after-item `(item list)`

Partition directly after each time `item` appears in `list`.

```el
(-partition-after-item 3 ()) ;; => ()
(-partition-after-item 3 '(1)) ;; => ((1))
(-partition-after-item 3 '(3)) ;; => ((3))
```

#### -group-by `(fn list)`

Separate `list` into an alist whose keys are `fn` applied to the
elements of `list`.  Keys are compared by `equal`.

```el
(-group-by 'even? ()) ;; => ()
(-group-by 'even? '(1 1 2 2 2 3 4 6 8)) ;; => ((nil 1 1 3) (t 2 2 2 4 6 8))
(--group-by (car (split-string it "/")) '("a/b" "c/d" "a/e")) ;; => (("a" "a/b" "a/e") ("c" "c/d"))
```

## Indexing

Functions retrieving or sorting based on list indices and
related predicates.

#### -elem-index `(elem list)`

Return the index of the first element in the given `list` which
is equal to the query element `elem`, or nil if there is no
such element.

```el
(-elem-index 2 '(6 7 8 2 3 4)) ;; => 3
(-elem-index "bar" '("foo" "bar" "baz")) ;; => 1
(-elem-index '(1 2) '((3) (5 6) (1 2) nil)) ;; => 2
```

#### -elem-indices `(elem list)`

Return the indices of all elements in `list` equal to the query
element `elem`, in ascending order.

```el
(-elem-indices 2 '(6 7 8 2 3 4 2 1)) ;; => (3 6)
(-elem-indices "bar" '("foo" "bar" "baz")) ;; => (1)
(-elem-indices '(1 2) '((3) (1 2) (5 6) (1 2) nil)) ;; => (1 3)
```

#### -find-index `(pred list)`

Take a predicate `pred` and a `list` and return the index of the
first element in the list satisfying the predicate, or nil if
there is no such element.

See also [`-first`](#-first-pred-list).

```el
(-find-index 'even? '(2 4 1 6 3 3 5 8)) ;; => 0
(--find-index (< 5 it) '(2 4 1 6 3 3 5 8)) ;; => 3
(-find-index (-partial 'string-lessp "baz") '("bar" "foo" "baz")) ;; => 1
```

#### -find-last-index `(pred list)`

Take a predicate `pred` and a `list` and return the index of the
last element in the list satisfying the predicate, or nil if
there is no such element.

See also [`-last`](#-last-pred-list).

```el
(-find-last-index 'even? '(2 4 1 6 3 3 5 8)) ;; => 7
(--find-last-index (< 5 it) '(2 7 1 6 3 8 5 2)) ;; => 5
(-find-last-index (-partial 'string-lessp "baz") '("q" "foo" "baz")) ;; => 1
```

#### -find-indices `(pred list)`

Return the indices of all elements in `list` satisfying the
predicate `pred`, in ascending order.

```el
(-find-indices 'even? '(2 4 1 6 3 3 5 8)) ;; => (0 1 3 7)
(--find-indices (< 5 it) '(2 4 1 6 3 3 5 8)) ;; => (3 7)
(-find-indices (-partial 'string-lessp "baz") '("bar" "foo" "baz")) ;; => (1)
```

#### -grade-up `(comparator list)`

Grade elements of `list` using `comparator` relation.
This yields a permutation vector such that applying this
permutation to `list` sorts it in ascending order.

```el
(-grade-up #'< '(3 1 4 2 1 3 3)) ;; => (1 4 3 0 5 6 2)
(let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-up #'< l) l)) ;; => (1 1 2 3 3 3 4)
```

#### -grade-down `(comparator list)`

Grade elements of `list` using `comparator` relation.
This yields a permutation vector such that applying this
permutation to `list` sorts it in descending order.

```el
(-grade-down #'< '(3 1 4 2 1 3 3)) ;; => (2 0 5 6 3 1 4)
(let ((l '(3 1 4 2 1 3 3))) (-select-by-indices (-grade-down #'< l) l)) ;; => (4 3 3 3 2 1 1)
```

## Set operations

Operations pretending lists are sets.

#### -union `(list list2)`

Return a new list containing the elements of `list` and elements of `list2` that are not in `list`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```el
(-union '(1 2 3) '(3 4 5)) ;; => (1 2 3 4 5)
(-union '(1 2 3 4) ()) ;; => (1 2 3 4)
(-union '(1 1 2 2) '(3 2 1)) ;; => (1 1 2 2 3)
```

#### -difference `(list list2)`

Return a new list with only the members of `list` that are not in `list2`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```el
(-difference () ()) ;; => ()
(-difference '(1 2 3) '(4 5 6)) ;; => (1 2 3)
(-difference '(1 2 3 4) '(3 4 5 6)) ;; => (1 2)
```

#### -intersection `(list list2)`

Return a new list containing only the elements that are members of both `list` and `list2`.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

```el
(-intersection () ()) ;; => ()
(-intersection '(1 2 3) '(4 5 6)) ;; => ()
(-intersection '(1 2 3 4) '(3 4 5 6)) ;; => (3 4)
```

#### -powerset `(list)`

Return the power set of `list`.

```el
(-powerset ()) ;; => (nil)
(-powerset '(x y z)) ;; => ((x y z) (x y) (x z) (x) (y z) (y) (z) nil)
```

#### -permutations `(list)`

Return the permutations of `list`.

```el
(-permutations ()) ;; => (nil)
(-permutations '(1 2)) ;; => ((1 2) (2 1))
(-permutations '(a b c)) ;; => ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
```

#### -distinct `(list)`

Return a new list with all duplicates removed.
The test for equality is done with `equal`,
or with `-compare-fn` if that's non-nil.

Alias: `-uniq`

```el
(-distinct ()) ;; => ()
(-distinct '(1 2 2 4)) ;; => (1 2 4)
(-distinct '(t t t)) ;; => (t)
```

## Other list operations

Other list functions not fit to be classified elsewhere.

#### -rotate `(n list)`

Rotate `list` `n` places to the right (left if `n` is negative).
The time complexity is O(n).

```el
(-rotate 3 '(1 2 3 4 5 6 7)) ;; => (5 6 7 1 2 3 4)
(-rotate -3 '(1 2 3 4 5 6 7)) ;; => (4 5 6 7 1 2 3)
(-rotate 16 '(1 2 3 4 5 6 7)) ;; => (6 7 1 2 3 4 5)
```

#### -repeat `(n x)`

Return a new list of length `n` with each element being `x`.
Return nil if `n` is less than 1.

```el
(-repeat 3 :a) ;; => (:a :a :a)
(-repeat 1 :a) ;; => (:a)
(-repeat 0 :a) ;; => nil
```

#### -cons* `(&rest args)`

Make a new list from the elements of `args`.
The last 2 elements of `args` are used as the final cons of the
result, so if the final element of `args` is not a list, the result
is a dotted list.  With no `args`, return nil.

```el
(-cons* 1 2) ;; => (1 . 2)
(-cons* 1 2 3) ;; => (1 2 . 3)
(-cons* 1) ;; => 1
```

#### -snoc `(list elem &rest elements)`

Append `elem` to the end of the list.

This is like `cons`, but operates on the end of list.

If `elements` is non nil, append these to the list as well.

```el
(-snoc '(1 2 3) 4) ;; => (1 2 3 4)
(-snoc '(1 2 3) 4 5 6) ;; => (1 2 3 4 5 6)
(-snoc '(1 2 3) '(4 5 6)) ;; => (1 2 3 (4 5 6))
```

#### -interpose `(sep list)`

Return a new list of all elements in `list` separated by `sep`.

```el
(-interpose "-" ()) ;; => ()
(-interpose "-" '("a")) ;; => ("a")
(-interpose "-" '("a" "b" "c")) ;; => ("a" "-" "b" "-" "c")
```

#### -interleave `(&rest lists)`

Return a new list of the first item in each list, then the second etc.

```el
(-interleave '(1 2) '("a" "b")) ;; => (1 "a" 2 "b")
(-interleave '(1 2) '("a" "b") '("A" "B")) ;; => (1 "a" "A" 2 "b" "B")
(-interleave '(1 2 3) '("a" "b")) ;; => (1 "a" 2 "b")
```

#### -iota `(count &optional start step)`

Return a list containing `count` numbers.
Starts from `start` and adds `step` each time.  The default `start` is
zero, the default `step` is 1.
This function takes its name from the corresponding primitive in
the `apl` language.

```el
(-iota 6) ;; => (0 1 2 3 4 5)
(-iota 4 2.5 -2) ;; => (2.5 0.5 -1.5 -3.5)
(-iota -1) ;; Wrong type argument: natnump, -1
```

#### -zip-with `(fn list1 list2)`

Zip the two lists `list1` and `list2` using a function `fn`.  This
function is applied pairwise taking as first argument element of
`list1` and as second argument element of `list2` at corresponding
position.

The anaphoric form `--zip-with` binds the elements from `list1` as symbol `it`,
and the elements from `list2` as symbol `other`.

```el
(-zip-with '+ '(1 2 3) '(4 5 6)) ;; => (5 7 9)
(-zip-with 'cons '(1 2 3) '(4 5 6)) ;; => ((1 . 4) (2 . 5) (3 . 6))
(--zip-with (concat it " and " other) '("Batman" "Jekyll") '("Robin" "Hyde")) ;; => ("Batman and Robin" "Jekyll and Hyde")
```

#### -zip `(&rest lists)`

Zip `lists` together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

If two lists are provided as arguments, return the groupings as a list
of cons cells. Otherwise, return the groupings as a list of lists.

Use [`-zip-lists`](#-zip-lists-rest-lists) if you need the return value to always be a list
of lists.

Alias: `-zip-pair`

See also: [`-zip-lists`](#-zip-lists-rest-lists)

```el
(-zip '(1 2 3) '(4 5 6)) ;; => ((1 . 4) (2 . 5) (3 . 6))
(-zip '(1 2 3) '(4 5 6 7)) ;; => ((1 . 4) (2 . 5) (3 . 6))
(-zip '(1 2) '(3 4 5) '(6)) ;; => ((1 3 6))
```

#### -zip-lists `(&rest lists)`

Zip `lists` together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

The return value is always list of lists, which is a difference
from `-zip-pair` which returns a cons-cell in case two input
lists are provided.

See also: [`-zip`](#-zip-rest-lists)

```el
(-zip-lists '(1 2 3) '(4 5 6)) ;; => ((1 4) (2 5) (3 6))
(-zip-lists '(1 2 3) '(4 5 6 7)) ;; => ((1 4) (2 5) (3 6))
(-zip-lists '(1 2) '(3 4 5) '(6)) ;; => ((1 3 6))
```

#### -zip-fill `(fill-value &rest lists)`

Zip `lists`, with `fill-value` padded onto the shorter lists. The
lengths of the returned groupings are equal to the length of the
longest input list.

```el
(-zip-fill 0 '(1 2 3 4 5) '(6 7 8 9)) ;; => ((1 . 6) (2 . 7) (3 . 8) (4 . 9) (5 . 0))
```

#### -unzip `(lists)`

Unzip `lists`.

This works just like [`-zip`](#-zip-rest-lists) but takes a list of lists instead of
a variable number of arguments, such that

    (-unzip (-zip `l1` `l2` `l3` ...))

is identity (given that the lists are the same length).

Note in particular that calling this on a list of two lists will
return a list of cons-cells such that the above identity works.

See also: [`-zip`](#-zip-rest-lists)

```el
(-unzip (-zip '(1 2 3) '(a b c) '("e" "f" "g"))) ;; => ((1 2 3) (a b c) ("e" "f" "g"))
(-unzip '((1 2) (3 4) (5 6) (7 8) (9 10))) ;; => ((1 3 5 7 9) (2 4 6 8 10))
(-unzip '((1 2) (3 4))) ;; => ((1 . 3) (2 . 4))
```

#### -cycle `(list)`

Return an infinite circular copy of `list`.
The returned list cycles through the elements of `list` and repeats
from the beginning.

```el
(-take 5 (-cycle '(1 2 3))) ;; => (1 2 3 1 2)
(-take 7 (-cycle '(1 "and" 3))) ;; => (1 "and" 3 1 "and" 3 1)
(-zip (-cycle '(1 2 3)) '(1 2)) ;; => ((1 . 1) (2 . 2))
```

#### -pad `(fill-value &rest lists)`

Appends `fill-value` to the end of each list in `lists` such that they
will all have the same length.

```el
(-pad 0 ()) ;; => (nil)
(-pad 0 '(1)) ;; => ((1))
(-pad 0 '(1 2 3) '(4 5)) ;; => ((1 2 3) (4 5 0))
```

#### -table `(fn &rest lists)`

Compute outer product of `lists` using function `fn`.

The function `fn` should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The dimension of the result is (length lists).

See also: [`-table-flat`](#-table-flat-fn-rest-lists)

```el
(-table '* '(1 2 3) '(1 2 3)) ;; => ((1 2 3) (2 4 6) (3 6 9))
(-table (lambda (a b) (-sum (-zip-with '* a b))) '((1 2) (3 4)) '((1 3) (2 4))) ;; => ((7 15) (10 22))
(apply '-table 'list (-repeat 3 '(1 2))) ;; => ((((1 1 1) (2 1 1)) ((1 2 1) (2 2 1))) (((1 1 2) (2 1 2)) ((1 2 2) (2 2 2))))
```

#### -table-flat `(fn &rest lists)`

Compute flat outer product of `lists` using function `fn`.

The function `fn` should have the same arity as the number of
supplied lists.

The outer product is computed by applying fn to all possible
combinations created by taking one element from each list in
order.  The results are flattened, ignoring the tensor structure
of the result.  This is equivalent to calling:

    (-flatten-n (1- (length lists)) (apply '-table fn lists))

but the implementation here is much more efficient.

See also: [`-flatten-n`](#-flatten-n-num-list), [`-table`](#-table-fn-rest-lists)

```el
(-table-flat 'list '(1 2 3) '(a b c)) ;; => ((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))
(-table-flat '* '(1 2 3) '(1 2 3)) ;; => (1 2 3 2 4 6 3 6 9)
(apply '-table-flat 'list (-repeat 3 '(1 2))) ;; => ((1 1 1) (2 1 1) (1 2 1) (2 2 1) (1 1 2) (2 1 2) (1 2 2) (2 2 2))
```

#### -first `(pred list)`

Return the first item in `list` for which `pred` returns non-nil.
Return nil if no such element is found.
To get the first item in the list no questions asked, use `car`.

Alias: `-find`.

This function's anaphoric counterpart is `--first`.

```el
(-first #'natnump '(-1 0 1)) ;; => 0
(-first #'null '(1 2 3)) ;; => nil
(--first (> it 2) '(1 2 3)) ;; => 3
```

#### -last `(pred list)`

Return the last x in `list` where (`pred` x) is non-nil, else nil.

```el
(-last 'even? '(1 2 3 4 5 6 3 3 3)) ;; => 6
(-last 'even? '(1 3 7 5 9)) ;; => nil
(--last (> (length it) 3) '("a" "looong" "word" "and" "short" "one")) ;; => "short"
```

#### -first-item `(list)`

Return the first item of `list`, or nil on an empty list.

See also: [`-second-item`](#-second-item-list), [`-last-item`](#-last-item-list).

```el
(-first-item '(1 2 3)) ;; => 1
(-first-item nil) ;; => nil
(let ((list (list 1 2 3))) (setf (-first-item list) 5) list) ;; => (5 2 3)
```

#### -second-item `(list)`

Return the second item of `list`, or nil if `list` is too short.

See also: [`-third-item`](#-third-item-list).

```el
(-second-item '(1 2 3)) ;; => 2
(-second-item nil) ;; => nil
```

#### -third-item `(list)`

Return the third item of `list`, or nil if `list` is too short.

See also: [`-fourth-item`](#-fourth-item-list).

```el
(-third-item '(1 2 3)) ;; => 3
(-third-item nil) ;; => nil
```

#### -fourth-item `(list)`

Return the fourth item of `list`, or nil if `list` is too short.

See also: [`-fifth-item`](#-fifth-item-list).

```el
(-fourth-item '(1 2 3 4)) ;; => 4
(-fourth-item nil) ;; => nil
```

#### -fifth-item `(list)`

Return the fifth item of `list`, or nil if `list` is too short.

See also: [`-last-item`](#-last-item-list).

```el
(-fifth-item '(1 2 3 4 5)) ;; => 5
(-fifth-item nil) ;; => nil
```

#### -last-item `(list)`

Return the last item of `list`, or nil on an empty list.

```el
(-last-item '(1 2 3)) ;; => 3
(-last-item nil) ;; => nil
(let ((list (list 1 2 3))) (setf (-last-item list) 5) list) ;; => (1 2 5)
```

#### -butlast `(list)`

Return a list of all items in list except for the last.

```el
(-butlast '(1 2 3)) ;; => (1 2)
(-butlast '(1 2)) ;; => (1)
(-butlast '(1)) ;; => nil
```

#### -sort `(comparator list)`

Sort `list`, stably, comparing elements using `comparator`.
Return the sorted list.  `list` is `not` modified by side effects.
`comparator` is called with two elements of `list`, and should return non-nil
if the first element should sort before the second.

```el
(-sort '< '(3 1 2)) ;; => (1 2 3)
(-sort '> '(3 1 2)) ;; => (3 2 1)
(--sort (< it other) '(3 1 2)) ;; => (1 2 3)
```

#### -list `(arg)`

Ensure `arg` is a list.
If `arg` is already a list, return it as is (not a copy).
Otherwise, return a new list with `arg` as its only element.

Another supported calling convention is (-list &rest `args`).
In this case, if `arg` is not a list, a new list with all of
`args` as elements is returned.  This use is supported for
backward compatibility and is otherwise deprecated.

```el
(-list 1) ;; => (1)
(-list ()) ;; => ()
(-list '(1 2 3)) ;; => (1 2 3)
```

#### -fix `(fn list)`

Compute the (least) fixpoint of `fn` with initial input `list`.

`fn` is called at least once, results are compared with `equal`.

```el
(-fix (lambda (l) (-non-nil (--mapcat (-split-at (/ (length it) 2) it) l))) '((1 2 3))) ;; => ((1) (2) (3))
(let ((l '((starwars scifi) (jedi starwars warrior)))) (--fix (-uniq (--mapcat (cons it (cdr (assq it l))) it)) '(jedi book))) ;; => (jedi starwars warrior scifi book)
```

## Tree operations

Functions pretending lists are trees.

#### -tree-seq `(branch children tree)`

Return a sequence of the nodes in `tree`, in depth-first search order.

`branch` is a predicate of one argument that returns non-nil if the
passed argument is a branch, that is, a node that can have children.

`children` is a function of one argument that returns the children
of the passed branch node.

Non-branch nodes are simply copied.

```el
(-tree-seq 'listp 'identity '(1 (2 3) 4 (5 (6 7)))) ;; => ((1 (2 3) 4 (5 (6 7))) 1 (2 3) 2 3 4 (5 (6 7)) 5 (6 7) 6 7)
(-tree-seq 'listp 'reverse '(1 (2 3) 4 (5 (6 7)))) ;; => ((1 (2 3) 4 (5 (6 7))) (5 (6 7)) (6 7) 7 6 5 4 (2 3) 3 2 1)
(--tree-seq (vectorp it) (append it nil) [1 [2 3] 4 [5 [6 7]]]) ;; => ([1 [2 3] 4 [5 [6 7]]] 1 [2 3] 2 3 4 [5 [6 7]] 5 [6 7] 6 7)
```

#### -tree-map `(fn tree)`

Apply `fn` to each element of `tree` while preserving the tree structure.

```el
(-tree-map '1+ '(1 (2 3) (4 (5 6) 7))) ;; => (2 (3 4) (5 (6 7) 8))
(-tree-map '(lambda (x) (cons x (expt 2 x))) '(1 (2 3) 4)) ;; => ((1 . 2) ((2 . 4) (3 . 8)) (4 . 16))
(--tree-map (length it) '("<body>" ("<p>" "text" "</p>") "</body>")) ;; => (6 (3 4 4) 7)
```

#### -tree-map-nodes `(pred fun tree)`

Call `fun` on each node of `tree` that satisfies `pred`.

If `pred` returns nil, continue descending down this node.  If `pred`
returns non-nil, apply `fun` to this node and do not descend
further.

```el
(-tree-map-nodes 'vectorp (lambda (x) (-sum (append x nil))) '(1 [2 3] 4 (5 [6 7] 8))) ;; => (1 5 4 (5 13 8))
(-tree-map-nodes 'keywordp (lambda (x) (symbol-name x)) '(1 :foo 4 ((5 6 :bar) :baz 8))) ;; => (1 ":foo" 4 ((5 6 ":bar") ":baz" 8))
(--tree-map-nodes (eq (car-safe it) 'add-mode) (-concat it (list :mode 'emacs-lisp-mode)) '(with-mode emacs-lisp-mode (foo bar) (add-mode a b) (baz (add-mode c d)))) ;; => (with-mode emacs-lisp-mode (foo bar) (add-mode a b :mode emacs-lisp-mode) (baz (add-mode c d :mode emacs-lisp-mode)))
```

#### -tree-reduce `(fn tree)`

Use `fn` to reduce elements of list `tree`.
If elements of `tree` are lists themselves, apply the reduction recursively.

`fn` is first applied to first element of the list and second
element, then on this result and third element from the list etc.

See [`-reduce-r`](#-reduce-r-fn-list) for how exactly are lists of zero or one element handled.

```el
(-tree-reduce '+ '(1 (2 3) (4 5))) ;; => 15
(-tree-reduce 'concat '("strings" (" on" " various") ((" levels")))) ;; => "strings on various levels"
(--tree-reduce (cond ((stringp it) (concat it " " acc)) (t (let ((sn (symbol-name it))) (concat "<" sn ">" acc "</" sn ">")))) '(body (p "some words") (div "more" (b "bold") "words"))) ;; => "<body><p>some words</p> <div>more <b>bold</b> words</div></body>"
```

#### -tree-reduce-from `(fn init-value tree)`

Use `fn` to reduce elements of list `tree`.
If elements of `tree` are lists themselves, apply the reduction recursively.

`fn` is first applied to `init-value` and first element of the list,
then on this result and second element from the list etc.

The initial value is ignored on cons pairs as they always contain
two elements.

```el
(-tree-reduce-from '+ 1 '(1 (1 1) ((1)))) ;; => 8
(--tree-reduce-from (-concat acc (list it)) nil '(1 (2 3 (4 5)) (6 7))) ;; => ((7 6) ((5 4) 3 2) 1)
```

#### -tree-mapreduce `(fn folder tree)`

Apply `fn` to each element of `tree`, and make a list of the results.
If elements of `tree` are lists themselves, apply `fn` recursively to
elements of these nested lists.

Then reduce the resulting lists using `folder` and initial value
`init-value`. See [`-reduce-r-from`](#-reduce-r-from-fn-init-list).

This is the same as calling [`-tree-reduce`](#-tree-reduce-fn-tree) after [`-tree-map`](#-tree-map-fn-tree)
but is twice as fast as it only traverse the structure once.

```el
(-tree-mapreduce 'list 'append '(1 (2 (3 4) (5 6)) (7 (8 9)))) ;; => (1 2 3 4 5 6 7 8 9)
(--tree-mapreduce 1 (+ it acc) '(1 (2 (4 9) (2 1)) (7 (4 3)))) ;; => 9
(--tree-mapreduce 0 (max acc (1+ it)) '(1 (2 (4 9) (2 1)) (7 (4 3)))) ;; => 3
```

#### -tree-mapreduce-from `(fn folder init-value tree)`

Apply `fn` to each element of `tree`, and make a list of the results.
If elements of `tree` are lists themselves, apply `fn` recursively to
elements of these nested lists.

Then reduce the resulting lists using `folder` and initial value
`init-value`. See [`-reduce-r-from`](#-reduce-r-from-fn-init-list).

This is the same as calling [`-tree-reduce-from`](#-tree-reduce-from-fn-init-value-tree) after [`-tree-map`](#-tree-map-fn-tree)
but is twice as fast as it only traverse the structure once.

```el
(-tree-mapreduce-from 'identity '* 1 '(1 (2 (3 4) (5 6)) (7 (8 9)))) ;; => 362880
(--tree-mapreduce-from (+ it it) (cons it acc) nil '(1 (2 (4 9) (2 1)) (7 (4 3)))) ;; => (2 (4 (8 18) (4 2)) (14 (8 6)))
(concat "{" (--tree-mapreduce-from (cond ((-cons-pair? it) (concat (symbol-name (car it)) " -> " (symbol-name (cdr it)))) (t (concat (symbol-name it) " : {"))) (concat it (unless (or (equal acc "}") (equal (substring it (1- (length it))) "{")) ", ") acc) "}" '((elisp-mode (foo (bar . booze)) (baz . qux)) (c-mode (foo . bla) (bum . bam))))) ;; => "{elisp-mode : {foo : {bar -> booze}, baz -> qux}, c-mode : {foo -> bla, bum -> bam}}"
```

#### -clone `(list)`

Create a deep copy of `list`.
The new list has the same elements and structure but all cons are
replaced with new ones.  This is useful when you need to clone a
structure such as plist or alist.

```el
(let* ((a '(1 2 3)) (b (-clone a))) (nreverse a) b) ;; => (1 2 3)
```

## Threading macros

Macros that conditionally combine sequential forms for brevity
or readability.

#### -> `(x &optional form &rest more)`

Thread the expr through the forms. Insert `x` as the second item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
second item in second form, etc.

```el
(-> '(2 3 5)) ;; => (2 3 5)
(-> '(2 3 5) (append '(8 13))) ;; => (2 3 5 8 13)
(-> '(2 3 5) (append '(8 13)) (-slice 1 -1)) ;; => (3 5 8)
```

#### ->> `(x &optional form &rest more)`

Thread the expr through the forms. Insert `x` as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
last item in second form, etc.

```el
(->> '(1 2 3) (-map 'square)) ;; => (1 4 9)
(->> '(1 2 3) (-map 'square) (-remove 'even?)) ;; => (1 9)
(->> '(1 2 3) (-map 'square) (-reduce '+)) ;; => 14
```

#### --> `(x &rest forms)`

Starting with the value of `x`, thread each expression through `forms`.

Insert `x` at the position signified by the symbol `it` in the first
form.  If there are more forms, insert the first form at the position
signified by `it` in in second form, etc.

```el
(--> "def" (concat "abc" it "ghi")) ;; => "abcdefghi"
(--> "def" (concat "abc" it "ghi") (upcase it)) ;; => "ABCDEFGHI"
(--> "def" (concat "abc" it "ghi") upcase) ;; => "ABCDEFGHI"
```

#### -as-> `(value variable &rest forms)`

Starting with `value`, thread `variable` through `forms`.

In the first form, bind `variable` to `value`.  In the second form, bind
`variable` to the result of the first form, and so forth.

```el
(-as-> 3 my-var (1+ my-var) (list my-var) (mapcar (lambda (ele) (* 2 ele)) my-var)) ;; => (8)
(-as-> 3 my-var 1+) ;; => 4
(-as-> 3 my-var) ;; => 3
```

#### -some-> `(x &optional form &rest more)`

When expr is non-nil, thread it through the first form (via [`->`](#--x-optional-form-rest-more)),
and when that result is non-nil, through the next form, etc.

```el
(-some-> '(2 3 5)) ;; => (2 3 5)
(-some-> 5 square) ;; => 25
(-some-> 5 even? square) ;; => nil
```

#### -some->> `(x &optional form &rest more)`

When expr is non-nil, thread it through the first form (via [`->>`](#--x-optional-form-rest-more)),
and when that result is non-nil, through the next form, etc.

```el
(-some->> '(1 2 3) (-map 'square)) ;; => (1 4 9)
(-some->> '(1 3 5) (-last 'even?) (+ 100)) ;; => nil
(-some->> '(2 4 6) (-last 'even?) (+ 100)) ;; => 106
```

#### -some--> `(expr &rest forms)`

Thread `expr` through `forms` via [`-->`](#---x-rest-forms), while the result is non-nil.
When `expr` evaluates to non-nil, thread the result through the
first of `forms`, and when that result is non-nil, thread it
through the next form, etc.

```el
(-some--> "def" (concat "abc" it "ghi")) ;; => "abcdefghi"
(-some--> nil (concat "abc" it "ghi")) ;; => nil
(-some--> '(0 1) (-remove #'natnump it) (append it it) (-map #'1+ it)) ;; => ()
```

#### -doto `(init &rest forms)`

Evaluate `init` and pass it as argument to `forms` with [`->`](#--x-optional-form-rest-more).
The `result` of evaluating `init` is threaded through each of `forms`
individually using [`->`](#--x-optional-form-rest-more), which see.  The return value is `result`,
which `forms` may have modified by side effect.

```el
(-doto (list 1 2 3) pop pop) ;; => (3)
(-doto (cons 1 2) (setcar 3) (setcdr 4)) ;; => (3 . 4)
(gethash 'k (--doto (make-hash-table) (puthash 'k 'v it))) ;; => v
```

## Binding

Macros that combine `let` and `let*` with destructuring and flow control.

#### -when-let `((var val) &rest body)`

If `val` evaluates to non-nil, bind it to `var` and execute body.

Note: binding is done according to [`-let`](#-let-varlist-rest-body).

```el
(-when-let (match-index (string-match "d" "abcd")) (+ match-index 2)) ;; => 5
(-when-let ((&plist :foo foo) (list :foo "foo")) foo) ;; => "foo"
(-when-let ((&plist :foo foo) (list :bar "bar")) foo) ;; => nil
```

#### -when-let* `(vars-vals &rest body)`

If all `vals` evaluate to true, bind them to their corresponding
`vars` and execute body. `vars-vals` should be a list of (`var` `val`)
pairs.

Note: binding is done according to [`-let*`](#-let-varlist-rest-body).  `vals` are evaluated
sequentially, and evaluation stops after the first nil `val` is
encountered.

```el
(-when-let* ((x 5) (y 3) (z (+ y 4))) (+ x y z)) ;; => 15
(-when-let* ((x 5) (y nil) (z 7)) (+ x y z)) ;; => nil
```

#### -if-let `((var val) then &rest else)`

If `val` evaluates to non-nil, bind it to `var` and do `then`,
otherwise do `else`.

Note: binding is done according to [`-let`](#-let-varlist-rest-body).

```el
(-if-let (match-index (string-match "d" "abc")) (+ match-index 3) 7) ;; => 7
(--if-let (even? 4) it nil) ;; => t
```

#### -if-let* `(vars-vals then &rest else)`

If all `vals` evaluate to true, bind them to their corresponding
`vars` and do `then`, otherwise do `else`. `vars-vals` should be a list
of (`var` `val`) pairs.

Note: binding is done according to [`-let*`](#-let-varlist-rest-body).  `vals` are evaluated
sequentially, and evaluation stops after the first nil `val` is
encountered.

```el
(-if-let* ((x 5) (y 3) (z 7)) (+ x y z) "foo") ;; => 15
(-if-let* ((x 5) (y nil) (z 7)) (+ x y z) "foo") ;; => "foo"
(-if-let* (((_ _ x) '(nil nil 7))) x) ;; => 7
```

#### -let `(varlist &rest body)`

Bind variables according to `varlist` then eval `body`.

`varlist` is a list of lists of the form (`pattern` `source`).  Each
`pattern` is matched against the `source` "structurally".  `source`
is only evaluated once for each `pattern`.  Each `pattern` is matched
recursively, and can therefore contain sub-patterns which are
matched against corresponding sub-expressions of `source`.

All the SOURCEs are evalled before any symbols are
bound (i.e. "in parallel").

If `varlist` only contains one (`pattern` `source`) element, you can
optionally specify it using a vector and discarding the
outer-most parens.  Thus

    (-let ((`pattern` `source`)) ...)

becomes

    (-let [`pattern` `source`] ...).

[`-let`](#-let-varlist-rest-body) uses a convention of not binding places (symbols) starting
with _ whenever it's possible.  You can use this to skip over
entries you don't care about.  However, this is not *always*
possible (as a result of implementation) and these symbols might
get bound to undefined values.

Following is the overview of supported patterns.  Remember that
patterns can be matched recursively, so every a, b, aK in the
following can be a matching construct and not necessarily a
symbol/variable.

Symbol:

    a - bind the `source` to `a`.  This is just like regular `let`.

Conses and lists:

    (a) - bind `car` of cons/list to `a`

    (a . b) - bind car of cons to `a` and `cdr` to `b`

    (a b) - bind car of list to `a` and `cadr` to `b`

    (a1 a2 a3 ...) - bind 0th car of list to `a1`, 1st to `a2`, 2nd to `a3`...

    (a1 a2 a3 ... aN . rest) - as above, but bind the `n`th cdr to `rest`.

Vectors:

    [a] - bind 0th element of a non-list sequence to `a` (works with
          vectors, strings, bit arrays...)

    [a1 a2 a3 ...] - bind 0th element of non-list sequence to `a0`, 1st to
                     `a1`, 2nd to `a2`, ...
                     If the `pattern` is shorter than `source`, the values at
                     places not in `pattern` are ignored.
                     If the `pattern` is longer than `source`, an `error` is
                     thrown.

    [a1 a2 a3 ... &rest rest] - as above, but bind the rest of
                                the sequence to `rest`.  This is
                                conceptually the same as improper list
                                matching (a1 a2 ... aN . rest)

Key/value stores:

    (&plist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                   `source` plist to aK.  If the
                                   value is not found, aK is nil.
                                   Uses `plist-get` to fetch values.

    (&alist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                   `source` alist to aK.  If the
                                   value is not found, aK is nil.
                                   Uses `assoc` to fetch values.

    (&hash key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                  `source` hash table to aK.  If the
                                  value is not found, aK is nil.
                                  Uses `gethash` to fetch values.

Further, special keyword &keys supports "inline" matching of
plist-like key-value pairs, similarly to &keys keyword of
`cl-defun`.

    (a1 a2 ... aN &keys key1 b1 ... keyN bK)

This binds `n` values from the list to a1 ... aN, then interprets
the cdr as a plist (see key/value matching above).

`a` shorthand notation for kv-destructuring exists which allows the
patterns be optionally left out and derived from the key name in
the following fashion:

- a key :foo is converted into `foo` pattern,
- a key 'bar is converted into `bar` pattern,
- a key "baz" is converted into `baz` pattern.

That is, the entire value under the key is bound to the derived
variable without any further destructuring.

This is possible only when the form following the key is not a
valid pattern (i.e. not a symbol, a cons cell or a vector).
Otherwise the matching proceeds as usual and in case of an
invalid spec fails with an error.

Thus the patterns are normalized as follows:

     ;; derive all the missing patterns
     (&plist :foo 'bar "baz") => (&plist :foo foo 'bar bar "baz" baz)

     ;; we can specify some but not others
     (&plist :foo 'bar explicit-bar) => (&plist :foo foo 'bar explicit-bar)

     ;; nothing happens, we store :foo in x
     (&plist :foo x) => (&plist :foo x)

     ;; nothing happens, we match recursively
     (&plist :foo (a b c)) => (&plist :foo (a b c))

You can name the source using the syntax `symbol` &as `pattern`.
This syntax works with lists (proper or improper), vectors and
all types of maps.

    (list &as a b c) (list 1 2 3)

binds `a` to 1, `b` to 2, `c` to 3 and `list` to (1 2 3).

Similarly:

    (bounds &as beg . end) (cons 1 2)

binds `beg` to 1, `end` to 2 and `bounds` to (1 . 2).

    (items &as first . rest) (list 1 2 3)

binds `first` to 1, `rest` to (2 3) and `items` to (1 2 3)

    [vect &as _ b c] [1 2 3]

binds `b` to 2, `c` to 3 and `vect` to [1 2 3] (_ avoids binding as usual).

    (plist &as &plist :b b) (list :a 1 :b 2 :c 3)

binds `b` to 2 and `plist` to (:a 1 :b 2 :c 3).  Same for &alist and &hash.

This is especially useful when we want to capture the result of a
computation and destructure at the same time.  Consider the
form (function-returning-complex-structure) returning a list of
two vectors with two items each.  We want to capture this entire
result and pass it to another computation, but at the same time
we want to get the second item from each vector.  We can achieve
it with pattern

    (result &as [_ a] [_ b]) (function-returning-complex-structure)

Note: Clojure programmers may know this feature as the ":as
binding".  The difference is that we put the &as at the front
because we need to support improper list binding.

```el
(-let (([a (b c) d] [1 (2 3) 4])) (list a b c d)) ;; => (1 2 3 4)
(-let [(a b c . d) (list 1 2 3 4 5 6)] (list a b c d)) ;; => (1 2 3 (4 5 6))
(-let [(&plist :foo foo :bar bar) (list :baz 3 :foo 1 :qux 4 :bar 2)] (list foo bar)) ;; => (1 2)
```

#### -let* `(varlist &rest body)`

Bind variables according to `varlist` then eval `body`.

`varlist` is a list of lists of the form (`pattern` `source`).  Each
`pattern` is matched against the `source` structurally.  `source` is
only evaluated once for each `pattern`.

Each `source` can refer to the symbols already bound by this
`varlist`.  This is useful if you want to destructure `source`
recursively but also want to name the intermediate structures.

See [`-let`](#-let-varlist-rest-body) for the list of all possible patterns.

```el
(-let* (((a . b) (cons 1 2)) ((c . d) (cons 3 4))) (list a b c d)) ;; => (1 2 3 4)
(-let* (((a . b) (cons 1 (cons 2 3))) ((c . d) b)) (list a b c d)) ;; => (1 (2 . 3) 2 3)
(-let* (((&alist "foo" foo "bar" bar) (list (cons "foo" 1) (cons "bar" (list 'a 'b 'c)))) ((a b c) bar)) (list foo a b c bar)) ;; => (1 a b c (a b c))
```

#### -lambda `(match-form &rest body)`

Return a lambda which destructures its input as `match-form` and executes `body`.

Note that you have to enclose the `match-form` in a pair of parens,
such that:

    (-lambda (x) body)
    (-lambda (x y ...) body)

has the usual semantics of `lambda`.  Furthermore, these get
translated into normal `lambda`, so there is no performance
penalty.

See [`-let`](#-let-varlist-rest-body) for a description of the destructuring mechanism.

```el
(-map (-lambda ((x y)) (+ x y)) '((1 2) (3 4) (5 6))) ;; => (3 7 11)
(-map (-lambda ([x y]) (+ x y)) '([1 2] [3 4] [5 6])) ;; => (3 7 11)
(funcall (-lambda ((_ . a) (_ . b)) (-concat a b)) '(1 2 3) '(4 5 6)) ;; => (2 3 5 6)
```

#### -setq `([match-form val] ...)`

Bind each `match-form` to the value of its `val`.

`match-form` destructuring is done according to the rules of [`-let`](#-let-varlist-rest-body).

This macro allows you to bind multiple variables by destructuring
the value, so for example:

    (-setq (a b) x
           (&plist :c c) plist)

expands roughly speaking to the following code

    (setq a (car x)
          b (cadr x)
          c (plist-get plist :c))

Care is taken to only evaluate each `val` once so that in case of
multiple assignments it does not cause unexpected side effects.

```el
(let (a) (-setq a 1) a) ;; => 1
(let (a b) (-setq (a b) (list 1 2)) (list a b)) ;; => (1 2)
(let (c) (-setq (&plist :c c) (list :c "c")) c) ;; => "c"
```

## Side effects

Functions iterating over lists for side effect only.

#### -each `(list fn)`

Call `fn` on each element of `list`.
Return nil; this function is intended for side effects.

Its anaphoric counterpart is `--each`.

For access to the current element's index in `list`, see
[`-each-indexed`](#-each-indexed-list-fn).

```el
(let (l) (-each '(1 2 3) (lambda (x) (push x l))) l) ;; => (3 2 1)
(let (l) (--each '(1 2 3) (push it l)) l) ;; => (3 2 1)
(-each '(1 2 3) #'identity) ;; => nil
```

#### -each-while `(list pred fn)`

Call `fn` on each `item` in `list`, while (`pred` `item`) is non-nil.
Once an `item` is reached for which `pred` returns nil, `fn` is no
longer called.  Return nil; this function is intended for side
effects.

Its anaphoric counterpart is `--each-while`.

```el
(let (l) (-each-while '(2 4 5 6) #'even? (lambda (x) (push x l))) l) ;; => (4 2)
(let (l) (--each-while '(1 2 3 4) (< it 3) (push it l)) l) ;; => (2 1)
(let ((s 0)) (--each-while '(1 3 4 5) (< it 5) (setq s (+ s it))) s) ;; => 8
```

#### -each-indexed `(list fn)`

Call `fn` on each index and element of `list`.
For each `item` at `index` in `list`, call (funcall `fn` `index` `item`).
Return nil; this function is intended for side effects.

See also: [`-map-indexed`](#-map-indexed-fn-list).

```el
(let (l) (-each-indexed '(a b c) (lambda (i x) (push (list x i) l))) l) ;; => ((c 2) (b 1) (a 0))
(let (l) (--each-indexed '(a b c) (push (list it it-index) l)) l) ;; => ((c 2) (b 1) (a 0))
(let (l) (--each-indexed () (push it l)) l) ;; => ()
```

#### -each-r `(list fn)`

Call `fn` on each element of `list` in reversed order.
Return nil; this function is intended for side effects.

Its anaphoric counterpart is `--each-r`.

```el
(let (l) (-each-r '(1 2 3) (lambda (x) (push x l))) l) ;; => (1 2 3)
(let (l) (--each-r '(1 2 3) (push it l)) l) ;; => (1 2 3)
(-each-r '(1 2 3) #'identity) ;; => nil
```

#### -each-r-while `(list pred fn)`

Call `fn` on each `item` in reversed `list`, while (`pred` `item`) is non-nil.
Once an `item` is reached for which `pred` returns nil, `fn` is no
longer called.  Return nil; this function is intended for side
effects.

Its anaphoric counterpart is `--each-r-while`.

```el
(let (l) (-each-r-while '(2 4 5 6) #'even? (lambda (x) (push x l))) l) ;; => (6)
(let (l) (--each-r-while '(1 2 3 4) (>= it 3) (push it l)) l) ;; => (3 4)
(let ((s 0)) (--each-r-while '(1 2 3 5) (> it 1) (setq s (+ s it))) s) ;; => 10
```

#### -dotimes `(num fn)`

Call `fn` `num` times, presumably for side effects.
`fn` is called with a single argument on successive integers
running from 0, inclusive, to `num`, exclusive.  `fn` is not called
if `num` is less than 1.

This function's anaphoric counterpart is `--dotimes`.

```el
(let (s) (-dotimes 3 (lambda (n) (push n s))) s) ;; => (2 1 0)
(let (s) (-dotimes 0 (lambda (n) (push n s))) s) ;; => ()
(let (s) (--dotimes 5 (push it s)) s) ;; => (4 3 2 1 0)
```

## Destructive operations

Macros that modify variables holding lists.

#### !cons `(car cdr)`

Destructive: Set `cdr` to the cons of `car` and `cdr`.

```el
(let (l) (!cons 5 l) l) ;; => (5)
(let ((l '(3))) (!cons 5 l) l) ;; => (5 3)
```

#### !cdr `(list)`

Destructive: Set `list` to the cdr of `list`.

```el
(let ((l '(3))) (!cdr l) l) ;; => ()
(let ((l '(3 5))) (!cdr l) l) ;; => (5)
```

## Function combinators

Functions that manipulate and compose other functions.

#### -partial `(fun &rest args)`

Return a function that is a partial application of `fun` to `args`.
`args` is a list of the first `n` arguments to pass to `fun`.
The result is a new function which does the same as `fun`, except that
the first `n` arguments are fixed at the values with which this function
was called.

```el
(funcall (-partial #'+ 5)) ;; => 5
(funcall (-partial #'- 5) 3) ;; => 2
(funcall (-partial #'+ 5 2) 3) ;; => 10
```

#### -rpartial `(fn &rest args)`

Return a function that is a partial application of `fn` to `args`.
`args` is a list of the last `n` arguments to pass to `fn`.  The result
is a new function which does the same as `fn`, except that the last
`n` arguments are fixed at the values with which this function was
called.  This is like [`-partial`](#-partial-fun-rest-args), except the arguments are fixed
starting from the right rather than the left.

```el
(funcall (-rpartial #'- 5)) ;; => -5
(funcall (-rpartial #'- 5) 8) ;; => 3
(funcall (-rpartial #'- 5 2) 10) ;; => 3
```

#### -juxt `(&rest fns)`

Return a function that is the juxtaposition of `fns`.
The returned function takes a variable number of `args`, applies
each of `fns` in turn to `args`, and returns the list of results.

```el
(funcall (-juxt) 1 2) ;; => ()
(funcall (-juxt #'+ #'- #'* #'/) 7 5) ;; => (12 2 35 1)
(mapcar (-juxt #'number-to-string #'1+) '(1 2)) ;; => (("1" 2) ("2" 3))
```

#### -compose `(&rest fns)`

Compose `fns` into a single composite function.
Return a function that takes a variable number of `args`, applies
the last function in `fns` to `args`, and returns the result of
calling each remaining function on the result of the previous
function, right-to-left.  If no `fns` are given, return a variadic
`identity` function.

```el
(funcall (-compose #'- #'1+ #'+) 1 2 3) ;; => -7
(funcall (-compose #'identity #'1+) 3) ;; => 4
(mapcar (-compose #'not #'stringp) '(nil "")) ;; => (t nil)
```

#### -applify `(fn)`

Return a function that applies `fn` to a single list of args.
This changes the arity of `fn` from taking `n` distinct arguments to
taking 1 argument which is a list of `n` arguments.

```el
(funcall (-applify #'+) nil) ;; => 0
(mapcar (-applify #'+) '((1 1 1) (1 2 3) (5 5 5))) ;; => (3 6 15)
(funcall (-applify #'<) '(3 6)) ;; => t
```

#### -on `(op trans)`

Return a function that calls `trans` on each arg and `op` on the results.
The returned function takes a variable number of arguments, calls
the function `trans` on each one in turn, and then passes those
results as the list of arguments to `op`, in the same order.

For example, the following pairs of expressions are morally
equivalent:

    (funcall (-on #'+ #'1+) 1 2 3) = (+ (1+ 1) (1+ 2) (1+ 3))
    (funcall (-on #'+ #'1+))       = (+)

```el
(-sort (-on #'< #'length) '((1 2 3) (1) (1 2))) ;; => ((1) (1 2) (1 2 3))
(funcall (-on #'min #'string-to-number) "22" "2" "1" "12") ;; => 1
(-min-by (-on #'> #'length) '((1 2 3) (4) (1 2))) ;; => (4)
```

#### -flip `(fn)`

Return a function that calls `fn` with its arguments reversed.
The returned function takes the same number of arguments as `fn`.

For example, the following two expressions are morally
equivalent:

    (funcall (-flip #'-) 1 2) = (- 2 1)

See also: [`-rotate-args`](#-rotate-args-n-fn).

```el
(-sort (-flip #'<) '(4 3 6 1)) ;; => (6 4 3 1)
(funcall (-flip #'-) 3 2 1 10) ;; => 4
(funcall (-flip #'1+) 1) ;; => 2
```

#### -rotate-args `(n fn)`

Return a function that calls `fn` with args rotated `n` places to the right.
The returned function takes the same number of arguments as `fn`,
rotates the list of arguments `n` places to the right (left if `n` is
negative) just like [`-rotate`](#-rotate-n-list), and applies `fn` to the result.

See also: [`-flip`](#-flip-fn).

```el
(funcall (-rotate-args -1 #'list) 1 2 3 4) ;; => (2 3 4 1)
(funcall (-rotate-args 1 #'-) 1 10 100) ;; => 89
(funcall (-rotate-args 2 #'list) 3 4 5 1 2) ;; => (1 2 3 4 5)
```

#### -const `(c)`

Return a function that returns `c` ignoring any additional arguments.

In types: a -> b -> a

```el
(funcall (-const 2) 1 3 "foo") ;; => 2
(mapcar (-const 1) '("a" "b" "c" "d")) ;; => (1 1 1 1)
(-sum (mapcar (-const 1) '("a" "b" "c" "d"))) ;; => 4
```

#### -cut `(&rest params)`

Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See `srfi-26` for detailed description.

```el
(funcall (-cut list 1 <> 3 <> 5) 2 4) ;; => (1 2 3 4 5)
(-map (-cut funcall <> 5) `(1+ 1- ,(lambda (x) (/ 1.0 x)))) ;; => (6 4 0.2)
(-map (-cut <> 1 2 3) '(list vector string)) ;; => ((1 2 3) [1 2 3] "\1\2\3")
```

#### -not `(pred)`

Return a predicate that negates the result of `pred`.
The returned predicate passes its arguments to `pred`.  If `pred`
returns nil, the result is non-nil; otherwise the result is nil.

See also: [`-andfn`](#-andfn-rest-preds) and [`-orfn`](#-orfn-rest-preds).

```el
(funcall (-not #'numberp) "5") ;; => t
(-sort (-not #'<) '(5 2 1 0 6)) ;; => (6 5 2 1 0)
(-filter (-not (-partial #'< 4)) '(1 2 3 4 5 6 7 8)) ;; => (1 2 3 4)
```

#### -orfn `(&rest preds)`

Return a predicate that returns the first non-nil result of `preds`.
The returned predicate takes a variable number of arguments,
passes them to each predicate in `preds` in turn until one of them
returns non-nil, and returns that non-nil result without calling
the remaining `preds`.  If all `preds` return nil, or if no `preds` are
given, the returned predicate returns nil.

See also: [`-andfn`](#-andfn-rest-preds) and [`-not`](#-not-pred).

```el
(-filter (-orfn #'natnump #'booleanp) '(1 nil "a" -4 b c t)) ;; => (1 nil t)
(funcall (-orfn #'symbolp (-cut string-match-p "x" <>)) "axe") ;; => 1
(funcall (-orfn #'= #'+) 1 1) ;; => t
```

#### -andfn `(&rest preds)`

Return a predicate that returns non-nil if all `preds` do so.
The returned predicate `p` takes a variable number of arguments and
passes them to each predicate in `preds` in turn.  If any one of
`preds` returns nil, `p` also returns nil without calling the
remaining `preds`.  If all `preds` return non-nil, `p` returns the last
such value.  If no `preds` are given, `p` always returns non-nil.

See also: [`-orfn`](#-orfn-rest-preds) and [`-not`](#-not-pred).

```el
(-filter (-andfn #'numberp (-cut < <> 5)) '(a 1 b 6 c 2)) ;; => (1 2)
(mapcar (-andfn #'numberp #'1+) '(a 1 b 6)) ;; => (nil 2 nil 7)
(funcall (-andfn #'= #'+) 1 1) ;; => 2
```

#### -iteratefn `(fn n)`

Return a function `fn` composed `n` times with itself.

`fn` is a unary function.  If you need to use a function of higher
arity, use [`-applify`](#-applify-fn) first to turn it into a unary function.

With n = 0, this acts as identity function.

In types: (a -> a) -> Int -> a -> a.

This function satisfies the following law:

    (funcall (-iteratefn fn n) init) = (-last-item (-iterate fn init (1+ n))).

```el
(funcall (-iteratefn (lambda (x) (* x x)) 3) 2) ;; => 256
(funcall (-iteratefn '1+ 3) 1) ;; => 4
(funcall (-iteratefn 'cdr 3) '(1 2 3 4 5)) ;; => (4 5)
```

#### -fixfn `(fn &optional equal-test halt-test)`

Return a function that computes the (least) fixpoint of `fn`.

`fn` must be a unary function. The returned lambda takes a single
argument, `x`, the initial value for the fixpoint iteration. The
iteration halts when either of the following conditions is satisfied:

 1. Iteration converges to the fixpoint, with equality being
      tested using `equal-test`. If `equal-test` is not specified,
      `equal` is used. For functions over the floating point
      numbers, it may be necessary to provide an appropriate
      approximate comparison test.

 2. `halt-test` returns a non-nil value. `halt-test` defaults to a
      simple counter that returns t after `-fixfn-max-iterations`,
      to guard against infinite iteration. Otherwise, `halt-test`
      must be a function that accepts a single argument, the
      current value of `x`, and returns non-nil as long as iteration
      should continue. In this way, a more sophisticated
      convergence test may be supplied by the caller.

The return value of the lambda is either the fixpoint or, if
iteration halted before converging, a cons with car `halted` and
cdr the final output from `halt-test`.

In types: (a -> a) -> a -> a.

```el
(funcall (-fixfn #'cos #'approx=) 0.7) ;; ~> 0.7390851332151607
(funcall (-fixfn (lambda (x) (expt (+ x 10) 0.25))) 2.0) ;; => 1.8555845286409378
(funcall (-fixfn #'sin #'approx=) 0.1) ;; => (halted . t)
```

#### -prodfn `(&rest fns)`

Take a list of n functions and return a function that takes a
list of length n, applying i-th function to i-th element of the
input list.  Returns a list of length n.

In types (for n=2): ((a -> b), (c -> d)) -> (a, c) -> (b, d)

This function satisfies the following laws:

    (-compose (-prodfn f g ...) (-prodfn f' g' ...)) = (-prodfn (-compose f f') (-compose g g') ...)
    (-prodfn f g ...) = (-juxt (-compose f (-partial 'nth 0)) (-compose g (-partial 'nth 1)) ...)
    (-compose (-prodfn f g ...) (-juxt f' g' ...)) = (-juxt (-compose f f') (-compose g g') ...)
    (-compose (-partial 'nth n) (-prod f1 f2 ...)) = (-compose fn (-partial 'nth n))

```el
(funcall (-prodfn '1+ '1- 'number-to-string) '(1 2 3)) ;; => (2 1 "3")
(-map (-prodfn '1+ '1-) '((1 2) (3 4) (5 6) (7 8))) ;; => ((2 1) (4 3) (6 5) (8 7))
(apply '+ (funcall (-prodfn 'length 'string-to-number) '((1 2 3) "15"))) ;; => 18
```

## Contribute

Yes, please do.  Pure functions in the list manipulation realm only,
please.  There's a suite of examples/tests in `dev/examples.el`, so
remember to add tests for your additions, or I might break them later.

You'll find the repo at:

    https://github.com/magnars/dash.el

Run the tests with:

    make check

Regenerate the docs with:

    make docs

I highly recommend that you install these as a pre-commit hook, so
that the tests are always running and the docs are always in sync:

    cp dev/pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` or `dash.texi` directly; they are
auto-generated.  Change `readme-template.md` or `dash-template.texi`
instead, respectively.

To ensure that `dash.el` can be distributed with GNU ELPA or Emacs, we
require that all contributors assign copyright to the Free Software
Foundation.  For more on this, see [`(info "(emacs) Copyright
Assignment")`](https://gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html).

## Contributors

- [Matus Goljer](https://github.com/Fuco1) contributed lots of features and
  functions.
- [Takafumi Arakaki](https://github.com/tkf) contributed `-group-by`.
- [tali713](https://github.com/tali713) is the author of `-applify`.
- [Víctor M. Valenzuela](https://github.com/vemv) contributed `-repeat`.
- [Nic Ferrier](https://github.com/nicferrier) contributed `-cons*`.
- [Wilfred Hughes](https://github.com/Wilfred) contributed `-slice`,
  `-first-item`, and `-last-item`.
- [Emanuel Evans](https://github.com/shosti) contributed `-if-let`, `-when-let`,
  and `-insert-at`.
- [Johan Andersson](https://github.com/rejeep) contributed `-sum`, `-product`,
  and `-same-items?`.
- [Christina Whyte](https://github.com/kurisuwhyte) contributed `-compose`.
- [Steve Lamb](https://github.com/steventlamb) contributed `-cycle`, `-pad`,
  `-annotate`, `-zip-fill`, and a variadic version of `-zip`.
- [Fredrik Bergroth](https://github.com/fbergroth) made the `-if-let` family use
  `-let` destructuring and improved the script for generating documentation.
- [Mark Oteiza](https://github.com/holomorph) contributed `-iota` and
  the script to create an Info manual.
- [Vasilij Schneidermann](https://github.com/wasamasa) contributed `-some`.
- [William West](https://github.com/occidens) made `-fixfn` more robust at
  handling floats.
- [Cam Saul](https://github.com/camsaul) contributed `-some->`, `-some->>`, and
  `-some-->`.
- [Basil L. Contovounesios](https://github.com/basil-conto) contributed
  `-common-prefix`, `-common-suffix`, and various other improvements.
- [Paul Pogonyshev](https://github.com/doublep) contributed `-each-r` and
  `-each-r-while`.

Thanks!

New contributors are very welcome.  See the
[`Contribute`](#contribute) section above.

## License

Copyright (C) 2012-2021 Free Software Foundation, Inc.

Author: Magnar Sveen <magnars@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
