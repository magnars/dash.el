# bang.el [![Build Status](https://secure.travis-ci.org/magnars/bang.el.png)](http://travis-ci.org/magnars/bang.el)

The startings of a modern list api for Emacs that does not require 'cl.

## Warning

This is so much a work in progress that you should definitely not be using it yet.

## Functions

* [!map](#map-fn-list) `(fn list)`
* [!reduce-from](#reduce-from-fn-initial-value-list) `(fn initial-value list)`
* [!reduce](#reduce-fn-list) `(fn list)`
* [!filter](#filter-fn-list) `(fn list)`
* [!remove](#remove-fn-list) `(fn list)`
* [!keep](#keep-fn-list) `(fn list)`
* [!concat](#concat-rest-lists) `(&rest lists)`
* [!mapcat](#mapcat-fn-list) `(fn list)`
* [!first](#first-fn-list) `(fn list)`
* [!partial](#partial-fn-rest-args) `(fn &rest args)`
* [!difference](#difference-list-list2) `(list list2)`
* [!intersection](#intersection-list-list2) `(list list2)`
* [!distinct](#distinct-list) `(list)`
* [!contains?](#contains-list-element) `(list element)`
* [!any?](#any-fn-list) `(fn list)`
* [!all?](#all-fn-list) `(fn list)`
* [!each](#each-list-fn) `(list fn)`

There are also anaphoric versions of these functions where that makes sense,
prefixed with two bangs instead of one.

## Anaphoric functions

While `!filter` takes a function to filter the list by, you can also use the
anaphoric form with double bangs - which will then be executed with `it` exposed
as the list item. Here's an example:

```cl
(!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; normal version

(!!filter (= 0 (% it 2)) '(1 2 3 4)) ;; anaphoric version
```

of course the original can also be written like

```cl
(defun even? (num) (= 0 (% num 2)))

(!filter even? '(1 2 3 4))
```

which demonstrates the usefulness of both versions.

## Documentation and examples

### !map `(fn list)`

Returns a new list consisting of the result of applying `fn` to the items in `list`.

```cl
(!map (lambda (num) (* num num)) '(1 2 3 4)) ;; => '(1 4 9 16)
(!map 'square '(1 2 3 4)) ;; => '(1 4 9 16)
(!!map (* it it) '(1 2 3 4)) ;; => '(1 4 9 16)
```

### !reduce-from `(fn initial-value list)`

Returns the result of applying `fn` to `initial-value` and the
first item in `list`, then applying `fn` to that result and the 2nd
item, etc. If `list` contains no items, returns `initial-value` and
`fn` is not called.

In the anaphoric form `!!reduce-from`, the accumulated value is
exposed as `acc`.

```cl
(!reduce-from '+ 7 '(1 2)) ;; => 10
(!reduce-from (lambda (memo item) (+ memo item)) 7 '(1 2)) ;; => 10
(!!reduce-from (+ acc it) 7 '(1 2 3)) ;; => 13
```

### !reduce `(fn list)`

Returns the result of applying `fn` to the first 2 items in `list`,
then applying `fn` to that result and the 3rd item, etc. If `list`
contains no items, `fn` must accept no arguments as well, and
reduce returns the result of calling `fn` with no arguments. If
`list` has only 1 item, it is returned and `fn` is not called.

In the anaphoric form `!!reduce`, the accumulated value is
exposed as `acc`.

```cl
(!reduce '+ '(1 2)) ;; => 3
(!reduce (lambda (memo item) (format "%s-%s" memo item)) '(1 2 3)) ;; => "1-2-3"
(!!reduce (format "%s-%s" acc it) '(1 2 3)) ;; => "1-2-3"
```

### !filter `(fn list)`

Returns a new list of the items in `list` for which `fn` returns a non-nil value.

Alias: `!select`

```cl
(!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => '(2 4)
(!filter 'even? '(1 2 3 4)) ;; => '(2 4)
(!!filter (= 0 (% it 2)) '(1 2 3 4)) ;; => '(2 4)
```

### !remove `(fn list)`

Returns a new list of the items in `list` for which `fn` returns nil.

Alias: `!reject`

```cl
(!remove (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; => '(1 3)
(!remove 'even? '(1 2 3 4)) ;; => '(1 3)
(!!remove (= 0 (% it 2)) '(1 2 3 4)) ;; => '(1 3)
```

### !keep `(fn list)`

Returns a new list of the non-nil results of applying `fn` to the items in `list`.

```cl
(!keep 'cdr '((1 2 3) (4 5) (6))) ;; => '((2 3) (5))
(!keep (lambda (num) (when (> num 3) (* 10 num))) '(1 2 3 4 5 6)) ;; => '(40 50 60)
(!!keep (when (> it 3) (* 10 it)) '(1 2 3 4 5 6)) ;; => '(40 50 60)
```

### !concat `(&rest lists)`

Returns a new list with the concatenation of the elements in
the supplied `lists`.

```cl
(!concat '(1)) ;; => '(1)
(!concat '(1) '(2)) ;; => '(1 2)
(!concat '(1) '(2 3) '(4)) ;; => '(1 2 3 4)
```

### !mapcat `(fn list)`

Returns the result of applying concat to the result of applying map to `fn` and `list`.
Thus function `fn` should return a collection.

```cl
(!mapcat 'list '(1 2 3)) ;; => '(1 2 3)
(!mapcat (lambda (item) (list 0 item)) '(1 2 3)) ;; => '(0 1 0 2 0 3)
(!!mapcat (list 0 it) '(1 2 3)) ;; => '(0 1 0 2 0 3)
```

### !first `(fn list)`

Returns the first x in `list` where (`fn` x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car`.

```cl
(!first 'even? '(1 2 3)) ;; => 2
(!first 'even? '(1 3 5)) ;; => nil
(!!first (> it 2) '(1 2 3)) ;; => 3
```

### !partial `(fn &rest args)`

Takes a function `fn` and fewer than the normal arguments to `fn`,
and returns a fn that takes a variable number of additional `args`.
When called, the returned function calls `fn` with `args` +
additional args.

```cl
(funcall (!partial '+ 5) 3) ;; => 8
(funcall (!partial '+ 5 2) 3) ;; => 10
```

### !difference `(list list2)`

Return a new list with only the members of `list` that are not in `list2`.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!difference '() '()) ;; => '()
(!difference '(1 2 3) '(4 5 6)) ;; => '(1 2 3)
(!difference '(1 2 3 4) '(3 4 5 6)) ;; => '(1 2)
```

### !intersection `(list list2)`

Return a new list containing only the elements that are members of both `list` and `list2`.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!intersection '() '()) ;; => '()
(!intersection '(1 2 3) '(4 5 6)) ;; => '()
(!intersection '(1 2 3 4) '(3 4 5 6)) ;; => '(3 4)
```

### !distinct `(list)`

Return a new list with all duplicates removed.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!distinct '()) ;; => '()
(!distinct '(1 2 2 4)) ;; => '(1 2 4)
```

### !contains? `(list element)`

Return whether `list` contains `element`.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!contains? '(1 2 3) 1) ;; => t
(!contains? '(1 2 3) 2) ;; => t
(!contains? '(1 2 3) 4) ;; => nil
```

### !any? `(fn list)`

Returns t if (`fn` x) is non-nil for any x in `list`, else nil.

Alias: `!some?`

```cl
(!any? 'even? '(1 2 3)) ;; => t
(!any? 'even? '(1 3 5)) ;; => nil
(!!any? (= 0 (% it 2)) '(1 2 3)) ;; => t
```

### !all? `(fn list)`

Returns t if (`fn` x) is non-nil for all x in `list`, else nil.

Alias: `!every?`

```cl
(!all? 'even? '(1 2 3)) ;; => nil
(!all? 'even? '(2 4 6)) ;; => t
(!!all? (= 0 (% it 2)) '(2 4 6)) ;; => t
```

### !each `(list fn)`

Calls `fn` with every item in `list`. Returns nil, used for side-effects only.

```cl
(let (s) (!each '(1 2 3) (lambda (item) (setq s (cons item s))))) ;; => nil
(let (s) (!each '(1 2 3) (lambda (item) (setq s (cons item s)))) s) ;; => '(3 2 1)
(let (s) (!!each '(1 2 3) (setq s (cons it s))) s) ;; => '(3 2 1)
```


## Development

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## License

Copyright (C) 2012 Magnar Sveen, Joel McCracken

Authors: Magnar Sveen <magnars@gmail.com>
         Joel McCracken
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
