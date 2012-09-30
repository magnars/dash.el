# bang.el [![Build Status](https://secure.travis-ci.org/magnars/bang.el.png)](http://travis-ci.org/magnars/bang.el)

The startings of a modern list api for Emacs. Does not require 'cl.

We're looking to Clojure for naming and signatures.

## Warning

This is so much a work in progress that you should definitely not be using it yet.

## Anaphoric functions

While `!filter` takes a function to filter the list by, you can also use the
anaphoric form with double bangs - which will then be executed with `it` exposed
as the list item. Here's an example:

    (!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; normal version

    (!!filter (= 0 (% it 2)) '(1 2 3 4)) ;; anaphoric version

of course the original can also be written like

    (defun even? (num) (= 0 (% num 2)))

    (!filter even? '(1 2 3 4))

which demonstrates the usefulness of both versions.

## Available functions

```cl
!map (fn list)
!reduce-from (fn initial-value list)
!reduce (fn list)
!filter (fn list)
!remove (fn list)
!concat (&rest lists)
!mapcat (fn list)
!partial (fn &rest args)
!difference (list list2)
!intersection (list list2)
!uniq (list)
!contains? (list element)
```cl

There are also anaphoric versions of these
functions where that makes sense, prefixed with two bangs
instead of one.

## Documentation and examples

### !map `(fn list)`

Returns a new list consisting of the result of applying `fn` to the items in `list`.

```cl
(!map (lambda (num) (* num num)) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
(!map (quote square) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
(!!map (* it it) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
```

### !reduce-from `(fn initial-value list)`

Returns the result of applying `fn` to `initial-value` and the
first item in `list`, then applying `fn` to that result and the 2nd
item, etc. If `list` contains no items, returns `initial-value` and
`fn` is not called.

```cl
(!reduce-from (quote +) 7 (quote (1 2))) ;; => 10
(!reduce-from (lambda (memo item) (+ memo item)) 7 (quote (1 2))) ;; => 10
(!!reduce-from (+ acc it) 7 (quote (1 2 3))) ;; => 13
```

### !reduce `(fn list)`

Returns the result of applying `fn` to the first 2 items in `list`,
then applying `fn` to that result and the 3rd item, etc. If `list`
contains no items, `fn` must accept no arguments as well, and
reduce returns the result of calling `fn` with no arguments. If
`list` has only 1 item, it is returned and `fn` is not called.

```cl
(!reduce (quote +) (quote (1 2))) ;; => 3
(!reduce (lambda (memo item) (format %s-%s memo item)) (quote (1 2 3))) ;; => 1-2-3
(!!reduce (format %s-%s acc it) (quote (1 2 3))) ;; => 1-2-3
```

### !filter `(fn list)`

Returns a new list of the items in `list` for which `fn` returns a non-nil value.

```cl
(!filter (lambda (num) (= 0 (% num 2))) (quote (1 2 3 4))) ;; => (quote (2 4))
(!filter (quote even?) (quote (1 2 3 4))) ;; => (quote (2 4))
(!!filter (= 0 (% it 2)) (quote (1 2 3 4))) ;; => (quote (2 4))
```

### !remove `(fn list)`

Returns a new list of the items in `list` for which `fn` returns nil.

```cl
(!remove (lambda (num) (= 0 (% num 2))) (quote (1 2 3 4))) ;; => (quote (1 3))
(!remove (quote even?) (quote (1 2 3 4))) ;; => (quote (1 3))
(!!remove (= 0 (% it 2)) (quote (1 2 3 4))) ;; => (quote (1 3))
```

### !concat `(&rest lists)`

Returns a new list with the concatenation of the elements in
the supplied `lists`.

```cl
(!concat (quote (1))) ;; => (quote (1))
(!concat (quote (1)) (quote (2))) ;; => (quote (1 2))
(!concat (quote (1)) (quote (2 3)) (quote (4))) ;; => (quote (1 2 3 4))
```

### !mapcat `(fn list)`

Returns the result of applying concat to the result of applying map to `fn` and `list`.
Thus function `fn` should return a collection.

```cl
(!mapcat (quote list) (quote (1 2 3))) ;; => (quote (1 2 3))
(!mapcat (lambda (item) (list 0 item)) (quote (1 2 3))) ;; => (quote (0 1 0 2 0 3))
(!!mapcat (list 0 it) (quote (1 2 3))) ;; => (quote (0 1 0 2 0 3))
```

### !partial `(fn &rest args)`

Takes a function `fn` and fewer than the normal arguments to `fn`,
and returns a fn that takes a variable number of additional `args`.
When called, the returned function calls `fn` with args +
additional args.

```cl
(funcall (!partial (quote +) 5) 3) ;; => 8
(funcall (!partial (quote +) 5 2) 3) ;; => 10
```

### !difference `(list list2)`

Return a new list with only the members of `list` that are not in LIST2.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!difference (quote nil) (quote nil)) ;; => (quote nil)
(!difference (quote (1 2 3)) (quote (4 5 6))) ;; => (quote (1 2 3))
(!difference (quote (1 2 3 4)) (quote (3 4 5 6))) ;; => (quote (1 2))
```

### !intersection `(list list2)`

Return a new list containing only the elements that are members of both `list` and LIST2.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!intersection (quote nil) (quote nil)) ;; => (quote nil)
(!intersection (quote (1 2 3)) (quote (4 5 6))) ;; => (quote nil)
(!intersection (quote (1 2 3 4)) (quote (3 4 5 6))) ;; => (quote (3 4))
```

### !uniq `(list)`

Return a new list with all duplicates removed.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!uniq (quote nil)) ;; => (quote nil)
(!uniq (quote (1 2 2 4))) ;; => (quote (1 2 4))
```

### !contains? `(list element)`

Return whether `list` contains `element`.
The test for equality is done with `equal`,
or with `!compare-fn` if that's non-nil.

```cl
(!contains? (quote (1 2 3)) 1) ;; => t
(!contains? (quote (1 2 3)) 2) ;; => t
(!contains? (quote (1 2 3)) 4) ;; => nil
```

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
