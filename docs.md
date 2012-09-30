## !map `(fn list)`

Returns a new list consisting of the result of applying FN to the items in list.

```cl
(!map (lambda (num) (* num num)) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
(!map (quote square) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
(!!map (* it it) (quote (1 2 3 4))) ;; => (quote (1 4 9 16))
```

## !reduce-from `(fn initial-value list)`

Returns the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, returns INITIAL-VALUE and
FN is not called.

```cl
(!reduce-from (quote +) 7 (quote nil)) ;; => 7
(!reduce-from (quote +) 7 (quote (1))) ;; => 8
(!reduce-from (quote +) 7 (quote (1 2))) ;; => 10
(!!reduce-from (+ acc it) 7 (quote (1 2 3))) ;; => 13
```

## !reduce `(fn list)`

Returns the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce returns the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called.

```cl
(!reduce (quote +) (quote nil)) ;; => 0
(!reduce (quote +) (quote (1))) ;; => 1
(!reduce (quote +) (quote (1 2))) ;; => 3
(!reduce (lambda (memo item) (format %s-%s memo item)) (quote (1 2 3))) ;; => 1-2-3
(!!reduce (format %s-%s acc it) (quote (1 2 3))) ;; => 1-2-3
(!!reduce (format %s-%s acc it) (quote nil)) ;; => nil-nil
```

## !filter `(fn list)`

Returns a new list of the items in LIST for which FN returns a non-nil value.

```cl
(!filter (lambda (num) (= 0 (% num 2))) (quote (1 2 3 4))) ;; => (quote (2 4))
(!filter (quote even?) (quote (1 2 3 4))) ;; => (quote (2 4))
(!!filter (= 0 (% it 2)) (quote (1 2 3 4))) ;; => (quote (2 4))
```

## !remove `(fn list)`

Returns a new list of the items in LIST for which FN returns nil.

```cl
(!remove (lambda (num) (= 0 (% num 2))) (quote (1 2 3 4))) ;; => (quote (1 3))
(!remove (quote even?) (quote (1 2 3 4))) ;; => (quote (1 3))
(!!remove (= 0 (% it 2)) (quote (1 2 3 4))) ;; => (quote (1 3))
```

## !concat `(&rest lists)`

Returns a new list with the concatenation of the elements in
the supplied LISTS.

```cl
(!concat) ;; => nil
(!concat (quote (1))) ;; => (quote (1))
(!concat (quote (1)) (quote (2))) ;; => (quote (1 2))
(!concat (quote (1)) (quote (2 3)) (quote (4))) ;; => (quote (1 2 3 4))
```

## !mapcat `(fn list)`

Returns the result of applying concat to the result of applying map to FN and LIST.
Thus function FN should return a collection.

```cl
(!mapcat (quote list) (quote (1 2 3))) ;; => (quote (1 2 3))
(!mapcat (lambda (item) (list 0 item)) (quote (1 2 3))) ;; => (quote (0 1 0 2 0 3))
(!!mapcat (list 0 it) (quote (1 2 3))) ;; => (quote (0 1 0 2 0 3))
```

## !partial `(fn &rest args)`

Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with args +
additional args.

```cl
(funcall (!partial (quote +) 5) 3) ;; => 8
(funcall (!partial (quote +) 5 2) 3) ;; => 10
```

## !difference `(list list2)`

Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil.

```cl
(!difference (quote nil) (quote nil)) ;; => (quote nil)
(!difference (quote (1 2 3)) (quote (4 5 6))) ;; => (quote (1 2 3))
(!difference (quote (1 2 3 4)) (quote (3 4 5 6))) ;; => (quote (1 2))
```

## !intersection `(list list2)`

Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil.

```cl
(!intersection (quote nil) (quote nil)) ;; => (quote nil)
(!intersection (quote (1 2 3)) (quote (4 5 6))) ;; => (quote nil)
(!intersection (quote (1 2 3 4)) (quote (3 4 5 6))) ;; => (quote (3 4))
```

## !uniq `(list)`

Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil.

```cl
(!uniq (quote nil)) ;; => (quote nil)
(!uniq (quote (1 2 2 4))) ;; => (quote (1 2 4))
```

## !contains? `(list element)`

Return whether LIST contains ELEMENT.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil.

```cl
(!contains? (quote (1 2 3)) 1) ;; => t
(!contains? (quote (1 2 3)) 2) ;; => t
(!contains? (quote nil) (quote nil)) ;; => nil
(!contains? (quote nil) 1) ;; => nil
(!contains? (quote (1 2 4)) 3) ;; => nil
```
