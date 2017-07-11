# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el [![Build Status](https://secure.travis-ci.org/magnars/dash.el.png)](http://travis-ci.org/magnars/dash.el)

A modern list api for Emacs. No 'cl required.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](https://melpa.org/):

    M-x package-install dash

Or you can just dump `dash.el` in your load
path somewhere.

If you want the function combinators, then also:

    M-x package-install dash-functional

## Using in a package

Add this to the big comment block at the top:

    ;; Package-Requires: ((dash "2.13.0"))

To get function combinators:

    ;; Package-Requires: ((dash "2.13.0") (dash-functional "1.2.0") (emacs "24"))

## Upcoming breaking change!

- For backward compatibility reasons `-zip` return a cons-cell instead of a list
  with two elements when called on two lists. This is a clunky API, and in an
  upcoming 3.0 release of Dash it will always return a list. If you rely on the
  cons-cell return value, use `-zip-pair` instead.

## Syntax highlighting of dash functions

Font lock of dash functions in emacs lisp buffers is now optional.
Include this in your emacs settings to get syntax highlighting:

    (eval-after-load 'dash '(dash-enable-font-lock))

## Functions

All functions and constructs in the library are prefixed with a dash (-).

There are also anaphoric versions of functions where that makes sense,
prefixed with two dashes instead of one.

While `-map` takes a function to map over the list, you can also use
the anaphoric form with double dashes - which will then be executed
with `it` exposed as the list item. Here's an example:

```el
(-map (lambda (n) (* n n)) '(1 2 3 4)) ;; normal version

(--map (* it it) '(1 2 3 4)) ;; anaphoric version
```

of course the original can also be written like

```el
(defun square (n) (* n n))

(-map 'square '(1 2 3 4))
```

which demonstrates the usefulness of both versions.

[[ function-list ]]

[[ function-docs ]]

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

### From 2.12 to 2.13

- `-let` now supports `&alist` in destructuring.
- Various performance improvements.
- `-zip` will change in future so it always returns lists. Added
  `-zip-pair` for users who explicitly want the old behavior.
- Added lexical binding pragma to dash.el, fixes
  [#130](https://github.com/magnars/dash.el/issues/130) in Emacs 24+.
- Added `-select-column` and `-select-columns`.
- Fixed an issue with `-map-last` and `--remove-last` where they
  modified their inputs
  ([#158](https://github.com/magnars/dash.el/issues/158)).
- Added `-each-indexed` and `--each-indexed`.
- Added `-take-last` and `-drop-last`.
- Added `-doto` macro.
- `-cut <>` is now treated as a function, consistent with SRFI 26
  ([#185](https://github.com/magnars/dash.el/issues/185))

### From 2.11 to 2.12

- Add GNU ELPA support. (Phillip Lord)
- Add `-some->`, `-some->>`, and `-some-->` macros. (Cam Saul)
- `-is-suffix?` no longer destroys input list.
- Faster hashtable implementation for `-union`.
- Improvements to docstrings and examples

### From 2.10 to 2.11

- Lots of clean up wrt byte compilation, debug macros and tests

### From 2.9 to 2.10

- Add `-let` destructuring to `-if-let` and `-when-let` (Fredrik Bergroth)

### From 2.8 to 2.9

- Add `-let`, `-let*` and `-lambda` with destructuring
- Add `-tree-seq` and `-tree-map-nodes`
- Add `-non-nil`
- Add `-fix`
- Add `-fixfn` (dash-functional 1.2)
- Add `-copy` (Wilfred Hughes)

### From 2.7 to 2.8

- Add `-butlast`

### From 2.6 to 2.7

- `-zip` now supports more than two lists (Steve Lamb)
- Add  `-cycle` ,  `-pad` ,  `-annotate` ,  `-zip-fill` (Steve Lamb)
- Add `-table`, `-table-flat` (finite cartesian product)
- Add `-flatten-n`
- `-slice` now supports "step" argument
- Add functional combinators `-iteratefn`, `-prodfn`
- Add `-replace`, `-splice`, `-splice-list` which generalize `-replace-at` and `-insert-at`
- Add `-compose`, `-iteratefn` and `-prodfn` (dash-functional 1.1)

### From 2.5 to 2.6

- Add `-is-prefix-p`, `-is-suffix-p`, `-is-infix-p` (Matus Goljer)
- Add `-iterate`, `-unfold` (Matus Goljer)
- Add `-split-on`, `-split-when` (Matus Goljer)
- Add `-find-last-index` (Matus Goljer)
- Add `-list` (Johan Andersson)

### From 2.4 to 2.5

- Add `-same-items?` (Johan Andersson)
- A few bugfixes

### From 2.3 to 2.4

- Add `-snoc` (Matus Goljer)
- Add `-replace-at`, `-update-at`, `-remove-at`, and `-remove-at-indices` (Matus Goljer)

### From 2.2 to 2.3

- Add tree operations (Matus Goljer)
- Make font-lock optional

### From 2.1 to 2.2

- Add `-compose` (Christina Whyte)

### From 2.0 to 2.1

- Add indexing operations (Matus Goljer)

### From 1.8 to 2.0

- Split out `dash-functional.el` (Matus Goljer)
- Add `-andfn`, `-orfn`, `-not`, `-cut`, `-const`, `-flip` and `-on`. (Matus Goljer)
- Fix `-min`, `-max`, `-min-by` and `-max-by` (Matus Goljer)

### From 1.7 to 1.8

- Add `-first-item` and `-last-item` (Wilfred Hughes)

### From 1.6 to 1.7

- Add `-rotate` (Matus Goljer)

### From 1.5 to 1.6

- Add `-min`, `-max`, `-min-by` and `-max-by` (Johan Andersson)

### From 1.4 to 1.5

- Add `-sum` and `-product` (Johan Andersson)

### From 1.3 to 1.4

- Add `-sort`
- Add `-reduce-r` (Matus Goljer)
- Add `-reduce-r-from` (Matus Goljer)

### From 1.2 to 1.3

- Add `-partition-in-steps`
- Add `-partition-all-in-steps`

### From 1.1 to 1.2

- Add `-last` (Matus Goljer)
- Add `-insert-at` (Emanuel Evans)
- Add `-when-let` and `-if-let` (Emanuel Evans)
- Add `-when-let*` and `-if-let*` (Emanuel Evans)
- Some bugfixes

## Contributors

 - [Matus Goljer](https://github.com/Fuco1) contributed lots of features and functions.
 - [Takafumi Arakaki](https://github.com/tkf) contributed `-group-by`.
 - [tali713](https://github.com/tali713) is the author of `-applify`.
 - [Víctor M. Valenzuela](https://github.com/vemv) contributed `-repeat`.
 - [Nic Ferrier](https://github.com/nicferrier) contributed `-cons*`.
 - [Wilfred Hughes](https://github.com/Wilfred) contributed `-slice`, `-first-item` and `-last-item`.
 - [Emanuel Evans](https://github.com/shosti) contributed `-if-let`, `-when-let` and `-insert-at`.
 - [Johan Andersson](https://github.com/rejeep) contributed `-sum`, `-product` and `-same-items?`
 - [Christina Whyte](https://github.com/kurisuwhyte) contributed `-compose`
 - [Steve Lamb](https://github.com/steventlamb) contributed `-cycle`, `-pad`, `-annotate`, `-zip-fill` and an n-ary version of `-zip`.
 - [Fredrik Bergroth](https://github.com/fbergroth) made the `-if-let` family use `-let` destructuring and improved script for generating documentation.
 - [Mark Oteiza](https://github.com/holomorph) contributed the script to create an info manual.
 - [Vasilij Schneidermann](https://github.com/wasamasa) contributed `-some`.
 - [William West](https://github.com/occidens) made `-fixfn` more robust at handling floats.
 - [Cam Saül](https://github.com/camsaul) contributed `-some->`, `-some->>`, and `-some-->`.

Thanks!

New contributors are welcome. To ensure that dash.el can be
distributed with ELPA or Emacs, we would request that all contributors
assign copyright to the Free Software Foundation.

## License

Copyright (C) 2012-2016 Free Software Foundation, Inc.

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
