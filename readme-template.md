![GitHub Workflow Status](https://img.shields.io/github/workflow/status/magnars/dash.el/CI)
[![MELPA](https://melpa.org/packages/dash-badge.svg)](https://melpa.org/#/dash)
[![MELPA Stable](https://stable.melpa.org/packages/dash-badge.svg)](https://stable.melpa.org/#/dash)

# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el

A modern list API for Emacs.  No
[`'cl`](https://gnu.org/software/emacs/manual/html_node/cl/) required.

## Installation

It's available on [GNU ELPA](https://elpa.gnu.org/) and
[MELPA](https://melpa.org/):

    M-x package-install dash

Or you can just dump `dash.el` in your `load-path` somewhere.

If you want the function combinators, then also:

    M-x package-install dash-functional

## Using in a package

Add something like this to the [library's
headers](https://gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html):

    ;; Package-Requires: ((dash "[[ dash-version ]]"))

To get function combinators:

    ;; Package-Requires: ((dash "[[ dash-version ]]") (dash-functional "[[ dash-functional-version ]]"))

## Upcoming breaking change!

- For backward compatibility reasons, `-zip` when called with two
  lists returns a list of cons cells, rather than a list of proper
  lists.  This is a clunky API, and may be changed in a future release
  to always return a list of proper lists, as `-zip-lists` currently
  does.

  **N.B.:** Do not rely on the current behavior of `-zip` for two
  lists.  Instead, use `-zip-pair` for a list of cons cells, and
  `-zip-lists` for a list of proper lists.

## Fontification of special variables

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

## Info symbol lookup

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

[[ function-list ]]

[[ function-docs ]]

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

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` or `dash.texi` directly; they are
auto-generated.  Change `readme-template.md` or `dash-template.texi`
instead, respectively.

To ensure that `dash.el` can be distributed with GNU ELPA or Emacs, we
require that all contributors assign copyright to the Free Software
Foundation.  For more on this, see [`(info "(emacs) Copyright
Assignment")`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html).

## Change log

### From 2.16 to 2.17

- Sped up `-uniq` by using hash-tables when possible (@cireu, #305).
- Fixed `-inits` to be non-destructive (@SwiftLawnGnome, #313).
- Fixed indent rules for `-some->` and family (@wbolster, #321).
- Added `-zip-lists` which always returns a list of proper lists, even for two
  input lists (see issue #135).

### From 2.15 to 2.16

- Added `--doto`, anaphoric version of `-doto` (#282).
- Aliased `-cons-pair-p` to `-cons-pair?` (#288).
- Generalized `-rotate` for `|N|` greater than the length of the list (@leungbk,
  #290).
- Added a mechanism to extend destructuring with custom matchers (@yyoncho,
  #277).

### From 2.14 to 2.15

This release brings new destructuring features, some new control flow
functions and performance optimizations.

- Added `-setq` with destructuring binding support similar to the `-let` family
  (#116).
- Added smarter key destructuring in `-let` and friends where variables are
  auto-derived from keys (#111).
- Allowed `-let` bindings without a source value form (#256).
- Added `-each-r` and `-each-r-while` (@doublep, #159).
- Added `-common-suffix` (@basil-conto, #263).
- Improved performance of folds (`-reduce` and friends) (@basil-conto, #264).

### From 2.13 to 2.14

This release retired Emacs 23 support.

- Added Edebug support for threading macros (@Wilfred).
- Added `-unzip`.
- Added support for `-first-item` and `-last-item` as [place
  forms](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generalized-Variables.html).
- Added `-powerset` and `-permutations` (@holomorph).
- Added `-as->` for threading a named variable (@zck).
- Added `-partition-after-pred`, `-partition-before-pred`,
  `-partition-after-item`, and `-partition-before-item` (@zck).
- Fixed a bug in `-any-p` and friends testing for `null` on lists containing
  `nil` (#239).
- Fixed infinite loop bug in `-zip` and `-interleave` when called with empty
  input.
- Added `-second-item` through `-fifth-item` as alternatives to `nth`
  (@Wilfred).
- Added `-tails` and `-inits`.
- Added `-running-sum` and `-running-product`.
- Added the `-reductions[-r][-from]` family of functions (like `-reduce` but
  collecting intermediate results).
- Added `-common-prefix` (@basil-conto).

### From 2.12 to 2.13

- `-let` now supports `&alist` destructuring.
- Various performance improvements.
- `-zip` might change in a future release to always return a list of proper
  lists.  Added `-zip-pair` for users who explicitly want the old behavior.
- Enabled lexical binding in `dash.el` for Emacs versions 24 or newer (#130).
- Added `-select-column` and `-select-columns`.
- Fixed `-map-last` and `--remove-last` to be non-destructive (#158).
- Added `-each-indexed` and `--each-indexed`.
- Added `-take-last` and `-drop-last`.
- Added the `-doto` macro.
- `-cut <>` is now treated as a function, consistent with [SRFI
  26](https://srfi.schemers.org/srfi-26/srfi-26.html) (#185).

### From 2.11 to 2.12

- Added GNU ELPA support (Phillip Lord).
- Added `-some->`, `-some->>`, and `-some-->` macros (Cam Saul).
- `-is-suffix?` is now non-destructive.
- Faster hash table implementation for `-union`.
- Improvements to docstrings and examples.

### From 2.10 to 2.11

- Lots of clean up w.r.t. byte compilation, debug macros, and tests.

### From 2.9 to 2.10

- Added `-let` destructuring to `-if-let` and `-when-let` (Fredrik Bergroth).

### From 2.8 to 2.9

- Added `-let`, `-let*`, and `-lambda` with destructuring.
- Added `-tree-seq` and `-tree-map-nodes`.
- Added `-non-nil`.
- Added `-fix`.
- Added `-fixfn` (`dash-functional` version `1.2`).
- Added `-copy` (Wilfred Hughes).

### From 2.7 to 2.8

- Added `-butlast`.

### From 2.6 to 2.7

- `-zip` now supports more than two lists (Steve Lamb).
- Added `-cycle`, `-pad`, `-annotate`, and `-zip-fill` (Steve Lamb).
- Added `-table`, `-table-flat` (finite Cartesian product).
- Added `-flatten-n`.
- `-slice` now supports a "step" argument.
- Added functional combinators `-iteratefn` and `-prodfn`.
- Added `-replace`, `-splice`, and `-splice-list` which generalize `-replace-at`
  and `-insert-at`.
- Added `-compose`, `-iteratefn`, and `-prodfn` (`dash-functional` version
  `1.1`).

### From 2.5 to 2.6

- Added `-is-prefix-p`, `-is-suffix-p`, and `-is-infix-p` (Matus Goljer).
- Added `-iterate` and `-unfold` (Matus Goljer).
- Added `-split-on` and `-split-when` (Matus Goljer).
- Added `-find-last-index` (Matus Goljer).
- Added `-list` (Johan Andersson).

### From 2.4 to 2.5

- Added `-same-items?` (Johan Andersson).
- Various bugfixes.

### From 2.3 to 2.4

- Added `-snoc` (Matus Goljer).
- Added `-replace-at`, `-update-at`, `-remove-at`, and `-remove-at-indices`
  (Matus Goljer).

### From 2.2 to 2.3

- Added tree operations (Matus Goljer).
- Made Font Lock optional.

### From 2.1 to 2.2

- Added `-compose` (Christina Whyte).

### From 2.0 to 2.1

- Added indexing operations (Matus Goljer).

### From 1.8 to 2.0

- Split out `dash-functional.el` (Matus Goljer).
- Added `-andfn`, `-orfn`, `-not`, `-cut`, `-const`, `-flip`, and `-on` (Matus
  Goljer).
- Fixed `-min`, `-max`, `-min-by`, and `-max-by` (Matus Goljer).

### From 1.7 to 1.8

- Added `-first-item` and `-last-item` (Wilfred Hughes).

### From 1.6 to 1.7

- Added `-rotate` (Matus Goljer).

### From 1.5 to 1.6

- Added `-min`, `-max`, `-min-by`, and `-max-by` (Johan Andersson).

### From 1.4 to 1.5

- Added `-sum` and `-product` (Johan Andersson).

### From 1.3 to 1.4

- Added `-sort`.
- Added `-reduce-r` (Matus Goljer).
- Added `-reduce-r-from` (Matus Goljer).

### From 1.2 to 1.3

- Added `-partition-in-steps`.
- Added `-partition-all-in-steps`.

### From 1.1 to 1.2

- Added `-last` (Matus Goljer).
- Added `-insert-at` (Emanuel Evans).
- Added `-when-let` and `-if-let` (Emanuel Evans).
- Added `-when-let*` and `-if-let*` (Emanuel Evans).
- Various bugfixes.

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
- [Mark Oteiza](https://github.com/holomorph) contributed the script to create
  an Info manual.
- [Vasilij Schneidermann](https://github.com/wasamasa) contributed `-some`.
- [William West](https://github.com/occidens) made `-fixfn` more robust at
  handling floats.
- [Cam Saul](https://github.com/camsaul) contributed `-some->`, `-some->>`, and
  `-some-->`.
- [Basil L. Contovounesios](https://github.com/basil-conto) contributed
  `-common-prefix`.
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
