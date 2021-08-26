# Dash NEWS -- history of user-visible changes

Copyright (C) 2012-2021 Free Software Foundation, Inc.

See the end of the file for license conditions.

## Change log

### From 2.19.0 to 2.19.1

#### Fixes

- Fixed a regression from `2.18` in `-is-suffix-p` which led to false
  negatives when parts of the suffix appeared multiple times in the
  list being searched (Bennett Rennier, #384).

### From 2.18.1 to 2.19.0

#### Fixes

- Reverted a breaking change introduced in `2.18.0` that caused the
  threading macro `-->` to be indented differently from `->` and `->>`
  (#375).
- Added and fixed Edebug specifications for many Dash macros (Philipp
  Stephani, #380, #381).

#### New features

- The combinators `-on`, `-flip`, `-not`, `-andfn`, and `-orfn` now
  return variadic functions that take any number of arguments (#308).
- New combinator `-rotate-args` similar to `-flip`, but for arbitrary
  arglist rotations (suggested by @vapniks, #72).
- New function `-every` and its anaphoric macro counterpart `--every`.
  They are like the existing `-every-p` and `--every-p`, respectively,
  but return the last non-`nil` result instead of just `t`.
- New macro `--partition-after-pred` which affords
  `-partition-after-pred` better performance (Per Weijnitz, #362).

### From 2.18.0 to 2.18.1

- Fixed a regression from `2.17` as well as a long-standing bug in
  `--iterate`, which evaluated its arguments one too many times.  This
  in turn could lead to errors in `-flatten-n` when it tried
  flattening certain structures too far (#373).

### From 2.17 to 2.18

This release absorbs the now obsolete `dash-functional` version
`1.3.0` into `dash`, and brings the very old version of `dash` on GNU
ELPA up to date.

Package maintainers should replace all uses of `dash-functional`,
which will eventually be deleted, with `dash` version `2.18.0`.  For
more information on this, see:
https://github.com/magnars/dash.el/wiki/Obsoletion-of-dash-functional.el

- New function `-iota` for generating arithmetic sequences
  (@holomorph, #215).

- Calling `-list` with more than one argument is now deprecated.

- `-lambda` now accepts an empty argument list.

- New anaphoric macros `--reductions-from`, `--reductions`,
  `--reductions-r-from`, and `--reductions-r` corresponding to the
  analogous non-anaphoric functions.

- `-doto` threading now works as with `->`.

- New buffer-local minor mode `dash-fontify-mode` and globalized
  counterpart `global-dash-fontify-mode` for fontifying special Dash
  variables such as `it`, `it-index`, `acc`, etc.  The minor mode also
  fontifies calls to Dash macros in older Emacs versions which did not
  dynamically detect macro calls.

  This obsoletes the user option `dash-enable-fontlock` and the
  function `dash-enable-font-lock`, which is now an alias of
  `global-dash-fontify-mode`.

- New command `dash-register-info-lookup` for integration with `C-h S`
  (`info-lookup-symbol`).  This command allows Dash symbols to be
  looked up in the Dash manual just like Elisp symbols are looked up
  in the Elisp manual.  The command can be called directly when
  needed, or automatically from your `user-init-file`.  For example:

  ```el
  (with-eval-after-load 'info-look
    (dash-register-info-lookup))
  ```

- Dash is now listed under the standard [Customization
  groups](https://gnu.org/software/emacs/manual/html_node/emacs/Customization-Groups.html)
  and [Finder
  keywords](https://gnu.org/software/emacs/manual/html_node/emacs/Package-Keywords.html)
  `extensions` and `lisp`.

- The Dash manual is now licensed under the GNU Free Documentation
  License version 1.3.

- Various other bug fix, performance, byte-compilation, and
  documentation improvements.

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
  forms](https://gnu.org/software/emacs/manual/html_node/elisp/Generalized-Variables.html).
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

## License

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
