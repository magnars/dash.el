# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el [![Build Status](https://secure.travis-ci.org/magnars/dash.el.png)](http://travis-ci.org/magnars/dash.el)

A modern list api for Emacs. No 'cl required.

## Breaking change 1.8.0 -> 2.0.0

- The `-min` and `-max` functions are no longer variadic, but take a
  list to be more in line with the other dash functions.

- `-min-by` and `-max-by` now take a comparator function to sort by.

The stated scope of dash is increasing. It now includes more
functional style functions, like combinators and threading macros.
These have been creeping in anyway, since they're so darn useful. Time
to make it official. :)

- `-rpartial`, `-juxt` and `-applify` are moved to a separate package.
  Note that `-partial` is still in dash for backwards compatibility
  reasons.

These new combinators require Emacs 24 for its lexical scope. So
they are offered in a separate package: `dash-functional`.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

    M-x package-install dash

Or you can just dump `dash.el` in your load
path somewhere.

If you want the function combinators, then also:

    M-x package-install dash-functional

## Using in a package

Add this to the big comment block at the top:

    ;; Package-Requires: ((dash "1.8.0"))

To get function combinators:

    ;; Package-Requires: ((dash "1.8.0") (dash-functional "1.0.0") (emacs "24"))

## Functions

[[ function-list ]]

## Anaphoric functions

There are also anaphoric versions of functions where that makes sense,
prefixed with two dashes instead of one.

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

### From 2.1.0 to 2.2.0

- Add `-compose` (Christina Whyte)

### From 2.0.0 to 2.1.0

- Add indexing operations (Matus Goljer)

### From 1.8.0 to 2.0.0

- Split out `dash-functional.el` (Matus Goljer)
- Add `-andfn`, `-orfn`, `-not`, `-cut`, `-const`, `-flip` and `-on`. (Matus Goljer)
- Fix `-min`, `-max`, `-min-by` and `-max-by` (Matus Goljer)

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

 - [Matus Goljer](https://github.com/Fuco1) contributed lots of features and functions.
 - [Takafumi Arakaki](https://github.com/tkf) contributed `-group-by`.
 - [tali713](https://github.com/tali713) is the author of `-applify`.
 - [Víctor M. Valenzuela](https://github.com/vemv) contributed `-repeat`.
 - [Nic Ferrier](https://github.com/nicferrier) contributed `-cons*`.
 - [Wilfred Hughes](https://github.com/Wilfred) contributed `-slice`, `-first-item` and `-last-item`.
 - [Emanuel Evans](https://github.com/shosti) contributed `-if-let`, `-when-let` and `-insert-at`.
 - [Johan Andersson](https://github.com/rejeep) contributed `-sum` and `-product`.
 - [Christina Whyte](https://github.com/kurisuwhyte) contributed `-compose`

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
