# <img align="right" src="https://raw.github.com/magnars/dash.el/master/rainbow-dash.png"> dash.el [![Build Status](https://secure.travis-ci.org/magnars/dash.el.png)](http://travis-ci.org/magnars/dash.el)

A modern list api for Emacs. No 'cl required.

## Installation

It's available on [marmalade](http://marmalade-repo.org/):

    M-x package-install dash

Or you can just dump `dash.el` in your load path somewhere.

## Functions

[[ function-list ]]

There are also anaphoric versions of these functions where that makes sense,
prefixed with two dashs instead of one.

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

[[ function-docs ]]

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

Copyright (C) 2012 Magnar Sveen

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
