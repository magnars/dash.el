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

    ;; Package-Requires: ((dash "[[ dash-version ]]"))

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
