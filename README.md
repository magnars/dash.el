# bang.el [![Build Status](https://secure.travis-ci.org/magnars/bang.el.png)](http://travis-ci.org/magnars/bang.el)

The startings of a modern list api for Emacs.

We're looking to Clojure for naming and signatures.

Right now it relies on `cl` which should be one of the very first things we fix.

## Warning

This is so much a work in progress that you should definitely not be using it yet.

## Anaphoric functions

While `!filter` takes a function to filter the list by, you can also pass
it a forms - which will then be executed with `it` exposed as the list item.
Here's an example:

    (!filter (lambda (num) (= 0 (% num 2))) '(1 2 3 4)) ;; normal version

    (!filter (= 0 (% it 2)) '(1 2 3 4)) ;; anaphoric version

of course the original can also be written like

    (!filter 'even? '(1 2 3 4))

which demonstrates the usefulness of both versions.

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
