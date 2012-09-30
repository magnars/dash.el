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
