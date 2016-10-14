#!/usr/bin/env bash

set -e

# Run all tests by default.
# To only run certain tests, set $ERT_SELECTOR as required.
# For example, to skip the test "-fixfn", run the following command:
#
# ERT_SELECTOR='(not "-fixfn")' ./run-tests.sh
#
if [ -z "$ERT_SELECTOR" ] ; then
    ERT_SELECTOR="nil"
fi

rm -f *.elc

cask exec emacs -batch \
       -L . \
       -l dev/examples-to-tests.el \
       -l dev/examples.el \
       --eval "(ert-run-tests-batch-and-exit (quote ${ERT_SELECTOR}))"

emacs -Q --batch \
      --eval '(setq byte-compile-error-on-warn t)' \
      -f batch-byte-compile dash.el
