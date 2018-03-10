#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

# Run all tests by default.
# To only run certain tests, set $ERT_SELECTOR as required.
# For example, to skip the test "-fixfn", run the following command:
#
# ERT_SELECTOR='(not "-fixfn")' ./run-tests.sh
#
if [ -z "$ERT_SELECTOR" ] ; then
    ERT_SELECTOR="nil"
fi

$EMACS -batch \
       -l dash.el \
       -l dash-functional.el \
       -l dev/examples-to-tests.el \
       -l dev/examples.el \
       --eval "(ert-run-tests-batch-and-exit (quote ${ERT_SELECTOR}))"

VERSION=`$EMACS -version | head -1 | cut -d" " -f3`

if [[ $VERSION == "24.1.1" ]] || [[ $VERSION == "24.2.1" ]] ; then
    echo Skipping byte compile check for early Emacs version
else
    $EMACS -Q --batch \
           --eval '(setq byte-compile-error-on-warn t)' \
           -f batch-byte-compile dash.el
fi
