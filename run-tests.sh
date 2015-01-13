#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch \
       $([[ $EMACS == "emacs23" ]] && echo -l dev/ert.el) \
       -l dash.el \
       -l dash-functional.el \
       -l dev/examples-to-tests.el \
       -l dev/examples.el \
       -f ert-run-tests-batch-and-exit

if [[ $EMACS != "emacs23" ]]; then
    $EMACS -Q --batch \
           --eval '(setq byte-compile-error-on-warn t)' \
           -f batch-byte-compile dash.el
fi
