#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch \
       -l dev/ert.el \
       -l dash.el \
       -l dash-functional.el \
       -l dev/examples-to-tests.el \
       -l dev/examples.el \
       $([[ $EMACS != "emacs23" ]] && echo -l dev/test-byte-compile.el) \
       -f ert-run-tests-batch-and-exit
