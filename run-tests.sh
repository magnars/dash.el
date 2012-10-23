#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l ert.el -l dash.el -l examples-to-tests.el -l examples.el -f ert-run-tests-batch-and-exit
