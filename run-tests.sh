#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l ert.el -l bang.el -l tests.el -f ert-run-tests-batch-and-exit
