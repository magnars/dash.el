#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l bang.el -l examples-to-docs.el -l examples.el -f create-docs-file
