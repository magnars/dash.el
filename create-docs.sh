#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l dash.el -l dash-functional.el -l dev/examples-to-docs.el -l dev/examples.el -f create-docs-file
$EMACS -batch -l dash.el -l dash-functional.el -l dev/examples-to-info.el -l dev/examples.el -f create-info-file
