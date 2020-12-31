#!/usr/bin/env sh

if [ -z "${EMACS}" ]; then
    EMACS=emacs
fi

"${EMACS}" -Q -batch -l dash.el -l dash-functional.el \
           -l dev/examples-to-docs.el -l dev/examples.el -f create-docs-file
"${EMACS}" -Q -batch -l dash.el -l dash-functional.el \
           -l dev/examples-to-info.el -l dev/examples.el -f create-info-file
