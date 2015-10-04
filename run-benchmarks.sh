#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

if [ -z "$BENCH_NUM_TRIES" ] ; then
    BENCH_NUM_TRIES="nil"
fi

# Run all benchmarks by default.  To only run some of them, set
# $BENCH_SELECTOR as required (see file 'dev/benchmarks.el').
#
# For example, to run only '-cons*' benchmarks:
#
#     BENCH_SELECTOR='"-cons\*"' ./run-benchmarks.sh
#
# Or, to skip the benchmarks in that group:
#
#     BENCH_SELECTOR='(not "-cons\*")' ./run-benchmarks.sh

if [ -z "$BENCH_SELECTOR" ] ; then
    BENCH_SELECTOR="nil"
fi

$EMACS -batch \
       -l dash.el \
       -l dash-functional.el \
       -l dev/benchmarks.el \
       --eval "(run-benchmarks-and-exit ${BENCH_NUM_TRIES} (quote ${BENCH_SELECTOR}))"
