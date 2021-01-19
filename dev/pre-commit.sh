#!/usr/bin/env sh

git stash -q --keep-index
make check
RESULT=$?
[ $RESULT -eq 0 ] && make docs && git add ./README.md
git stash pop -q
[ $RESULT -ne 0 ] && exit 1
exit 0
