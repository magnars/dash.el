#!/bin/sh

git stash -q --keep-index
./run-tests.sh
RESULT=$?
[ $RESULT == 0 ] && ./create-docs.sh && git add ./README.md
git stash pop -q
[ $RESULT -ne 0 ] && exit 1
exit 0
