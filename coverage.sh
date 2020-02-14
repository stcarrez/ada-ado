#!/bin/sh
NAME=ado.cov
lcov --quiet --base-directory . --directory . -c --include "*/ada-ado/src/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
