#!/bin/sh

# this script simply runs the test pipeline
mypy yadasmlib  --disallow-untyped-defs \
&& pytest --cov=. yadasmlib/test/ -v \
&& pylint yadasmlib \
