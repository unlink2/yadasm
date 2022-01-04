#!/bin/sh

# this script simply runs the test pipeline
mypy yadasm  --disallow-untyped-defs \
&& pytest --cov=. yadasm/test/ -v \
&& pylint yadasm \
