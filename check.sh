#!/bin/sh

# this script simply runs the test pipeline
mypy lyadasm  --disallow-untyped-defs \
&& pytest --cov=. lyadasm/test/ -v \
&& pylint lyadasm \
