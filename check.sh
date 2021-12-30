#!/bin/sh

# this script simply runs the test pipeline
mypy core cli test  --disallow-untyped-defs \
&& pytest --cov=. test/ -v \
&& pylint core cli test \
