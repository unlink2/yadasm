#!/bin/sh

# this script simply runs the test pipeline
mypy core cli test  --disallow-untyped-defs && python -m unittest  --verbose && pylint core cli test
