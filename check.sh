#!/bin/sh

# this script simply runes the test pipeline
mypy core cli test  --disallow-untyped-defs && pylint core cli test && python -m unittest  --verbose
