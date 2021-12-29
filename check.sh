#!/bin/sh
mypy core cli test  --disallow-untyped-defs && pylint core cli test && python -m unittest  --verbose
