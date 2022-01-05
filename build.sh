#!/bin/sh
# python setup.py bdist_wheel clean
# python setup.py bdist_wheel
python3 -m build

# publish with python3 -m twine upload --repository testpypi dist/*
