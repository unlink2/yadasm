NAME=yadasm
LNAME=lyadasm

build:
	# python setup.py bdist_wheel clean
	# python setup.py bdist_wheel
	python3 -m build

	# publish with python3 -m twine upload --repository testpypi dist/*

typecheck:
	mypy $(LNAME)  --disallow-untyped-defs

unittests:
	pytest --cov=. $(LNAME)/test/ -v

lint:
	pylint $(LNAME)

test:
	# this script simply runs the test pipeline
	make typecheck
	make unittests
	make lint
