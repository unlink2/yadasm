
build:
	# python setup.py bdist_wheel clean
	# python setup.py bdist_wheel
	python3 -m build

	# publish with python3 -m twine upload --repository testpypi dist/*

typecheck:
	mypy lyadasm  --disallow-untyped-defs

unittests:
	pytest --cov=. lyadasm/test/ -v

lint:
	pylint lyadasm

test:
	# this script simply runs the test pipeline
	make typecheck
	make unittests
	make lint
