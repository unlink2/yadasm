#!/usr/bin/env python

import unittest

if __name__ == "__main__":
    loader = unittest.TestLoader()
    tests = loader.discover("tests/")
    alltests = unittest.TestSuite((tests))
    unittest.TextTestRunner(verbosity=2).run(alltests)
