import unittest

class TestFileFail(unittest.TestCase):

    def test_fail(self):
        self.fail("I will fail")
