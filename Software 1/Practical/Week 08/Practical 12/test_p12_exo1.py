import unittest
from practical12 import wildcard_pattern

class TestWildcard(unittest.TestCase):

    def test_empty(self):
        self.assertEquals({''}, wildcard_pattern('', '?'))

    def test_nowildcard(self):
        self.assertEquals({'1100'}, wildcard_pattern('1100','?'))

    def test_multiple_wildcard(self):
        self.assertEquals({'1100', '1101', '1001', '1000'}, 
                            wildcard_pattern('1?0?', '?'))

    def test_onlywildcard(self):
        self.assertEquals({'00','01','10','11'}, 
                            wildcard_pattern('??', '?'))


if __name__ == '__main__':
    unittest.main()