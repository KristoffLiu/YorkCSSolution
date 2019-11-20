import unittest
from practical12 import test

class TestWildcard(unittest.TestCase):

    def test_onlywildcard(self):
        self.assertEquals({'1','2'}, 
                            test())


if __name__ == '__main__':
    print(type({'1', '2'}))
    # unittest.main()