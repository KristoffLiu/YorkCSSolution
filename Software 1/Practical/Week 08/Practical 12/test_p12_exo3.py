import unittest
from practical12 import fill_bag

class TestFillBag(unittest.TestCase):

    def test_zeroweight(self):
        self.assertEquals(0, fill_bag(0,[1,2],[5,5]))


    def test_empty_values(self):
        self.assertEquals(0, fill_bag(10,[],[]))

    def test_invalid_parameters(self):
        self.assertRaises(ValueError, fill_bag, 10, [1,2], [1])

    def test_allitems(self):
        self.assertEquals(70,fill_bag(10, [20,5,40,5],[1,2,2,3]))

    def test_someitems(self):
        self.assertEquals(60,fill_bag(10, [20,5,5,10,40,15,25],[1,2,2,3,8,7,4]))

    def test_tooheavy(self):
        self.assertEquals(0, fill_bag(10, [10,20], [15,25]))


if __name__ == '__main__':
    unittest.main()