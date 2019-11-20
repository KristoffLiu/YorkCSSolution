import unittest
from practical12 import allwords

class TestAllWords(unittest.TestCase):
    t9_keypad = {'2':['a','b','c'], '3':['d','e','f'],
                '4':['g','h','i'], '5':['j','k','l'],
                '6':['m','n','o'], '7':['p','q','r','s'],
                '8':['t','u','v'], '9':['w','x','y','z']}

    def test_empty(self):
        self.assertEquals({''}, allwords('', self.t9_keypad))

    def test_invalid_digit(self):
        self.assertRaises(ValueError, allwords,'204', self.t9_keypad)  

    def test_allwords(self):
        self.assertEquals({'AI','AH','CH','BG','BH','BI','CI','CG','AG'}, 
                            allwords('24', self.t9_keypad))          
        self.assertEquals({'A','C','B'}, allwords('2', self.t9_keypad))


if __name__ == '__main__':
    unittest.main()                                              