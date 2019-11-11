'''
Created on 7 Nov 2019

@author: Lilian
'''
import unittest
import inspect
from recursion import sum_digits

class TestExercise2(unittest.TestCase):

    def test_sum_positive(self):
        self.assertEqual(10, sum_digits(1234), "sum_digits(1234) incorrect")
        self.assertEqual(10, sum_digits(244), "sum_digits(244]) incorrect")
        self.assertEqual(9, sum_digits(9), "sum_digits(9]) incorrect")
        

    def test_sum_negative(self):
        self.assertEqual(1, sum_digits(-1), "sum_digits(-1) incorrect!")
        self.assertEqual(10, sum_digits(-244), "sum_digits(-244) incorrect!")
 
    def test_sum_zero(self):
        self.assertEqual(0, sum_digits(0), "sum_digits(0) incorrect!")
        

if __name__ == "__main__":
    unittest.main()
