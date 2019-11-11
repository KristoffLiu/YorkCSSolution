'''
Created on 1 Dec 2016

@author: Lilian
'''
import unittest
import inspect
from recursion import rec_sum

class TestExercise3(unittest.TestCase):
    
    def test_sum_normal(self):
        self.assertEqual(10, rec_sum([0,1,2,3,4]), "rec_sum([0,1,2,3,4]) incorrect")
        self.assertEqual(10, rec_sum([0,2,4,4]), "rec_sum([0,2,4,4]) incorrect")
         

    def test_sum_empty(self):
        self.assertEqual(0, rec_sum([]), "rec_sum([]) incorrect!")
        

if __name__ == "__main__":
    unittest.main()
