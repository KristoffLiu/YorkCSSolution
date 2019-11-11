'''
Created on 1 Dec 2016

@author: Lilian
'''
import unittest
import inspect
from recursion import is_power

class TestExercise1(unittest.TestCase):
    
    def testPowerTrue(self):
        self.assertEqual(True, is_power(16,2), "is_power(16,2) should be true")
         
    def testPowerFalse(self):
        self.assertEqual(False, is_power(45,3), "is_power(45,3) should be false")
        self.assertEqual(False, is_power(16,0), "is_power(16,0) should be false")
         
    def testPowerOne(self):
        self.assertEqual(False, is_power(16,1), "is_power(16,1) should be false")
         
    def testPowerZero(self):
        self.assertEqual(True, is_power(1,3), "is_power(1,3) should be true as 3^0 = 1")
        self.assertEqual(False, is_power(1,0), "is_power(1,0) should be False, as 0^0 is undefined")
         

    def testPowerOfItself(self):
        self.assertEqual(True, is_power(3,3), "is_power(3,3) should be true")
        self.assertEqual(True, is_power(1,1), "is_power(1,1) should be true")
        self.assertEqual(True, is_power(0,0), "is_power(0,0) should be True, as 0^2 is 0")
        

if __name__ == "__main__":
    unittest.main()
