'''
Created on 1 Dec 2016

@author: Lilian
'''
import unittest
import inspect
from recursion import flatten

class TestExercise5(unittest.TestCase):
    
    def test_flatten_flatlist(self):
        self.assertEqual([0,1,2,3,4], flatten([0,1,2,3,4]), "flatten([0,1,2,3,4]) incorrect")

    def test_flatten_empty(self):
        self.assertEqual([], flatten([]), "flatten([]) incorrect!")
        
    def test_flatten_nestedlist(self):
        self.assertEqual([1,1,2,3,3,4], flatten([[1], [1,[2,3]],3,4]), "flatten([[1], [1,[2,3]],3,4]) incorrect!")
    
    def test_flatten_mixedemptylist(self):
        self.assertEqual([1,2,3], flatten([[1], [], [], [2,[],[3]]]), "flatten([[1], [], [], [2,[],[3]]]) incorrect!")

    def test_flatten_not_changed(self):
        mixed_list = [[1], [1,[2,3]],3,4]
        flattened = flatten(mixed_list)
        self.assertEqual([[1], [1,[2,3]],3,4], mixed_list, "flatten must not modified the list passed as parameter!")

if __name__ == "__main__":
    unittest.main()
