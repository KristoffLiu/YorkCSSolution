'''
Created on 1 Dec 2016

@author: Lilian
'''
import unittest
import inspect
from recursion import merge

class TestExercise6(unittest.TestCase):
    
    def test_merge_disjoint_list(self):
        self.assertEqual([0,1,2,3,4,5], merge([1,3,5],[0,2,4]), "merge([1,3,5],[0,2,4]) incorrect")

    def test_merge_overlapping_list(self):
        self.assertEqual([1,1,2,3,4,5,5,5], merge([1,3,5],[1,2,4,5,5]), "merge([1,3,5],[1,2,4,5,5]) incorrect")

    def test_merge_empty(self):
        self.assertEqual([], merge([],[]), "merge([],[]) incorrect!")
        self.assertEqual([1,3,5], merge([1,3,5],[]), "merge([1,3,5],[]) incorrect!")
        self.assertEqual([1,3,5], merge([],[1,3,5]), "merge([],[1,3,5]) incorrect!")
        
    def test_merge_not_changed(self):
        list1 = [1,3,5]
        list2 = [0,2,4]
        list3 = []
        merged = merge(list1,list2)
        self.assertEqual([1,3,5], list1, "merge must not modified the lists passed as parameters!")
        self.assertEqual([0,2,4], list2, "merge must not modified the lists passed as parameters!")

        merged = merge(list1,list3)
        self.assertEqual([1,3,5], list1, "merge must not modified the lists passed as parameters!")
        self.assertEqual([], list3, "merge must not modified the lists passed as parameters!")

        merged = merge(list3, list1)
        self.assertEqual([1,3,5], list1, "merge must not modified the lists passed as parameters!")
        self.assertEqual([], list3, "merge must not modified the lists passed as parameters!")

if __name__ == "__main__":
    unittest.main()
