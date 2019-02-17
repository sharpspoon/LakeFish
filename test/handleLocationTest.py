'''
Created on Feb 17, 2019

@author: Tyler
'''
import unittest
import LakeFish.handlefortran as fortran

class Test(unittest.TestCase):


    def test100_010_LocationTest(self):
        locationString = "Duluth, Minnesota"
        toReturn = fortran.handleLocation(locationString, 1980)
        self.assertEqual("MNDULU80", toReturn)