'''

Created on Feb 17, 2019

@author: Tyler
'''
import unittest
import LakeFish.handlefortran as fortran

class Test(unittest.TestCase):


    def test100_010_LocationTest(self):
        paramString = {'startdate' : '05211997', 'enddate' : '02172019', 'location' : 'Duluth, Minnesota'}
        toReturn = fortran.createInitFile(paramString)
        self.assertEqual("MNDULU80", toReturn)