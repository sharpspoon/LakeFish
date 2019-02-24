'''
Created on Feb 24, 2019

@author: Tyler
'''
import unittest
import LakeFish.createLakeInitFile as LakeFish

class Test(unittest.TestCase):


    def test600FirstDevTest(self):
        state, city = LakeFish.handleStationName("Montgomery", "Alabama")
        self.assertEqual(state, '10')
        self.assertEqual(city, '4')


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()