import pyfimex0
import numpy
import os.path
import unittest

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")

class TestVerticalInterpolator(unittest.TestCase):

    def test_FixedLevelss(self):
        test_ncfile = os.path.join(test_srcdir, 'testdata_altitude_height_in.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        vertical = pyfimex0.createVerticalInterpolator(r, 'altitude', 'linear_const_extra')

        level1 = [800,400,100,50]
        vertical.interpolateToFixed(level1)
        vertical.ignoreValidityMin(False)
        vertical.ignoreValidityMax(False)

        v_cdm = vertical.getCDM()
        self.assertEqual(len(level1), v_cdm.getDimension('height_above_msl').getLength())

        values = vertical.getDataSlice('vmro3', 0).values()
        self.assertEqual(len(values), 19 * 10 * 4)
        self.assertTrue(abs(values[0] - 3.01479730069332e-10) < 1e-12)


if __name__ == '__main__':
    unittest.main()
