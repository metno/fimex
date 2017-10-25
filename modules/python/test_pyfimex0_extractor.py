import pyfimex0
import numpy
import os.path
import unittest

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")

class TestExtractor(unittest.TestCase):

    def test_reduceDimension(self):
        test_ncfile = os.path.join(test_srcdir, 'testdata_vertical_ensemble_in.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        extra = pyfimex0.createExtractor(r)
        extra.reduceDimension('x', [0, 1, 3])
        extra.reduceDimension('y', 1, 2)
        e_cdm = extra.getCDM()
        self.assertEqual(3, e_cdm.getDimension('x').getLength())
        self.assertEqual(2, e_cdm.getDimension('y').getLength())
        del extra
        del r


    def test_reduceLatLonBoundingBox(self):
        test_ncfile = os.path.join(test_srcdir, 'erai.sfc.40N.0.75d.200301011200.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        extra = pyfimex0.createExtractor(r)
        extra.reduceLatLonBoundingBox(59, 60, 9, 11)

        del extra
        del r

    def test_selectVariables(self):
        test_ncfile = os.path.join(test_srcdir, 'erai.sfc.40N.0.75d.200301011200.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        extra = pyfimex0.createExtractor(r)
        extra.selectVariables(['x_wind_10m', 'x_wind_10m'])

        e_cdm = extra.getCDM()
        e_vars = e_cdm.getVariableNames()
        self.assertTrue('specific_humidity_ml' not in e_vars)

        del extra
        del r


if __name__ == '__main__':
    unittest.main()
