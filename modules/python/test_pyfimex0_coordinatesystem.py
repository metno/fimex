import pyfimex0
import numpy
import os.path
import unittest

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")

class TestReader(unittest.TestCase):

    def test_listCoordinateSystems(self):
        test_ncfile = os.path.join(test_srcdir, 'testdata_vertical_ensemble_in.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        cs = pyfimex0.listCoordinateSystems(r)
        self.assertEqual(len(cs), 6)

        cs_var = pyfimex0.findCompleteCoordinateSystemFor(cs, 'x_wind_10m')
        self.assertEqual(cs_var.id(), 'CF-1.X:ensemble_member,height7,latitude,longitude,time,x,y')

        cax1 = cs_var.findAxisOfType(pyfimex0.CoordinateAxisType.GeoX)
        self.assertEqual(cax1.getName(), 'x')

        cax2 = cs_var.findAxisOfType([pyfimex0.CoordinateAxisType.GeoX, pyfimex0.CoordinateAxisType.Lon])
        self.assertEqual(cax2.getName(), 'x')

if __name__ == '__main__':
    unittest.main()
