#!/usr/bin/env python

import pyfimex0
import numpy
import os.path
import unittest

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")

class TestNetCDFWriter(unittest.TestCase):

    def test_writeExtracted(self):
        test_ncfile = os.path.join(test_srcdir, 'testdata_vertical_ensemble_in.nc')
        r1 = pyfimex0.createFileReader('netcdf', test_ncfile)

        v_x_wind = 'x_wind_10m'
        r1v_x_wind = r1.getDataSlice(v_x_wind, 0).values()

        extra = pyfimex0.createExtractor(r1)
        extra.selectVariables([v_x_wind])

        outfile = "extracted_x_wind.nc"
        pyfimex0.createNetCDFWriter(extra, outfile)

        del extra
        del r1

        r2 = pyfimex0.createFileReader('netcdf', outfile)
        r2v_x_wind = r2.getDataSlice(v_x_wind, 0).values()

        for v1, v2 in zip(r1v_x_wind, r2v_x_wind):
            self.assertEqual(v1, v2)


    def test_writeExtracted(self):
        test_ncfile = os.path.join(test_srcdir, 'testdata_vertical_ensemble_in.nc')
        r1 = pyfimex0.createFileReader('netcdf', test_ncfile)

        v_x_wind = 'x_wind_10m'
        r1v_x_wind = r1.getDataSlice(v_x_wind, 0).values()

        extra = pyfimex0.createExtractor(r1)
        extra.selectVariables([v_x_wind])

        outfile = "extracted_wind_x.nc"
        pyfimex0.createFileWriter(extra, 'netcdf', outfile)

        del extra
        del r1

        r2 = pyfimex0.createFileReader('netcdf', outfile)
        r2v_x_wind = r2.getDataSlice(v_x_wind, 0).values()

        for v1, v2 in zip(r1v_x_wind, r2v_x_wind):
            self.assertEqual(v1, v2)


if __name__ == '__main__':
    unittest.main()
