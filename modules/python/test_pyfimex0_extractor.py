# Fimex, modules/python/test_pyfimex0_extractor.py
#
# Copyright (C) 2018-2026 met.no
#
# Contact information:
# Norwegian Meteorological Institute
# Box 43 Blindern
# 0313 OSLO
# NORWAY
# email: diana@met.no
#
# Project Info:  https://github.com/metno/fimex/wiki
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
# USA.

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


    def test_reduceTimeIso8601(self):
        test_ncfile = os.path.join(test_srcdir, 'erai.sfc.40N.0.75d.200301011200.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        extra = pyfimex0.createExtractor(r)
        extra.reduceTimeStartEnd('2017-05-22T06:00:00', '2017-05-22T07:00:00')

        with self.assertRaises(Exception):
            extra.reduceTimeStartEnd('ooops', '2017-05-22T07:00:00')

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

    def test_removeVariable(self):
        test_ncfile = os.path.join(test_srcdir, 'erai.sfc.40N.0.75d.200301011200.nc')
        r = pyfimex0.createFileReader('netcdf', test_ncfile)

        self.assertTrue('ga_skt' in r.getCDM().getVariableNames())

        extra = pyfimex0.createExtractor(r)
        extra.removeVariable('ga_skt')

        self.assertTrue('ga_skt' not in extra.getCDM().getVariableNames())

        del extra
        del r


if __name__ == '__main__':
    unittest.main()
