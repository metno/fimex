# Fimex, modules/python/test_pyfimex0_coordinatesystem.py
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
