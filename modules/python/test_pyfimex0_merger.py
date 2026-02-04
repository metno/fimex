# Fimex, modules/python/test_pyfimex0_merger.py
#
# Copyright (C) 2019-2026 met.no
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

import pyfimex0 as pyfi

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")

class TestMerger(unittest.TestCase):
    def test_grid_from_inner(self):
        fileNameInner = os.path.join(test_srcdir, "test_merge_inner.nc")
        fileNameOuter = os.path.join(test_srcdir, "test_merge_outer.nc")

        readerI = pyfi.createFileReader("", fileNameInner)
        readerO = pyfi.createFileReader("", fileNameOuter)

        merger = pyfi.createMerger(readerI, readerO)
        merger.setSmoothing("LINEAR(2,5)")
        merger.setTargetGridFromInner()

        sliceM = merger.getDataSlice("ga_2t_1", 0)
        self.assertTrue(sliceM is not None)


if __name__ == '__main__':
    unittest.main()
