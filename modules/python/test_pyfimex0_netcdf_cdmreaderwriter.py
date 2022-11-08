#!/usr/bin/env python

# Fimex, modules/python/test_pyfimex0_netcdf_cdmreaderwriter.py
#
# Copyright (C) 2018-2022 met.no
#
# Contact information:
# Norwegian Meteorological Institute
# Box 43 Blindern
# 0313 OSLO
# NORWAY
# email: diana@met.no
#
# Project Info:  https://wiki.met.no/fimex/start
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
import os.path
import shutil
import unittest

test_srcdir = os.path.join(os.path.dirname(__file__), "..", "..", "test")


# based on testNetCDFReaderWriter.cc
class TestNetCDFReaderWriter(unittest.TestCase):

    def test_update(self):
        test_rwfile = 'test_update.nc'
        shutil.copyfile(os.path.join(test_srcdir, 'test_merge_inner.nc'), test_rwfile)

        diff = 10.0
        scale = 1.2

        rw = pyfimex0.createFileReaderWriter('netcdf', test_rwfile)
        self.assertIsNotNone(rw)

        read1 = rw.getDataSlice("ga_2t_1", 0)
        self.assertIsNotNone(read1)

        write1 = pyfimex0.createData(pyfimex0.CDMDataType.DOUBLE, [diff + x * scale for x in read1.values()])
        rw.putDataSlice("ga_2t_1", 0, write1)
        rw.sync()

        del rw

        r = pyfimex0.createFileReader('netcdf', test_rwfile)
        self.assertIsNotNone(r)

        read2 = r.getScaledDataSlice("ga_2t_1", 0)
        self.assertIsNotNone(read2)

        values1 = read1.values()
        values2 = read2.values()
        self.assertEqual(len(values1), len(values2))
        for i, (v1, v2), in enumerate(zip(values1, values2)):
            self.assertAlmostEqual(diff + v1 * scale, v2, msg="at index {}".format(i))

    def test_scaled(self):
        test_rwfile = 'test_scaled.nc'
        shutil.copyfile(os.path.join(test_srcdir, 'test_merge_inner.nc'), test_rwfile)

        addF = 1.0
        addK = addF * 5.0/9.0

        rw = pyfimex0.createFileReaderWriter('netcdf', test_rwfile)
        self.assertIsNotNone(rw)

        read1 = rw.getScaledDataSlice("ga_2t_1", 0)
        self.assertIsNotNone(read1)

        write1 = rw.getScaledDataSliceInUnit("ga_2t_1", "deg_F", 0)
        self.assertIsNotNone(write1)

        wmod1 = pyfimex0.createData(pyfimex0.CDMDataType.DOUBLE, [x + addF for x in write1.values()])
        rw.putScaledDataSliceInUnit("ga_2t_1", "deg_F", 0, wmod1)
        rw.sync()

        del rw

        # read back and compare
        r = pyfimex0.createFileReader('netcdf', test_rwfile)
        self.assertIsNotNone(r)

        read2 = r.getScaledDataSlice("ga_2t_1", 0)
        self.assertIsNotNone(read2)

        values1 = read1.values()
        values2 = read2.values()
        self.assertEqual(len(values1), len(values2))
        for i, (v1, v2), in enumerate(zip(values1, values2)):
            self.assertAlmostEqual(v1 + addK, v2, msg="at index {}".format(i))


if __name__ == '__main__':
    unittest.main()
