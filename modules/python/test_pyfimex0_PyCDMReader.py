# Fimex, modules/python/test_pyfimex0.py
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

import pyfimex0 as pyfi
import numpy as np
import sys

import unittest

class PyCDMReader(pyfi.CDMReader):
    def __init__(self):
        pyfi.CDMReader.__init__(self)
        cdm = self.getInternalCDM()
        cdm.addDimension(pyfi.CDMDimension('dimx', 12))
        cdm.addVariable(pyfi.CDMVariable('varx', pyfi.CDMDataType.FLOAT, ['dimx']))

    def getDataSlice(self, var, uldp):
        if var == 'varx':
            return pyfi.createData(np.arange(12, dtype=np.float32))
        else:
            return None


class PyCDMFilter(pyfi.CDMReader):
    def __init__(self, reader):
        pyfi.CDMReader.__init__(self)
        self.reader = reader
        self.setInternalCDM(reader.getCDM())

    def transform(self, var, data):
        if var == 'varx' and data is not None:
            v = data.values() * -1
            data = pyfi.createData(v)
        return data

    def getDataSlice(self, var, uldp):
        return self.transform(var, self.reader.getDataSlice(var, uldp))

class TestPyCDMReader(unittest.TestCase):

    def test_CDM(self):
        r = PyCDMReader()
        cdm = r.getCDM()
        self.assertEqual(["dimx"], cdm.getDimensionNames())
        self.assertEqual(["varx"], cdm.getVariableNames())
        pyfi.createFileWriter(r, "null", "")

    def test_DataSliceUDP(self):
        r = PyCDMReader()
        self.assertIsNone(r.getDataSlice("no-such-var", 0))
        d = r.getDataSlice("varx", 0)
        self.assertEqual(12, d.size())

    def test_DataSliceSB(self):
        r = PyCDMReader()
        sb = pyfi.SliceBuilder(r.getCDM(), 'varx', True)
        sb.setStartAndSize('dimx', 4, 4)
        d = r.getDataSliceSB("varx", sb)
        self.assertEqual([4,5,6,7], list(d.values()))

    def test_Filter(self):
        r = PyCDMReader()
        f = PyCDMFilter(r)

        sb = pyfi.SliceBuilder(f.getCDM(), 'varx', True)
        sb.setStartAndSize('dimx', 4, 4)
        d = f.getDataSliceSB("varx", sb)
        self.assertEqual([-4,-5,-6,-7], list(d.values()))
        pyfi.createFileWriter(f, "null", "")

if __name__ == '__main__':
    unittest.main()
