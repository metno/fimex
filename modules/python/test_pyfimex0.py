# Fimex, modules/python/test_pyfimex0.py
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

    def test_MifiVersion(self):
        mv = pyfimex0.mifi_version()
        self.assertEqual(4, len(mv))
        major, minor, patch, status = mv
        self.assertTrue(type(major) is int)
        self.assertTrue((major == 0 and minor > 0) or (major > 0 and minor >= 0))

    def test_DataString(self):
        d = pyfimex0.createData("hello")
        self.assertEqual(pyfimex0.CDMDataType.STRING, d.getDataType())
        self.assertEqual(5, d.size())
        self.assertEqual("hello", d.values())

        d = pyfimex0.createData(pyfimex0.CDMDataType.STRING, "values")
        self.assertEqual(pyfimex0.CDMDataType.STRING, d.getDataType())
        self.assertEqual(6, d.size())
        self.assertEqual("values", d.values())

    def test_DataFloat(self):
        d = pyfimex0.createData(pyfimex0.CDMDataType.FLOAT, [1, 2, 3])
        self.assertEqual(pyfimex0.CDMDataType.FLOAT, d.getDataType())
        self.assertEqual(3, d.size())
        self.assertEqual(numpy.float32, d.values().dtype)

    def test_DataChar(self):
        d = pyfimex0.createData(numpy.arange(5, dtype=numpy.int8))
        self.assertEqual(pyfimex0.CDMDataType.CHAR, d.getDataType())
        self.assertEqual(5, d.size())

    def test_DataUChar(self):
        d = pyfimex0.createData(numpy.arange(5, dtype=numpy.uint8))
        self.assertEqual(pyfimex0.CDMDataType.UCHAR, d.getDataType())

    def test_AttributeString(self):
        att = pyfimex0.CDMAttribute("name", "value")
        self.assertEqual("name", att.getName())
        self.assertEqual("value", att.getStringValue())
        self.assertEqual(pyfimex0.CDMDataType.STRING, att.getDataType())

        att.setName("navn")
        self.assertEqual("navn", att.getName())

        att.setData(pyfimex0.createData("content"))
        self.assertEqual("content", att.getStringValue())
        self.assertEqual(pyfimex0.CDMDataType.STRING, att.getDataType())

    def test_AttributeFloat(self):
        att = pyfimex0.CDMAttribute("f", pyfimex0.createData(pyfimex0.CDMDataType.FLOAT, [1]))
        self.assertEqual("f", att.getName())
        self.assertEqual("1", att.getStringValue())
        self.assertEqual([1.0], att.getData().values())
        self.assertEqual(pyfimex0.CDMDataType.FLOAT, att.getDataType())

    def test_AttributeUChar(self):
        l = [0, 12, 234]
        att = pyfimex0.CDMAttribute("u", pyfimex0.createData(pyfimex0.CDMDataType.UCHAR, l))
        self.assertEqual(" ".join([str(x) for x in l]), att.getStringValue())
        d = att.getData()
        self.assertEqual(len(l), d.size())
        v = d.values()
        self.assertEqual(numpy.uint8, v.dtype)
        self.assertEqual(l, list(v))

    def test_VariableFloat(self):
        shp = ['x', 'y', 'time']
        var = pyfimex0.CDMVariable("f", pyfimex0.CDMDataType.FLOAT, shp)
        self.assertEqual("f", var.getName())
        self.assertEqual(shp, var.getShape())

    def test_CDM_variables(self):
        cdm = pyfimex0.CDM()
        self.assertEqual(0, len(cdm.getVariableNames()))

        cdm.addVariable(pyfimex0.CDMVariable("varf", pyfimex0.CDMDataType.FLOAT, ['x', 'y']))
        self.assertEqual(['varf'], cdm.getVariableNames())

        cdm.addAttribute('varf', pyfimex0.CDMAttribute('_FillValue', pyfimex0.createData(pyfimex0.CDMDataType.FLOAT, -1)))
        self.assertEqual(1, len(cdm.getAttributeNames('varf')))

        cdm.removeVariable('varf')
        self.assertEqual(0, len(cdm.getVariableNames()))

if __name__ == '__main__':
    unittest.main()
