/*
 * Fimex, testFileReaderFactory.cc
 *
 * (C) Copyright 2010, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: May 7, 2010
 *      Author: Heiko Klein
 */

#include "testinghelpers.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"
#ifdef HAVE_NETCDF_H
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NetCDF_CDMReader.h"
#endif

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_CDMconstants)
{
    string nc(mifi_get_filetype_name(MIFI_FILETYPE_NETCDF));
    TEST4FIMEX_CHECK_EQ(nc, "netcdf");
    int nc_id = mifi_get_filetype(nc.c_str());
    TEST4FIMEX_CHECK_EQ(nc_id, MIFI_FILETYPE_NETCDF);
#ifdef HAVE_NETCDF_H
    TEST4FIMEX_CHECK(fimexHas(MIFI_FILETYPE_NETCDF));
#else
    TEST4FIMEX_CHECK_EQ(fimexHas(MIFI_FILETYPE_NETCDF), 0);
#endif
}

TEST4FIMEX_TEST_CASE(test_fileDetection)
{
    const string fileName = pathTest("coordTest.nc");
    TEST4FIMEX_CHECK_EQ(CDMFileReaderFactory::detectFileType(fileName), MIFI_FILETYPE_NETCDF);
    if (hasTestExtra()) {
        const std::string feltFile = pathTestExtra("flth00.dat");
        TEST4FIMEX_CHECK_EQ(CDMFileReaderFactory::detectFileType(feltFile), MIFI_FILETYPE_FELT);
    }
    if (fimexHas(MIFI_FILETYPE_NETCDF)) {
        CDMReader_p reader = CDMFileReaderFactory::create("netcdf", fileName);
        TEST4FIMEX_CHECK(reader);
#ifdef HAVE_NETCDF_H
        TEST4FIMEX_CHECK(std::dynamic_pointer_cast<NetCDF_CDMReader>(reader));
#endif
    }
}
