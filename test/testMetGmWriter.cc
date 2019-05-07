/*
 * Fimex, testMetGmWriter.cc
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Jun 13, 2011
 *      Author: Aleksandar Babic
 */

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Logger.h"

#include <memory>
#include <vector>

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_write_hirlam12_nc)
{
    const string ncSource = pathTest("hirlam12.nc");

    defaultLogLevel(Logger::INFO);

    // create reader that reads ncSource
    CDMReader_p netcdfReader = CDMFileReaderFactory::create("netcdf", ncSource);

    // use the metgm writer to create mgm file based on nc source
    const std::string mgmFile("hirlam12.mgm");
    MetNoFimex::createWriter(netcdfReader, "metgm", mgmFile, pathShareEtc("cdmMetGmWriterConfig.xml"));

    // create metgm reader to read newly writen mgm file
    CDMReader_p mgmReader = CDMFileReaderFactory::create("metgm", mgmFile, pathShareEtc("cdmMetGmReaderConfig.xml"));

    // from metgm reader write again to nc file and compare:
    // hirlam12.nc and hirlam12.mgm.nc
    const std::string newNcFile("hirlam12.mgm.nc");
    MetNoFimex::createWriter(mgmReader, "null", newNcFile);
}
