/*
 * Fimex, testNetCDFReaderWriter.cc
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Feb 19, 2013
 *      Author: Alexander Bürger
 */

#include "../config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <fstream>
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/Utils.h"
#ifdef HAVE_NETCDF_H
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NetCDF_CDMReader.h"
#endif
using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_update )
{
    if (not fimexHas(MIFI_FILETYPE_NETCDF)) {
        // no netcdf support, skip test
        return;
    }

    const string fileName("test_netcdfrw.nc");
    {
        const string topSrcDir(TOP_SRCDIR);
        const string origFileName(topSrcDir+"/test/test_merge_inner.nc");
        ifstream orig(origFileName.c_str());
        BOOST_CHECK(orig);
        ofstream copy(fileName.c_str());
        copy << orig.rdbuf();
        BOOST_CHECK(copy);
    }

    const double diff = 10.0, scale = 1.2;
    DataPtr read1, read2;
    {
        boost::shared_ptr<CDMReaderWriter> rw = boost::shared_ptr<CDMReaderWriter>(new NetCDF_CDMReader(fileName, true));
        BOOST_CHECK(rw.get() != 0);

        read1 = rw->getDataSlice("ga_2t_1", 0);
        BOOST_CHECK(read1.get() != 0);

        DataPtr write1 = read1->clone();
        const size_t size1 = read1->size();
        for(size_t i=0; i<size1; ++i)
            write1->setValue(i, diff + scale * read1->getDouble(i));

        rw->putDataSlice("ga_2t_1", 0, write1);
    }
    {
        boost::shared_ptr<CDMReader> r = boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(fileName));
        BOOST_CHECK(r.get() != 0);

        read2 = r->getDataSlice("ga_2t_1", 0);
        BOOST_CHECK(read2.get() != 0);

        const size_t size2 = read2->size();
        BOOST_CHECK(size2 == read1->size());
        for(size_t i=0; i<size2; ++i) {
            const double actual = read2->getDouble(i);
            const double expect = diff + scale*read1->getDouble(i);
            BOOST_CHECK(abs(actual - expect) < 1e-3);
        }
    }
}

BOOST_AUTO_TEST_CASE( test_scaled )
{
    if (not fimexHas(MIFI_FILETYPE_NETCDF)) {
        // no netcdf support, skip test
        return;
    }

    const string fileName("test_netcdfrw.nc");
    {
        const string topSrcDir(TOP_SRCDIR);
        const string origFileName(topSrcDir+"/test/test_merge_inner.nc");
        ifstream orig(origFileName.c_str());
        BOOST_CHECK(orig);
        ofstream copy(fileName.c_str());
        copy << orig.rdbuf();
        BOOST_CHECK(copy);
    }

    const double addF = 1.0, addK = addF * 5.0/9.0;
    DataPtr read1, read2;
    {
        boost::shared_ptr<CDMReaderWriter> rw = boost::shared_ptr<CDMReaderWriter>(new NetCDF_CDMReader(fileName, true));
        BOOST_CHECK(rw.get() != 0);

        read1 = rw->getScaledDataSlice("ga_2t_1", 0);
        BOOST_CHECK(read1.get() != 0);

        DataPtr write1 = rw->getScaledDataSliceInUnit("ga_2t_1", "deg_F", 0);
        BOOST_CHECK(write1.get() != 0);

        const size_t size1 = write1->size();
        for(size_t i=0; i<size1; ++i) {
            const double newF = write1->getDouble(i) + addF;
            write1->setValue(i, newF);
        }

        rw->putScaledDataSliceInUnit("ga_2t_1", "deg_F", 0, write1);
    }
    {
        boost::shared_ptr<CDMReader> r = boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(fileName));
        BOOST_CHECK(r.get() != 0);

        read2 = r->getScaledDataSlice("ga_2t_1", 0);
        BOOST_CHECK(read2.get() != 0);

        const size_t size2 = read2->size();
        BOOST_CHECK(size2 == read1->size());
        for(size_t i=0; i<size2; ++i) {
            const double actual = read2->getDouble(i);
            const double expect = addK + read1->getDouble(i);
            BOOST_CHECK(abs(actual - expect) < 1e-3);
        }
    }
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif

