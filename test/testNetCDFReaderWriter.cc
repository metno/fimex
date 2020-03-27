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

#include "testinghelpers.h"

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReaderWriter.h"
#include "fimex/Data.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_update)
{
    const string fileName("test_update.nc");
    copyFile(pathTest("test_merge_inner.nc"), fileName);

    const double diff = 10.0, scale = 1.2;
    DataPtr read1, read2;
    {
        CDMReaderWriter_p rw = CDMFileReaderFactory::createReaderWriter("netcdf", fileName);
        TEST4FIMEX_CHECK(rw);

        read1 = rw->getDataSlice("ga_2t_1", 0);
        TEST4FIMEX_CHECK(read1);

        DataPtr write1 = read1->clone();
        const size_t size1 = read1->size();
        for(size_t i=0; i<size1; ++i)
            write1->setValue(i, diff + scale * read1->getDouble(i));

        rw->putDataSlice("ga_2t_1", 0, write1);
    }
    {
        CDMReader_p r = CDMFileReaderFactory::create("netcdf", fileName);
        TEST4FIMEX_CHECK(r);

        read2 = r->getDataSlice("ga_2t_1", 0);
        TEST4FIMEX_CHECK(read2);

        const size_t size2 = read2->size();
        TEST4FIMEX_CHECK_EQ(size2, read1->size());
        for(size_t i=0; i<size2; ++i) {
            const double actual = read2->getDouble(i);
            const double expect = diff + scale*read1->getDouble(i);
            TEST4FIMEX_CHECK(abs(actual - expect) < 1e-3);
        }
        remove(fileName);
    }
}

TEST4FIMEX_TEST_CASE(test_scaled)
{
    const string fileName("test_scaled.nc");
    copyFile(pathTest("test_merge_inner.nc"), fileName);

    const double addF = 1.0, addK = addF * 5.0/9.0;
    DataPtr read1, read2;
    {
        CDMReaderWriter_p rw = CDMFileReaderFactory::createReaderWriter("netcdf", fileName);
        TEST4FIMEX_CHECK(rw);

        read1 = rw->getScaledDataSlice("ga_2t_1", 0);
        TEST4FIMEX_CHECK(read1);

        DataPtr write1 = rw->getScaledDataSliceInUnit("ga_2t_1", "deg_F", 0);
        TEST4FIMEX_CHECK(write1);

        const size_t size1 = write1->size();
        for(size_t i=0; i<size1; ++i) {
            const double newF = write1->getDouble(i) + addF;
            write1->setValue(i, newF);
        }

        rw->putScaledDataSliceInUnit("ga_2t_1", "deg_F", 0, write1);
    }
    {
        CDMReader_p r = CDMFileReaderFactory::create("netcdf", fileName);
        TEST4FIMEX_CHECK(r);

        read2 = r->getScaledDataSlice("ga_2t_1", 0);
        TEST4FIMEX_CHECK(read2);

        const size_t size2 = read2->size();
        TEST4FIMEX_CHECK_EQ(size2, read1->size());
        for(size_t i=0; i<size2; ++i) {
            const double actual = read2->getDouble(i);
            const double expect = addK + read1->getDouble(i);
            TEST4FIMEX_CHECK(abs(actual - expect) < 1e-3);
        }
        remove(fileName);
    }
}
