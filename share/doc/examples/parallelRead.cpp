/*
 * Fimex, parallelRead.cpp
 *
 * (C) Copyright 2015-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 *  Created on: Jan 7, 2015
 *      Author: heikok
 */


#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/SliceBuilder.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include <boost/shared_array.hpp>
#include "fimex/Data.h"

#include <boost/interprocess/anonymous_shared_memory.hpp>
//#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <unistd.h>
#include <wait.h>

#include <numeric>
#include <functional>

using namespace MetNoFimex;
using namespace std;

DataPtr getParallelScaledDataSliceInUnit(size_t maxProcs, std::shared_ptr<CDMReader> reader, const string& parName, const string& parUnit,
                                         const vector<SliceBuilder>& slices)
{
    vector<size_t> sliceLengths(slices.size(), 1);
    for (int i = 0; i < slices.size(); i++) {
        vector<size_t> ssize = slices.at(i).getDimensionSizes();
        sliceLengths.at(i) = accumulate(ssize.begin(), ssize.end(), 1, std::multiplies<int>());
    }
    size_t total = accumulate(sliceLengths.begin(), sliceLengths.end(), 0);

    //create a anonymous mapped shm-obj in this process
    boost::interprocess::mapped_region region(boost::interprocess::anonymous_shared_memory(total*sizeof(float)));

    // fork the sub-processes
    pid_t pid;
    vector<pid_t> children;
    for (size_t i = 0; i < maxProcs; i++) {
        pid = fork();
        if(pid < 0) {
            printf("Error forking");
            exit(1);
        } else if (pid == 0) {
            assert(region.get_size() == (total*sizeof(float)));
            float* regionFloat = reinterpret_cast<float*>(region.get_address());
            size_t startPos = 0;
            for (size_t j = 0; j < slices.size(); j++) {
                if ((j % maxProcs) == i) {
                    DataPtr data;
                    try {
                        data = reader->getScaledDataSliceInUnit(parName, parUnit, slices.at(j));
                    } catch (runtime_error& ex) {
                        cerr << "error fetching data on '" << parName << "', '" << parUnit << "' slice " << j << ": " << ex.what() << endl;
                        data = createData(CDM_FLOAT, 0);
                    }
                    boost::shared_array<float> array;
                    if (data->size() == 0) {
                        array = boost::shared_array<float>(new float[sliceLengths.at(j)]);
                        for (size_t k = 0; k < sliceLengths.at(j); k++) array[k] = MIFI_UNDEFINED_F;
                    } else {
                        assert(data->size() == sliceLengths.at(j));
                        array = data->asFloat();
                    }
                    std::copy(array.get(), array.get()+sliceLengths.at(j), regionFloat + startPos);
                }
                startPos += sliceLengths.at(j);
            }
            // use _exit, to be sure that no cleanup-code is called for the child
            _exit(0); // child
        } else  {
            // parent, handled below, should fork more
            children.push_back(pid);
        }
    }

    // parent code
    // wait for all children
    for (int i = 0; i < maxProcs; ++i) {
        int status;
        while (-1 == waitpid(children.at(i), &status, 0));
        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            std::cerr << "Process " << i << " (pid " << children.at(i) << ") failed" << std::endl;
            throw runtime_error("child-process did not finish correctly when fetching data");
            exit(1);
        }
    }
    // read the shared_memory
    boost::shared_array<float> allFloats(new float[total]);
    assert(region.get_size() == (total*sizeof(float)));
    float* regionFloat = reinterpret_cast<float*>(region.get_address());
    std::copy(regionFloat, regionFloat+total, allFloats.get());
    return createData(total, allFloats);
}


int main(int argc, char* args[]) {
    std::shared_ptr<CDMReader> reader = CDMFileReaderFactory::create("netcdf", "/opdata/arome2_5_main/AROME_MetCoOp_00_fp.nc");
    const CDM& cdm = reader->getCDM();

    string parName = "air_temperature_2m";
    string parUnit = "degC";
    SliceBuilder sb(reader->getCDM(), parName);
    // select a cell
    sb.setStartAndSize("x", 1, 500);
    sb.setStartAndSize("y", 1, 500);

    size_t maxSize = cdm.getUnlimitedDim()->getLength();
    vector<SliceBuilder> slices;
    for (size_t i = 0; i < maxSize; i++) {
        sb.setStartAndSize("time", i, 1);
        slices.push_back(sb); // copy of current slice
    }
    size_t maxProcs = 0;
    if (argc > 1) {
        maxProcs = string2type<size_t>(args[1]);
    }
    if (maxProcs <= 0) {
#ifdef _SC_NPROCESSORS_ONLN
        maxProcs = sysconf( _SC_NPROCESSORS_ONLN );
        // better, but needs c++11: std::thread::hardware_concurrency();
#endif
        if (maxProcs == 0) maxProcs = 1; // cannot detect
    }
    printf("maxProcs = %d\n", maxProcs);
    DataPtr data;
    //for (size_t i = 0; i < 1000; i++) { // stress-test
        data = getParallelScaledDataSliceInUnit(maxProcs, reader, parName, parUnit, slices);
    //}
    boost::shared_array<float> arrayP = data->asFloat();

    for (size_t i = 0; i < data->size(); i++) {
        //cout << "i: " << arrayP[i] << endl;
    }
}

