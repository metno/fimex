/*
 * Fimex, c_fimex.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Oct 20, 2009
 *      Author: Heiko Klein
 */

#include <cstdlib>
#include <algorithm>
#include <vector>
#include "fimex/c_fimex.h"
#include "fimex/mifi_cdm_reader.h"
#include "../config.h"
#include "fimex/CDM.h"
#include "boost/shared_ptr.hpp"
#include "boost/shared_array.hpp"
#include "fimex/CDMReader.h"
#include "fimex/FeltCDMReader2.h"
#include "fimex/XMLInput.h"
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NetCDF_CDMReader.h"
#endif
#ifdef HAVE_GRIB_API_H
#include "fimex/GribApiCDMWriter.h"
#include "fimex/GribCDMReader.h"
#endif
#include "fimex/NcmlCDMReader.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/C_CDMReader.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/Logger.h"
#include "fimex/Data.h"

using namespace MetNoFimex;
using namespace std;

/**
 * This is the C-wrapper for the C-API to fimex.
 */

static LoggerPtr logger = getLogger("c_fimex");

void mifi_free_cdm_reader(mifi_cdm_reader* reader)
{
    delete(reader);
}

mifi_cdm_reader* mifi_new_felt_reader(const char* filename, const char* configFile)
{
    try {
        boost::shared_ptr<FeltCDMReader2> reader(new FeltCDMReader2(filename, configFile));
        return new mifi_cdm_reader(reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in felt-reader: " << ex.what());
    }
    return 0;
}

#ifdef HAVE_NETCDF_H
mifi_cdm_reader* mifi_new_netcdf_reader(const char* filename)
{
    try {
        boost::shared_ptr<NetCDF_CDMReader> reader(new NetCDF_CDMReader(filename));
        return new mifi_cdm_reader(reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_netcdf_reader: " << ex.what());
    }
    return 0;
}
#endif

#ifdef HAVE_GRIB_API_H
mifi_cdm_reader* mifi_new_grib_reader(const char* filename, const char* configFile)
{
    try {
        std::vector<string> files;
        files.push_back(filename);
        boost::shared_ptr<GribCDMReader> reader(new GribCDMReader(files, XMLInputFile(configFile)));
        return new mifi_cdm_reader(reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_grib_reader: " << ex.what());
    }
    return 0;
}
#endif


mifi_cdm_reader* mifi_new_ncml_reader(const char* ncmlFile)
{
    try {
        boost::shared_ptr<NcmlCDMReader> reader(new NcmlCDMReader(XMLInputFile(ncmlFile)));
        return new mifi_cdm_reader(reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in ncmlCDMReader: " << ex.what());
    }
    return 0;
}

mifi_cdm_reader* mifi_new_ncml_modifier(mifi_cdm_reader* reader, const char* ncmlFile)
{
    try {
        boost::shared_ptr<NcmlCDMReader> ncml_reader(new NcmlCDMReader(reader->get(), XMLInputFile(ncmlFile)));
        return new mifi_cdm_reader(ncml_reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in ncml_modifier: " << ex.what());
    }
    return 0;
}


#ifdef HAVE_NETCDF_H
int mifi_netcdf_writer(mifi_cdm_reader* reader, const char* filename, const char* configFile, int version)
{
    try {
        string file = (filename == 0) ? "" : filename;
        string config = (configFile == 0) ? "" : configFile;
        NetCDF_CDMWriter(reader->get(), file, config, version);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in netcdf-cdmwriter: " << ex.what());
    }
    return -1;
}
#endif

#ifdef HAVE_GRIB_API_H
int mifi_grib_writer(mifi_cdm_reader* reader, const char* filename, const char* configFile, int version)
{
    try {
        string file = (filename == 0) ? "" : filename;
        string config = (configFile == 0) ? "" : configFile;
        GribApiCDMWriter(reader->get(), file, version, config);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_grib_writer: " << ex.what());
    }
    return -1;
}
#endif

int mifi_nullcdm_writer(mifi_cdm_reader* reader)
{
    try {
        Null_CDMWriter(reader->get(),"");
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in null-cdmwriter: " << ex.what());
    }
    return -1;
}

mifi_cdm_reader* mifi_new_cdminterpolator(mifi_cdm_reader* reader, int method, const char* proj_input, const char* out_x_axis, const char* out_y_axis, const char* out_x_axis_unit, const char* out_y_axis_unit)
{
    try {
        boost::shared_ptr<CDMInterpolator> interpol(new CDMInterpolator(reader->get()));
        interpol->changeProjection(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit);
        return new mifi_cdm_reader(interpol);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_cdminterpolator: " << ex.what());
    }
    return 0;
}


mifi_cdm_reader* mifi_new_c_reader(mifi_cdm_reader* reader)
{
    try {
        boost::shared_ptr<C_CDMReader> c_reader(new C_CDMReader(reader->get()));
        return new mifi_cdm_reader(c_reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_c_reader: " << ex.what());
    }
    return 0;
}

int mifi_set_callback_double(mifi_cdm_reader* c_reader, const char* varName, doubleDatasliceCallbackPtr callback)
{
    try {
        boost::shared_ptr<CDMReader> reader = c_reader->get();
        C_CDMReader* cReaderPtr = dynamic_cast<C_CDMReader*>(reader.get());
        cReaderPtr->setDoubleCallbackFunction(string(varName), callback);
        return 0;
    } catch (std::bad_cast& bc) {
        LOG4FIMEX(logger, Logger::WARN, "mifi_set_callback_double called with a different CDMReader, not C_CDMReader: " + string(bc.what()));
        return -2;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_c_reader: " << ex.what());
    }
    return -1;
}


size_t mifi_get_variable_number(mifi_cdm_reader* reader)
{
    try {
        return reader->get()->getCDM().getVariables().size();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_variable_number: " << ex.what());
    }
    return 0;
}

const char* mifi_get_variable_name(mifi_cdm_reader* reader, size_t pos)
{
    try {
        return reader->get()->getCDM().getVariables().at(pos).getName().c_str();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_variable_name: " << ex.what());
    }
    return 0;
}

int mifi_get_double_dataslize(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double** data, size_t* size)
{
    try {
        boost::shared_ptr<Data> vData = reader->get()->getScaledDataSlice(varName, unLimDimPos);
        *size = vData->size();
        *data = (double*) malloc(*size * sizeof(double));
        if (*data == 0) {
            LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_double_dataslize: Cannot allocate data");
            *size = 0;
            return -1;
        }
        boost::shared_array<double> vArray = vData->asDouble();
        copy(&vArray[0], &vArray[*size], *data);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_double_dataslize: " << ex.what());
    }
    return -1;
}

int mifi_get_double_data(mifi_cdm_reader* reader, const char* varName, double** data, size_t* size)
{
    try {
        boost::shared_ptr<Data> vData = reader->get()->getScaledData(varName);
        *size = vData->size();
        *data = (double*) malloc(*size * sizeof(double));
        if (*data == 0) {
            LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_double_dataslize: Cannot allocate data");
            *size = 0;
            return -1;
        }
        boost::shared_array<double> vArray = vData->asDouble();
        copy(&vArray[0], &vArray[*size], *data);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_double_dataslize: " << ex.what());
    }
    return -1;
}


