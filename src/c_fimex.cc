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
#include "fimex_config.h"
#include "fimex/CDM.h"
#include "boost/shared_ptr.hpp"
#include "boost/shared_array.hpp"
#include "fimex/CDMReader.h"
#include "fimex/CoordinateSystemSliceBuilder.h"
#include "fimex/XMLInput.h"
#include "fimex/CDMFileReaderFactory.h"
#ifdef HAVE_NETCDF_H
#include "fimex/NetCDF_CDMWriter.h"
#endif
#ifdef HAVE_GRIB_API_H
#include "fimex/GribApiCDMWriter.h"
#endif
#include "fimex/NcmlCDMReader.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/C_CDMReader.h"
#include "fimex/CDMReaderWriter.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/Logger.h"
#include "fimex/Data.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"

using namespace MetNoFimex;
using namespace std;

/**
 * This is the C-wrapper for the C-API to fimex.
 */

static LoggerPtr logger = getLogger("c_fimex");

static const char EMPTY_C_STR[] = "";

static int string_cpy(const std::string& src, char* dest, int n)
{
    char* d = dest;
    if (src.size()+1 <= n) {
        d = std::copy(src.begin(), src.end(), dest);
        *d++ = '\0';
    }
    std::fill(d, dest + n, '\0');
    return d - dest;
}

static char* string_dup(const std::string& src)
{
    return strdup(src.c_str());
}

void mifi_free_cdm_reader(mifi_cdm_reader* reader)
{
    delete(reader);
}

void mifi_free_slicebuilder(mifi_slicebuilder* sb)
{
    delete(sb);
}

mifi_cdm_reader* mifi_new_io_reader(int file_type, const char* filename, const char* configFile)
{
    try {
            CDMReader_p reader = CDMFileReaderFactory::create(file_type, std::string(filename), XMLInputFile(configFile));
            return new mifi_cdm_reader(reader);
        } catch (exception& ex) {
            LOG4FIMEX(logger, Logger::WARN, "error in reader: " << ex.what());
        }
        return 0;
}

mifi_cdm_reader* mifi_new_felt_reader(const char* filename, const char* configFile)
{
    return mifi_new_io_reader(MIFI_FILETYPE_FELT, filename, configFile);
}

mifi_cdm_reader* mifi_new_netcdf_reader(const char* filename)
{
    return mifi_new_io_reader(MIFI_FILETYPE_NETCDF, filename, "");
}

mifi_cdm_reader* mifi_new_grib_reader(const char* filename, const char* configFile)
{
    return mifi_new_io_reader(MIFI_FILETYPE_GRIB, filename, configFile);
}


mifi_cdm_reader* mifi_new_ncml_reader(const char* ncmlFile)
{
    return mifi_new_io_reader(MIFI_FILETYPE_NCML, ncmlFile, "");
}

mifi_cdm_reader* mifi_new_ncml_modifier(mifi_cdm_reader* reader, const char* ncmlFile)
{
    try {
        boost::shared_ptr<NcmlCDMReader> ncml_reader(new NcmlCDMReader(reader->reader_, XMLInputFile(ncmlFile)));
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
        NetCDF_CDMWriter(reader->reader_, file, config, version);
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
        GribApiCDMWriter(reader->reader_, file, version, config);
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
        Null_CDMWriter(reader->reader_,"");
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in null-cdmwriter: " << ex.what());
    }
    return -1;
}

mifi_cdm_reader* mifi_new_cdminterpolator(mifi_cdm_reader* reader, int method, const char* proj_input, const char* out_x_axis, const char* out_y_axis, const char* out_x_axis_unit, const char* out_y_axis_unit)
{
    try {
        boost::shared_ptr<CDMInterpolator> interpol(new CDMInterpolator(reader->reader_));
        interpol->changeProjection(method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit);
        return new mifi_cdm_reader(interpol);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_cdminterpolator: " << ex.what());
    }
    return 0;
}

mifi_cdm_reader* mifi_new_lonlat_interpolator(mifi_cdm_reader* reader, int method, int n, const double* lonVals, const double* latVals)
{
    try {
        boost::shared_ptr<CDMInterpolator> interpol(new CDMInterpolator(reader->reader_));
        std::vector<double> lons(lonVals, lonVals+n);
        std::vector<double> lats(latVals, latVals+n);
        interpol->changeProjection(method, lons, lats);
        return new mifi_cdm_reader(interpol);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_cdminterpolator: " << ex.what());
    }
    return 0;
}



mifi_cdm_reader* mifi_new_c_reader(mifi_cdm_reader* reader)
{
    try {
        boost::shared_ptr<C_CDMReader> c_reader(new C_CDMReader(reader->reader_));
        return new mifi_cdm_reader(c_reader);
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_new_c_reader: " << ex.what());
    }
    return 0;
}

int mifi_set_callback_double(mifi_cdm_reader* c_reader, const char* varName, doubleDatasliceCallbackPtr callback)
{
    try {
        CDMReader_p reader = c_reader->reader_;
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
        return reader->reader_->getCDM().getVariables().size();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_variable_number: " << ex.what());
    }
    return 0;
}

const char* mifi_get_variable_name(mifi_cdm_reader* reader, size_t pos)
{
    try {
        // c_str from a member in CDMVariable, no strdup needed
        return reader->reader_->getCDM().getVariables().at(pos).getName().c_str();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_variable_name: " << ex.what());
    }
    return 0;
}

unsigned int mifi_get_variable_type(mifi_cdm_reader* reader, const char* varName)
{
    try {
        return reader->reader_->getCDM().getVariable(varName).getDataType();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_variable_type: " << ex.what());
    }
    return CDM_NAT;
}


size_t mifi_get_dimension_number(mifi_cdm_reader* reader)
{
    try {
        return reader->reader_->getCDM().getDimensions().size();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_dimension_number: " << ex.what());
    }
    return 0;
}

const char* mifi_get_dimension_name(mifi_cdm_reader* reader, size_t pos)
{
    try {
        // c_str from a member in CDMDimension, no strdup needed
        return reader->reader_->getCDM().getDimensions().at(pos).getName().c_str();
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_dimension_name: " << ex.what());
    }
    return 0;
}

size_t mifi_get_dimension_size(mifi_cdm_reader* reader, const char* dimName)
{
    if (reader->reader_->getCDM().hasDimension(dimName)) {
        try {
            return reader->reader_->getCDM().getDimension(dimName).getLength();
        } catch (exception& ex) {
            LOG4FIMEX(logger, Logger::WARN, "error in mifi_get_dimension_size: " << ex.what());
        }
    }
    return 0;
}

const char* mifi_get_unlimited_dimension_name(mifi_cdm_reader* reader)
{
    // c_str from a member in CDMDimension, or const char*, no strdup needed
    const CDMDimension* udim = reader->reader_->getCDM().getUnlimitedDim();
    if (udim != 0) {
        return udim->getName().c_str();
    }
    return EMPTY_C_STR;
}

static std::string do_mifi_get_var_longitude(mifi_cdm_reader* reader, const char* varName)
{
    std::string lon,lat;
    if (reader->reader_->getCDM().getLatitudeLongitude(std::string(varName), lat, lon)) {
        return lon;
    }
    return "";
}

char* mifi_get_var_longitude(mifi_cdm_reader* reader, const char* varName)
{
    // we get a temporary, we need strdup and caller must call free
    return string_dup(do_mifi_get_var_longitude(reader, varName));
}

int mifi_get_var_longitude_cpy(mifi_cdm_reader* reader, const char* varName, char* lonName, int n)
{
    return string_cpy(do_mifi_get_var_longitude(reader, varName), lonName, n);
}

static std::string do_mifi_get_var_latitude(mifi_cdm_reader* reader, const char* varName)
{
    std::string lon,lat;
    if (reader->reader_->getCDM().getLatitudeLongitude(std::string(varName), lat, lon)) {
        return lat;
    }
    return "";
}

char* mifi_get_var_latitude(mifi_cdm_reader* reader, const char* varName)
{
    // we get a temporary, we need strdup and caller must call free
    return string_dup(do_mifi_get_var_latitude(reader, varName));
}

int mifi_get_var_latitude_cpy(mifi_cdm_reader* reader, const char* varName, char* latName, int n)
{
    return string_cpy(do_mifi_get_var_latitude(reader, varName), latName, n);
}

mifi_slicebuilder* mifi_new_slicebuilder(mifi_cdm_reader* reader, const char* varName)
{
    try {
        boost::shared_ptr<const CoordinateSystem> cs = findCompleteCoordinateSystemFor(reader->csVec_, varName);
        if (cs.get()) {
            boost::shared_ptr<SliceBuilder> sb(new CoordinateSystemSliceBuilder(reader->reader_->getCDM(), cs));
            return new mifi_slicebuilder(sb, cs);
        } else {
            boost::shared_ptr<SliceBuilder> sb(new SliceBuilder(reader->reader_->getCDM(), varName));
            return new mifi_slicebuilder(sb);
        }
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error getting slicebuilder for " << varName << ": " << ex.what());
    }
    return 0;
}

int mifi_slicebuilder_has_CS(mifi_slicebuilder* sb)
{
    if (sb != 0) {
        if (sb->cs_.get() != 0) {
            return 1;
        }
    }
    return 0;
}

int mifi_slicebuilder_ndims(mifi_slicebuilder* sb)
{
    if (sb != 0) {
        return sb->sb_->getDimensionSizes().size();
    }
    return 0;
}

static std::string do_mifi_slicebuilder_dimname(mifi_slicebuilder* sb, int pos)
{
    if (sb != 0) {
        return sb->sb_->getDimensionNames().at(pos);
    }
    return "";
}

const char* mifi_slicebuilder_dimname(mifi_slicebuilder* sb, int pos)
{
    // we get a temporary, we need strdup and caller must call free
    return string_dup(do_mifi_slicebuilder_dimname(sb, pos));
}

int mifi_slicebuilder_dimname_cpy(mifi_slicebuilder* sb, int pos, char* dimName, int n)
{
    return string_cpy(do_mifi_slicebuilder_dimname(sb, pos), dimName, n);
}

int mifi_slicebuilder_get_start_size(mifi_slicebuilder* sb, unsigned int* start, unsigned int* size)
{
    if (sb != 0) {
        const vector<size_t>& startV = sb->sb_->getDimensionStartPositions();
        copy(startV.begin(), startV.end(), start);
        const vector<size_t>& sizeV = sb->sb_->getDimensionSizes();
        copy(sizeV.begin(), sizeV.end(), size);
        return 0;
    }
    return -1;
}

int mifi_slicebuilder_get_axistype(mifi_slicebuilder* sb, int* axistype)
{
    if (mifi_slicebuilder_has_CS(sb) == 1) {
        CoordinateSystemSliceBuilder* csb = dynamic_cast<CoordinateSystemSliceBuilder*>(sb->sb_.get());
        assert(csb != 0);
        try {
            vector<CoordinateAxis::AxisType> axes = csb->getAxisTypes();
            transform(axes.begin(), axes.end(), axistype, CoordinateAxis::type2int);
            return 0;
        } catch (CDMException& ex) {
            return -2;
        }
    }
    return -1;
}

static std::string do_mifi_slicebuilder_get_proj4(mifi_slicebuilder* sb)
{
    string retVal = "";
    if (mifi_slicebuilder_has_CS(sb) == 1) {
        CoordinateSystemSliceBuilder* csb = dynamic_cast<CoordinateSystemSliceBuilder*>(sb->sb_.get());
        assert(csb != 0);
        boost::shared_ptr<const Projection> pr = csb->getCoordinateSystem()->getProjection();
        if (pr.get() != 0) {
            retVal = pr->getProj4String();
        }
    }
    return retVal;
}

const char* mifi_slicebuilder_get_proj4(mifi_slicebuilder* sb)
{
    // we get a temporary, we need strdup and caller must call free
    return string_dup(do_mifi_slicebuilder_get_proj4(sb));
}

int mifi_slicebuilder_get_proj4_cpy(mifi_slicebuilder* sb, char* proj4, int n)
{
    return string_cpy(do_mifi_slicebuilder_get_proj4(sb), proj4, n);
}

int mifi_slicebuilder_set_dim_start_size(mifi_slicebuilder* sb, const char* dimName, unsigned int start, unsigned int size)
{
    if (sb != 0) {
        sb->sb_->setStartAndSize(dimName, start, size);
        return 0;
    }
    return -1;
}



int mifi_get_double_dataslice(mifi_cdm_reader* reader, const char* varName, size_t unLimDimPos, double** data, size_t* size)
{
    try {
        DataPtr vData = reader->reader_->getScaledDataSlice(varName, unLimDimPos);
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
        DataPtr vData = reader->reader_->getScaledData(varName);
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

int mifi_fill_scaled_double_dataslice(mifi_cdm_reader* reader, const char* varName, mifi_slicebuilder* sb, const char* units, double* data, size_t* size)
{
    if (data == 0) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_fill_scaled_double_dataslice: data not pre-allocated");
        *size = 0;
        return -1;
    }
    try {
        DataPtr vData;
        string unitStr(units);
        if (unitStr != "") {
            vData = reader->reader_->getScaledDataSliceInUnit(varName, unitStr, *(sb->sb_));
        } else {
            vData = reader->reader_->getScaledDataSlice(varName, *(sb->sb_));
        }
        *size = vData->size();
        boost::shared_array<double> vArray = vData->asDouble();
        copy(&vArray[0], &vArray[*size], data);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_fill_scaled_double_dataslice: " << ex.what());
    }
    return -1;
}

int mifi_write_scaled_double_dataslice(mifi_cdm_reader* rwreader, const char* varName, mifi_slicebuilder* sb, const char* units, double* data, size_t size)
{
    if (size == 0) return 0;
    if (data == 0) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_fill_scaled_double_dataslice: data not pre-allocated");
        size = 0;
        return -99;
    }
    CDMReaderWriter* rw = dynamic_cast<CDMReaderWriter*>(rwreader->reader_.get());
    if (rw == 0) {
        return -98; // not a readerwriter
    }
    try {
        boost::shared_array<double> dptr(data, null_deleter());
        DataPtr d = createData(size, dptr);
        rw->putScaledDataSliceInUnit(varName, units, *(sb->sb_), d);
        return 0;
    } catch (exception& ex) {
        LOG4FIMEX(logger, Logger::WARN, "error in mifi_write_scaled_double_dataslice: " << ex.what());
    }
    return -1;
}

double mifi_get_unique_forecast_reference_time(mifi_cdm_reader* reader, const char* units)
{
    double retVal;
    try {
        TimeUnit tu(units);
        retVal = tu.posixTime2unitTime(getUniqueForecastReferenceTime(reader->reader_));
    } catch (CDMException& ex) {
        return MIFI_UNDEFINED_D;
    }
    return retVal;
}


