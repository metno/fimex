/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/WdbCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED

#include <sstream>
#include <ctime>
#include <iterator>

#include <boost/foreach.hpp>
#include <boost/assign/list_of.hpp>

#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "MutexLock.h"
#include "wdb/Wdb2CdmBuilder.h"
#include "wdb/WdbIndex.h"
#include "wdb/config/GlobalWdbConfiguration.h"
#include "wdb/database_access/WdbConnection.h"
#include "wdb/gridInformation/GridInformation.h"
#include "wdb/config/WdbConfiguration.h"


namespace MetNoFimex
{
static LoggerPtr logger = getLogger("fimex.WdbCDMReader");

// mutex for the get*Data*
static MutexType wdbmutex;

/**
 * All data members of WdbCDMReader are kept "hidden" in this class, in
 * order to avoid header file dependencies on anything other than
 * WdbCDMReader
 */
class WdbCDMReader::InternalData
{
public:
    InternalData(const std::string& source, const XMLInput& configXML, CDM & cdm) :
        wdbConnection(0), dataIndex(0), translator(0)
    {
        try
        {
            wdb::WdbConfiguration configuration(source);

            translator = new wdb::GlobalWdbConfiguration(configXML);

            wdbConnection = new wdb::WdbConnection(configuration.pqDatabaseConnectString(), configuration.wciUser());

            std::vector<wdb::GridData> data;
            wdbConnection->readGid(data, configuration.query());

            dataIndex = new wdb::Wdb2CdmBuilder(data, * translator);
            dataIndex->populate(cdm);
        }
        catch (...)
        {
            delete dataIndex;
            delete wdbConnection;
            delete translator;
            throw;
        }
    }

    ~InternalData()
    {
        delete dataIndex;
        delete wdbConnection;
        delete translator;
    }

    wdb::WdbConnection * wdbConnection;
    wdb::Wdb2CdmBuilder * dataIndex;
    wdb::GlobalWdbConfiguration * translator;
};


WdbCDMReader::WdbCDMReader(const std::string& source, const XMLInput& configXML)
{
    d_ = new InternalData(source, configXML, * cdm_);
}

WdbCDMReader::WdbCDMReader(const std::string& source, const std::string & configFile )
{
    d_ = new InternalData(source, XMLInputFile(configFile), * cdm_);
}

WdbCDMReader::~WdbCDMReader()
{
    delete d_;
}

DataPtr WdbCDMReader::getDataSlice(
        const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(logger, Logger::DEBUG, __func__ << "(\"" << varName << "\", " << unLimDimPos << ")");

    const CDMVariable& variable = cdm_->getVariable(varName);

    ScopedCritical lock(wdbmutex);
    if ( d_->dataIndex->isDatabaseField(varName) )
        return getDatabaseFields(variable, unLimDimPos);
    else
    {
        BOOST_FOREACH(const wdb::DataHandler::Ptr & handler, d_->dataIndex->dataHandlers())
            if ( handler->canHandle(varName) )
                return handler->getData(variable, unLimDimPos);
    }
    throw CDMException("internal error: " + varName + ": unrecognized variable");
}

DataPtr WdbCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    LOG4FIMEX(logger, Logger::DEBUG, __func__ << "(\"" << varName << "\", <slice>)");

    const CDMVariable& variable = cdm_->getVariable(varName);

    DataPtr data;
    if ( d_->dataIndex->isDatabaseField(varName) ) {
        ScopedCritical lock(wdbmutex);
        data = getDatabaseFields(variable, sb);
    } else {
        data = CDMReader::getDataSlice(varName, sb);
    }
    return data;
}

std::size_t WdbCDMReader::getXSize(const CDMVariable& variable) const
{
    const std::vector<std::string> & dimensions = variable.getShape();
    const std::string & xName = dimensions[0];
    CDMAttribute axis;
    if ( cdm_->getAttribute(xName, "axis", axis) )
        if ( axis.getStringValue() != "X" )
            throw CDMException("Unable to find x dimension for parameter");
    const CDMDimension & dimension = cdm_->getDimension(xName);
    return dimension.getLength();
}

std::size_t WdbCDMReader::getYSize(const CDMVariable& variable) const
{
    const std::vector<std::string> & dimensions = variable.getShape();
    const std::string & yName = dimensions[1];
    CDMAttribute axis;
    if ( cdm_->getAttribute(yName, "axis", axis) )
        if ( axis.getStringValue() != "Y" )
            throw CDMException("Unable to find y dimension for parameter");
    const CDMDimension & dimension = cdm_->getDimension(yName);
    return dimension.getLength();
}



std::size_t WdbCDMReader::getGridSize(const CDMVariable& variable) const
{
    return getXSize(variable) * getYSize(variable);
}

DataPtr WdbCDMReader::extractDataFromField(const CDMVariable& variable, const std::vector<long long> & fieldIdentifiers) const
{
    DataPtr ret = createData(variable.getDataType(), fieldIdentifiers.size() * getGridSize(variable));

    float * dataIdx = reinterpret_cast<float *>(ret->getDataPtr());
    for ( std::vector<wdb::Wdb2CdmBuilder::gid>::const_iterator it = fieldIdentifiers.begin(); it != fieldIdentifiers.end(); ++ it )
        if ( * it != wdb::WdbIndex::UNDEFINED_GID )
            dataIdx = d_->wdbConnection->getGrid(dataIdx, * it);
        else
        {
            const wdb::GridInformation & gridInfo = d_->dataIndex->gridInformation();
            unsigned size = gridInfo.numberX() * gridInfo.numberY();
            std::fill(dataIdx, dataIdx + size, std::numeric_limits<float>::quiet_NaN());
            dataIdx += size;
        }
    return ret;
}

DataPtr WdbCDMReader::cutGrid(const DataPtr & d, const CDMVariable& variable, const SliceBuilder & sb) const
{
    // We assume that the two last dimensions are x and y in the grid.

    const std::vector<size_t> & starts = sb.getDimensionStartPositions();
    const std::vector<size_t> & sizes = sb.getDimensionSizes();

    const std::size_t x_start = starts[0];
    const std::size_t x_size = sizes[0];
    const std::size_t y_start = starts[1];
    const std::size_t y_size = sizes[1];

    const std::size_t grid_x_size = getXSize(variable);
    const std::size_t grid_y_size = getYSize(variable);

    if ( x_start == 0 and x_size == grid_x_size and y_start == 0 and y_size == grid_y_size )
        return d; // no cutting requested

    const std::size_t grid_size = grid_x_size * grid_y_size;
    const std::size_t grid_count = d->size() / grid_size;
    if ( d->size() % grid_size )
        throw CDMException("Internal error: grid size/spec mismatch");

    using boost::assign::list_of;
    std::vector<size_t> orgDimSize = list_of(getXSize(variable))(getYSize(variable))(grid_count);
    std::vector<size_t> startDims = list_of(x_start)(y_start)(0);
    std::vector<size_t> outputDimSize = list_of(x_size)(y_size)(grid_count);

    return d->slice(orgDimSize, startDims, outputDimSize);
}

DataPtr WdbCDMReader::getDatabaseFields(const CDMVariable& variable, size_t unLimDimPos) const
{
    std::vector<wdb::Wdb2CdmBuilder::gid> fieldIdentifiers = d_->dataIndex->getGridIdentifiers(variable.getName(), unLimDimPos);
    return extractDataFromField(variable, fieldIdentifiers);
}

DataPtr WdbCDMReader::getDatabaseFields(const CDMVariable& variable, const SliceBuilder & sb) const
{
    std::vector<wdb::Wdb2CdmBuilder::gid> fieldIdentifiers = d_->dataIndex->getGridIdentifiers(variable.getName(), sb, * cdm_);
    DataPtr ret = extractDataFromField(variable, fieldIdentifiers);
    return cutGrid(ret, variable, sb);
}

}

