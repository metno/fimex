/*
 * Fimex, NcmlCDMReader.h
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
 *  Created on: Apr 29, 2009
 *      Author: Heiko Klein
 */

#ifndef NCMLCDMREADER_H_
#define NCMLCDMREADER_H_

#include "fimex/CDMReader.h"
#include "fimex/CDMDataType.h"
#include "fimex/XMLInput.h"
#include <map>
#include <memory>

namespace MetNoFimex
{
/* forward declarations */
class XMLDoc;
class MutexType;

/**
 * The NcmlCDMReader can be used as both standard reader of a data and
 * as a manipulator for an existing CDM provided by a CDMReader.
 *
 * In the case of a real reader, the ncml-configuration file needs to have the
 * 'location' field set, which must point to a file readable by CDMReader
 *
 * The configuration file must be a standard ncml-file (versionn 2.2) as defined by
 * http://www.unidata.ucar.edu/software/netcdf/ncml/. The following changes have been
 * made to the unidata-version to improve operability with the file-types supported by
 * fimex:
 *
 * - location
 *   The location of a file can be given as simple string, or as space-separated list
 *   with "filename filetype configFile" to allow for reading of filetypes which
 *   require external configurations. This is also try for location in scan-elements
 *   where the filename is replaced by directory-name. filetype and configFile are then
 *   valid for all found-files.
 * - relative locations
 *   All locations within ncml-files are currently relative to the current working
 *   directory, not relative to the ncml-file. *This will change in the future!*
 *   It is currently safest to start fimex in the same directory as the ncml-file.
 * - spatial_vector
 *   For horizontal reprojections, vectors like x-wind/y-wind need to be rotated. Not
 *   all vectors can be detected from the CF standard. In fimex, it is possible to
 *   add an variable-attribute like  @code<spatial_vector direction="x" counterpart="y_wind" />@endcode
 *   To forbid auto-rotation, set the direction to longitude.
 * - joinNew aggregations
 *   are not implemented yet.
 * - forecastModelRunCollection aggregations
 *   are not implemented.
 * - forecastModelRunSingleCollection aggregations
 *   are not implemented.
 * - olderThan scan-attribute
 *   is not implemented.
 *
 * Examples:
 *
 * @code
 *     <aggregation type="joinExisting">
 *        <scan location="." regExp="\w{3}\.nc" />
 *     </aggregation>
 * @endcode
 * @code
 *     <aggregation type="union">
 *       <netcdf location="cldc.mean.nc"/>
 *       <netcdf location="lflx.mean.nc"/>
 *     </aggregation>
 * @endcode
 *
 *
 * @warning Aggregation keep all files open. Don't try to aggregate more that
 * your OS limit on open files per process, e.g. 1024 as default for Ubuntu 12.04.
 *
 */
class NcmlCDMReader: public MetNoFimex::CDMReader
{

public:
    /**
     * @param configXML ncml-file with location set
     * @throw CDMException
     */
    NcmlCDMReader(const XMLInput& configXML);
    /**
     * @param dataReader a file reader opened elsewhere
     * @param configXML ncml-file with location set
     * @throw CDMException
     */
    NcmlCDMReader(const boost::shared_ptr<CDMReader> dataReader, const XMLInput& configXML);
    virtual ~NcmlCDMReader();
    /**
     * reading the data from the required source
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
    /**
     * reading the data from the required source with SliceBuilder
     */
    virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);


private:
    /**
     * main function for all constructors
     */
    void init();
    void setConfigDoc(const XMLInput& configXML);
    /* log a info of msg if xpath in config file */
    void initRemove();
    void warnUnsupported(std::string xpath, std::string msg);
    void initWarnUnsupported();
    void initVariableShapeChange();
    void initVariableNameChange();
    void initVariableTypeChange();
    void initVariableDataChange();
    void initVariableSpatialVector();
    void initDimensionNameChange();
    void initDimensionUnlimited();
    void initAttributeNameChange();
    void initAddReassignAttribute();

    DataPtr applyVariableTypeChange(DataPtr data, const std::string &varName, const std::string &orgVarName);

    std::string configId;
    boost::shared_ptr<XMLDoc> doc;
    std::auto_ptr<MutexType> mutex_;
    boost::shared_ptr<CDMReader> dataReader;
    /*
     * maps containing the changes. The key will reflect
     * this CDM, while the value references to dataReader->cdm
     */
    std::map<std::string, std::string> variableNameChanges;
    std::map<std::string, CDMDataType> variableTypeChanges;
    std::map<std::string, std::string> dimensionNameChanges;
    // maps dimension-name to original dimension-name of the
    // dimensions which changed to or from unlimited dimension
    std::map<std::string, std::string> unlimitedDimensionChanges;


};

}

#endif /* NCMLCDMREADER_H_ */
