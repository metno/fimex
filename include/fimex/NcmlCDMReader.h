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
#include <map>

namespace MetNoFimex
{
/* forward declarations */
class XMLDoc;

/**
 * The NcmlCDMReader can be used as both standard reader of a data and
 * as a manipulator for an existing CDM provided by a CDMReader.
 *
 * In the case of a real reader, the ncml-configuration file needs to have the
 * 'location' field set, which must point to a netcdf-file readable by #NetCDF_CF10_CDMReader
 *
 * The configuration file must be a standard ncml-file (versionn 2.2) as defined by
 * http://www.unidata.ucar.edu/software/netcdf/ncml/.
 *
 * @warning The current version does not support aggregation.
 *
 */
class NcmlCDMReader: public MetNoFimex::CDMReader
{

public:
    /**
     * @param configFile ncml-file with location set
     * @throw CDMException
     */
    NcmlCDMReader(std::string configFile);
    /**
     * @param cdmReader a file reader opened elsewhere
     * @param configFile ncml-file with location set
     * @throw CDMException
     */
    NcmlCDMReader(const boost::shared_ptr<CDMReader> dataReader, std::string configFile);
    virtual ~NcmlCDMReader();
    /**
     * reading the data from the required source
     */
    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0);


private:
    /**
     * main function for all constructors
     */
    void init();
    void setConfigDoc();
    /* log a info of msg if xpath in config file */
    void initRemove();
    void warnUnsupported(std::string xpath, std::string msg);
    void initWarnUnsupported();
    void initVariableNameChange();
    void initVariableTypeChange();
    void initVariableDataChange();
    void initDimensionNameChange();
    void initDimensionUnlimited();
    void initAttributeNameChange();
    void initAddReassignAttribute();

    std::string configFile;
    XMLDoc* doc;
    boost::shared_ptr<CDMReader> dataReader;
    /*
     * maps containing the changes. The key will reflect
     * this CDM, while the value references to dataReader->cdm
     */
    std::map<std::string, std::string> variableNameChanges;
    std::map<std::string, CDMDataType> variableTypeChanges;
    // maps dimension-name to original dimension-name of the
    // dimensions which changed the unlimited dimension
    std::map<std::string, std::string> unlimitedDimensionChanges;


};

}

#endif /* NCMLCDMREADER_H_ */
