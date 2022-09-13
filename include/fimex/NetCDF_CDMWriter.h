/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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
 */

#ifndef NETCDF_CDMWRITER_H_
#define NETCDF_CDMWRITER_H_

#include "fimex/CDMWriter.h"
#include "fimex/CDM.h"

#include <map>
#include <string>

namespace MetNoFimex {

/* forward declarations */
class XMLDoc;
class Nc;

class NetCDF_CDMWriter : public CDMWriter
{
    typedef std::map<std::string, int> NcDimIdMap;
    typedef std::map<std::string, int> NcVarIdMap;

public:
    /**
     * @param cdmReader dataSource
     * @param outputFile file-name to write to
     * @param configFile xml-configuration
     * @param version netcdf version, can be 3 or 4; 4 requires compilation against netcdf-4.0 or higher
     */
    NetCDF_CDMWriter(const CDMReader_p cdmReader, const std::string& outputFile, std::string configFile = "", int version = 3);
    virtual ~NetCDF_CDMWriter();
    /**
     * @warning only public for testing
     * @return the new name of a variable, eventually changed by the writers config
     */
    const std::string& getVariableName(const std::string& varName) const;
    /**
     * @warning only public for testing
     * @return the new name of a dimension, eventually changed by the writers config
     */
    const std::string& getDimensionName(const std::string& dimName) const;
    /**
     * @warning only public for testing
     * @param varName original variable name  (before config: newname)
     * @param attName original attribute name (before config: newname)
     * @return an attribute contained in the writers attribute, possibly added by config
     */
    const CDMAttribute& getAttribute(const std::string& varName, const std::string& attName) const;

private:
    void init();
    void initNcmlReader(std::unique_ptr<XMLDoc>& doc);
    void initFillRenameDimension(std::unique_ptr<XMLDoc>& doc);
    void initFillRenameVariable(std::unique_ptr<XMLDoc>& doc);
    void initFillRenameAttribute(std::unique_ptr<XMLDoc>& doc);
    /** clear all fields to remove */
    void initRemove(std::unique_ptr<XMLDoc>& doc);
    /** test if the variable exists in the cdmReader or throw an CDMException */
    void testVariableExists(const std::string& varName);

    NcDimIdMap defineDimensions();
    NcVarIdMap defineVariables(const NcDimIdMap& dimMap);
    void writeAttributes(const NcVarIdMap& varMap);
    void writeData(const NcVarIdMap& varMap);

    DataPtr convertData(const CDMVariable& var, DataPtr data);

private:
    CDM cdm; /* local storage of the changed cdm-outline, except variable name changes */
    std::unique_ptr<Nc> ncFile;
    std::map<std::string, std::string> variableNameChanges;
    std::map<std::string, CDMDataType> variableTypeChanges;
    std::map<std::string, unsigned int> variableCompression;
    std::map<std::string, unsigned int> dimensionChunkSize;
    std::map<std::string, std::string> dimensionNameChanges;
};

} // namespace MetNoFimex

#endif /*NETCDF_CDMWRITER_H_*/
