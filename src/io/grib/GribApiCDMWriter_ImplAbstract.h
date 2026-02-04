/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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
 */

#ifndef GRIBAPICDMWRITER_IMPLABSTRACT_H_
#define GRIBAPICDMWRITER_IMPLABSTRACT_H_

#include "fimex/CDMWriter.h"

#include "fimex/CDMException.h"
#include "fimex/TimeUnit.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInput.h"

#include <fstream>

// forward declaration
struct grib_handle;

namespace MetNoFimex {

class GribApiCDMWriter_ImplAbstract : public CDMWriter
{
public:
    /**
     * Constructor of the general writer. It should be called during
     * construction of derived classes.
     *
     * @warning remember to call run to actually do something
     */
    GribApiCDMWriter_ImplAbstract(int gribVersion, CDMReader_p cdmReader, const std::string& outputFile, const XMLInput& config);
    virtual ~GribApiCDMWriter_ImplAbstract();
    /**
     * @brief actually write the data
     *
     * The run function has be to called after construction the object to
     * actually fetch and write the data.
     */
    void run();

protected:
    /**
     * add the global attributes from the config to the default grib-handle
     */
    virtual void setGlobalAttributes();
    /**
     * add the attributes named attName from the config to the current grib-handle
     *
     * @param attName xpath to search
     * @param node the xml-node to search below, if 0 start from root-node
     */
    void setNodesAttributes(std::string attName, void* node = 0);

    virtual void setData(const DataPtr& data);
    /**
     * set the projection parameters, throw an exception if none are available
     * @param varName
     * @throw CDMException if parameters cannot be set
     */
    virtual void setProjection(const std::string& varName) = 0;
    virtual void setParameter(const std::string& varName, double levelValue) = 0;
    virtual void setTime(const std::string& varName, const FimexTime& rTime, const FimexTime& vTime, const std::string& stepUnit);
    virtual void setLevel(const std::string& varName, double levelValue, size_t levelPos) = 0;
    /**
     * get the levels from the cdm scaled to values used in grib (units/scale-factor)
     * assign at least 1 level, give it a default value if none is found in the cdm
     *
     */
    virtual std::vector<double> getLevels(const std::string& varName);
    /**
     * get the times from the cdm as FimexTime (including unit)
     * assign at least 1 time, give it a default value if none is found in the cdm
     */
    virtual std::vector<FimexTime> getTimes(const std::string& varName);
    /**
     * add the missing value to the gribHandle, rescale the data if needed
     * and change the datatype if needed, change the missingValue of the data if need
     * @return modified data
     */
    virtual DataPtr handleTypeScaleAndMissingData(const std::string& varName, double levelValue, DataPtr inData) = 0;
    virtual void writeGribHandleToFile();
    /**
     * check if the varName exists in the config file
     *
     * @param varName
     * @param usedXPath return xpath under which results have been found
     * @return true if nodes are found in config
     */
    bool hasNodePtr(const std::string& varName, std::string& usedXPath);
    /**
     * get the node belonging to varName, level and time from the
     * config file
     * @param varName name of the variable
     * @param levelValue curent level
     */
    xmlNode* getNodePtr(const std::string& varName, double levelValue);

protected:
    int gribVersion;
    const std::string configFile;
    const XMLDoc_p xmlConfig;
    bool omitEmptyFields;
    std::shared_ptr<grib_handle> gribHandle;

private:
    std::ofstream gribFile;
};

} // namespace MetNoFimex

#endif /* GRIBAPICDMWRITER_IMPLABSTRACT_H_ */
