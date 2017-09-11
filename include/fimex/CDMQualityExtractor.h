/*
 * Fimex, CDMQualityExtractor.h
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
 *  Created on: May 25, 2009
 *      Author: Heiko Klein
 */

#ifndef CDMQUALITYEXTRACTOR_H_
#define CDMQUALITYEXTRACTOR_H_

#include "CDMReader.h"
#include <vector>
#include <map>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMQualityExtractor.h
 */
/**
 * @brief Extract data with defined quality status
 *
 * The CDMQualityExtractor will select data from data-sources
 * matching only configurable quality constraints. Data not matching
 * these constraints will be set to undefined.
 *
 * The configuration works either semi-automatic by interpreting the quality
 * flags as given in CF-1.x at http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#flags
 * or by using a configuration-file describing the quality-relations between the different varibles.
 *
 * All variables with no quality-configuration will not be changed.
 *
 * @warning The CDMQualityExtractor will read the status-variable after applying eventual quality-flags to
 * them. It is therefore the task of the writer of the configuration, that no circular quality-flags exist.
 *
 */
class CDMQualityExtractor: public MetNoFimex::CDMReader
{
public:
    /**
     * Initialize the CDMQualityExtractor
     *
     * @param dataReader the data-source
     * @param autoConfString the default value for CF-1.4 compatible status_flags, i.e. "all, highest, lowest, values=0,1,...,3", the values here
     * might be overwritten by the config-file. If empty, no quality extraction on the basis of CF-1.4 will be used.
     * @param configFile filename of a cdmQualityConfig.xml file. If empty, no quality-file will be used.
     */
    CDMQualityExtractor(CDMReader_p dataReader, std::string autoConfString="", std::string configFile="");
    virtual ~CDMQualityExtractor() {};
    using CDMReader::getDataSlice;
    /**
     * Read and manipulate the data
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
    /**
     * Read the internals of statusVariable. This code is mainly thought for testing/debugging.
     */
    const std::map<std::string, std::string>& getStatusVariable() const {return statusVariable;}
    /**
     * Read the internals of variableFlags, for testing/debugging.
     */
    const std::map<std::string, std::string>& getVariableFlags() const {return variableFlags;}
    /**
     * Read the internals of variableValues, for testing/debugging.
     */
    const std::map<std::string, std::vector<double> >& getVariableValues() const {return variableValues;}
private:
    const CDMReader_p dataReader;
    /* map of variableName to the variable which contains the flags */
    std::map<std::string, std::string> statusVariable;
    /*
     * map of the variableName to the flags, which should be valid in the statusVariable, i.e. highest,lowest,all.
     * statusVariable needs to be set!
     */
    std::map<std::string, std::string> variableFlags;
    /*
     * fillValue for each variable (default = _FillValue, or set per variable in config)
     */
    std::map<std::string, double> variableFill;
    /*
     * map of the variableName to the valid values in the statusVariable.
     * statusVariable needs to be set!
     */
    std::map<std::string, std::vector<double> > variableValues;
    /*
     * map of the variableName to the reader used for getting values of the statusVariable.
     */
    std::map<std::string, CDMReader_p> statusReaders;
};

}

#endif /* CDMQUALITYEXTRACTOR_H_ */
