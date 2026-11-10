/*
 * Fimex, CDMQualityExtractor.h
 *
 * (C) Copyright 2009-2026, met.no
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
 *  Created on: May 25, 2009
 *      Author: Heiko Klein
 */

#ifndef CDMQUALITYEXTRACTOR_H_
#define CDMQUALITYEXTRACTOR_H_

#include "CDMReader.h"

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace MetNoFimex {

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
class CDMQualityExtractor : public CDMReader
{
public:
    struct QualityAssignment
    {
        /* the variable which contains the flags */
        std::string statusVariable;

        /* the flags, which should be valid in the statusVariable, i.e. highest,lowest,all; used if values is not set */
        std::string flags;

        /* the valid values in the statusVariable; if set, takes precedence over flags */
        std::optional<std::set<double>> values;

        /* fillValue for the variable (default = _FillValue of the variable, or set per variable in config) */
        std::optional<double> fillValue;

        /* reader used for getting values of the statusVariable, null if dataReader is used */
        CDMReader_p statusReader;
    };

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

    using CDMReader::getDataSlice;
    /**
     * Read and manipulate the data
     */
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);

    std::shared_ptr<const QualityAssignment> getQualityAssignment(const std::string& varName);

private:
    const CDMReader_p dataReader;

    /* map of variableName to its quality-assignment; may be shared between variables */
    std::map<std::string, std::shared_ptr<const QualityAssignment>> qualities;
};

} // namespace MetNoFimex

#endif /* CDMQUALITYEXTRACTOR_H_ */
