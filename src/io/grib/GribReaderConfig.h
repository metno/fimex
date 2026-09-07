/*
 * Fimex, GribReaderConfig.h
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#ifndef FIMEX_GRIBREADERCONFIG_H_
#define FIMEX_GRIBREADERCONFIG_H_

#include "GribFileIndex.h"

#include "fimex/CDMAttribute.h"
#include "fimex/CDMDataType.h"
#include "fimex/XMLUtils.h"

#include <map>
#include <memory>
#include <regex>
#include <set>
#include <vector>

namespace MetNoFimex {

class GribFileMessage;

namespace GribReader {

struct Precision
{
    double scale_factor;
    double add_offset;
    bool has_precision() const { return scale_factor != 0; }
};

//! Information extracted from grib reader config xml
struct VariableConfig
{
    std::string varName;

    CDMDataType type;
    bool constantTime;
    std::vector<CDMAttribute> attributes;
    std::string vectorDirection;
    std::string vectorCounterpart;

    Precision precision;
};
typedef std::shared_ptr<VariableConfig> VariableConfig_p;
typedef std::shared_ptr<const VariableConfig> VariableConfig_cp;

//! Information extracted from grib reader config xml
struct ParamConfig
{
    std::map<std::string, std::set<long>> optionals_long;
    std::map<std::string, std::set<std::string>> optionals_string;
    VariableConfig_p variable;
};

struct GribConfig
{
    std::string configId;
    XMLDoc_p doc;

    std::vector<VariableConfig_p> variable_configs;
    std::map<int, std::vector<ParamConfig>> nodeIdx1;
    std::map<int, std::vector<ParamConfig>> nodeIdx2;
    std::string replaceEarthString;
    std::string extraKeys;
    bool select_only_defined;
    std::vector<std::pair<std::string, std::regex>> ensembleMemberIds;

    /**
     * find the node in the xml-config corresponding to the GribFileMessage
     * @return 0 if not found, otherwise a valid node
     */
    VariableConfig_cp findVariableConfig(const GribFileMessage& msg) const;

    static std::string getVariableName(const GribFileMessage& gfm, GribReader::VariableConfig_cp vc);

    /**
     * Find the valid time of the gfm, or not_a_date_time if variable is defined to be constant.
     * @param gfm
     * @return time or not_a_date_time
     */
    static FimexTime getVariableValidTime(const GribFileMessage& gfm, GribReader::VariableConfig_cp vc);
};
typedef std::shared_ptr<GribConfig> GribConfig_p;
typedef std::shared_ptr<const GribConfig> GribConfig_cp;

GribConfig_cp configFromXML(const XMLInput& configXML, const std::vector<std::pair<std::string, std::regex>>& members);

XMLDoc_p initXMLConfig(const XMLInput& configXML);
std::string getConfigEarthFigure(XMLDoc_p doc);
std::string getConfigExtraKeys(XMLDoc_p doc);

} // namespace GribReader

} // namespace MetNoFimex

#endif /* FIMEX_GRIBREADERCONFIG_H_ */
