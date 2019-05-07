/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "FeltParameters.h"

#include "fimex/CDMconstants.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>

namespace MetNoFelt {

static MetNoFimex::Logger_p logger = MetNoFimex::getLogger("fimex.FeltParameters");

FeltParameters::FeltParameters() {
    init();
}

FeltParameters::FeltParameters(std::string filename)
{
    init(filename);
}

FeltParameters::FeltParameters(const std::vector<std::string>& dianaFeltParams, const std::string& globalRestrictions) {
    for (std::vector<std::string>::const_iterator it = dianaFeltParams.begin(); it != dianaFeltParams.end(); ++it) {
        std::string paramName(*it);
        std::string dataType("none");
        std::smatch what;
        std::regex dTypeRegex(":dataType=([a-z]*)");
        // get dataType and remove from paramName
        if (std::regex_search(paramName, what, dTypeRegex)) {
            dataType = what[1].str();
            if (!(dataType == "short" || dataType == "float" || dataType == "double")) {
                Felt_File_Error("unknown type for variable "+paramName+": "+dataType+ " must be float|double|short");
            }
            paramName = std::regex_replace(paramName, dTypeRegex, "");
        }
        // remove the global restrictions from the parameter-name
        // which might be used as ID, too
        string cleanParamName = paramName;
        if (globalRestrictions != "") {
            cleanParamName = std::regex_replace(paramName, std::regex(":?" + MetNoFimex::regex_escape(globalRestrictions)), "");
        }

        // get fillValue and remove from paramName
        std::regex fillValueRegex(":?fillValue=([^:]+)");
        if (std::regex_search(paramName, what, fillValueRegex)) {
            std::stringstream ss;
            ss << what[1].str();
            double fillValue;
            ss >> fillValue;
            cleanParamName = std::regex_replace(cleanParamName, fillValueRegex, "");
            parameterFillValueMap[cleanParamName] = fillValue;
        }

        parameterMap[cleanParamName] = diana2feltparameters(paramName);
        if (dataType != "none") {
            parameterDatatypeMap[cleanParamName] = dataType;
            if (parameterFillValueMap.find(paramName) != parameterFillValueMap.end()) {
                parameterFillValueMap[cleanParamName] = parameterFillValueMap[paramName];
            }
        }
        //cerr << "Parameter " << paramName << "/" << cleanParamName << ": " << parameterDatatypeMap[paramName] << " " << parameterFillValueMap[paramName] << endl;
    }
}

void FeltParameters::init(std::string configFile) {
    std::regex sectionEx("\\s*<([^/].*)>\\s*");
    std::regex parameterEx("\\s*([^=\\s]+)=(\\S+)");
    std::smatch what;

    std::ifstream dianaFeltDeclarations(configFile.c_str());
    if (dianaFeltDeclarations.is_open()) {
        std::string line;
        std::string section;
        std::string lastLine("");
        bool have_endSectionEx = false;
        std::regex endSectionEx;
        while (std::getline(dianaFeltDeclarations, line)) {
            line = std::regex_replace(line, std::regex("#.*"), "");
            if (std::regex_match(line, std::regex("\\s*"))) {
                continue;
            }

            if (std::regex_match(line, what, std::regex("(.*)\\\\.*"))) {
                // continuing line
                lastLine.append(what[1].first, what[1].second);
                continue;
            } else if (lastLine.length() > 0) {
                    line.assign(lastLine + " " + line);
                    lastLine.assign("");
            }

            if (std::regex_match(line, what, sectionEx)) {
                section = what[1].str();
                endSectionEx = std::regex("\\s*</" + MetNoFimex::regex_escape(section) + ">\\s*");
                have_endSectionEx = true;
            } else if (have_endSectionEx && std::regex_match(line, what, endSectionEx)) {
                section.erase();
               } else if (section == "METNOFIELDFILE_PARAMETERS") {
                   std::string::const_iterator start, end;
                   std::regex_constants::match_flag_type flags = std::regex_constants::match_default;
                   start = line.begin();
                   end = line.end();
                   while (std::regex_search(start, end, what, parameterEx)) {
                       // std::cerr << "Debug: " << what[1] << "===" << what[2] << std::endl;
                       // update start position
                       start = what[0].second;
                       // update flags to allow for --first as start-position
                       flags |= std::regex_constants::match_prev_avail;
                       // flags |= std::regex_constants::match_not_bob; // FIXME not available in std::regex

                       parameterMap[what[1].str()] = diana2feltparameters(what[2].str());
                }
            }

        }
    }
}
FeltParameters::~FeltParameters()
{
}

std::array<short, 16> FeltParameters::diana2feltparameters(const std::string& dianaString)
{
    std::regex equalSeparatedRegex("\\s*(\\w*)=(\\d+)\\s*");
    std::smatch what;
    std::array<short, 16> diana2feltParameters;
    for (int i = 0; i < 16; i++) {
        diana2feltParameters[i] = ANY_VALUE();
    }

    const std::vector<std::string> dianaStrings = MetNoFimex::split_any(dianaString, ":");

    int i = 0;
    for (const std::string& ii : MetNoFimex::split_any(dianaStrings.front(), ",")) {
        short value(std::atoi(ii.c_str()));
        switch (i++) {
            case 0: diana2feltParameters[11] = value; break; // param
            case 1: diana2feltParameters[10] = value; break; // v.coord
            case 2: diana2feltParameters[12] = value; break; // level
        }
    }

    for (std::vector<std::string>::const_iterator tokIt = ++dianaStrings.begin(); tokIt != dianaStrings.end(); ++tokIt) {
        if (std::regex_match(*tokIt, what, equalSeparatedRegex)) {
            short id(std::atoi(what[2].str().c_str()));
            //cerr << what[1].str() << ": " << id << endl;
            if (what[1].str() == "prod") {
                diana2feltParameters[0] = id;
            } else if (what[1].str() == "grid") {
                diana2feltParameters[1] = id;
            } else if (what[1].str() == "dtype") {
                diana2feltParameters[8] = id;
            } else if (what[1].str() == "level") {
                diana2feltParameters[12] = id;
            } else if (what[1].str() == "idnum") {
                diana2feltParameters[13] = id;
            } else if (what[1].str() == "param") {
                diana2feltParameters[11] = id;
            } else if (what[1].str() == "vcoord") {
                diana2feltParameters[10] = id;
            } else {
                LOG4FIMEX(logger, MetNoFimex::Logger::WARN, "unknown restriction: " << id << ", must be (prod|grid|dtype|param|vcoord|level|idnum)");
            }
        }
    }
    //cerr << diana2feltParameters[11] << "," << diana2feltParameters[10]<< "," << diana2feltParameters[12] << endl;
    return diana2feltParameters;
}

const std::array<short, 16>& FeltParameters::getParameters(const std::string& input)
{
    std::map<std::string, std::array<short, 16>>::iterator it = parameterMap.find(input);
    if (it == parameterMap.end()) {
        // not found
        return ANY_ARRAY();
    } else {
        return it->second;
    }
}

const std::string& FeltParameters::getParameterName(const std::array<short, 16>& feltParams)
{
    std::map<std::string, std::array<short, 16>>::iterator it;
    for	(it = parameterMap.begin(); it != parameterMap.end(); ++it) {
        std::array<short, 16> value = it->second;
        int errors = 0;
        for (int i = 0; i < 16; i++) {
            if ((value[i] != ANY_VALUE()) && value[i] != feltParams[i]) {
                errors++;
                break;
            }
        }
        if (errors == 0) {
            return it->first;
        }
    }
    return UNDEFINED();
}

std::string FeltParameters::getParameterDatatype(const std::string& parameterName) const {
    std::map<std::string, std::string>::const_iterator it = parameterDatatypeMap.find(parameterName);
    if (it != parameterDatatypeMap.end()) {
        return it->second;
    } else {
        return "short";
    }
}
double FeltParameters::getParameterFillValue(const std::string& parameterName) const {
    std::map<std::string, double>::const_iterator it = parameterFillValueMap.find(parameterName);
    if (it != parameterFillValueMap.end()) {
        return it->second;
    } else {
        return ANY_VALUE();
    }
}

std::ostream& operator<<(std::ostream& os, const FeltParameters& fp) {
    std::map<std::string, std::array<short, 16>>::const_iterator it;
    for (it = fp.parameterMap.begin(); it != fp.parameterMap.end(); ++it) {
        const std::array<short, 16>& value = it->second;
        const std::string& name = it->first;
        os << name << ": ";
        for (int i = 0; i < 15; i++) os << value[i] << ",";
        os << endl;
    }
    return os;
}

std::string getProjString(int gridType, const std::array<float, 6>& gridParameters)
{
    std::ostringstream tempProj;
    std::string earth("+ellps=sphere +a="+MetNoFimex::type2string(MIFI_EARTH_RADIUS_M)+" +e=0");
    switch (gridType) {
        case 1:
        case 4: tempProj << "+proj=stere +lat_0=90 +lat_ts=" << gridParameters[4] << " +lon_0=" << gridParameters[3] << " " << earth;
                break;
        case 2: tempProj << "+proj=latlong " << earth; // geographic
                break;
        case 3: tempProj << "+proj=ob_tran +o_proj=latlong +o_lat_p="<< (90-gridParameters[5]) << " +o_lon_p=0 +lon_0=" << gridParameters[4] <<  " " << earth; // rotated geographic
                break;
        case 5: tempProj << "+proj=gstmerc +lat_1=" << gridParameters[4] << " " << earth; // mercator ???
                break;
        case 6: tempProj << "+proj=lcc +lon_0="<< gridParameters[4] << " +lat_0="<< gridParameters[5] << " +lat_1="<< gridParameters[5] << " +lat_2=" << gridParameters[5] << " " << earth;
                break;
        default: throw Felt_File_Error("unknown projection-id: " + MetNoFimex::type2string(gridType));
    }
    return tempProj.str();
}

const std::string& UNDEFINED() {
    static std::string s("");
    return s;
}
const std::array<short, 16>& ANY_ARRAY()
{
    const static std::array<short, 16> ary = {{ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
                                               ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE()}};
    return ary;
}
const std::array<short, 20>& ANY_ARRAY20()
{
    const static std::array<short, 20> ary = {{ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
                                               ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
                                               ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE()}};
    return ary;
}


} // end namespace MetNoFelt
