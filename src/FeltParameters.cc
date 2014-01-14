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
#include "fimex/Utils.h"
#include "fimex/Logger.h"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>

namespace MetNoFelt {

static MetNoFimex::LoggerPtr logger = MetNoFimex::getLogger("fimex.FeltParameters");


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
        boost::smatch what;
        boost::regex dTypeRegex(":dataType=([a-z]*)");
        // get dataType and remove from paramName
        if (boost::regex_search(paramName, what, dTypeRegex)) {
            dataType = what[1].str();
            if (!(dataType == "short" || dataType == "float" || dataType == "double")) {
                Felt_File_Error("unknown type for variable "+paramName+": "+dataType+ " must be float|double|short");
            }
            paramName = boost::regex_replace(paramName, dTypeRegex, "");
        }
        // remove the global restrictions from the parameter-name
        // which might be used as ID, too
        string cleanParamName = paramName;
        if (globalRestrictions != "") {
            cleanParamName = boost::regex_replace(paramName, boost::regex(":?\\Q"+globalRestrictions+"\\E"), "");
        }

        // get fillValue and remove from paramName
        boost::regex fillValueRegex(":?fillValue=([^:]+)");
        if (boost::regex_search(paramName, what, fillValueRegex)) {
            std::stringstream ss;
            ss << what[1].str();
            double fillValue;
            ss >> fillValue;
            cleanParamName = boost::regex_replace(cleanParamName, fillValueRegex, "");
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
    boost::regex sectionEx("\\s*<([^/].*)>\\s*");
    boost::regex parameterEx("\\s*([^=\\s]+)=(\\S+)");
    boost::smatch what;

    std::ifstream dianaFeltDeclarations(configFile.c_str());
    if (dianaFeltDeclarations.is_open()) {
        std::string line;
        std::string section;
        std::string lastLine("");
        boost::regex endSectionEx;
        while (std::getline(dianaFeltDeclarations, line)) {
            line = boost::regex_replace(line, boost::regex("#.*"), "");
            if (boost::regex_match(line, boost::regex("\\s*"))) {
                continue;
            }

            if (boost::regex_match(line, what, boost::regex("(.*)\\\\.*"))) {
                // continuing line
                lastLine.append(what[1].first, what[1].second);
                continue;
            } else if (lastLine.length() > 0) {
                    line.assign(lastLine + " " + line);
                    lastLine.assign("");
            }

               if (boost::regex_match(line, what, sectionEx)) {
                   section = what[1].str();
                   endSectionEx = boost::regex("\\s*</\\Q"+section+"\\E>\\s*");
               } else if (!endSectionEx.empty() &&  boost::regex_match(line, what, endSectionEx)) {
                   section.erase();
               } else if (section == "METNOFIELDFILE_PARAMETERS") {
                std::string::const_iterator start, end;
                boost::match_flag_type flags = boost::match_default;
                   start = line.begin();
                   end = line.end();
                while (boost::regex_search(start, end, what, parameterEx)) {
                    //std::cerr << "Debug: " << what[1] << "===" << what[2] << std::endl;
                    // update start position
                    start = what[0].second;
                    // update flags to allow for --first as start-position
                      flags |= boost::match_prev_avail;
                      flags |= boost::match_not_bob;

                      parameterMap[ what[1].str() ] = diana2feltparameters(what[2].str());
                }
            }

        }
    }
}
FeltParameters::~FeltParameters()
{
}

boost::array<short, 16> FeltParameters::diana2feltparameters(const std::string& dianaString)
{
    boost::regex equalSeparatedRegex("\\s*(\\w*)=(\\d+)\\s*");
    boost::smatch what;
    boost::array<short, 16> diana2feltParameters;
    for (int i = 0; i < 16; i++) {
        diana2feltParameters[i] = ANY_VALUE();
    }

    boost::char_separator<char> colonSep(":");
    boost::char_separator<char> commaSep(",");
    boost::tokenizer<boost::char_separator<char> > tok(dianaString, colonSep);
    boost::tokenizer<boost::char_separator<char> >::iterator tokIt = tok.begin();

    boost::tokenizer<boost::char_separator<char> > tok2(*tokIt, commaSep);
    boost::tokenizer<boost::char_separator<char> >::iterator tok2It = tok2.begin();
    for (int i = 0; tok2It != tok2.end(); ++tok2It, ++i) {
        short value(std::atoi((*tok2It).c_str()));
        switch (i) {
            case 0: diana2feltParameters[11] = value; break; // param
            case 1: diana2feltParameters[10] = value; break; // v.coord
            case 2: diana2feltParameters[12] = value; break; // level
        }
    }

    ++tokIt;
    for (;tokIt != tok.end(); ++tokIt) {
        if (boost::regex_match(*tokIt, what, equalSeparatedRegex)) {
            short id(std::atoi(what[2].str().c_str()));
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
    return diana2feltParameters;
}

const boost::array<short, 16>& FeltParameters::getParameters(const std::string& input) {
    std::map<std::string, boost::array<short, 16> >::iterator it = parameterMap.find(input);
    if (it == parameterMap.end()) {
        // not found
        return ANY_ARRAY();
    } else {
        return it->second;
    }
}

const std::string& FeltParameters::getParameterName(const boost::array<short, 16>&  feltParams) {
    std::map<std::string, boost::array<short, 16> >::iterator it;
    for	(it = parameterMap.begin(); it != parameterMap.end(); ++it) {
        boost::array<short, 16>  value = it->second;
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


std::string getProjString(int gridType, const boost::array<float, 6>& gridParameters)
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
        default: throw Felt_File_Error("unknown projection-id: " + gridType);
    }
    return tempProj.str();
}

const std::string& UNDEFINED() {
    static std::string s("");
    return s;
}
const boost::array<short, 16>& ANY_ARRAY() {
    const static boost::array<short, 16> ary =
    { {ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE()} };
    return ary;
}
const boost::array<short, 20>& ANY_ARRAY20() {
    const static boost::array<short, 20> ary =
    { {ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE(),
       ANY_VALUE(), ANY_VALUE(), ANY_VALUE(), ANY_VALUE()} };
    return ary;
}


} // end namespace MetNoFelt
