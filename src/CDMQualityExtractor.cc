/*
 * Fimex, CDMQualityExtractor.cc
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

#include "fimex/CDMQualityExtractor.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"

using namespace std;

namespace MetNoFimex
{

static LoggerPtr logger = getLogger("fimex.CDMQualityExtractor");

/**
 * find the variable which is quality-assured by the status-variable
 * @param cdm
 * @param statusVarName the status variable
 * @return the quality assured variables
 */
static vector<string> findCorrespondingVariables(const CDM& cdm, string statusVarName) {
    vector<string> correspondingVars;
    // the standard name of a status variable is: "corresponding_standard_name status_flag" as of CF-1.3
    CDMAttribute attr;
    if (cdm.getAttribute(statusVarName, "standard_name", attr)) {
        std::string standardName = attr.getData()->asString();
        vector<string> parts = tokenize(standardName, " ");
        if (parts.size() >=2) {
            string foreignStandardName = parts[0];
            vector<string> correspondingVars = cdm.findVariables("standard_name", foreignStandardName);
        } else {
            LOG4FIMEX(logger, Logger::WARN, "standard_name "+ standardName + "not suitabale for a status_flag in statusVariable "+ statusVarName);
        }
    } else {
        LOG4FIMEX(logger, Logger::INFO, "unable to find standard_name for status variable "+statusVarName + ": autoconfig not possible");
    }
    return correspondingVars;
}


CDMQualityExtractor::CDMQualityExtractor(boost::shared_ptr<CDMReader> dataReader, std::string autoConfString, std::string configFile) throw(CDMException)
: dataReader(dataReader)
{
    const CDM& cdm = dataReader->getCDM();
    if (autoConfString != "") {
        vector<string> vars = cdm.findVariables("flag_values",".*");
        for (vector<string>::iterator varIt = vars.begin(); varIt != vars.end(); ++varIt) {
            vector<string> correspondingVars = findCorrespondingVariables(cdm, *varIt);
        }
    }

}

const boost::shared_ptr<Data> CDMQualityExtractor::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
    // TODO
}

}
