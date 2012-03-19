/*
 * Fimex, CDMProcessor.cc
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Mar 19, 2012
 *      Author: Heiko Klein
 */

#include "fimex/CDMProcessor.h"
#include "fimex/CDMVariable.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include <set>
#include <algorithm>
#include <functional>

namespace MetNoFimex
{


struct CDMProcessorImpl {
    boost::shared_ptr<CDMReader> dataReader;
    std::set<std::string> deaccumulateVars;
};

CDMProcessor::CDMProcessor(boost::shared_ptr<CDMReader> dataReader)
: p_(new CDMProcessorImpl())
{
    p_->dataReader = dataReader;
    *cdm_ = dataReader->getCDM();
}

CDMProcessor::~CDMProcessor()
{
}

void CDMProcessor::deAccumulate(const std::string& varName)
{
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (cdm_->hasUnlimitedDim(variable)) {
        p_->deaccumulateVars.insert(varName);
    } else {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::WARN, varName <<  " is not unlimited, ignoring deaccumulate");
    }

}

boost::shared_ptr<Data> CDMProcessor::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    boost::shared_ptr<Data> data = p_->dataReader->getDataSlice(varName, unLimDimPos);
    // deaccumulation
    if (p_->deaccumulateVars.find(varName) != p_->deaccumulateVars.end()) {
        LOG4FIMEX(getLogger("fimex.CDMProcessor"), Logger::DEBUG, varName << " at slice " << unLimDimPos << " deaccumulate");
        if (unLimDimPos != 0) { // cannot deaccumulate first
            boost::shared_ptr<Data> dataP = p_->dataReader->getDataSlice(varName, unLimDimPos-1);
            if ((data->size() != 0) && (dataP->size() != 0)) {
                assert(data->size() == dataP->size());
                boost::shared_array<double> d = data->asDouble();
                boost::shared_array<double> dp = dataP->asDouble();
                // this might modify the original data in the reader
                std::transform(&d[0], &d[0]+data->size(), &dp[0], &d[0], std::minus<double>());
                data = createData(data->size(), d);
            }
        }
    }

    return data;
}


} /* namespace MetNoFimex */
