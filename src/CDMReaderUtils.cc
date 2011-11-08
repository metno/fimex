/*
 * Fimex, CDMReaderUtils.cc
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Sep 10, 2010
 *      Author: Heiko Klein
 */

#include "fimex/CDMReaderUtils.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <vector>
#include <set>

namespace MetNoFimex
{

using namespace std;

boost::posix_time::ptime
getUniqueForecastReferenceTime(boost::shared_ptr<CDMReader> reader)
{
    const CDM& cdm = reader->getCDM();
    vector<string> refVarnames = cdm.findVariables("standard_name", "forecast_reference_time");
    set<boost::posix_time::ptime> refTimes;
    for (vector<string>::iterator varname = refVarnames.begin(); varname != refVarnames.end(); ++varname) {
        string units = cdm.getUnits(*varname);
        TimeUnit tu(units);
        boost::shared_ptr<Data> timeData = reader->getData(*varname);
        const boost::shared_array<const double> times = timeData->asConstDouble();
        const double* tPtr = &times[0];
        const double* end = tPtr + timeData->size();
        while (tPtr != end) {
            refTimes.insert(tu.unitTime2posixTime(*tPtr++));
        }
    }
    if (refTimes.size() == 0) {
        throw CDMException("no forecast reference time found");
    } else if (refTimes.size() > 1) {
        throw CDMException("forecast reference time not unique");
    }
    return *(refTimes.begin());
}

}
