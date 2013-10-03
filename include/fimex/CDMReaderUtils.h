/*
 * Fimex, CDMReaderUtils.h
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

#ifndef CDMREADERUTILS_H_
#define CDMREADERUTILS_H_

#include "fimex/CDMReader.h"
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/shared_ptr.hpp>
#include <vector>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMReaderUtils.h
 */

/**
 * avoid deletion of the CDMReader* when used as
 * boost::shared_ptr<CDMReader>(this, noDelete)
 * @param r
 */
void noDelete(CDMReader* r);

/**
 * Try to find the forecast reference time of the reader. This has currently
 * only be implemented for CF-1.x.
 * @param reader the CDMReader to check for the reference time
 * @return the reference time
 * @throw CDMException if eithern no reference time has been found, or if more than 1 different reference
 *        times have been found
 */
boost::posix_time::ptime getUniqueForecastReferenceTime(boost::shared_ptr<CDMReader> reader);

/**
 * This is the same function as CDMReader::getScaledDataSliceInUnit, but it converts
 * the data to a vector<double>.
 *
 * @param reader
 * @param var
 * @param unit
 * @param unLimDimPos
 * @return
 */
std::vector<double> getDataSliceInUnit(const boost::shared_ptr<CDMReader>& reader, const std::string& var, const std::string& unit, int unLimDimPos);

/**
 * return estimated size of CDM-data in bytes
 */
std::size_t estimateCDMDataSize(const CDM& cdm);
}
#endif /* CDMREADERUTILS_H_ */
