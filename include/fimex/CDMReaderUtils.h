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
 *  @example parallelRead.cpp
 * Example on reading several slices in parallel with a CDMReader.
 */

/**
 * avoid deletion of the shared_ptr or shared_array when used as
 * boost::shared_ptr<T>(this, null_deleter())
 * @param r
 */
struct null_deleter
{
    void operator()(void const *) const {}
};

/**
 * Try to find the forecast reference time of the reader. This has currently
 * only be implemented for CF-1.x.
 * @param reader the CDMReader to check for the reference time
 * @return the reference time
 * @throw CDMException if eithern no reference time has been found, or if more than 1 different reference
 *        times have been found
 */
boost::posix_time::ptime getUniqueForecastReferenceTime(CDMReader_p reader);

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
std::vector<double> getDataSliceInUnit(CDMReader_p reader, const std::string& var, const std::string& unit, int unLimDimPos);

/**
 * return estimated size of CDM-data in bytes
 */
std::size_t estimateCDMDataSize(const CDM& cdm);

/**
 * check if the shapes of two variables (eventually belonging to two different CDMs) have the same
 * sizes. Dimensions of size 1 are ignored.
 *
 * @param cdm1 first CDM
 * @param varName1 first variable-name
 * @param cdm2 second CDM
 * @param varName2 second variable-name
 * @return
 */
bool compareCDMVarShapes(const CDM& cdm1, const std::string& varName1, const CDM& cdm2, const std::string& varName2);


/**
 * find a unique variable and dimension name, starting with baseVar
 * @param cdm
 * @param baseVar the basic variable name
 * @return a st
 */
std::string findUniqueDimVarName(const CDM& cdm, std::string baseVar);

/**
 * generate projection coordinates if they don't exist for a coordinate system
 */
void generateProjectionCoordinates(CDMReader_p reader);

}
#endif /* CDMREADERUTILS_H_ */
