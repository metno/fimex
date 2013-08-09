/*
 * Fimex, SpatialAxisSpec.h
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
 *  Created on: Mar 18, 2009
 *      Author: Heiko Klein
 */

#ifndef TIMESPEC_H_
#define TIMESPEC_H_

#include "fimex/TimeUnit.h"
#include "fimex/CDMException.h"
#include <vector>
#include <string>

namespace MetNoFimex
{

/**
 * @headerfile fimex/SpatialAxisSpec.h
 */
/**
 * This class can be used to describe a list of spatial units in an efficient textual way.
 *
 *
 * - UNIT: see <a href="http://www.unidata.ucar.edu/software/udunits/">udunit</a>, compatible with degree or m: default: m
 * - RELVALUE: float-number
 * - RELVALUES: comma-separated list of values with possible ... extension, ... meaning continuation of the difference of the previous two values
 *         0 is the first time in the original time-axis, x is the last time-value in the original time-axis
 *
 * A SpatialAxisSpec consists of at least of values:
 *
 * - axisspec := VALUES[;unit=UNIT] |
 *               RELVALUES;relativeStart=VALUE[;unit=UNIT]
 *
 * relativeStart will reset the relative value 0 to the first value larger
 * than x0 (original start time)
 * with x0 =  i * (v1-v0)* unit with i being a integer.
 *
 * @subsubsection secSpatialAxisSpecEx1 Example: absolute axis values, every 50km
 *
 * @code
 * axisspec = -450000,-400000,...,50000
 * @endcode
 *
 * @subsubsection secSpatialAxisSpecEx2 Example: relative axis, each 50km starting at 17km from the northpole
 *
 * @code
 * timespec = -50,0,...,x,x+50;relativeStart=17;unit=km
 * @endcode
 *
 * @warning The 'unit' parameter is currently not supported, please enter values as m or degree
 * @warning the RELVALUES currently must be in m, degree not supported (yet?)
 */
class SpatialAxisSpec
{
private:
    void init();
public:
    /**
     * Define a spatialAxisSpec. Depending on the axisSpec (relativeStart?), start and end must be given later
     * @param axisSpec string representation as explained above
     */
    SpatialAxisSpec(const std::string& axisSpec) throw(CDMException) :
        axisSpec(axisSpec), startEndInitialized(false), axisInitialized(false) {}
    /**
     * Define a spatialAxisSpec
     * @param axisSpec string representation as explained above
     * @param start place of data start, in degree or m
     * @param end place to end, in degree or m
     */
    SpatialAxisSpec(const std::string& axisSpec, double start, double end) throw(CDMException) :
        axisSpec(axisSpec), start(start), end(end), startEndInitialized(true), axisInitialized(false) {}
    virtual ~SpatialAxisSpec() {};
    /**
     * Check if axisSpec still requires start and end place. This returns false if
     * a) start and end have been given already
     * b) the axisSpec is independant of start and end
     */
    bool requireStartEnd();
    void setStartEnd(double start, double end) {this->start = start; this->end = end; this->startEndInitialized = true;}
    /**
     * @return steps on the axis in degree or m
     */
    const std::vector<double>& getAxisSteps() {if (!axisInitialized) init(); return axisSteps;}

private:
    std::string axisSpec;
    double start;
    double end;
    bool startEndInitialized;
    bool axisInitialized;
    std::vector<double> axisSteps;
};

} /* MetNoFimex */

#endif /* TIMESPEC_H_ */
