/*
 * Fimex, CDMTimeInterpolator.h
 *
 * (C) Copyright 2008-2019, met.no
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
 *  Created on: Dec 3, 2008
 *      Author: heikok
 */

#ifndef FIMEX_CDMTIMEINTERPOLATOR_H_
#define FIMEX_CDMTIMEINTERPOLATOR_H_

#include "CDMReader.h"
#include "fimex/coordSys/CoordSysDecl.h"

#include <map>
#include <vector>

namespace MetNoFimex {

/**
 * @headerfile fimex/CDMTimeInterpolator.h
 */
class CDMTimeInterpolator: public MetNoFimex::CDMReader
{
public:
    CDMTimeInterpolator(CDMReader_p dataReader);
    ~CDMTimeInterpolator();

    using CDMReader::getDataSlice;

    /**
     * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
     *
     * @param varName name of variable
     * @param unLimDimPos position of the unlimited dimension, most commonly time-position of the output as set in #changeTimeAxis
     */
    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override;

    /**
     * change the time-axis from from the one given to a new specification
     * @param timeSpec string of time-specification
     * @throws CDMException on unparsable timeSpec
     * @see MetNoFimex::TimeSpec
     */
    virtual void changeTimeAxis(const std::string& timeSpec);

private:
    CDMReader_p dataReader_;

    // map each new time-position to the closest time-positions in the old times
    typedef std::map<std::string, std::vector<std::pair<size_t,size_t> > > TimeChangeMap;
    TimeChangeMap timeChangeMap_;

    CoordinateSystem_cp_v coordSystems_;

    // store the datareaders times as doubles of the new units
    std::map<std::string, std::vector<double> > dataReaderTimesInNewUnits_;
};

} // namespace MetNoFimex

#endif /* FIMEX_CDMTIMEINTERPOLATOR_H_ */
