/*
 * Fimex, WRFCoordSysBuilder.h
 *
 * (C) Copyright 2012-2026, met.no
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
 *  Created on: Mar 28, 2012
 *      Author: Heiko Klein
 */

#ifndef WRFCOORDSYSBUILDER_H_
#define WRFCOORDSYSBUILDER_H_

#include "coordSys/CoordSysBuilder.h"

namespace MetNoFimex
{

/**
 * class to build a CoordinateSystems for WRF model output, except ARW (Advanced Research WRF)
 *
 * I'm trying to follow:
 * http://www.mmm.ucar.edu/wrf/users/docs/user_guide_V3.1/users_guide_chap5.htm
 *
 * @see ucar.nc2.dataset.conv.WRFConvention.java http://grepcode.com/file/repo1.maven.org/maven2/edu.ucar/netcdf/4.2/ucar/nc2/dataset/conv/WRFConvention.java
 *
 */
class WRFCoordSysBuilder: public MetNoFimex::CoordSysBuilder
{
public:
    WRFCoordSysBuilder();
    virtual ~WRFCoordSysBuilder();
    virtual std::string getName() {return "WRF";}
    virtual bool isMine(const CDM& cdm);
    /**
     * List the CoordinateSystems belonging to WRF convention. This will actually
     * change the cdm and require some data from the reader.
     * @return list of WRF coordinate systems found
     * @warning listCoordinateSystems can currently not be called several times on the same reader
     */
    virtual CoordinateSystem_cp_v listCoordinateSystems(CDM& cdm);
    virtual CoordinateSystem_cp_v listCoordinateSystems(CDMReader_p reader);
    virtual void enhanceVectorProperties(CDMReader_p reader);
};

} /* namespace MetNoFimex */
#endif /* WRFCOORDSYSBUILDER_H_ */
