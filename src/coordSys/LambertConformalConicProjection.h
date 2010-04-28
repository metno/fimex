/*
 * Fimex, LambertConformalConicProjection.h
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
 *  Created on: Apr 28, 2010
 *      Author: Heiko Klein
 */

#ifndef LAMBERTCONFORMALCONICPROJECTION_H_
#define LAMBERTCONFORMALCONICPROJECTION_H_

#include "ProjectionImpl.h"
#include "fimex/Data.h"

namespace MetNoFimex
{

class LambertConformalConicProjection: public MetNoFimex::ProjectionImpl
{

public:
    LambertConformalConicProjection() : ProjectionImpl("lambert_conformal_conic", false) {}
    virtual ~LambertConformalConicProjection() {}
protected:
    virtual std::ostream& getProj4ProjectionPart(std::ostream& oproj) const {
        oproj << "+proj=lcc";
        std::vector<CDMAttribute>::const_iterator foundAttr = std::find_if(params_.begin(), params_.end(), CDMNameEqual("standard_parallel"));
        if (foundAttr != params_.end()) {
            // standard_parallel - There may be 1 or 2 values.
            boost::shared_ptr<Data> spData = foundAttr->getData();
            oproj << " +lat_1=" << spData->asConstDouble()[0];
            if (spData->size() > 2) {
                oproj << " +lat_2=" << spData->asConstDouble()[1];
            } else {
                oproj << " +lat_2=" << spData->asConstDouble()[0];
            }
        } else {
            oproj << " +lat_1=0 +lat_2=0";
        }
        addParameterToStream(oproj, "longitude_of_central_meridian", " +lon_0=");
        addParameterToStream(oproj, "latitude_of_projection_origin", " +lat_0=");
        addParameterToStream(oproj, "false_easting", " +x_0=");
        addParameterToStream(oproj, "false_northing", " +y_0=");

        return oproj;
    }


};

}


#endif /* LAMBERTCONFORMALCONICPROJECTION_H_ */
