/*
 * Fimex, ProradXMLCDMReader.cc
 *
 * (C) Copyright 2014, met.no
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
 *  Created on: Jan 27, 2014
 *      Author: Heiko Klein
 */
#include <ProradXMLCDMReader.h>
#include "fimex/MutexLock.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"
#include "fimex/coordSys/LatitudeLongitudeProjection.h"
#include "fimex/coordSys/RotatedLatitudeLongitudeProjection.h"
#include "DataImpl.h"

#include <boost/shared_array.hpp>
#include <vector>
#include <string>
#include <libxml/xinclude.h>
#include <proradxmlrw.h>
#include <prorad-radarprod.h>

namespace MetNoFimex
{

static MutexType mutex;
static LoggerPtr logger = getLogger("Fimex::ProradXMLCDMReader");

ProradXMLCDMReader::ProradXMLCDMReader(const std::string& source)
: CDMReader()
{
    using namespace std;
    LOG4FIMEX(logger, Logger::DEBUG, "reading file "+ source );
    cdm_->addAttribute(CDM::globalAttributeNS(), CDMAttribute("Conventions", "CF-1.6"));
    cdm_->addAttribute(CDM::globalAttributeNS(), CDMAttribute("history", "fimex creation from " + source));

    LIBXML_TEST_VERSION
    xmlInitParser();

    td_radarprod_meta_st meta;
    init_td_radarprod_meta_st(&meta);
    if ( !prorad_xmlprod_read(&meta, const_cast<char*>(source.c_str())) ) {
        if (meta.cartesian_st == 0) {
            free_td_radarprod_meta_st(&meta);
            throw CDMException("Unable to read prorad-xml file: " + source + ": not a cartesian product");
        }
        td_cartesian_st* cartesian = meta.cartesian_st;
        string time = "time";
        {   // time
            CDMDimension tDim(time, 1);
            tDim.setUnlimited(1);
            cdm_->addDimension(tDim);
            vector<string> tshape;
            tshape.push_back(time);
            CDMVariable tVar(time, CDM_INT, tshape);
            boost::shared_array<int> t(new int[1]());
            t[0] = meta.product_epoch;
            tVar.setData(createData(1, t));
            cdm_->addVariable(tVar);
            cdm_->addAttribute(time, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
        }
        boost::shared_ptr<Projection> proj = Projection::createByProj4(cartesian->projdef);
        string xname, yname;
        if (proj->getName() == LatitudeLongitudeProjection::NAME()) {
            xname = "lon";
            yname = "lat";
        } else if (proj->getName() == RotatedLatitudeLongitudeProjection::NAME()) {
            xname = "rlon";
            yname = "rlat";

        } else {
            xname = "x";
            yname = "y";
        }
        {
            cdm_->addDimension(CDMDimension(xname, cartesian->x_size));
            cdm_->addDimension(CDMDimension(yname, cartesian->y_size));
            vector<string> shape(1);
            shape[0] = xname;
            cdm_->addVariable(CDMVariable(xname, CDM_DOUBLE, shape));
            shape[0] = yname;
            cdm_->addVariable(CDMVariable(yname, CDM_DOUBLE, shape));
            // Projection
            shape.resize(0);
            cdm_->addVariable(CDMVariable(proj->getName(), CDM_SHORT, shape));
            vector<CDMAttribute> attrs = proj->getParameters();
            for (vector<CDMAttribute>::iterator it = attrs.begin(); it != attrs.end(); ++it) {
                cdm_->addAttribute(proj->getName(), *it);
            }
        }
        if (proj->getName() == LatitudeLongitudeProjection::NAME()) {
            cdm_->addAttribute(xname, CDMAttribute("units", "degrees_east"));
            cdm_->addAttribute(yname, CDMAttribute("units", "degrees_north"));
        } else if (proj->getName() == RotatedLatitudeLongitudeProjection::NAME()) {
            cdm_->addAttribute(xname, CDMAttribute("units", "degrees"));
            cdm_->addAttribute(yname, CDMAttribute("units", "degrees"));
            cdm_->addAttribute(xname, CDMAttribute("standard_name", "grid_longitude"));
            cdm_->addAttribute(yname, CDMAttribute("standard_name", "grid_latitude"));
        } else {
            cdm_->addAttribute(xname, CDMAttribute("units", "m"));
            cdm_->addAttribute(yname, CDMAttribute("units", "m"));
            cdm_->addAttribute(xname, CDMAttribute("standard_name", "projection_x_axis"));
            cdm_->addAttribute(yname, CDMAttribute("standard_name", "projection_y_axis"));
        }
        {
            // axes values
            boost::shared_array<double> xvals(new double[cartesian->x_size]());
            boost::shared_array<double> yvals(new double[cartesian->y_size]());
            for (size_t i = 0; i < cartesian->x_size; ++i) {
                xvals[i] = cartesian->west_ucs + i*cartesian->x_pix_size;
            }
            for (size_t i = 0; i < cartesian->y_size; ++i) {
                yvals[i] = cartesian->north_ucs - i*cartesian->y_pix_size;
            }
            cdm_->getVariable(xname).setData(createData(cartesian->x_size, xvals));
            cdm_->getVariable(yname).setData(createData(cartesian->y_size, yvals));
        }
        //coordinates
        string coordinates;
        if (proj->getName() != LatitudeLongitudeProjection::NAME()) {
            //
            // data will be added to only on request in getDataSlice
            vector<string> shape(2);
            shape[0] = xname;
            shape[1] = yname;
            cdm_->addVariable(CDMVariable("lat", CDM_DOUBLE, shape));
            cdm_->addVariable(CDMVariable("lon", CDM_DOUBLE, shape));
            cdm_->addAttribute("lat", CDMAttribute("units", "degrees_north"));
            cdm_->addAttribute("lon", CDMAttribute("units", "degrees_east"));
            cdm_->addAttribute("lat", CDMAttribute("standard_name", "latitude"));
            cdm_->addAttribute("lon", CDMAttribute("standard_name", "longitude"));
            coordinates = "lon lat";
        }


        assert(cartesian->buffer_size == (cartesian->x_size * cartesian->y_size));
        { // flags and values
            vector<string> shape(3);
            shape[0] = xname;
            shape[1] = yname;
            shape[2] = time;
            string varNm = cartesian->datatype;
            cdm_->addVariable(CDMVariable(varNm, CDM_SHORT, shape));
            cdm_->addAttribute(varNm, CDMAttribute("_FillValue", static_cast<short>(0)));
            //cdm_->addAttribute(varNm, CDMAttribute("units", cartesian->))
            cdm_->addAttribute(varNm, CDMAttribute("scale_factor", cartesian->alfa));
            cdm_->addAttribute(varNm, CDMAttribute("add_offset", cartesian->beta));
            if (coordinates != "") {
                cdm_->addAttribute(varNm, CDMAttribute("coordinates", coordinates));
            }
            cdm_->addAttribute(varNm, CDMAttribute("grid_mapping", proj->getName()));
            boost::shared_array<short> d(new short[cartesian->buffer_size]());
            copy(cartesian->buffer, cartesian->buffer + cartesian->buffer_size, &d[0]);
            DataPtr dp = createData(cartesian->buffer_size, d);
            cdm_->getVariable(varNm).setData(dp);

            if (cartesian->packed_flags_size > 0) { // packed_flags_size == 0 in all examples?
                assert(cartesian->packed_flags_size == cartesian->buffer_size);
                string flgNm = varNm + "_flags";
                cdm_->addVariable(CDMVariable(flgNm, CDM_SHORT, shape));
                cdm_->addAttribute(flgNm, CDMAttribute("standard_name", varNm + " status_flag")); // should link to standard_name, not varNm
                if (coordinates != "") {
                    cdm_->addAttribute(flgNm, CDMAttribute("coordinates", coordinates));
                }
                boost::shared_array<short> d(new short[cartesian->packed_flags_size]());
                copy(cartesian->buffer, cartesian->packed_flags + cartesian->packed_flags_size, &d[0]);
                DataPtr dp = createData(cartesian->packed_flags_size, d);
                cdm_->getVariable(flgNm).setData(dp);
            }

        }


    } else {
        free_td_radarprod_meta_st(&meta);
        throw CDMException("Unable to read prorad-xml file: " + source);
    }
    free_td_radarprod_meta_st(&meta);
}

DataPtr ProradXMLCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    ScopedCritical sc(mutex);
    LOG4FIMEX(logger, Logger::DEBUG, "getDataSlice(var,unlimDimPos): (" << varName << ", " << unLimDimPos << ")");
    // return unchanged data from this CDM
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(logger, Logger::DEBUG, "fetching data from memory");
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    if (varName == "lat" || varName == "lon") {
        // generate lat/lon data
        std::vector<std::string> shape = variable.getShape();
        CDM newCdm(*cdm_);
        newCdm.removeVariable("lat");
        newCdm.removeVariable("lon");
        std::vector<std::string> projections = newCdm.findVariables("grid_mapping_name", ".*");
        LOG4FIMEX(logger, Logger::DEBUG, "generating coordinates with " << projections.at(0) << ", " << shape.at(0) << ", " << shape.at(1));
        newCdm.generateProjectionCoordinates(projections.at(0), shape.at(0), shape.at(1), "lon", "lat");
        cdm_->getVariable("lat").setData(newCdm.getVariable("lat").getData());
        cdm_->getVariable("lon").setData(newCdm.getVariable("lon").getData());
        return getDataSliceFromMemory(variable, unLimDimPos);
    }
    return createData(CDM_NAT, 0);
}


} /* namespace MetNoFimex */
