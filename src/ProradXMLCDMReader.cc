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

class ProradXMLImpl {
public:
    ProradXMLImpl() : meta(boost::shared_ptr<td_radarprod_meta_st>(new td_radarprod_meta_st, free_td_radarprod_meta_st)) {}
    boost::shared_ptr<td_radarprod_meta_st> meta;
    std::string varNm;
    std::string xname;
    std::string yname;
    std::string time;
};


ProradXMLCDMReader::ProradXMLCDMReader(const std::string& source)
: CDMReader(), pimpl_(new ProradXMLImpl())
{
    using namespace std;
    LOG4FIMEX(logger, Logger::DEBUG, "reading file "+ source );
    cdm_->addAttribute(CDM::globalAttributeNS(), CDMAttribute("Conventions", "CF-1.6"));
    cdm_->addAttribute(CDM::globalAttributeNS(), CDMAttribute("history", "fimex creation from " + source));

    LIBXML_TEST_VERSION
    xmlInitParser();

    init_td_radarprod_meta_st(pimpl_->meta.get());
    if ( !prorad_xmlprod_read(pimpl_->meta.get(), const_cast<char*>(source.c_str())) ) {
        if (pimpl_->meta->cartesian_st == 0) {
            throw CDMException("Unable to read prorad-xml file: " + source + ": not a cartesian product");
        }
        td_cartesian_st* cartesian = pimpl_->meta->cartesian_st;
        pimpl_->time = "time";
        {   // time
            CDMDimension tDim(pimpl_->time, 1);
            tDim.setUnlimited(1);
            cdm_->addDimension(tDim);
            vector<string> tshape;
            tshape.push_back(pimpl_->time);
            CDMVariable tVar(pimpl_->time, CDM_INT, tshape);
            boost::shared_array<int> t(new int[1]());
            t[0] = pimpl_->meta->product_epoch;
            tVar.setData(createData(1, t));
            cdm_->addVariable(tVar);
            cdm_->addAttribute(pimpl_->time, CDMAttribute("units", "seconds since 1970-01-01 00:00:00 +00:00"));
        }
        boost::shared_ptr<Projection> proj = Projection::createByProj4(cartesian->projdef);
        if (proj->getName() == LatitudeLongitudeProjection::NAME()) {
            pimpl_->xname = "lon";
            pimpl_->yname = "lat";
        } else if (proj->getName() == RotatedLatitudeLongitudeProjection::NAME()) {
            pimpl_->xname = "rlon";
            pimpl_->yname = "rlat";

        } else {
            pimpl_->xname = "x";
            pimpl_->yname = "y";
        }
        {
            cdm_->addDimension(CDMDimension(pimpl_->xname, cartesian->x_size));
            cdm_->addDimension(CDMDimension(pimpl_->yname, cartesian->y_size));
            vector<string> shape(1);
            shape[0] = pimpl_->xname;
            cdm_->addVariable(CDMVariable(pimpl_->xname, CDM_DOUBLE, shape));
            shape[0] = pimpl_->yname;
            cdm_->addVariable(CDMVariable(pimpl_->yname, CDM_DOUBLE, shape));
            // Projection
            shape.resize(0);
            cdm_->addVariable(CDMVariable(proj->getName(), CDM_SHORT, shape));
            vector<CDMAttribute> attrs = proj->getParameters();
            for (vector<CDMAttribute>::iterator it = attrs.begin(); it != attrs.end(); ++it) {
                cdm_->addAttribute(proj->getName(), *it);
            }
        }
        if (proj->getName() == LatitudeLongitudeProjection::NAME()) {
            cdm_->addAttribute(pimpl_->xname, CDMAttribute("units", "degrees_east"));
            cdm_->addAttribute(pimpl_->yname, CDMAttribute("units", "degrees_north"));
        } else if (proj->getName() == RotatedLatitudeLongitudeProjection::NAME()) {
            cdm_->addAttribute(pimpl_->xname, CDMAttribute("units", "degrees"));
            cdm_->addAttribute(pimpl_->yname, CDMAttribute("units", "degrees"));
            cdm_->addAttribute(pimpl_->xname, CDMAttribute("standard_name", "grid_longitude"));
            cdm_->addAttribute(pimpl_->yname, CDMAttribute("standard_name", "grid_latitude"));
        } else {
            cdm_->addAttribute(pimpl_->xname, CDMAttribute("units", "m"));
            cdm_->addAttribute(pimpl_->yname, CDMAttribute("units", "m"));
            cdm_->addAttribute(pimpl_->xname, CDMAttribute("standard_name", "projection_x_axis"));
            cdm_->addAttribute(pimpl_->yname, CDMAttribute("standard_name", "projection_y_axis"));
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
            cdm_->getVariable(pimpl_->xname).setData(createData(cartesian->x_size, xvals));
            cdm_->getVariable(pimpl_->yname).setData(createData(cartesian->y_size, yvals));
        }
        //coordinates
        string coordinates;
        if (proj->getName() != LatitudeLongitudeProjection::NAME()) {
            //
            // data will be added to only on request in getDataSlice
            vector<string> shape(2);
            shape[0] = pimpl_->xname;
            shape[1] = pimpl_->yname;
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
            shape[0] = pimpl_->xname;
            shape[1] = pimpl_->yname;
            shape[2] = pimpl_->time;
            pimpl_->varNm = string(cartesian->datatype) + "_radar";
            cdm_->addVariable(CDMVariable(pimpl_->varNm, CDM_SHORT, shape));
            cdm_->addAttribute(pimpl_->varNm, CDMAttribute("_FillValue", static_cast<short>(-32767)));
            //cdm_->addAttribute(varNm, CDMAttribute("units", cartesian->))
            cdm_->addAttribute(pimpl_->varNm, CDMAttribute("scale_factor", cartesian->alfa));
            cdm_->addAttribute(pimpl_->varNm, CDMAttribute("add_offset", cartesian->beta));
            if (coordinates != "") {
                cdm_->addAttribute(pimpl_->varNm, CDMAttribute("coordinates", coordinates));
            }
            cdm_->addAttribute(pimpl_->varNm, CDMAttribute("grid_mapping", proj->getName()));
            boost::shared_array<short> d(new short[cartesian->buffer_size]());
            for (size_t i = 0; i < cartesian->buffer_size; ++i) {
                if (cartesian->flags[i].is_nodata) {
                    d[i] = -32767;
                } else {
                    d[i] = cartesian->buffer[i];
                }
            }
            DataPtr dp = createData(cartesian->buffer_size, d);
            cdm_->getVariable(pimpl_->varNm).setData(dp);

            string extraVars[] = {"nodata", "lowele", "highele", "blocked",
                                  "seaclutter", "groundclutter", "otherclutter", "status_flag",
                                  "classification", "block_percent", "clutter_probability"};
            for (size_t i = 0; i < 11; ++i) {
                string flgNm = string(cartesian->datatype) + "_" + extraVars[i] + "_radar";
                cdm_->addVariable(CDMVariable(flgNm, CDM_CHAR, shape));
                if (coordinates != "") {
                    cdm_->addAttribute(flgNm, CDMAttribute("coordinates", coordinates));
                }
            }
            string stFlgNm = string(cartesian->datatype) + "_status_flag_radar";
            cdm_->addAttribute(stFlgNm, CDMAttribute("standard_name", pimpl_->varNm + " status_flag")); // should link to standard_name, not varNm
            boost::shared_array<char> mask(new char[7]);
            for (size_t i = 0; i < 7; i++) {
                mask[i] = 1 << i;
            }
            cdm_->addAttribute(stFlgNm, CDMAttribute("flag_masks", CDM_CHAR, createData(7, mask)));
            cdm_->addAttribute(stFlgNm, CDMAttribute("flag_meanings", "no_data low_elevation high_elevation blocked sea_clutter ground_clutter other_clutter"));
            string percFlgNm = string(cartesian->datatype) + "_block_percent_radar";
            cdm_->addAttribute(percFlgNm, CDMAttribute("units", "%"));
            string probFlgNm = string(cartesian->datatype) + "_clutter_probability_radar";
            cdm_->addAttribute(probFlgNm, CDMAttribute("units", "%"));

            if (pimpl_->varNm == "dbz") {
                cdm_->addAttribute(pimpl_->varNm + "_radar", CDMAttribute("standard_name", "equivalent_reflectivity_factor"));
                // following unit allowed in CF, but not in udunits2:
                cdm_->addAttribute(pimpl_->varNm + "_radar", CDMAttribute("units", "dBZ"));
                cdm_->addOrReplaceAttribute(stFlgNm, CDMAttribute("standard_name", "equivalent_reflectivity_factor status_flag"));
            }

        }
    } else {
        throw CDMException("Unable to read prorad-xml file: " + source);
    }
}


#define PRGETDATAFLAG(XXX, YYY) if (varName.find(std::string("_") + XXX +"_radar") != std::string::npos) { \
    td_cartesian_st* cartesian = pimpl_->meta->cartesian_st; \
    boost::shared_array<char> d(new char[cartesian->buffer_size]()); \
    for (size_t i = 0; i < cartesian->buffer_size; ++i) { \
        d[i] = cartesian->flags[i].YYY ; \
    } \
    return createData(cartesian->buffer_size, d); \
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

    PRGETDATAFLAG("nodata", is_nodata)
    PRGETDATAFLAG("lowele", is_lowele)
    PRGETDATAFLAG("highele", is_highele)
    PRGETDATAFLAG("blocked", is_blocked)
    PRGETDATAFLAG("seaclutter", is_seaclutter)
    PRGETDATAFLAG("groundclutter", is_groundclutter)
    PRGETDATAFLAG("otherclutter", is_otherclutter)
    PRGETDATAFLAG("classification", classification)
    PRGETDATAFLAG("block_percent", block_percent)
    PRGETDATAFLAG("clutter_probability", clutter_probability)
    if (varName.find("status_flag_radar") != std::string::npos) {
        td_cartesian_st* cartesian = pimpl_->meta->cartesian_st;
        boost::shared_array<char> d(new char[cartesian->buffer_size]());
        for (size_t i = 0; i < cartesian->buffer_size; ++i) {
            td_flags_st flag = cartesian->flags[i];
            d[i] = flag.is_nodata | (flag.is_lowele << 1) | (flag.is_highele << 2) |
                    (flag.is_blocked << 3) |
                    (flag.is_seaclutter << 4) | (flag.is_groundclutter << 5) | (flag.is_otherclutter << 6);
        }
        return createData(cartesian->buffer_size, d);
    }

    return createData(CDM_NAT, 0);
}


} /* namespace MetNoFimex */
