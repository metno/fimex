/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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
 */

#include "fimex/CDMAttribute.h"

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <cmath>
#include "fimex/DataImpl.h"
#include "fimex/Utils.h"
#include "fimex/CDMException.h"

// include projects.h since I need to access some of projs internals (proj -V)
// PJ_LIB__ required for LP.phi, LP.lam
#include <projects.h>


namespace MetNoFimex
{

CDMAttribute::CDMAttribute()
{
}

CDMAttribute::CDMAttribute(std::string name, std::string value)
: name(name), datatype(CDM_STRING)
{
	boost::shared_array<char> cstr(new char [value.size()]);
	for (size_t i = 0; i < value.size(); i++) {
		cstr[i] = value.at(i);
	}
	data = boost::shared_ptr<Data>(new DataImpl<char>(cstr, value.size()));
}


CDMAttribute::CDMAttribute(std::string name, double value)
: name(name), datatype(CDM_DOUBLE)
{
	boost::shared_array<double> xvalue(new double[1]);
	xvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<double>(xvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, int value)
: name(name), datatype(CDM_INT)
{
	boost::shared_array<int> xvalue(new int[1]);
	xvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<int>(xvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, short value)
: name(name), datatype(CDM_SHORT)
{
	boost::shared_array<short> xvalue(new short[1]);
	xvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<short>(xvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, char value)
: name(name), datatype(CDM_CHAR)
{
	boost::shared_array<char> xvalue(new char[1]);
	xvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<char>(xvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, float value)
: name(name), datatype(CDM_FLOAT)
{
	boost::shared_array<float> xvalue(new float[1]);
	xvalue[0] = value;
	data = boost::shared_ptr<Data>(new DataImpl<float>(xvalue, 1));
}

CDMAttribute::CDMAttribute(std::string name, CDMDataType datatype, boost::shared_ptr<Data> data)
: name(name), datatype(datatype), data(data)
{
}

CDMAttribute::CDMAttribute(const std::string& name, const std::string& datatype, const std::string& value) throw(CDMException)
: name(name)
{
	this->datatype = string2datatype(datatype);
	std::vector<std::string> vals;
	vals.push_back(value);
	initDataByArray(vals);
}

CDMAttribute::CDMAttribute(const std::string& name, CDMDataType datatype, const std::vector<std::string>& values) throw(CDMException)
: name(name), datatype(datatype)
{
    initDataByArray(values);
}

void CDMAttribute::initDataByArray(const std::vector<std::string>& values)
{
    switch (datatype) {
    case CDM_FLOAT: {
        initDataArray<float>(values);
        break;
    }
    case CDM_DOUBLE: {
        initDataArray<double>(values);
        break;
    }
    case CDM_INT:  {
        initDataArray<int>(values);
        break;
    }
    case CDM_SHORT: {
        initDataArray<short>(values);
        break;
    }
    case CDM_CHAR: {
        initDataArray<char>(values);
        break;
    }
    case CDM_STRING: {
        /* string may only have one dimension */
        *this = CDMAttribute(name, join(values.begin(), values.end(), " "));
        break;
    }
    default: {
        throw CDMException("Unknown type to generate attribute " + name);
    }
    }
}

CDMAttribute::~CDMAttribute()
{
}

const std::string CDMAttribute::getStringValue() const
{
    return data->asString();
}

void CDMAttribute::toXMLStream(std::ostream& out) const
{
	out << "<attribute name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" value=\"" << getStringValue() << "\" />" << std::endl;
}

/* init data arrays for all types */
template<typename T>
void CDMAttribute::initDataArray(const std::vector<std::string>& values) {
    std::vector<T> vec;
    std::transform(values.begin(), values.end(), std::back_inserter(vec), &string2type<T>);
    data = createData(datatype, values.size(), vec.begin(), vec.end());
}


std::vector<CDMAttribute> projStringToAttributes(std::string projStr)
{
	// init projections
	// make sure that pj is freed when going out of scope
	boost::shared_ptr<PJ> pj(pj_init_plus(projStr.c_str()), pj_free);
	if (!pj.get()) {
		std::cerr << "pj_init error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
		throw std::exception(); // not initialized
	}
	LP lp, lpOrg;
	boost::smatch what;
	if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
		lpOrg.u = std::strtod(what[1].str().c_str(), (char **)NULL);
		lp.u = DEG_TO_RAD * lpOrg.u;
	} else {
		lpOrg.u = 0;
		lp.u = 0;
	}
	if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
		lpOrg.v = std::strtod(what[1].str().c_str(), (char **)NULL);
		lp.v = DEG_TO_RAD * lpOrg.v;
		// work around HALFPI which is singularity in proj (Proj BUGZILLA: 1605)
		double delta = std::fabs(lp.v) - HALFPI;
		const double DerivDelta(1e-5);
		if (std::fabs(delta) < DerivDelta) {
			lp.v = (lp.v > 0) ? (HALFPI - DerivDelta) : (-1*HALFPI + DerivDelta);
		}
	} else {
		lpOrg.v = 0;
		lp.v = 0;
	}

	std::string projType;
	if (boost::regex_search(projStr, what, boost::regex("\\+proj=(\\S+)"))) {
		projType = what[1].str();
	} else {
		std::cerr << "no projection found" << std::endl;
		throw std::exception();
	}

	std::vector<CDMAttribute> attrList;
	attrList.push_back(CDMAttribute("proj4", projStr));
	if (projType == "stere") {
		// stereographic projection
		// pj_factors
		FACTORS factors;
		factors.code = 0; // flag what to calculate. 0 means calc everything? undocumented, default: random
		if (pj_factors(lp, pj.get(), 0., &factors) != 0) {
			std::cerr << "pj_factors error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
			throw std::exception();
		}
		attrList.push_back(CDMAttribute("grid_mapping_name", "stereographic"));
		attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", factors.k));
		attrList.push_back(CDMAttribute("longitude_of_projection_origin", lpOrg.u));
		attrList.push_back(CDMAttribute("latitude_of_projection_origin", lpOrg.v));
	} else if (projType == "ob_tran") {
		// rotated pole, at least for felt?
		std::string orgProj;
		if (boost::regex_search(projStr, what, boost::regex("\\+o_proj=(\\S+)"))) {
			orgProj = what[1].str();
		} else {
			std::cerr << "no o_proj found" << std::endl;
			throw std::exception();
		}
		if (orgProj == "latlong" || orgProj == "longlat" || orgProj == "eqc") {
			double north_pole_lat = 90;
			double north_pole_lon = 0;
			if (boost::regex_search(projStr, what, boost::regex("\\+o_lat_p=(\\S+)"))) {
				north_pole_lat = string2type<double>(what[1].str());
			}
            // ignore optional o_lon_b (rotation after lat rotation)
            // since it doesn't match with FGDC parameters
			// just use lon_0 (lon rotation in the original system)
			if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
				north_pole_lon = string2type<double>(what[1].str());
			}
			attrList.push_back(CDMAttribute("grid_mapping_name", "rotated_latitude_longitude"));
			attrList.push_back(CDMAttribute("grid_north_pole_longitude", 180+north_pole_lon));
			attrList.push_back(CDMAttribute("grid_north_pole_latitude", north_pole_lat));
		}

	} else if (projStr == "merc") {
        attrList.push_back(CDMAttribute("grid_mapping_name", "mercator"));

        // longitude at origin
        double longOfProjOrigin = 0.;
	    if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
	        longOfProjOrigin = string2type<double>(what[1].str());
	    }
        attrList.push_back(CDMAttribute("longitude_of_projection_origin", longOfProjOrigin));

        // standard_paralll or scale_factor
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_ts=(\\S+)"))) {
            double standardParallel = string2type<double>(what[1].str());
            attrList.push_back(CDMAttribute("standard_parallel", standardParallel));
        }
        if (boost::regex_search(projStr, what, boost::regex("\\+k=(\\S+)"))) {
            attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));
        }
    } else if (projStr == "lcc") {
	    // lambert conic conformal
        attrList.push_back(CDMAttribute("grid_mapping_name", "lambert_conformal_conic"));

        double lat1 = 0.;
        double lat2 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_1=(\\S+)"))) {
            lat1 = string2type<double>(what[1].str());
        }
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_2=(\\S+)"))) {
            lat2 = string2type<double>(what[1].str());
        }
        if (lat1 == lat2) {
            attrList.push_back(CDMAttribute("standard_parallel", lat1));
        } else {
            boost::shared_ptr<Data> stdParallels = createData(CDM_DOUBLE, 2);
            stdParallels->setValue(0, lat1);
            stdParallels->setValue(1, lat2);
            CDMAttribute("standard_parallel", CDM_DOUBLE, stdParallels);
        }
        attrList.push_back(CDMAttribute("scale_factor_at_projection_origin", string2type<double>(what[1].str())));

        double lon0 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
            lon0 = string2type<double>(what[1].str());
        }
        attrList.push_back(CDMAttribute("longitude_of_central_meridian", lon0));

        double lat0 = 0.;
        if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
            lat0 = string2type<double>(what[1].str());
        }
        attrList.push_back(CDMAttribute("latitude_of_projection_origin", lat0));
    } else {
	    std::cerr << "translation of proj4 '" << projStr << "' to FGDC/CF not supported" << std::endl;
	    throw CDMException("proj-string "+projStr+" to FGDC/CF not supported");
	}

	// convert x_0 to false_easting, y_0 to false_northing
	if (boost::regex_search(projStr, what, boost::regex("\\+x_0=(\\S+)"))) {
	    attrList.push_back(CDMAttribute("false_easting", string2type<double>(what[1].str())));
	}
    if (boost::regex_search(projStr, what, boost::regex("\\+y_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_northing", string2type<double>(what[1].str())));
    }

	return attrList;
}

std::string attributesToProjString(const std::vector<CDMAttribute>& attrs)
{
	// TODO really implement conversion from FGDC, currently only using proj4 attribute (non-standard)
	std::string projString;
	for (std::vector<CDMAttribute>::const_iterator atIt = attrs.begin(); atIt != attrs.end(); ++atIt) {
		if (atIt->getName() == "proj4") {
			projString = atIt->getStringValue();
		}
	}


	return projString;
}


}
