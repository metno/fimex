/*
 * Fimex, ProjectionImpl.cc
 *
 * (C) Copyright 2010-2019, met.no
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/ProjectionImpl.h"

#include "fimex/CDMException.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/TokenizeDotted.h"

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <regex>
#include <sstream>

namespace MetNoFimex {

using namespace std;

ProjectionImpl::ProjectionImpl(std::string name, bool isDegree)
: name_(name), isDegree_(isDegree)
{
    // set a default, spherical world, might be overwritten later
    params_.push_back(CDMAttribute("earth_radius", MIFI_EARTH_RADIUS_M));
}

ProjectionImpl::~ProjectionImpl() {}

std::vector<CDMAttribute> ProjectionImpl::getParameters() const
{
    return params_;
}

void ProjectionImpl::addParameter(CDMAttribute attribute)
{
    // ensure that name not changes
    if (attribute.getName() == "grid_mapping_name") {
        if (attribute.getStringValue() != getName()) {
            throw CDMException("cannot change grid_mapping_name of projection from "+getName()+ " to "+attribute.getStringValue());
        }
    }

    // earth can be set with a) earth_radius
    //                       b) semi_major_axis + optional semi_minor_axis + optional inverse_flattening
    string semi_set[] = {"semi_major_axis", "semi_minor_axis", "inverse_flattening"};
    if (attribute.getName() == "earth_radius") {
        for (int i = 0; i < 3; ++i) {
            removeParameter(semi_set[i]);
        }
    }
    for (int i = 0; i < 3; ++i) {
        if (attribute.getName() == semi_set[i]) {
            removeParameter("earth_radius");
        }
    }

    // replace the parameter
    vector<CDMAttribute>::iterator found = find_if(params_.begin(), params_.end(), CDMNameEqual(attribute.getName()));
    if (found == params_.end()) {
        params_.push_back(attribute);
    } else {
        // replace attribute
        *found = attribute;
    }
}

void ProjectionImpl::addParameters(std::vector<CDMAttribute> attributes)
{
    for (vector<CDMAttribute>::const_iterator attr = attributes.begin(); attr != attributes.end(); ++attr) {
        addParameter(*attr);
    }
}


void ProjectionImpl::removeParameter(std::string paramName)
{
    vector<CDMAttribute>::iterator found = find_if(params_.begin(), params_.end(), CDMNameEqual(paramName));
    if (found != params_.end()) params_.erase(found);
}

const std::string& ProjectionImpl::getName() const
{
    return name_;
}

bool ProjectionImpl::isDegree() const
{
    return isDegree_;
}

std::string ProjectionImpl::getProj4String() const
{
    // check for proj4 parameter
    std::vector<CDMAttribute>::const_iterator proj4Par = find_if(params_.begin(), params_.end(), CDMNameEqual("proj4"));
    if (proj4Par != params_.end()) {
        return proj4Par->getStringValue();
    }

    std::ostringstream out;
    getProj4ProjectionPart(out);

    // offset
    addParameterToStream(out, "false_easting", " +x_0=");
    addParameterToStream(out, "false_northing", " +y_0=");

    // earth parameters
    out << " " << getProj4EarthString();

    // remove default definitions
    out << " +no_defs";
    return out.str();
}

std::string ProjectionImpl::getProj4EarthString() const
{
    std::ostringstream out;
    if (addParameterToStream(out, "earth_radius", " +a=")) {
        out << " +e=0";
    } else if (addParameterToStream(out, "semi_major_axis", " +a=")) {
        out << " ";
        bool hasSemiMinor = addParameterToStream(out, "semi_minor_axis", " +b=");
        bool hasInverseFlattening = addParameterToStream(out, "inverse_flattening", " +rf=");
        if  (!(hasInverseFlattening || hasSemiMinor)) {
            out << " +e=0"; // sphere
        }
    } else {
        out << " +a=" << MIFI_EARTH_RADIUS_M << " +e=0"; // default
    }

    // use towgs84, as proposed in https://cf-pcmdi.llnl.gov/trac/ticket/80
    addParameterToStream(out, "towgs84", " +towgs84=");

    return out.str();
}

namespace {

/** Insert doubles with max precision into an std::ostream. */
template<class T>
struct MaxPrecisionFormatter {
    void operator()(std::ostream& out, const T& v) const
      { out << std::setprecision(std::numeric_limits<T>::digits10 + 1) << v; }
};

} // namespace

bool ProjectionImpl::addParameterToStream(std::ostream& outStream, const std::string& name, const std::string& replaceName) const
{
    std::vector<CDMAttribute>::const_iterator found = std::find_if(params_.begin(), params_.end(), CDMNameEqual(name));
    if (found != params_.end()) {
        const std::string& nm = replaceName.empty() ? name : replaceName;
        if (found->getDataType() == CDM_STRING) {
            outStream << nm << found->getStringValue();
            return true;
        } else if (found->getData()->size() > 0) {
            shared_array<double> d = found->getData()->asDouble();
            outStream << nm << join_formatted(&d[0], &d[0] + found->getData()->size(), MaxPrecisionFormatter<double>(), ",");
            return true;
        }
    }
    return false;
}


/** get a string representation */
std::string ProjectionImpl::toString() const
{
    std::ostringstream buffer;
    buffer << getName() << ":";
    vector<CDMAttribute> pars = getParameters();
    stable_sort(pars.begin(), pars.end(), CDMNameCompare());
    for (vector<CDMAttribute>::const_iterator par = pars.begin(); par != pars.end(); ++par) {
        buffer << par->getName() << "=" << par->getStringValue() << ";";
    }
    return buffer.str();
}

bool ProjectionImpl::proj4ProjectionMatchesName(const std::string& proj4Str, const std::string& name)
{
    std::smatch what;
    if (std::regex_search(proj4Str, what, std::regex("\\+proj=(\\S+)"))) {
        return what[1].str() == name;
    } else {
        return false;
    }
}

void ProjectionImpl::proj4GetEarthAttributes(const std::string& proj4Str, std::vector<CDMAttribute>& attrList)
{
    std::smatch what;
    // this assumes currently all units to be 'm'
    // convert x_0 to false_easting, y_0 to false_northing
    if (std::regex_search(proj4Str, what, std::regex("\\+x_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_easting", string2type<double>(what[1].str())));
    }
    if (std::regex_search(proj4Str, what, std::regex("\\+y_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_northing", string2type<double>(what[1].str())));
    }

    // a (or R) and e (or b) to semi_major_axis and semi_minor_axis
    if (std::regex_search(proj4Str, what, std::regex("\\+[aR]=(\\S+)"))) {
        double major_axis = string2type<double>(what[1].str());
        attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
        if (std::regex_search(proj4Str, what, std::regex("\\+e=(\\S+)")) && what[1].str() != "0") {
            double ecc = string2type<double>(what[1].str());
            double minor_axis = sqrt(major_axis*major_axis*(1-(ecc*ecc)));
            attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
            attrList.push_back(CDMAttribute("semi_minor_axis", minor_axis));
        } else if (std::regex_search(proj4Str, what, std::regex("\\+b=(\\S+)"))) {
            attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
            attrList.push_back(CDMAttribute("semi_minor_axis", string2type<double>(what[1].str())));
        } else {
            attrList.push_back(CDMAttribute("earth_radius", major_axis));
        }
    }

    // get ellipsoid from ellps/datum, source: http://en.wikipedia.org/wiki/Figure_of_earth
    if (std::regex_search(proj4Str, what, std::regex("\\+ellps=(\\S+)")) || std::regex_search(proj4Str, what, std::regex("\\+datum=(\\S+)"))) {
        string ellps = string2lowerCase(what[1].str());
        // this list has been manually generated from "proj -le"
        if (ellps == "wgs84") {
            attrList.push_back(CDMAttribute("semi_major_axis", 6378137.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356752.3142));
        } else if (ellps == "nad83") {
            attrList.push_back(CDMAttribute("semi_major_axis", 6378137.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356752.3));
        } else if (ellps == "grs80" || ellps == "etrs89") { // grs80 ellipsoid
            attrList.push_back(CDMAttribute("semi_major_axis", 6378137.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356752.3141));
        } else if (ellps == "nad27") { // clarke 1866 ellipsoid
            attrList.push_back(CDMAttribute("semi_major_axis", 6378206.4));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356583.8));
        } else if (ellps == "wgs72") {
            attrList.push_back(CDMAttribute("semi_major_axis", 6378135.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356750.52));
        } else if (ellps == "iers89") {
            attrList.push_back(CDMAttribute("semi_major_axis", 6378136.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356751.302));
        } else if (ellps == "iers03") {
            attrList.push_back(CDMAttribute("semi_major_axis", 6378136.6));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356751.9));
        } else if (ellps == "intl") { // international 1924 / hayford 1909
            attrList.push_back(CDMAttribute("semi_major_axis", 6378388.0));
            attrList.push_back(CDMAttribute("semi_minor_axis", 6356909.0));
        } else if (ellps == "bessel") { // Bessel 1841
            attrList.push_back(CDMAttribute("semi_major_axis", 6377397.155));
            attrList.push_back(CDMAttribute("inverse_flattening", 299.1528128));
        }
    }

    // get towgs84 parameters, see https://cf-pcmdi.llnl.gov/trac/ticket/80 (CF-1.7?)
    if (std::regex_search(proj4Str, what, std::regex("\\+towgs84=(\\S+)"))) {
        vector<double> bw = tokenizeDotted<double>(what[1].str(), ",");
        // bw (Bursa Wolff parameters)
        // should have 3,6 or 7 characters, if less, fill up with 0
        if (bw.size() <= 3) {
            bw.resize(3,0);
        } else if (bw.size() <= 6) {
            bw.resize(6,0);
        } else {
            bw.resize(7,0);
        }
        DataPtr towgs = createData(CDM_DOUBLE, bw.begin(), bw.end());
        attrList.push_back(CDMAttribute("towgs84", towgs));
    } else if (std::regex_search(proj4Str, what, std::regex("\\+datum=(\\S+)"))) {
        // towgs84 parameters implicitly defined by datum (using towgs84=0,0,0 as default)
        string datum = string2lowerCase(what[1].str());
        vector<double> bw;
        // proj -ld
        if (datum == "wgs84") {
            bw.resize(3, 0);
        } else if (datum == "ggrs87") {
            bw = vector<double>{-199.87, 74.79, 246.62};
        } else {
            bw.resize(3, 0); // default
        }
        DataPtr towgs = createData(CDM_DOUBLE, bw.begin(), bw.end());
        attrList.push_back(CDMAttribute("towgs84", towgs));
    }
}

} // namespace MetNoFimex
