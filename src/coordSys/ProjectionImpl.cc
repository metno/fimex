/*
 * Fimex, ProjectionImpl.cc
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#include "fimex/coordSys/ProjectionImpl.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include <algorithm>
#include <iostream>
#include <sstream>
#include "boost/regex.hpp"
#include <cmath>


namespace MetNoFimex
{

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

const bool ProjectionImpl::isDegree() const
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

    // use towgs84=0,0,0 as default datum to make sure that proj starts
    // geodetic->gecentric conversion -- this needs to be changed
    // when a future CF adds Bursa Wolf parameters or similar
    out << " towgs84=0,0,0";

    return out.str();
}


bool ProjectionImpl::addParameterToStream(std::ostream& outStream, const std::string& name, std::string replaceName) const
{
    std::vector<CDMAttribute>::const_iterator found = std::find_if(params_.begin(), params_.end(), CDMNameEqual(name));
    if (found != params_.end()) {
        if (replaceName == "") replaceName = name;
        if (found->getData()->size() > 0) {
            outStream << replaceName << found->getData()->asConstDouble()[0];
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
    boost::smatch what;
    if (boost::regex_search(proj4Str, what, boost::regex("\\+proj=(\\S+)"))) {
        return what[1].str() == name;
    } else {
        return false;
    }
}

void ProjectionImpl::proj4GetEarthAttributes(const std::string& proj4Str, std::vector<CDMAttribute>& attrList)
{
    boost::smatch what;
    // this assumes currently all units to be 'm'
    // convert x_0 to false_easting, y_0 to false_northing
    if (boost::regex_search(proj4Str, what, boost::regex("\\+x_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_easting", string2type<double>(what[1].str())));
    }
    if (boost::regex_search(proj4Str, what, boost::regex("\\+y_0=(\\S+)"))) {
        attrList.push_back(CDMAttribute("false_northing", string2type<double>(what[1].str())));
    }

    // a (or R) and e (or b) to semi_major_axis and semi_minor_axis
    if (boost::regex_search(proj4Str, what, boost::regex("\\+[aR]=(\\S+)"))) {
        double major_axis = string2type<double>(what[1].str());
        attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
        if (boost::regex_search(proj4Str, what, boost::regex("\\+e=(\\S+)")) && what[1].str() != "0") {
            double ecc = string2type<double>(what[1].str());
            double minor_axis = sqrt(major_axis*major_axis*(1-(ecc*ecc)));
            attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
            attrList.push_back(CDMAttribute("semi_minor_axis", minor_axis));
        } else if (boost::regex_search(proj4Str, what, boost::regex("\\+b=(\\S+)"))) {
            attrList.push_back(CDMAttribute("semi_major_axis", major_axis));
            attrList.push_back(CDMAttribute("semi_minor_axis", string2type<double>(what[1].str())));
        } else {
            attrList.push_back(CDMAttribute("earth_radius", major_axis));
        }
    }

    // get ellipsoid from ellps/datum, source: http://en.wikipedia.org/wiki/Figure_of_earth
    if (boost::regex_search(proj4Str, what, boost::regex("\\+ellps=(\\S+)"))
        || boost::regex_search(proj4Str, what, boost::regex("\\+datum=(\\S+)"))) {
        string ellps = string2lowerCase(what[1].str());
        // TODO: this list should be autogenerated from "proj -le"
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
}

}
