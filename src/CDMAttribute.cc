#include "CDMAttribute.h"

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <cmath>

// include projects.h since I need to access some of projs internals (proj -V)
// PJ_LIB__ required for LP.phi, LP.lam
#include <projects.h>


namespace MetNoUtplukk
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


CDMAttribute::~CDMAttribute()
{
}

void CDMAttribute::toXMLStream(std::ostream& out) const 
{
	out << "<attribute name=\"" << getName() << "\" type=\"" << datatype2string(getDataType()) << "\" value=\"" << getStringValue() << "\" />" << std::endl;
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
		if (orgProj == "latlong") {
			double north_pole_lat = 90;
			double north_pole_lon = 0;
			if (boost::regex_search(projStr, what, boost::regex("\\+o_lat_p=(\\S+)"))) {
				north_pole_lat = std::strtod(what[1].str().c_str(), (char **)NULL);
			}
			if (boost::regex_search(projStr, what, boost::regex("\\+o_lon_b=(\\S+)"))) {
				north_pole_lon = std::strtod(what[1].str().c_str(), (char **)NULL);
			}
			attrList.push_back(CDMAttribute("grid_mapping_name", "rotated_latitude_longitude"));
			 // TODO: find out if its 180-lon or lon-180 (find example file and test with java-netcdf 4.0 toolsUI)
			attrList.push_back(CDMAttribute("grid_north_pole_longitude", 180-north_pole_lon));
			attrList.push_back(CDMAttribute("grid_north_pole_latitude", north_pole_lat));			
		}
		
	}
	// TODO implement more projections (merc, ...)
	
	return attrList;
}


}
