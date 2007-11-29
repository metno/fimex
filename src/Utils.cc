#include "Utils.h"
#include <cstdlib>
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <exception>
#include <cmath>

// include projects.h since I need to access some of projs internals (proj -V)
// PJ_LIB__ required for LP.phi, LP.lam
#include <projects.h>

namespace MetNoUtplukk
{

std::vector<CDMAttribute> projStringToAttributes(std::string projStr)
{
	// init projections
	// make sure that pj is freed when going out of scope
	const double DerivDelta(1e-5);
	boost::shared_ptr<PJ> pj(pj_init_plus(projStr.c_str()), pj_free); 
	if (!pj.get()) {
		std::cerr << "pj_init error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
		throw std::exception(); // not initialized
	}
	FACTORS factors;
	LP lp;
	boost::smatch what;
	if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
		lp.u = DEG_TO_RAD * std::strtod(what[1].str().c_str(), (char **)NULL);
	} else {
		lp.u = 0;
	}
	if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
		lp.v = DEG_TO_RAD * std::strtod(what[1].str().c_str(), (char **)NULL);
		// work around HALFPI which is singularity in proj (Proj BUGZILLA: 1605)
		double delta = std::fabs(lp.v) - HALFPI;
		if (std::fabs(delta) < DerivDelta) {
			lp.v = (lp.v > 0) ? (HALFPI - DerivDelta) : (-1*HALFPI + DerivDelta);
		}
	} else {
		lp.v = 0;
	}

	// pj_factors requires one conversion before it will work
	LP lptest = pj_fwd(lp, pj.get());
	if (pj_factors(lp, pj.get(), 0., &factors) != 0) {
		std::cerr << "pj_factors error: " << pj_errno << " " << pj_strerrno(pj_errno) << std::endl;
		throw std::exception();
	}
	std::string projType;
	std::vector<CDMAttribute> attrList;
	if (boost::regex_search(projStr, what, boost::regex("\\+proj=(\\S+)"))) {
		projType = what[1].str();
	} else {
		std::cerr << "no projection found" << std::endl;
		throw std::exception();
	}

	if (projType == "stere") {
		// stereographic projection
		std::string s("");
		s+= lp.u;
		CDMAttribute name("grid_mapping_name", "stereographic");
		// TODO: set double values, test
		CDMAttribute lon_opo("longitude_of_projection_origin", s);
//		CDMAttribute lat_opo("latitude_of_projection_origin", std::string(lp.v));
//		CDMAttribute scale("scale_factor_at_projection_origin", std::string(factors.k));
		attrList.push_back(name);
		attrList.push_back(lon_opo);
//		attrList.push_back(lat_opo);
//		attrList.push_back(scale);
	}
	
	return attrList;
}

}
