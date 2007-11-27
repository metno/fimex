#include "Utils.h"
#include <cstdlib>
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>
#include <exception>

// include projects.h since I need to access some of projs internals (proj -V)
#include <projects.h>

namespace MetNoUtplukk
{

std::vector<CDMAttribute> projStringToAttributes(std::string projStr)
{
	// init projections
	// make sure that pj is freed when going out of scope
	boost::shared_ptr<PJ> pj(pj_init_plus(projStr.c_str()), pj_free); 
	if (!pj.get()) {
		throw std::exception(); // not initialized
	}
	FACTORS factors;
	LP lp;
	boost::smatch what;
	if (boost::regex_search(projStr, what, boost::regex("\\+lat_0=(\\S+)"))) {
		// lambda
		lp.u = std::strtod(what[1].str().c_str(), (char **)NULL);
	} else {
		lp.u = 0;
	}
	if (boost::regex_search(projStr, what, boost::regex("\\+lon_0=(\\S+)"))) {
		// phi
		lp.v = std::strtod(what[1].str().c_str(), (char **)NULL);
	} else {
		lp.v = 0;
	}
	if (!pj_factors(lp, pj.get(), 0., &factors)) {
		//throw std::exception();
	}
	std::string projType;
	std::vector<CDMAttribute> attrList;
	if (boost::regex_search(projStr, what, boost::regex("\\+proj=(\\S+)"))) {
		projType = what[1].str();
	} else {
		throw std::exception();
	}
	std::cerr << projType << lp.u << lp.v << std::endl;
	return attrList;
}

}
