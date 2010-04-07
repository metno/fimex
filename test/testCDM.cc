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

#include "fimex/config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;


#include "fimex/CDM.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include <iostream>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE(test_cdm) {
	CDM cdm;
	BOOST_CHECK(true); // cdm initialized

}


BOOST_AUTO_TEST_CASE( test_variable) {
	vector<std::string> noDim;
	string varName("test");
	CDMVariable testVar(varName, CDM_NAT, noDim);
	CDM cdm;
	cdm.addVariable(testVar);
	BOOST_CHECK(cdm.hasVariable(varName));

	try {
		CDMVariable failVar(varName, CDM_NAT, noDim);
		cdm.addVariable(failVar); // adding new variable with same name should fail
		BOOST_CHECK(false);
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}

	CDMVariable& varRef = cdm.getVariable(varName);
	BOOST_CHECK(varRef.getName() == varName);
	try {
		cdm.getVariable("dummy");
		BOOST_CHECK(false);
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}

	string newName = varName + "xx";
	cdm.renameVariable(varName, newName);
	BOOST_CHECK(cdm.hasVariable(newName));
	BOOST_CHECK(!cdm.hasVariable(varName));
	BOOST_CHECK(cdm.getVariable(newName).getName() == newName);

	cdm.removeVariable(newName);
	BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( test_attributes)
{
	CDM cdm;
	string varName("test");
	vector<std::string> noDim;
	CDMVariable testVar(varName, CDM_NAT, noDim);
	cdm.addVariable(testVar);
	string varName2("test2");
	CDMVariable testVar2(varName2, CDM_NAT, noDim);
	cdm.addVariable(testVar2);
	string varName3("test3");

	cdm.addAttribute(varName, CDMAttribute("attr", "value"));
	cdm.addAttribute(varName, CDMAttribute("attr2", "value"));
	cdm.addAttribute(varName2, CDMAttribute("attr", "value"));
	cdm.addAttribute(varName2, CDMAttribute("attr2", "valueX"));

	vector<std::string> vars = cdm.findVariables("attr", "value");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) != vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
	vars = cdm.findVariables("attr2", "valueX");
	BOOST_CHECK(find(vars.begin(), vars.end(), varName) == vars.end());
	BOOST_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());

	/* find attributes of renamed variables */
	cdm.renameVariable(varName2, varName3);
    vars = cdm.findVariables("attr", "value");
    BOOST_CHECK(find(vars.begin(), vars.end(), varName2) == vars.end());
    BOOST_CHECK(find(vars.begin(), vars.end(), varName3) != vars.end());


	try {
		cdm.addAttribute(varName, CDMAttribute("attr", "value"));
		BOOST_CHECK(false); // should throw an error
	} catch (CDMException& ex) {
		BOOST_CHECK(true);
	}
	cdm.addOrReplaceAttribute(varName, CDMAttribute("attr", "valueNew"));
	BOOST_CHECK(cdm.findVariables("attr", "valueNew").size() > 0);
	cdm.removeAttribute("bla", "blub");
	BOOST_CHECK(true); // no error

	cdm.removeAttribute(varName, "attr");
	BOOST_CHECK(cdm.findVariables("attr", "valueNew").size() == 0);
}

BOOST_AUTO_TEST_CASE( test_dimension)
{
    CDM cdm;
    string dim1Str("dim1");
    string dim2Str("dim2");
    string dim3Str("dim3");
    CDMDimension dim1(dim1Str, 5);
    CDMDimension dim2(dim2Str, 6);

    cdm.addDimension(dim1);
    cdm.addDimension(dim2);

    vector<std::string> varDims;
    varDims.push_back(dim1Str);
    varDims.push_back(dim2Str);
    CDMVariable var("var", CDM_NAT, varDims);
    cdm.addVariable(var);

    BOOST_CHECK(cdm.getDimension(dim1Str).getName() == dim1Str);
    BOOST_CHECK(cdm.hasDimension(dim2Str));
    BOOST_CHECK(!cdm.hasDimension(dim3Str));
    BOOST_CHECK(cdm.getVariable("var").checkDimension(dim2Str));

    BOOST_CHECK(cdm.testDimensionInUse(dim2Str));
    BOOST_CHECK(!cdm.testDimensionInUse(dim3Str));


    cdm.renameDimension(dim2Str, dim3Str);
    BOOST_CHECK(cdm.hasDimension(dim3Str));
    BOOST_CHECK(!cdm.hasDimension(dim2Str));
    BOOST_CHECK(cdm.getVariable("var").checkDimension(dim3Str));
    BOOST_CHECK(!cdm.getVariable("var").checkDimension(dim2Str));


    try {
        cdm.renameDimension(dim3Str, dim1Str); // fails, dim1Str in use
        BOOST_CHECK(false);
    } catch (CDMException& ex) {
        BOOST_CHECK(true);
    }

    cdm.addDimension(dim2);
    BOOST_CHECK(!cdm.testDimensionInUse(dim2Str));
    BOOST_CHECK(cdm.removeDimension(dim2Str));
    BOOST_CHECK(!cdm.removeDimension(dim2Str));

}

BOOST_AUTO_TEST_CASE( test_coordinateSystem)
{
	// preparing a cs
	CDM cdm;

	cdm.addAttribute(cdm.globalAttributeNS(), CDMAttribute("Conventions", "CF-1.0"));

	string x("x");
	string y("y");
	string p("p");
	string t("t");
    string l("l");
	string var("var");
	string var2("var2");
	string var3d("var3d");
	cdm.addDimension(CDMDimension(x, 1));
	cdm.addDimension(CDMDimension(y, 1));
	cdm.addDimension(CDMDimension(p, 1));
    cdm.addDimension(CDMDimension(l, 1));
	cdm.addDimension(CDMDimension(t, 1));
	vector<std::string> shape;
	shape.push_back(x);
	shape.push_back(y);
	shape.push_back(t);
	shape.push_back(p);
	shape.push_back(l);
	for (vector<string>::iterator sit = shape.begin(); sit != shape.end(); ++sit) {
		vector<string> dimShape;
		dimShape.push_back(*sit);
		cdm.addVariable(CDMVariable(*sit, CDM_INT, dimShape));
	}
	shape.pop_back(); // remove l
	cdm.addVariable(CDMVariable(var, CDM_INT, shape));
	shape.pop_back(); // remove p
	shape.push_back(l);
	cdm.addVariable(CDMVariable(var2, CDM_INT, shape));
	shape.pop_back(); // remove l
	cdm.addVariable(CDMVariable(var3d, CDM_INT, shape));

	// define units
	cdm.addAttribute(x, CDMAttribute("units", "m"));
	cdm.addAttribute(y, CDMAttribute("units", "m"));
	cdm.addAttribute(p, CDMAttribute("units", "bar"));
	cdm.addAttribute(l, CDMAttribute("positive", "UP"));
	cdm.addAttribute(t, CDMAttribute("units", "days since 1973-06-26 09:51:00"));

	// define projection params
	cdm.addAttribute(x, CDMAttribute("standard_name", "projection_x_coordinate"));
	cdm.addAttribute(y, CDMAttribute("standard_name", "projection_y_coordinate"));

#if 0
	typedef std::vector<boost::shared_ptr<const CoordinateSystem> > CsList;
	CsList cs = listCoordinateSystems(cdm);
	for (CsList::const_iterator cit = cs.begin(); cit != cs.end(); ++cit) {
	    std::cerr << **cit << std::endl;
	}
#endif

	BOOST_CHECK(x == cdm.getHorizontalXAxis(var));
	BOOST_CHECK(y == cdm.getHorizontalYAxis(var));
	BOOST_CHECK(t == cdm.getTimeAxis(var));
	BOOST_CHECK(p == cdm.getVerticalAxis(var));
	BOOST_CHECK(l == cdm.getVerticalAxis(var2));
	BOOST_CHECK("" == cdm.getVerticalAxis(var3d));

	// test cdm.getLatitudeLongitude
	string lat("lat");
	string lon("lon");
	vector<string> xyShape;
	xyShape.push_back(x);
	xyShape.push_back(y);
	cdm.addVariable(CDMVariable(lat, CDM_INT, xyShape));
	cdm.addVariable(CDMVariable(lon, CDM_INT, xyShape));
	cdm.addAttribute(lat, CDMAttribute("units", "degreesN"));
	cdm.addAttribute(lon, CDMAttribute("units", "degreesE"));
	cdm.addAttribute(var, CDMAttribute("coordinates", lat + " " + lon));

	string latRetVal, lonRetVal;
	BOOST_CHECK(cdm.getLatitudeLongitude(var, latRetVal, lonRetVal));
	BOOST_CHECK(latRetVal == lat);
	BOOST_CHECK(lonRetVal == lon);

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
