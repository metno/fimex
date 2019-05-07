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

#include "testinghelpers.h"
#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/coordSys/CoordinateSystem.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_variable)
{
    vector<std::string> noDim;
    string varName("test");
    CDMVariable testVar(varName, CDM_NAT, noDim);
    CDM cdm;
    cdm.addVariable(testVar);
    TEST4FIMEX_CHECK(cdm.hasVariable(varName));

    CDMVariable failVar(varName, CDM_NAT, noDim);
    TEST4FIMEX_CHECK_THROW(cdm.addVariable(failVar), CDMException); // adding new variable with same name should fail

    CDMVariable& varRef = cdm.getVariable(varName);
    TEST4FIMEX_CHECK_EQ(varRef.getName(), varName);
    TEST4FIMEX_CHECK_THROW(cdm.getVariable("dummy"), CDMException);

    string newName = varName + "xx";
    cdm.renameVariable(varName, newName);
    TEST4FIMEX_CHECK(cdm.hasVariable(newName));
    TEST4FIMEX_CHECK(!cdm.hasVariable(varName));
    TEST4FIMEX_CHECK_EQ(cdm.getVariable(newName).getName(), newName);

    cdm.removeVariable(newName);
}

TEST4FIMEX_TEST_CASE(test_attribute_types)
{
    TEST4FIMEX_CHECK_EQ(CDM_FLOAT, CDMAttribute("att1", 1.0f).getDataType());
    TEST4FIMEX_CHECK_EQ(CDM_DOUBLE, CDMAttribute("att1", 1.0).getDataType());

    TEST4FIMEX_CHECK_EQ(CDM_CHAR, CDMAttribute("att1", ' ').getDataType());
    TEST4FIMEX_CHECK_EQ(CDM_STRING, CDMAttribute("att1", "value").getDataType());

    std::vector<std::string> v_1_2;
    v_1_2.push_back("1");
    v_1_2.push_back("2");
    std::vector<std::string> v_123_234;
    v_123_234.push_back("123");
    v_123_234.push_back("234");

    {
        const CDMAttribute a("att", CDM_CHAR, v_1_2);
        TEST4FIMEX_CHECK_EQ(CDM_CHAR, a.getDataType());
        const DataPtr d = a.getData();
        TEST4FIMEX_REQUIRE(d);
        TEST4FIMEX_CHECK_EQ(2, d->size());
        const shared_array<char> v = d->asChar();
        TEST4FIMEX_CHECK_EQ(1, v[0]);
        TEST4FIMEX_CHECK_EQ(2, v[1]);
    }
    {
        const CDMAttribute a("att", CDM_DOUBLE, v_1_2);
        TEST4FIMEX_CHECK_EQ(CDM_DOUBLE, a.getDataType());
        const DataPtr d = a.getData();
        TEST4FIMEX_REQUIRE(d);
        TEST4FIMEX_CHECK_EQ(2, d->size());
        const shared_array<double> v = d->asDouble();
        TEST4FIMEX_CHECK_EQ(1.0, v[0]);
        TEST4FIMEX_CHECK_EQ(2.0, v[1]);
        TEST4FIMEX_CHECK_EQ("1 2", a.getStringValue());
    }
    {
        const CDMAttribute a("att", "char", "65");
        TEST4FIMEX_CHECK_EQ(CDM_CHAR, a.getDataType());
        TEST4FIMEX_CHECK_EQ("65", a.getStringValue());
        TEST4FIMEX_CHECK_EQ(65, a.getData()->getDouble(0));
    }
    {
        const CDMAttribute a("att", "double", "1.25");
        TEST4FIMEX_CHECK_EQ(CDM_DOUBLE, a.getDataType());
        TEST4FIMEX_CHECK_EQ("1.25", a.getStringValue());
        TEST4FIMEX_CHECK_EQ(1.25, a.getData()->getDouble(0));
    }
    {
        const CDMAttribute a_string("att", "string", "hello");
        TEST4FIMEX_CHECK_EQ(CDM_STRING, a_string.getDataType());
        TEST4FIMEX_CHECK_EQ("hello", a_string.getStringValue());
    }
    {
        const CDMAttribute a("u", CDM_USHORT, v_123_234);
        TEST4FIMEX_CHECK_EQ(2, a.getData()->size());
        TEST4FIMEX_CHECK_EQ("123 234", a.getStringValue());
    }
    {
        const CDMAttribute a("u", CDM_UCHAR, v_123_234);
        TEST4FIMEX_CHECK_EQ(2, a.getData()->size());
        TEST4FIMEX_CHECK_EQ("123 234", a.getStringValue());
    }
}

TEST4FIMEX_TEST_CASE(test_attributes)
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
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName) != vars.end());
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());
    vars = cdm.findVariables("attr2", "valueX");
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName) == vars.end());
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName2) != vars.end());

    /* find attributes of renamed variables */
    cdm.renameVariable(varName2, varName3);
    vars = cdm.findVariables("attr", "value");
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName2) == vars.end());
    TEST4FIMEX_CHECK(find(vars.begin(), vars.end(), varName3) != vars.end());

    TEST4FIMEX_CHECK_THROW(cdm.addAttribute(varName, CDMAttribute("attr", "value")), CDMException); // should throw an error

    cdm.addOrReplaceAttribute(varName, CDMAttribute("attr", "valueNew"));
    TEST4FIMEX_CHECK(cdm.findVariables("attr", "valueNew").size() > 0);
    cdm.removeAttribute("bla", "blub");

    cdm.removeAttribute(varName, "attr");
    TEST4FIMEX_CHECK_EQ(cdm.findVariables("attr", "valueNew").size(), 0);
}

TEST4FIMEX_TEST_CASE(test_dimension)
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

    TEST4FIMEX_CHECK_EQ(cdm.getDimension(dim1Str).getName(), dim1Str);
    TEST4FIMEX_CHECK(cdm.hasDimension(dim2Str));
    TEST4FIMEX_CHECK(!cdm.hasDimension(dim3Str));
    TEST4FIMEX_CHECK(cdm.getVariable("var").checkDimension(dim2Str));

    TEST4FIMEX_CHECK(cdm.testDimensionInUse(dim2Str));
    TEST4FIMEX_CHECK(!cdm.testDimensionInUse(dim3Str));

    cdm.renameDimension(dim2Str, dim3Str);
    TEST4FIMEX_CHECK(cdm.hasDimension(dim3Str));
    TEST4FIMEX_CHECK(!cdm.hasDimension(dim2Str));
    TEST4FIMEX_CHECK(cdm.getVariable("var").checkDimension(dim3Str));
    TEST4FIMEX_CHECK(!cdm.getVariable("var").checkDimension(dim2Str));

    TEST4FIMEX_CHECK_THROW(cdm.renameDimension(dim3Str, dim1Str), CDMException); // fails, dim1Str in use

    cdm.addDimension(dim2);
    TEST4FIMEX_CHECK(!cdm.testDimensionInUse(dim2Str));
    TEST4FIMEX_CHECK(cdm.removeDimension(dim2Str));
    TEST4FIMEX_CHECK(!cdm.removeDimension(dim2Str));
}

TEST4FIMEX_TEST_CASE(test_constructor)
{
    // test constructors and assignment
    CDM cdm, cdm2;
    cdm.addDimension(CDMDimension("test1",1));
    cdm.addDimension(CDMDimension("test2",2));
    cdm2.addDimension(CDMDimension("xxx", 1));

    // assignment
    cdm2 = cdm;
    cdm.addDimension(CDMDimension("test3",3));
    cdm2.addDimension(CDMDimension("xxx2",1));
    TEST4FIMEX_CHECK(cdm2.hasDimension("test2"));
    TEST4FIMEX_CHECK(!cdm2.hasDimension("test3"));
    TEST4FIMEX_CHECK(!cdm2.hasDimension("xxx"));
    TEST4FIMEX_CHECK(cdm2.hasDimension("xxx2"));
    TEST4FIMEX_CHECK(!cdm.hasDimension("xxx2"));

    // copy
    CDM cdm3(cdm);
    TEST4FIMEX_CHECK(cdm3.hasDimension("test1"));
    TEST4FIMEX_CHECK(cdm3.hasDimension("test2"));
    TEST4FIMEX_CHECK(cdm3.hasDimension("test3"));
    cdm.removeDimension("test3");
    TEST4FIMEX_CHECK(!cdm.hasDimension("test3"));
    TEST4FIMEX_CHECK(cdm3.hasDimension("test3"));
    cdm3.addDimension(CDMDimension("xxx2",1));
    TEST4FIMEX_CHECK(!cdm.hasDimension("xxx2"));
    TEST4FIMEX_CHECK(cdm3.hasDimension("xxx2"));
}

TEST4FIMEX_TEST_CASE(test_coordinateSystem)
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
    CoordinateSystem_cp_v cs = listCoordinateSystems(cdm);
    for (CoordinateSystem_cp_v::const_iterator cit = cs.begin(); cit != cs.end(); ++cit) {
        std::cerr << **cit << std::endl;
    }
#endif

    TEST4FIMEX_CHECK_EQ(x, cdm.getHorizontalXAxis(var));
    TEST4FIMEX_CHECK_EQ(y, cdm.getHorizontalYAxis(var));
    TEST4FIMEX_CHECK_EQ(t, cdm.getTimeAxis(var));
    TEST4FIMEX_CHECK_EQ(p, cdm.getVerticalAxis(var));
    TEST4FIMEX_CHECK_EQ(l, cdm.getVerticalAxis(var2));
    TEST4FIMEX_CHECK_EQ("", cdm.getVerticalAxis(var3d));

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
    TEST4FIMEX_CHECK(cdm.getLatitudeLongitude(var, latRetVal, lonRetVal));
    TEST4FIMEX_CHECK_EQ(latRetVal, lat);
    TEST4FIMEX_CHECK_EQ(lonRetVal, lon);
}
