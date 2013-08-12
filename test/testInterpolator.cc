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

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include <fstream>

#include <boost/foreach.hpp>

#include "FeltCDMReader2.h"
#include "fimex/NetCDF_CDMReader.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMProcessor.h"
#include "fimex/interpolation.h"
#include "fimex/Logger.h"

using namespace std;
using namespace MetNoFimex;

static int DEBUG = 0;

BOOST_AUTO_TEST_CASE(test_interpolator)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
    vector<double> xAxis, yAxis;
    for (int i = -100; i < 10; i++) {
        xAxis.push_back(i * 50000);
        yAxis.push_back(i * 50000);
    }
    interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
    //interpolator->changeProjection(MIFI_INTERPOL_COORD_NN, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m");
    //NetCDF_CDMWriter(interpolator, "testInterpolator.nc");
    //interpolator->getCDM().toXMLStream(cerr);
    BOOST_CHECK(true);
    DataPtr altitudeData = interpolator->getDataSlice("altitude");
    boost::shared_array<double> altArray = altitudeData->asDouble();
    int found = 0;
    for (size_t i = 0; i < altitudeData->size(); i++) {
        if (altArray[i] > 2000) {
            found++;
        }
    }
    BOOST_CHECK(found > 100); // at least 100 cells above 2000m

    NetCDF_CDMWriter(interpolator, "testInterpolator.nc");
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(test_interpolatorKDTree)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
    vector<double> xAxis, yAxis;
    for (int i = -100; i < 10; i++) {
        xAxis.push_back(i * 50000);
        yAxis.push_back(i * 50000);
    }
    interpolator->changeProjection(MIFI_INTERPOL_COORD_NN_KD, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
    BOOST_CHECK(true);
    DataPtr altitudeData = interpolator->getDataSlice("altitude");
    boost::shared_array<double> altArray = altitudeData->asDouble();
    int found = 0;
    for (size_t i = 0; i < altitudeData->size(); i++) {
        if (altArray[i] > 2000) {
            found++;
        }
    }
    BOOST_CHECK(found > 100); // at least 100 cells above 2000m
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(test_interpolator2coords)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/twoCoordsTest.nc");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> reader(new NetCDF_CDMReader(fileName));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(reader));
    {
        vector<double> xAxis, yAxis;
        for (int i = 0; i < 12; i++) {
            xAxis.push_back(-1705516 + i * 50000);
            yAxis.push_back(-6872225 + i * 50000);
        }
        interpolator->changeProjection(MIFI_INTERPOL_COORD_NN_KD, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
        //NetCDF_CDMWriter(interpolator, "test2coordsNNKDInterpolator.nc");
    }
    BOOST_CHECK(true);
    {
        DataPtr temp2Data = interpolator->getDataSlice("temp2");
        boost::shared_array<double> tmpArray = temp2Data->asDouble();
        int found = 0;
        for (size_t i = 0; i < temp2Data->size(); i++) {
            if (tmpArray[i] > 29000) {
                found++;
            }
        }
        BOOST_CHECK(found > 100); // at least 100 cells above 2000m
    }

    interpolator = boost::shared_ptr<CDMInterpolator>(new CDMInterpolator(reader));
    {
        vector<double> xAxis, yAxis;
        for (int i = 0; i < 12; i++) {
            xAxis.push_back(-1705516 + i * 50000);
            yAxis.push_back(-6872225 + i * 50000);
        }
        interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
        //NetCDF_CDMWriter(interpolator, "test2nearestneighborInterpolator.nc");
    }
    BOOST_CHECK(true);
    {
        int found = 0;
        DataPtr temp2Data = interpolator->getDataSlice("temp2");
        boost::shared_array<double> tmpArray = temp2Data->asDouble();
        for (size_t i = 0; i < temp2Data->size(); i++) {
            if (tmpArray[i] > 29000) {
                found++;
            }
        }
        BOOST_CHECK(found > 100); // at least 100 cells above 2000m
    }
    BOOST_CHECK(true);
}


BOOST_AUTO_TEST_CASE(test_interpolator2)
{
    string topSrcDir(TOP_SRCDIR);
    string grdFile("/disk1/opdata/hirlam20/grdn06.dat");
    ifstream inputFile(grdFile.c_str());
    if (inputFile.is_open()) {
        inputFile.close();
        boost::shared_ptr<CDMReader> feltReader = boost::shared_ptr<CDMReader>(new FeltCDMReader2(grdFile, topSrcDir+"/share/etc/felt2nc_variables_hirlam20.xml"));
        boost::shared_ptr<CDMInterpolator> interpolator = boost::shared_ptr<CDMInterpolator>(new CDMInterpolator(feltReader));
        vector<double> xAxis, yAxis;
        xAxis = vector<double>();
        yAxis = vector<double>();
        for (int i = -114; i < 114; i++) {
            xAxis.push_back(i * 50000);
        }
        for (int i = -147; i < 48; i++) {
            yAxis.push_back(i * 50000);
        }
        interpolator->changeProjection(MIFI_INTERPOL_BILINEAR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
        BOOST_CHECK(true);
        NetCDF_CDMWriter(interpolator, "testInterpolator2.nc");
        BOOST_CHECK(true);
    }
}

BOOST_AUTO_TEST_CASE(test_interpolatorRelative)
{
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(feltReader));
    interpolator->changeProjection(MIFI_INTERPOL_BILINEAR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", "0,50000,...,x;relativeStart=0", "0,50000,...,x;relativeStart=0", "m", "m");
    //interpolator->getCDM().toXMLStream(cerr);
    BOOST_CHECK(true);
    BOOST_CHECK(interpolator->getDataSlice("x")->size() == 297);
    BOOST_CHECK(interpolator->getDataSlice("y")->size() == 286);
//    NetCDF_CDMWriter(interpolator, "testInterpolator3.nc");
//    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(test_interpolatorNcml)
{
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/coordTest.nc");
    string ncmlName(topSrcDir+"/test/test.ncml");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> reader(CDMFileReaderFactory::create(MIFI_FILETYPE_NETCDF, fileName, XMLInputFile(ncmlName)));
    const CDM cdm = reader->getCDM();
    if (cdm.hasVariable("x_wind")) {
        CDMVariable var = cdm.getVariable("x_wind");
        BOOST_CHECK(var.isSpatialVector());
        BOOST_CHECK(var.getSpatialVectorCounterpart() == "y_wind");
    } else {
        BOOST_CHECK(false);
    }
    if (cdm.hasVariable("y_wind")) {
        CDMVariable var = cdm.getVariable("y_wind");
        BOOST_CHECK(var.isSpatialVector());
        BOOST_CHECK(var.getSpatialVectorCounterpart() == "x_wind");
    } else {
        BOOST_CHECK(false);
    }
}

BOOST_AUTO_TEST_CASE(test_interpolator_template)
{
    string topSrcDir(TOP_SRCDIR);
    string ncFileName(topSrcDir+"/test/erai.sfc.40N.0.75d.200301011200.nc");
    if (!ifstream(ncFileName.c_str())) {
        // no testfile, skip test
        return;
    }
    string templateFileName(topSrcDir+"/test/template_noaa17.nc");
    if (!ifstream(templateFileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> ncReader(new NetCDF_CDMReader(ncFileName));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(ncReader));
    interpolator->changeProjection(MIFI_INTERPOL_BICUBIC, templateFileName);
    BOOST_CHECK(true);
    BOOST_CHECK(interpolator->getDataSlice("x")->size() == 29);
    BOOST_CHECK(interpolator->getDataSlice("y")->size() == 31);
    BOOST_CHECK(interpolator->getDataSlice("longitude")->size() == 29*31);
    BOOST_CHECK(interpolator->getDataSlice("longitude")->size() == interpolator->getDataSlice("latitude")->size());
    BOOST_CHECK(interpolator->getCDM().hasVariable("ga_skt"));
    boost::shared_array<double> array = interpolator->getData("ga_skt")->asDouble();
    BOOST_CHECK( (!mifi_isnan(array[0])) && (array[0] < 280) && (array[0] > 270));
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(test_interpolator_latlon)
{
    double lat[] = {59.109, 59.052, 58.994, 58.934, 58.874, 58.812, 58.749, 58.685, 58.62};
    double lon[] = {4.965, 5.13, 5.296, 5.465, 5.637, 5.81, 5.986, 6.164001, 6.344};
    vector<double> latVals(&lat[0], &lat[0]+9);
    vector<double> lonVals(&lon[0], &lon[0]+9);

    string topSrcDir(TOP_SRCDIR);
    string ncFileName(topSrcDir+"/test/erai.sfc.40N.0.75d.200301011200.nc");
    if (!ifstream(ncFileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> ncReader(new NetCDF_CDMReader(ncFileName));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(ncReader));
    interpolator->changeProjection(MIFI_INTERPOL_BICUBIC, lonVals, latVals);
    BOOST_CHECK(true);
    BOOST_CHECK(interpolator->getDataSlice("longitude")->size() == lonVals.size());
    BOOST_CHECK(interpolator->getDataSlice("longitude")->size() == interpolator->getDataSlice("latitude")->size());
    BOOST_CHECK(interpolator->getCDM().hasVariable("ga_skt"));
    boost::shared_array<double> array = interpolator->getData("ga_skt")->asDouble();
    BOOST_CHECK( (!mifi_isnan(array[0])) && (array[0] < 280) && (array[0] > 270));
    //interpolator->getCDM().toXMLStream(cout);
    BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(test_interpolator_vectorlatlon)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string fileName(topSrcDir+"/test/flth00.dat");
    if (!ifstream(fileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader2(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
    boost::shared_ptr<CDMProcessor> processor(new CDMProcessor(feltReader));
    vector<string> x(1, "x_wind_10m");
    vector<string> y(1, "y_wind_10m");
    processor->rotateVectorToLatLon(true, x, y);
    SliceBuilder sbX0(feltReader->getCDM(), x[0]);
    SliceBuilder sbY0(feltReader->getCDM(), y[0]);
    {
        // 0deg longitude
        sbX0.setStartAndSize("x", 114, 1);
        sbX0.setStartAndSize("y", 85, 1);
        sbY0.setStartAndSize("x", 114, 1);
        sbY0.setStartAndSize("y", 85, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (int i = 0; i < xDataOrg->size(); i++) {
            // no change in x
            BOOST_CHECK_CLOSE((xDataOrg->asFloat())[i], (xDataRot->asFloat())[i], 1e-2);
            BOOST_CHECK_CLOSE((yDataOrg->asFloat())[i], (yDataRot->asFloat())[i], 1e-2);
        }
    }
    {
        // 90deg longitude
        sbX0.setStartAndSize("x", 182, 1);
        sbX0.setStartAndSize("y", 147, 1);
        sbY0.setStartAndSize("x", 182, 1);
        sbY0.setStartAndSize("y", 147, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (int i = 0; i < xDataOrg->size(); i++) {
            BOOST_CHECK_CLOSE((xDataOrg->asFloat())[i], -1.*(yDataRot->asFloat())[i], 1e-1);
            BOOST_CHECK_CLOSE((yDataOrg->asFloat())[i], (xDataRot->asFloat())[i], 1e-1);
        }
    }
    {
        // ~0deg longitude (no org data at 180)
        sbX0.setStartAndSize("x", 113, 1);
        sbX0.setStartAndSize("y", 10, 1);
        sbY0.setStartAndSize("x", 113, 1);
        sbY0.setStartAndSize("y", 10, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (int i = 0; i < xDataOrg->size(); i++) {
            float error = ((xDataOrg->asFloat())[i] < 1) ? 50 : 3;
            BOOST_CHECK_CLOSE((xDataOrg->asFloat())[i], (xDataRot->asFloat())[i], error);
            error = ((yDataOrg->asFloat())[i] < 1) ? 50 : 3;
            BOOST_CHECK_CLOSE((yDataOrg->asFloat())[i], (yDataRot->asFloat())[i], error);
        }
    }
    {
        // -90deg longitude
        sbX0.setStartAndSize("x", 38, 1);
        sbX0.setStartAndSize("y", 147, 1);
        sbY0.setStartAndSize("x", 38, 1);
        sbY0.setStartAndSize("y", 147, 1);
        DataPtr xDataOrg = feltReader->getScaledDataSlice(x[0], sbX0);
        DataPtr xDataRot = processor->getScaledDataSlice(x[0], sbX0);
        DataPtr yDataOrg = feltReader->getScaledDataSlice(y[0], sbY0);
        DataPtr yDataRot = processor->getScaledDataSlice(y[0], sbY0);
        for (int i = 0; i < xDataOrg->size(); i++) {
            float error = ((xDataOrg->asFloat())[i] < 1) ? 1 : .1;
            BOOST_CHECK_CLOSE((xDataOrg->asFloat())[i], (yDataRot->asFloat())[i], error);
            error = ((yDataOrg->asFloat())[i] < 1) ? 1 : .1;
            BOOST_CHECK_CLOSE((yDataOrg->asFloat())[i], -1*(xDataRot->asFloat())[i], error);
        }
    }
}

BOOST_AUTO_TEST_CASE(test_interpolator_vcross)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    string topSrcDir(TOP_SRCDIR);
    string ncFileName(topSrcDir+"/test/erai.sfc.40N.0.75d.200301011200.nc");
    if (!ifstream(ncFileName.c_str())) {
        // no testfile, skip test
        return;
    }
    boost::shared_ptr<CDMReader> ncReader(new NetCDF_CDMReader(ncFileName));
    boost::shared_ptr<CDMInterpolator> interpolator(new CDMInterpolator(ncReader));

    vector<CrossSectionDefinition> vc;
    vector<pair<double, double> > lonLat;
    lonLat.push_back(make_pair<double,double>(10.74,59.9)); // Oslo
    lonLat.push_back(make_pair<double,double>(10.3951,63.4305)); // Tronheim
    lonLat.push_back(make_pair<double,double>(18.9551,69.6489)); // Tromso
    vc.push_back(CrossSectionDefinition("OsloTrondheimTromso", lonLat));
    lonLat.clear();
    lonLat.push_back(make_pair<double,double>(5.3290, 60.3983)); // Bergen
    lonLat.push_back(make_pair<double,double>(10.74,59.9)); // Oslo
    vc.push_back(CrossSectionDefinition("BergenOslo", lonLat));
    interpolator->changeProjectionToCrossSections(MIFI_INTERPOL_BILINEAR, vc);
    BOOST_CHECK(true);
    BOOST_CHECK(interpolator->getCDM().hasVariable("vcross_name"));
    BOOST_CHECK(interpolator->getCDM().hasDimension("nvcross"));
    BOOST_CHECK_EQUAL(interpolator->getCDM().getDimension("nvcross").getLength(), 2);
    BOOST_CHECK(interpolator->getCDM().getDimension("x").getLength() >  5);
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
