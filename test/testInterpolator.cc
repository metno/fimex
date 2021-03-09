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

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/MathUtils.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/Type2String.h"
#include "fimex/XMLInputFile.h"
#include "fimex/interpolation.h"

#include "testinghelpers.h"

using namespace std;
using namespace MetNoFimex;

static const int DEBUG = 0;

#if defined(HAVE_FELT)
TEST4FIMEX_TEST_CASE(interpolator)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(feltReader);

    vector<double> xAxis, yAxis;
    for (int i = -100; i < 10; i++) {
        xAxis.push_back(i * 50000);
        yAxis.push_back(i * 50000);
    }
    interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);

    DataPtr altitudeData = interpolator->getDataSlice("altitude");
    shared_array<double> altArray = altitudeData->asDouble();
    int found = 0;
    for (size_t i = 0; i < altitudeData->size(); i++) {
        if (altArray[i] > 2000) {
            found++;
        }
    }
    TEST4FIMEX_CHECK(found > 100); // at least 100 cells above 2000m

    TEST4FIMEX_CHECK(writeToFile(interpolator, "test_interpolator.nc"));
}

TEST4FIMEX_TEST_CASE(interpolatorKDTree)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;

    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(feltReader);
    vector<double> xAxis, yAxis;
    for (int i = -100; i < 10; i++) {
        xAxis.push_back(i * 50000);
        yAxis.push_back(i * 50000);
    }
    interpolator->changeProjection(MIFI_INTERPOL_COORD_NN_KD, "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);

    DataPtr altitudeData = interpolator->getDataSlice("altitude");
    TEST4FIMEX_REQUIRE(altitudeData);
    shared_array<double> altArray = altitudeData->asDouble();
    TEST4FIMEX_REQUIRE(altArray);

    int found = 0;
    for (size_t i = 0; i < altitudeData->size(); i++) {
        if (altArray[i] > 2000) {
            found++;
        }
    }
    TEST4FIMEX_CHECK(found > 100); // at least 100 cells above 2000m
}

TEST4FIMEX_TEST_CASE(interpolatorRelative)
{
    if (DEBUG)
        defaultLogLevel(Logger::DEBUG);
    CDMReader_p feltReader = getFLTH00Reader();
    if (!feltReader)
        return;
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(feltReader);
    interpolator->changeProjection(MIFI_INTERPOL_BILINEAR,
                                   "+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +a=" + type2string(MIFI_EARTH_RADIUS_M) + " +e=0",
                                   "0,50000,...,x;relativeStart=0", "0,50000,...,x;relativeStart=0", "m", "m");
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("x")->size(), 297);
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("y")->size(), 286);
}
#endif // HAVE_FELT

#if defined(HAVE_NETCDF_H)
TEST4FIMEX_TEST_CASE(interpolatorSatellite)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    const string fileName = pathTest("satellite_cma.nc");
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", fileName));
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(reader);
    vector<double> xAxis, yAxis;
    for (int i = 0; i < 10; i++) {
        xAxis.push_back(55+ i * 0.1);
        yAxis.push_back(-106 + i * 0.1);
    }
    interpolator->changeProjection(MIFI_INTERPOL_COORD_NN_KD, "+proj=latlon +R=" + type2string(MIFI_EARTH_RADIUS_M) + " +e=0", xAxis, yAxis, "degrees_east",
                                   "degrees_north", CDM_DOUBLE, CDM_DOUBLE);
    DataPtr cmaData = interpolator->getDataSlice("cma", 0);
    TEST4FIMEX_REQUIRE(cmaData);
    shared_array<double> cmaArray = cmaData->asDouble();
    TEST4FIMEX_REQUIRE(cmaArray);

    SliceBuilder cmaSb(interpolator->getCDM(), "cma");
    DataPtr cmaData2 = interpolator->getDataSlice("cma", cmaSb);
    TEST4FIMEX_REQUIRE(cmaData2);

    TEST4FIMEX_CHECK_EQ(cmaData2->size(), cmaData->size());
}

TEST4FIMEX_TEST_CASE(interpolator2coords)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    const string fileName = pathTest("twoCoordsTest.nc");
    CDMReader_p reader(CDMFileReaderFactory::create("netcdf", fileName));
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(reader);
    {
        vector<double> xAxis, yAxis;
        for (int i = 0; i < 12; i++) {
            xAxis.push_back(-1705516 + i * 50000);
            yAxis.push_back(-6872225 + i * 50000);
        }
        interpolator->changeProjection(MIFI_INTERPOL_COORD_NN_KD, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
        //NetCDF_CDMWriter(interpolator, "test2coordsNNKDInterpolator.nc");
    }
    {
        DataPtr temp2Data = interpolator->getDataSlice("temp2");
        shared_array<double> tmpArray = temp2Data->asDouble();
        int found = 0;
        for (size_t i = 0; i < temp2Data->size(); i++) {
            if (tmpArray[i] > 29000) {
                found++;
            }
        }
        TEST4FIMEX_CHECK(found > 100); // at least 100 cells above 2000m
    }

    interpolator = std::make_shared<CDMInterpolator>(reader);
    {
        vector<double> xAxis, yAxis;
        for (int i = 0; i < 12; i++) {
            xAxis.push_back(-1705516 + i * 50000);
            yAxis.push_back(-6872225 + i * 50000);
        }
        interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +ellps=sphere +a="+type2string(MIFI_EARTH_RADIUS_M)+" +e=0", xAxis, yAxis, "m", "m", CDM_INT, CDM_INT);
        //NetCDF_CDMWriter(interpolator, "test2nearestneighborInterpolator.nc");
    }
    {
        int found = 0;
        DataPtr temp2Data = interpolator->getDataSlice("temp2");
        shared_array<double> tmpArray = temp2Data->asDouble();
        for (size_t i = 0; i < temp2Data->size(); i++) {
            if (tmpArray[i] > 29000) {
                found++;
            }
        }
        TEST4FIMEX_CHECK(found > 100); // at least 100 cells above 2000m
    }
}

TEST4FIMEX_TEST_CASE(interpolator_template)
{
    const string ncFileName(pathTest("erai.sfc.40N.0.75d.200301011200.nc"));
    const string templateFileName(pathTest("template_noaa17.nc"));
    CDMReader_p ncReader(CDMFileReaderFactory::create("netcdf", ncFileName));
    TEST4FIMEX_REQUIRE(ncReader);

    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(ncReader);
    interpolator->changeProjection(MIFI_INTERPOL_BICUBIC, templateFileName);

    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("x")->size(), 29);
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("y")->size(), 31);
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("longitude")->size(), 29 * 31);
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("longitude")->size(), interpolator->getDataSlice("latitude")->size());
    TEST4FIMEX_REQUIRE(interpolator->getCDM().hasVariable("ga_skt"));
    DataPtr data = interpolator->getData("ga_skt");
    TEST4FIMEX_REQUIRE(data);
    shared_array<double> array = data->asDouble();
    TEST4FIMEX_REQUIRE(array);
    for (size_t i = 0; i < 7; ++i) { // only first 7 datapoints are defined
        TEST4FIMEX_CHECK((!mifi_isnan(array[i])) && (array[i] < 280) && (array[i] > 270));
    }
}

TEST4FIMEX_TEST_CASE(interpolator_latlon)
{
    double lat[] = {59.109, 59.052, 58.994, 58.934, 58.874, 58.812, 58.749, 58.685, 58.62, 64.};
    double lon[] = {4.965, 5.13, 5.296, 5.465, 5.637, 5.81, 5.986, 6.164001, 6.344, 3.};
    vector<double> latVals(&lat[0], &lat[0]+10);
    vector<double> lonVals(&lon[0], &lon[0]+10);

    const string ncFileName(pathTest("erai.sfc.40N.0.75d.200301011200.nc"));
    CDMReader_p ncReader(CDMFileReaderFactory::create("netcdf", ncFileName));

    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(ncReader);
    interpolator->changeProjection(MIFI_INTERPOL_BILINEAR, lonVals, latVals);
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("longitude")->size(), lonVals.size());
    TEST4FIMEX_CHECK_EQ(interpolator->getDataSlice("longitude")->size(), interpolator->getDataSlice("latitude")->size());
    TEST4FIMEX_CHECK(interpolator->getCDM().hasVariable("ga_skt"));

    DataPtr data = interpolator->getData("ga_skt");
    shared_array<double> array = data->asDouble();
    TEST4FIMEX_CHECK((!mifi_isnan(array[0])) && (array[0] < 280) && (array[0] > 270));
    for (size_t i = 0; i < data->size(); ++i) {
        TEST4FIMEX_CHECK((!mifi_isnan(array[i])) && (array[i] < 281.1) && (array[i] > 266));
    }
    //interpolator->getCDM().toXMLStream(cout);
}

TEST4FIMEX_TEST_CASE(interpolator_wrongaxes_latlon)
{
    double lat[] = {60.0};
    double lon[] = {10.0};
    vector<double> latVals(&lat[0], &lat[0]+1);
    vector<double> lonVals(&lon[0], &lon[0]+1);

    const string ncmlFileName = pathTest("c11.ncml");
    const string ncFileName = pathTest("c11.nc");
    CDMReader_p ncReader = CDMFileReaderFactory::create("netcdf", ncFileName);
    CDMReader_p ncmlReader = std::make_shared<NcmlCDMReader>(ncReader, XMLInputFile(ncmlFileName));
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(ncmlReader);
    interpolator->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, lonVals, latVals);

    DataPtr lonData = interpolator->getDataSlice("longitude");
    DataPtr latData = interpolator->getDataSlice("latitude");
    TEST4FIMEX_REQUIRE(lonData && latData);
    TEST4FIMEX_CHECK_EQ(lonData->size(), lonVals.size());
    TEST4FIMEX_CHECK_EQ(lonData->size(), latData->size());

    TEST4FIMEX_REQUIRE(interpolator->getCDM().hasVariable("x_wind_pl"));
#if 1
    SliceBuilder sb(interpolator->getCDM(), "x_wind_pl");
    DataPtr data = interpolator->getDataSlice("x_wind_pl", sb);
#else
    DataPtr data = interpolator->getDataSlice("x_wind_pl", 0);
#endif
    TEST4FIMEX_REQUIRE(data);
    shared_array<double> array = data->asDouble();
    TEST4FIMEX_REQUIRE(array);
    for (size_t i = 0; i < data->size(); ++i) {
        TEST4FIMEX_CHECK((!mifi_isnan(array[i])));
    }
}

namespace {
struct IP {
    IP(string proj, string xAxis, string yAxis, string unit, string lonAxis="-180,-179,...,180", string latAxis="-90,-89,...,90", double delta=1e-4)
        : proj(proj), xAxis(xAxis), yAxis(yAxis), unit(unit), lonAxis(lonAxis), latAxis(latAxis), delta(delta) {}
    string proj;
    string xAxis;
    string yAxis;
    string unit;
    string lonAxis;
    string latAxis;
    double delta;
};

template<class T>
class TestMany {
    vector<T> all;
public:
    TestMany& operator()(T ip) {
        all.push_back(ip);
        return *this;
    }
    size_t size() {return all.size();}
    T& get(size_t i) {return all.at(i);}
};
} // namespace

TEST4FIMEX_TEST_CASE(interpolator_vector_backforth)
{
    TestMany<IP> tests;
    tests(IP("+proj=stere +lat_0=90 +lon_0=-32 +lat_ts=60 +ellps=sphere +R="+type2string(MIFI_EARTH_RADIUS_M),
             "-30000000,-29950000,...,30000000", "-30000000,-29950000,...,30000000", "m",
             "-180,-179,...,179", "55,56,...,87", 8e-2))
         (IP("+proj=stere +lat_0=90 +lon_0=0 +lat_ts=90 +ellps=sphere +R="+type2string(MIFI_EARTH_RADIUS_M),
             "-30000000,-29950000,...,30000000", "-30000000,-29950000,...,30000000", "m",
             "-180,-179,...,179", "55,56,...,87", 8e-2))
         (IP("+proj=stere +lat_0=-90 +lon_0=0 +lat_ts=-90 +ellps=sphere +R="+type2string(MIFI_EARTH_RADIUS_M),
             "-30000000,-29950000,...,30000000", "-30000000,-29950000,...,30000000", "m",
             "0,1,359", "-55,-56,...,-87", 8e-2)) // south-pole - difficult around -180,180 edge, +-ywind???
         (IP("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06",
              "-922000,-902000,...,922000", "-1130000,-1110000,...,1230000", "m",
              "-30,-29,...,40","50,51,...,85", 1e-2)) // arome-norway
         (IP("+proj=ob_tran +o_proj=longlat +lon_0=-40 +o_lat_p=22 +R=6.371e+06 +no_defs",
             "16.5,16.6,...,24.2","-3.8,-3.7,...,14.9","degree",
             "-30,-29,...,40","50,51,...,85", 5e-3)) // hirlam8
         (IP("+proj=ob_tran +o_proj=longlat +lon_0=0 +o_lat_p=25 +R=6.371e+06 +no_defs",
             "-46.4,-46.2,...,46.","-36.4,-36.2,...,38.8","degree",
             "-90,-89,...,90","40,51,...,85", 3e-2)) // hirlam12
         (IP("+proj=latlon +R=6.371e+06 +no_defs",
             "-179,-178,...,179","-89.5,-89,...,89.5","degree",
             "-180,-179,...,179","-90,-89,...,90", 1e-3)) // latlon
         (IP("+proj=utm +zone=33 +datum=WGS84 +no_defs",
             "0,1000,...,x;relativeStart=0","0,1000,...,x;relativeStart=0","m",
             "-30,-29,...,40","50,51,...,85", 1e-2)) // UTM
         ;

    for (size_t k = 0; k < 2; ++k) {
        CDMReader_p reader;
        string fileName = pathTest("data/north.nc");
        double xWind = 0;
        double yWind = 1;
        if (k == 1) {
            fileName = pathTest("data/east.nc");
            xWind = 1;
            yWind = 0;
        }
        try {
            reader = CDMFileReaderFactory::create("netcdf", fileName);
        } catch (CDMException& ex) {
            // ignore, most likely nc4 not readable
            continue;
        }
        for (size_t i = 0; i < tests.size(); ++i) {
            IP ip(tests.get(i));
            CDMInterpolator_p interp = std::make_shared<CDMInterpolator>(reader);
            interp->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR, ip.proj,
                    ip.xAxis, ip.yAxis, ip.unit, ip.unit);

            CDMInterpolator_p iback = std::make_shared<CDMInterpolator>(interp);
            iback->changeProjection(MIFI_INTERPOL_NEAREST_NEIGHBOR,
                    "+proj=latlon +R=" + type2string(MIFI_EARTH_RADIUS_M),
                    ip.lonAxis, ip.latAxis, "degrees_east", "degrees_north");

            DataPtr dxwind = iback->getScaledData("x_wind");
            shared_array<float> xwind = dxwind->asFloat();
            shared_array<float> ywind = iback->getScaledData("y_wind")->asFloat();

            for (size_t i = 0; i < dxwind->size(); ++i) {
                if (!(mifi_isnan(xwind[i]) || (mifi_isnan(ywind[i])))) {
                    TEST4FIMEX_CHECK_MESSAGE((std::abs(xWind - xwind[i]) < ip.delta) && (std::abs(yWind - ywind[i]) < ip.delta),
                                             "(xWind,yWind) -> i, (xwind[i],ywind[i]), proj:(" << xWind << "," << yWind << ") -> " << i << ": (" << xwind[i]
                                                                                               << "," << ywind[i] << "): " << ip.proj);
                }
            }
        }
    }
}

TEST4FIMEX_TEST_CASE(interpolator_vcross)
{
    if (DEBUG) defaultLogLevel(Logger::DEBUG);
    const string ncFileName = pathTest("erai.sfc.40N.0.75d.200301011200.nc");
    CDMReader_p ncReader = CDMFileReaderFactory::create("netcdf", ncFileName);
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(ncReader);

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

    const CDM& cdm = interpolator->getCDM();

    TEST4FIMEX_CHECK(cdm.hasDimension("nvcross_strlen"));
    const int vcross_strlen = cdm.getDimension("nvcross_strlen").getLength();

    TEST4FIMEX_CHECK(cdm.hasVariable("vcross_name"));
    DataPtr vcross_name_data = interpolator->getData("vcross_name");
    TEST4FIMEX_CHECK(vcross_name_data);
    TEST4FIMEX_CHECK_EQ(vcross_name_data->getDataType(), CDM_STRING);
    TEST4FIMEX_CHECK_EQ(vcross_name_data->size(), 2u * vcross_strlen);
    const std::string vcross_names = vcross_name_data->asString();
    TEST4FIMEX_CHECK_EQ(vcross_names.substr(0, 19), "OsloTrondheimTromso");
    TEST4FIMEX_CHECK_EQ(vcross_names.substr(vcross_strlen, 10), "BergenOslo");

    TEST4FIMEX_CHECK(cdm.hasDimension("nvcross"));
    TEST4FIMEX_CHECK_EQ(cdm.getDimension("nvcross").getLength(), 2);
    TEST4FIMEX_CHECK(cdm.hasDimension("x"));
    TEST4FIMEX_CHECK(cdm.getDimension("x").getLength() > 5);
}

namespace {
std::vector<double> range(double start, double step, double end)
{
    std::vector<double> r;
    for (; step > 0 && start < end + step; start += step)
        r.push_back(start);
    return r;
}
#include "testInterpolator_forward_ex.cc"
} // namespace

TEST4FIMEX_TEST_CASE(interpolator_forward)
{
    if (DEBUG)
        defaultLogLevel(Logger::DEBUG);
    const string ncFileName = pathTest("interpolator_forward_in.nc");
    CDMReader_p ncReader(CDMFileReaderFactory::create("netcdf", ncFileName));
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(ncReader);
    interpolator->changeProjection(MIFI_INTERPOL_FORWARD_MEAN, "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                                   range(411386.521566, 50, 413886.672091), range(7539081.567715, 50, 7541081.780868), "m", "m", CDM_DOUBLE, CDM_DOUBLE);

    DataPtr interpolatedData = interpolator->getDataSlice("Amplitude_VV", 0);
    TEST4FIMEX_REQUIRE(interpolatedData);
    TEST4FIMEX_REQUIRE_EQ(interpolator_forward_N, interpolatedData->size());
    shared_array<unsigned short> interpolatedValues = interpolatedData->asUShort();
    int bad = 0;
    for (size_t i = 0; i < interpolator_forward_N; ++i) {
        if (interpolator_forward_ex[i] != interpolatedValues[i]) {
            bad += 1;
        }
    }
    TEST4FIMEX_CHECK_EQ(0, bad);
}
#endif // HAVE_NETCDF_H
