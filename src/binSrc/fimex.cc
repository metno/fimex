/*
 * Fimex
 *
 * (C) Copyright 2008-2019, met.no
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
#include "fimex/CDMExtractor.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMMerger.h"
#include "fimex/CDMPressureConversions.h"
#include "fimex/CDMProcessor.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/CDMVerticalInterpolator.h"
#include "fimex/CDMconstants.h"
#include "fimex/FillWriter.h"
#include "fimex/Logger.h"
#ifdef HAVE_MPI
#include "fimex/mifi_mpi.h"
#endif
#include "fimex/NcmlCDMReader.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/ThreadPool.h"
#include "fimex/TimeUnit.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/XMLInputFile.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/interpolation.h"

#include "CDMMergeUtils.h"
#include "fimex_config.h"

#include <mi_programoptions.h>

#include <cctype>
#include <fstream>
#include <iostream>
#include <memory>
#include <numeric>
#include <regex>
#ifdef HAVE_LOG4CPP
#include "log4cpp/PropertyConfigurator.hh"
#endif

namespace po = miutil::program_options;
using namespace std;
using namespace MetNoFimex;

namespace {

Logger_p logger = getLogger("fimex");

// generic options
const po::option op_help = po::option("help", "help message").set_shortkey("h").set_narg(0);
const po::option op_version = po::option("version", "program version").set_narg(0);
const po::option op_debug = po::option("debug", "debug program").set_narg(0);
const po::option op_log4cpp = po::option("log4cpp", "log4cpp property file (- = log4cpp without prop-file)");
const po::option op_print_options = po::option("print-options", "print all options").set_narg(0);
const po::option op_config = po::option("config", "configuration file").set_shortkey("c");
const po::option op_num_threads = po::option("num_threads", "number of threads").set_shortkey("n");

// options for command line and config file
const po::option op_input_file = po::option("input.file", "input file");
const po::option op_input_type = po::option("input.type", "filetype of input file, e.g. nc, nc4, ncml, felt, grib1, grib2");
const po::option op_input_config = po::option("input.config", "non-standard input configuration");
const po::option op_input_optional = po::option("input.optional", "additional options, e.g. multiple files for grib").set_composing();
const po::option op_input_printNcML = po::option("input.printNcML", "print NcML description of input").set_implicit_value("-");
const po::option op_input_printCS = po::option("input.printCS", "print CoordinateSystems of input file").set_narg(0);
const po::option op_input_printSize = po::option("input.printSize", "print size estimate").set_narg(0);
const po::option op_output_file = po::option("output.file", "output file");
const po::option op_output_fillFile = po::option("output.fillFile", "existing output file to be filled");
const po::option op_output_type = po::option("output.type", "filetype of output file, e.g. nc, nc4, grib1, grib2");
const po::option op_output_config = po::option("output.config", "non-standard output configuration");
const po::option op_output_printNcML = po::option("output.printNcML", "print NcML description of input").set_implicit_value("-");
const po::option op_output_printCS = po::option("output.printCS", "print CoordinateSystems of input file").set_narg(0);
const po::option op_output_printSize = po::option("output.printSize", "print size estimate").set_narg(0);
const po::option op_process_accumulateVariable = po::option("process.accumulateVariable",
        "accumulate variable along unlimited dimension").set_composing();
const po::option op_process_deaccumulateVariable = po::option("process.deaccumulateVariable",
        "deaccumulate variable along unlimited dimension").set_composing();
// const po::option op_process_rotateVectorToLatLonX = po::option("process.rotateVectorToLatLonX",
//         "deprecated: rotate this vector x component from grid-direction to latlon direction").set_composing();
// const po::option op_process_rotateVectorToLatLonY = po::option("process.rotateVectorToLatLonY",
//         "deprecated: rotate this vector y component from grid-direction to latlon direction").set_composing();
const po::option op_process_rotateVector_direction = po::option("process.rotateVector.direction",
        "set direction: to latlon or grid");
const po::option op_process_rotateVector_angle = po::option("process.rotateVector.angle",
        "rotate these angles (in degree) to the direction ").set_composing();
const po::option op_process_rotateVector_x = po::option("process.rotateVector.x",
        "rotate this vector x component to direction").set_composing();
const po::option op_process_rotateVector_stdNameX = po::option("process.rotateVector.stdNameX",
        "new standard_name for the rotated vector").set_composing();
const po::option op_process_rotateVector_y = po::option("process.rotateVector.y",
        "rotate this vector y component from grid-direction to latlon direction").set_composing();
const po::option op_process_rotateVector_stdNameY = po::option("process.rotateVector.stdNameY",
        "new standard_name for the rotated vector").set_composing();
const po::option op_process_rotateVector_all = po::option("process.rotateVector.all",
        "rotate all known vectors (e.g. standard_name) to given direction").set_narg(0);
const po::option op_process_addVerticalVelocity = po::option("process.addVerticalVelocity",
        "calculate upward_air_velocity_ml");
const po::option op_process_printNcML = po::option("process.printNcML",
        "print NcML description of process").set_implicit_value("-");
const po::option op_process_printCS = po::option("process.printCS", "print CoordinateSystems of process").set_narg(0);
const po::option op_process_printSize = po::option("process.printSize", "print size estimate").set_narg(0);
const po::option op_extract_removeVariable = po::option("extract.removeVariable",
        "remove variables").set_composing();
const po::option op_extract_selectVariables = po::option("extract.selectVariables",
        "select only those variables").set_composing();
const po::option op_extract_selectVariables_noAuxiliary = po::option("extract.selectVariables.noAuxiliary", "don't add auxiliary variables").set_narg(0);
const po::option op_extract_reduceDimension_name = po::option("extract.reduceDimension.name", "name of a dimension to reduce").set_composing();
const po::option op_extract_reduceDimension_start = po::option("extract.reduceDimension.start", "start position of the dimension to reduce (>=0)").set_composing();
const po::option op_extract_reduceDimension_end = po::option("extract.reduceDimension.end", "end position of the dimension to reduce").set_composing();
const po::option op_extract_pickDimension_name = po::option("extract.pickDimension.name", "name of a dimension to pick levels").set_composing();
const po::option op_extract_pickDimension_list = po::option("extract.pickDimension.list", "list of dim-positions (including dots), starting at 0").set_composing();
const po::option op_extract_reduceTime_start = po::option("extract.reduceTime.start", "start-time as iso-string");
const po::option op_extract_reduceTime_end = po::option("extract.reduceTime.end", "end-time by iso-string");
const po::option op_extract_reduceVerticalAxis_unit = po::option("extract.reduceVerticalAxis.unit", "unit of vertical axis to reduce");
const po::option op_extract_reduceVerticalAxis_start = po::option("extract.reduceVerticalAxis.start", "start value of vertical axis");
const po::option op_extract_reduceVerticalAxis_end = po::option("extract.reduceVerticalAxis.end", "end value of the vertical axis");
const po::option op_extract_reduceToBoundingBox_south = po::option("extract.reduceToBoundingBox.south", "geographical bounding-box in degree");
const po::option op_extract_reduceToBoundingBox_north = po::option("extract.reduceToBoundingBox.north", "geographical bounding-box in degree");
const po::option op_extract_reduceToBoundingBox_east = po::option("extract.reduceToBoundingBox.east", "geographical bounding-box in degree");
const po::option op_extract_reduceToBoundingBox_west = po::option("extract.reduceToBoundingBox.west", "geographical bounding-box in degree");
const po::option op_extract_printNcML = po::option("extract.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_extract_printCS = po::option("extract.printCS", "print CoordinateSystems of extractor").set_narg(0);
const po::option op_extract_printSize = po::option("extract.printSize", "print size estimate").set_narg(0);
const po::option op_qualityExtract_autoConfString = po::option("qualityExtract.autoConfString", "configure the quality-assignment using CF-1.3 status-flag");
const po::option op_qualityExtract_config = po::option("qualityExtract.config", "configure the quality-assignment with a xml-config file");
const po::option op_qualityExtract_printNcML = po::option("qualityExtract.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_qualityExtract_printCS = po::option("qualityExtract.printCS", "print CoordinateSystems of extractor").set_narg(0);
const po::option op_qualityExtract_printSize = po::option("qualityExtract.printSize", "print size estimate").set_narg(0);
const po::option op_interpolate_projString = po::option("interpolate.projString", "proj init string describing the new projection");
const po::option op_interpolate_method = po::option("interpolate.method", "interpolation method, one of nearestneighbor, bilinear, bicubic, coord_nearestneighbor, coord_kdtree, forward_max, forward_min, forward_mean, forward_median, forward_sum or forward_undef_*");
const po::option op_interpolate_xAxisValues = po::option("interpolate.xAxisValues", "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition");
const po::option op_interpolate_yAxisValues = po::option("interpolate.yAxisValues", "string with values on y-Axis, use ... to continue, i.e. 55.5,56,...,65.5, see Fimex::SpatialAxisSpec for full definition");
const po::option op_interpolate_xAxisUnit = po::option("interpolate.xAxisUnit", "unit of x-Axis given as udunits string, i.e. m or degrees_east");
const po::option op_interpolate_yAxisUnit = po::option("interpolate.yAxisUnit", "unit of y-Axis given as udunits string, i.e. m or degrees_north");
const po::option op_interpolate_xAxisType = po::option("interpolate.xAxisType", "datatype of x-axis (double,float,int,short)").set_default_value("double");
const po::option op_interpolate_yAxisType = po::option("interpolate.yAxisType", "datatype of y-axis").set_default_value("double");
const po::option op_interpolate_distanceOfInterest = po::option("interpolate.distanceOfInterest", "optional distance of interest used differently depending on method");
const po::option op_interpolate_latitudeName = po::option("interpolate.latitudeName", "name for auto-generated projection coordinate latitude");
const po::option op_interpolate_longitudeName = po::option("interpolate.longitudeName", "name for auto-generated projection coordinate longitude");
const po::option op_interpolate_preprocess = po::option("interpolate.preprocess", "add a 2d preprocess before the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2[,defaultValue=0.0])\"");
const po::option op_interpolate_postprocess = po::option("interpolate.postprocess", "add a 2d postprocess after the interpolation, e.g. \"fill2d(critx=0.01,cor=1.6,maxLoop=100)\" or \"creepfill2d(repeat=20,weight=2[,defaultValue=0.0])\"");
const po::option op_interpolate_latitudeValues = po::option("interpolate.latitudeValues",
        "latitude values, in degrees north, of a list of points to interpolate to, e.g. 60.5,70,90"
        " (use with 'longitudeValues' -- to produce a grid, use 'projString', 'xAxisValues', 'yAxisValues', ...)");
const po::option op_interpolate_longitudeValues = po::option("interpolate.longitudeValues",
        "longitude values, in degrees east, of a list of points to interpolate to, e.g. -10.5,-10.5,29.5"
        " (use with 'latitudeValues' -- to produce a grid, use 'projString', 'xAxisValues', 'yAxisValues', ...)");
const po::option op_interpolate_vcrossNames = po::option("interpolate.vcrossNames", "string with comma-separated names for vertical cross sections");
const po::option op_interpolate_vcrossNoPoints = po::option("interpolate.vcrossNoPoints", "string with comma-separated number of lat/lon values for each vertical cross sections");
const po::option op_interpolate_template = po::option("interpolate.template", "netcdf file containing lat/lon list used in interpolation");
const po::option op_interpolate_printNcML = po::option("interpolate.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_interpolate_printCS = po::option("interpolate.printCS", "print CoordinateSystems of interpolator").set_narg(0);
const po::option op_interpolate_printSize = po::option("interpolate.printSize", "print size estimate").set_narg(0);

const po::option op_merge_inner_file = po::option("merge.inner.file", "inner file for merge");
const po::option op_merge_inner_type = po::option("merge.inner.type", "filetype of inner merge file, e.g. nc, nc4, ncml, felt, grib1, grib2");
const po::option op_merge_inner_config = po::option("merge.inner.config", "non-standard configuration for inner merge file");
const po::option op_merge_inner_cfg = po::option("merge.inner.cfg", "recursive fimex.cfg setup-file to enable all fimex-processing steps (i.e. not input and output) to the merge.inner source before merging");
const po::option op_merge_smoothing = po::option("merge.smoothing", "smoothing function for merge, e.g. \"LINEAR(5,2)\" for linear smoothing, 5 grid points transition, 2 grid points border");
const po::option op_merge_keepOuterVariables =
    po::option("merge.keepOuterVariables", "keep all outer variables, default: only keep variables existing in inner and outer").set_narg(0);
const po::option op_merge_method = po::option("merge.method", "interpolation method for grid conversions, one of nearestneighbor, bilinear, bicubic,"
        " coord_nearestneighbor, coord_kdtree, forward_max, forward_min, forward_mean, forward_median, forward_sum or forward_undef_* (*=max,min,mean,median,sum");
const po::option op_merge_projString = po::option("merge.projString", "proj init string describing the new projection, default: use inner projection extended to outer grid");
const po::option op_merge_xAxisValues = po::option("merge.xAxisValues", "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition");
const po::option op_merge_yAxisValues = po::option("merge.yAxisValues", "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5, see Fimex::SpatialAxisSpec for full definition");
const po::option op_merge_xAxisUnit = po::option("merge.xAxisUnit", "unit of x-Axis given as udunits string, i.e. m or degrees_east");
const po::option op_merge_yAxisUnit = po::option("merge.yAxisUnit", "unit of y-Axis given as udunits string, i.e. m or degrees_north");
const po::option op_merge_xAxisType = po::option("merge.xAxisType", "datatype of x-axis (double,float,int,short)").set_default_value("double");
const po::option op_merge_yAxisType = po::option("merge.yAxisType", "datatype of y-axis").set_default_value("double");
const po::option op_merge_printNcML = po::option("merge.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_merge_printCS = po::option("merge.printCS", "print CoordinateSystems of interpolator").set_narg(0);
const po::option op_merge_printSize = po::option("merge.printSize", "print size estimate").set_narg(0);

const std::string verticalInterpolate_config = "verticalInterpolate.config", vertialInterpolate_toAxis = "verticalInterpolate.toAxis";
const po::option op_verticalInterpolate_config = po::option(verticalInterpolate_config, "ncml configuration to be applied before vertical interpolation (typically used to define a vertical axis for use together with --" + vertialInterpolate_toAxis + ")");
const po::option op_verticalInterpolate_type = po::option("verticalInterpolate.type", "pressure, height (above ground) or depth");
const po::option op_verticalInterpolate_ignoreValidityMin = po::option("verticalInterpolate.ignoreValidityMin", "ignore minimum value from vertical transformation (e.g. ocean surface)");
const po::option op_verticalInterpolate_ignoreValidityMax = po::option("verticalInterpolate.ignoreValidityMax", "ignore maximum value from vertical transformation (e.g. ocean bathymetry)");
const po::option op_verticalInterpolate_method = po::option("verticalInterpolate.method", "linear, linear_weak_extra, linear_no_extra, linear_const_extra, log, loglog or nearestneighbor interpolation");
const po::option op_verticalInterpolate_templateVar = po::option("verticalInterpolate.templateVar", "specification template variable for interpolation to fixed or hybrid levels");
const po::option op_verticalInterpolate_toAxis = po::option(vertialInterpolate_toAxis, "specify final vertical axis for interpolation to fixed or hybrid levels (typically used together with --" + verticalInterpolate_config + ")");
const po::option op_verticalInterpolate_level1 = po::option("verticalInterpolate.level1", "specification of first level, see Fimex::CDMVerticalInterpolator for a full definition");
const po::option op_verticalInterpolate_level2 = po::option("verticalInterpolate.level2", "specification of second level, only required for hybrid levels, see Fimex::CDMVerticalInterpolator for a full definition");
const po::option op_verticalInterpolate_dataConversion = po::option("verticalInterpolate.dataConversion", "vertical data-conversion: theta2T, omega2vwind or add4Dpressure").set_composing();
const po::option op_verticalInterpolate_printNcML = po::option("verticalInterpolate.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_verticalInterpolate_printCS = po::option("verticalInterpolate.printCS", "print CoordinateSystems of vertical interpolator").set_narg(0);
const po::option op_verticalInterpolate_printSize = po::option("verticalInterpolate.printSize", "print size estimate").set_narg(0);
const po::option op_timeInterpolate_timeSpec = po::option("timeInterpolate.timeSpec", "specification of times to interpolate to, see MetNoFimex::TimeSpec for a full definition");
const po::option op_timeInterpolate_printNcML = po::option("timeInterpolate.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_timeInterpolate_printCS = po::option("timeInterpolate.printCS", "print CoordinateSystems of timeInterpolator").set_narg(0);
const po::option op_timeInterpolate_printSize = po::option("timeInterpolate.printSize", "print size estimate").set_narg(0);
const po::option op_qualityExtract2_autoConfString = po::option("qualityExtract2.autoConfString", "configure the quality-assignment using CF-1.3 status-flag");
const po::option op_qualityExtract2_config = po::option("qualityExtract2.config", "configure the quality-assignment with a xml-config file");
const po::option op_qualityExtract2_printNcML = po::option("qualityExtract2.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_qualityExtract2_printCS = po::option("qualityExtract2.printCS", "print CoordinateSystems of extractor").set_narg(0);
const po::option op_qualityExtract2_printSize = po::option("qualityExtract2.printSize", "print size estimate").set_narg(0);
const po::option op_ncml_config = po::option("ncml.config", "modify/configure with ncml-file");
const po::option op_ncml_printNcML = po::option("ncml.printNcML", "print NcML description of extractor").set_implicit_value("-");
const po::option op_ncml_printCS = po::option("ncml.printCS", "print CoordinateSystems after ncml-configuration").set_narg(0);
const po::option op_ncml_printSize = po::option("ncml.printSize", "print size estimate").set_narg(0);

po::option_set config_file_options;

CDMReader_p applyFimexStreamTasks(const po::value_set& vm, CDMReader_p dataReader);

void writeUsage(ostream& out, const po::option_set& config)
{
    out << "usage: fimex --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "             [--output.file FILENAME | --output.fillFile [--output.type OUTPUT_TYPE]]" << endl;
    out << "             [--input.config CFGFILENAME] [--output.config CFGFILENAME]" << endl;
    out << "             [--input.optional OPT1 --input.optional OPT2 ...]" << endl;
    out << "             [--num_threads ...]" << endl;
    out << "             [--process....]" << endl;
    out << "             [--qualityExtract....]" << endl;
    out << "             [--extract....]" << endl;
    out << "             [--interpolate....]" << endl;
    out << "             [--verticalInterpolate....]" << endl;
    out << "             [--timeInterpolate....]" << endl;
    out << "             [--merge....]" << endl;
    out << "             [--qualityExtract2....]" << endl;
    out << "             [--ncml.config NCMLFILE]" << endl;
    out << endl;
    config.help(out);
}

string getType(const string& io, const po::value_set& vm)
{
    if (po::option_cx opt = vm.find(io + ".type")) {
        return vm.value(opt);
    }
    return string();
}

template <typename T>
bool getOption(po::option_cx opt, const po::value_set& vm, T& t)
{
    if (vm.is_set(opt)) {
        t = string2type<T>(vm.value(opt));
        return true;
    } else {
        return false;
    }
}

template <typename T>
bool getOptions(po::option_cx opt, const po::value_set& vm, std::vector<T>& t)
{
    if (vm.is_set(opt)) {
        t = strings2types<T>(vm.values(opt));
        return true;
    } else {
        t.clear();
        return false;
    }
}

template <typename T>
T getOption(po::option_cx opt, const po::value_set& vm)
{
    T t;
    getOption(opt, vm, t);
    return t;
}

template <typename T>
std::vector<T> getOptions(po::option_cx opt, const po::value_set& vm)
{
    std::vector<T> t;
    getOptions(opt, vm, t);
    return t;
}

template <typename T>
T getOption(const string& key, const po::value_set& vm)
{
    return getOption<T>(vm.find(key), vm);
}

template <typename T>
std::vector<T> getOptions(const string& key, const po::value_set& vm)
{
    return getOptions<T>(vm.find(key), vm);
}

template <typename T>
bool getOption(const string& key, const po::value_set& vm, T& t)
{
    return getOption<T>(vm.find(key), vm, t);
}

template <typename T>
bool getOptions(const string& key, const po::value_set& vm, std::vector<T>& t)
{
    return getOptions<T>(vm.find(key), vm, t);
}

template <typename T>
T getOption(const po::option& opt, const po::value_set& vm)
{
    return getOption<T>(&opt, vm);
}

template <typename T>
std::vector<T> getOptions(const po::option& opt, const po::value_set& vm)
{
    return getOptions<T>(&opt, vm);
}

template <typename T>
bool getOption(const po::option& opt, const po::value_set& vm, T& t)
{
    return getOption<T>(&opt, vm, t);
}

template <typename T>
bool getOptions(const po::option& opt, const po::value_set& vm, std::vector<T>& t)
{
    return getOptions<T>(&opt, vm, t);
}

std::string getFile(const string& io, const po::value_set& vm)
{
    return getOption<string>(io + ".file", vm);
}

std::string getConfig(const string& io, const po::value_set& vm)
{
    return getOption<string>(io + ".config", vm);
}

void printReaderStatements(const string& readerName, const po::value_set& vm, CDMReader_p reader)
{
    string ncmlout;
    if (getOption(readerName+".printNcML", vm, ncmlout)) {
        cout << readerName << " as NcML:" << endl;
        if (ncmlout == "-") {
            reader->getCDM().toXMLStream(cout);
            cout << endl;
        } else {
            ofstream file;
            file.open(ncmlout.c_str(), ios::out);
            if (file.is_open()) {
                reader->getCDM().toXMLStream(file);
                file.close();
            } else {
                throw CDMException("cannot write ncml-file: '" + ncmlout + "'");
            }
        }
    }
    if (po::option_cx opt = vm.find(readerName + ".printCS")) {
        cout << readerName + " CoordinateSystems: ";
        CoordinateSystem_cp_v csVec = listCoordinateSystems(reader);
        cout << csVec.size() << ": ";
        cout << joinPtr(csVec.begin(), csVec.end(), " | ");
        cout << endl;
    }
    if (po::option_cx opt = vm.find(readerName + ".printSize")) {
        cout << readerName << " size: ~" << ceil(estimateCDMDataSize(reader->getCDM())/1024./1024.) << "MB";
        cout << endl;
    }
}

const string FELT_VARIABLES = (string(FIMEX_DATADIR) + "/felt2nc_variables.xml");

CDMReader_p getCDMFileReader(const po::value_set& vm, const string& io = "input")
{
    const string type = getType(io, vm);
    const std::string fileName = getFile(io, vm);
    const string config = getConfig(io, vm);
    const std::vector<std::string> optional = getOptions<string>(io + ".optional", vm);

    LOG4FIMEX(logger, Logger::DEBUG, "reading file of type '" << type << "' file '" << fileName << "' with config '" << config << "'");
    if (CDMReader_p reader = CDMFileReaderFactory::create(type, fileName, config, optional)) {
        printReaderStatements(io, vm, reader);
        return reader;
    }

    LOG4FIMEX(logger, Logger::FATAL, "unable to read type: " << type);
    exit(1);
}

int getInterpolationMethod(const po::value_set& vm, const po::option& opt)
{
    int method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
    if (vm.is_set(opt)) {
        const string& m = vm.value(opt);
        method = mifi_string_to_interpolation_method(m.c_str());
        if (method == MIFI_INTERPOL_UNKNOWN) {
            LOG4FIMEX(logger, Logger::WARN, "unknown " << opt.key() << ": " << m << " using nearestneighbor");
            method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
        }
    }
    return method;
}

CDMReader_p getCDMProcessor(const po::value_set& vm, CDMReader_p dataReader)
{
    if (!(vm.is_set(op_process_accumulateVariable) || vm.is_set(op_process_deaccumulateVariable) || vm.is_set(op_process_rotateVector_direction) ||
          vm.is_set(op_process_addVerticalVelocity))) {
        LOG4FIMEX(logger, Logger::DEBUG, "process.[de]accumulateVariable or rotateVector.direction or addVerticalVelocity not found, no process used");
        return dataReader;
    }
    std::shared_ptr<CDMProcessor> processor(new CDMProcessor(dataReader));
    if (vm.is_set(op_process_deaccumulateVariable)) {
        for (const std::string& v : vm.values(op_process_deaccumulateVariable))
            processor->deAccumulate(v);
    }
    if (vm.is_set(op_process_accumulateVariable)) {
        for (const std::string& v : vm.values(op_process_accumulateVariable))
            processor->accumulate(v);
    }
    if (vm.is_set(op_process_rotateVector_direction)) {
        bool toLatLon = true;
        const string& direction = vm.value(op_process_rotateVector_direction);
        if (direction == "latlon") {
            toLatLon = true;
        } else if (direction == "grid") {
            toLatLon = false;
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "process.rotateVector.direction != 'latlon' or 'grid' : " << direction << " invalid");
            exit(1);
        }
        if (vm.is_set(op_process_rotateVector_x) && vm.is_set(op_process_rotateVector_y)) {
            const vector<string>& xvars = vm.values(op_process_rotateVector_x);
            const vector<string>& yvars = vm.values(op_process_rotateVector_y);
            vector<string> stdX = getOptions<string>(op_process_rotateVector_stdNameX, vm);
            vector<string> stdY = getOptions<string>(op_process_rotateVector_stdNameY, vm);
            processor->rotateVectorToLatLon(toLatLon, xvars, yvars, stdX, stdY);
        } else if (vm.is_set(op_process_rotateVector_all)) {
            processor->rotateAllVectorsToLatLon(toLatLon);
        } else if (vm.is_set(op_process_rotateVector_angle)) {
            // do nothing here, but don't abort either (see below)
        } else {
            LOG4FIMEX(logger, Logger::FATAL, "process.rotateVector.x and process.rotateVector.y, or process.rotateVector.angle not found");
            exit(1);
        }
        if (vm.is_set(op_process_rotateVector_angle)) {
            const vector<string>& angles = vm.values(op_process_rotateVector_angle);
            processor->rotateDirectionToLatLon(toLatLon, angles);
        }
    }
    if (vm.is_set(op_process_addVerticalVelocity)) {
        processor->addVerticalVelocity();
    }
    return processor;
}

CDMReader_p getCDMExtractor(const po::value_set& vm, CDMReader_p dataReader)
{
    if (!(vm.is_set(op_extract_reduceDimension_name) || vm.is_set(op_extract_pickDimension_name) || vm.is_set(op_extract_removeVariable) ||
          vm.is_set(op_extract_selectVariables) || vm.is_set(op_extract_reduceTime_start) || vm.is_set(op_extract_reduceTime_start) ||
          vm.is_set(op_extract_reduceVerticalAxis_unit) || vm.is_set(op_extract_reduceToBoundingBox_south) || vm.is_set(op_extract_reduceToBoundingBox_north) ||
          vm.is_set(op_extract_reduceToBoundingBox_west) || vm.is_set(op_extract_reduceToBoundingBox_east))) {
        LOG4FIMEX(logger, Logger::DEBUG, "extract.reduceDimension.name and extract.removeVariable not found, no extraction used");
        return dataReader;
    }
    std::shared_ptr<CDMExtractor> extractor(new CDMExtractor(dataReader));
    if (vm.is_set(op_extract_reduceDimension_name)) {
        vector<string> vars = vm.values(op_extract_reduceDimension_name);
        vector<int> startPos = getOptions<int>(op_extract_reduceDimension_start, vm);
        vector<int> endPos = getOptions<int>(op_extract_reduceDimension_end, vm);
        if (startPos.size() != vars.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.reduceDimension.start has different number of elements than extract.reduceDimension.name; "
                                             "use start = 0 if you don't want to reduce the start-position");
        }
        if (endPos.size() != vars.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.reduceDimension.end has different number of elements than extract.reduceDimension.name; "
                                             "use end = 0 (with start != 0) if you don't want to reduce the end-position");
        }
        for (size_t i = 0; i < vars.size(); ++i) {
            if (startPos.at(i) == 0 && endPos.at(i) == 0) {
                // exception to be able to extract only first element
                extractor->reduceDimension(vars.at(i), 0, 1);
            } else {
                extractor->reduceDimensionStartEnd(vars[i], startPos[i], endPos[i]);
            }
        }
    }
    if (vm.is_set(op_extract_pickDimension_name)) {
        const vector<string>& dims = vm.values(op_extract_pickDimension_name);
        vector<string> lists = getOptions<string>(op_extract_pickDimension_list, vm);
        if (dims.size() != lists.size()) {
            LOG4FIMEX(logger, Logger::ERROR, "extract.pickDimension.name has different number of elements than extract.pickDimension.list");
        }
        for (size_t i = 0; i < dims.size(); ++i) {
            vector<int> pos = tokenizeDotted<int>(lists.at(i)); // tokenizeDotted does not work with unsigned values
            set<size_t> posSet(pos.begin(), pos.end());
            extractor->reduceDimension(dims.at(i), posSet);
        }
    }
    if (vm.is_set(op_extract_reduceTime_start) || vm.is_set(op_extract_reduceTime_end)) {
        FimexTime start(FimexTime::min_date_time);
        if (vm.is_set(op_extract_reduceTime_start)) {
            const string tStart = vm.value(op_extract_reduceTime_start);
            if (!start.parseISO8601(tStart)) {
                LOG4FIMEX(logger, Logger::FATAL, "cannot parse time '" << tStart << "'");
                exit(1);
            }
        }
        FimexTime end(FimexTime::max_date_time);
        if (vm.is_set(op_extract_reduceTime_end)) {
            const string tEnd = vm.value(op_extract_reduceTime_end);
            if (! end.parseISO8601(tEnd) ) {
                LOG4FIMEX(logger, Logger::FATAL, "cannot parse time '" << tEnd << "'");
                exit(1);
            }
        }
        extractor->reduceTime(start, end);
    }
    if (vm.is_set(op_extract_reduceVerticalAxis_unit)) {
        if (!(vm.is_set(op_extract_reduceVerticalAxis_start) && vm.is_set(op_extract_reduceVerticalAxis_end))) {
            LOG4FIMEX(logger, Logger::FATAL, "extract.reduceVerticalAxis requires all 'start','end','unit'");
            exit(1);
        }
        const string& unit = vm.value(op_extract_reduceVerticalAxis_unit);
        double start = string2type<double>(vm.value(op_extract_reduceVerticalAxis_start));
        double end = string2type<double>(vm.value(op_extract_reduceVerticalAxis_end));
        extractor->reduceVerticalAxis(unit, start, end);
    }
    if (vm.is_set(op_extract_reduceToBoundingBox_south) || vm.is_set(op_extract_reduceToBoundingBox_north) || vm.is_set(op_extract_reduceToBoundingBox_west) ||
        vm.is_set(op_extract_reduceToBoundingBox_east)) {
        po::option_cx bb[4] = {&op_extract_reduceToBoundingBox_south, &op_extract_reduceToBoundingBox_north, &op_extract_reduceToBoundingBox_west,
                               &op_extract_reduceToBoundingBox_east};
        double bbVals[4];
        for (int i = 0; i < 4; i++) {
            if (!vm.is_set(bb[i])) {
                LOG4FIMEX(logger, Logger::FATAL, "extract.reduceToBoundingBox." << bb[i] << " missing");
                exit(1);
            }
            bbVals[i] = string2type<double>(vm.value(bb[i]));
        }
        LOG4FIMEX(logger, Logger::DEBUG, "reduceLatLonBoudingBox(" << join(&bbVals[0], &bbVals[0]+4, ",")<<")");
        extractor->reduceLatLonBoundingBox(bbVals[0], bbVals[1], bbVals[2], bbVals[3]);
    }
    if (vm.is_set(op_extract_selectVariables)) {
        const vector<string>& vars = vm.values(op_extract_selectVariables);
        bool addAuxiliaryVariables = !vm.is_set(op_extract_selectVariables_noAuxiliary);
        extractor->selectVariables(set<string>(vars.begin(), vars.end()), addAuxiliaryVariables);
    }

    if (vm.is_set(op_extract_removeVariable)) {
        for (const std::string& v : vm.values(op_extract_removeVariable))
            extractor->removeVariable(v);
    }
    printReaderStatements("extract", vm, extractor);

    return extractor;
}

CDMReader_p getCDMQualityExtractor(const string& version, const po::value_set& vm, CDMReader_p dataReader)
{
    const string io = "qualityExtract"+version, autoConfKey = io +".autoConfigString";
    string autoConf;
    if (po::option_cx opt = vm.find(autoConfKey))
        autoConf = vm.value(opt);
    const string config = getConfig(io, vm);
    if (autoConf != "" || config != "") {
        LOG4FIMEX(logger, Logger::DEBUG, "adding CDMQualityExtractor with (" << autoConf << "," << config <<")");
        dataReader = std::make_shared<CDMQualityExtractor>(dataReader, autoConf, config);
    }
    printReaderStatements(io, vm, dataReader);
    return dataReader;
}

CDMReader_p getCDMTimeInterpolator(const po::value_set& vm, CDMReader_p dataReader)
{
    string timeSpec;
    if (!getOption(op_timeInterpolate_timeSpec, vm, timeSpec)) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "timeInterpolate.timeSpec found with spec: " << timeSpec);
    std::shared_ptr<CDMTimeInterpolator> timeInterpolator(new CDMTimeInterpolator(dataReader));
    timeInterpolator->changeTimeAxis(timeSpec);
    printReaderStatements("timeInterpolate", vm, timeInterpolator);

    return timeInterpolator;
}

CDMReader_p getCDMVerticalInterpolator(const po::value_set& vm, CDMReader_p dataReader)
{
    const string config = getConfig("verticalInterpolate", vm);
    if (!config.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "adding NcmlCDMReader with (" << config <<")");
        dataReader = std::make_shared<NcmlCDMReader>(dataReader, XMLInputFile(config));
    }
    vector<string> operations;
    if (getOptions(op_verticalInterpolate_dataConversion, vm, operations)) {
        try {
            dataReader = std::make_shared<CDMPressureConversions>(dataReader, operations);
        } catch (CDMException& ex) {
            LOG4FIMEX(logger, Logger::FATAL, "invalid verticalInterpolate.dataConversion: " + join(operations.begin(), operations.end(), ",") + " " + ex.what());
            exit(1);
        }
    }
    string vtype, vmethod;
    if (!getOption(op_verticalInterpolate_type, vm, vtype) || !getOption(op_verticalInterpolate_method, vm, vmethod)) {
        return dataReader;
    }
    LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate found");
    std::shared_ptr<CDMVerticalInterpolator> verticalReader = std::make_shared<CDMVerticalInterpolator>(dataReader, vtype, vmethod);
    string template_var, replace_z;
    if (getOption(op_verticalInterpolate_toAxis, vm, replace_z)) {
        LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate replace vertical axis with '" << replace_z << "'");
        verticalReader->interpolateToAxis(replace_z);
    } else if (getOption(op_verticalInterpolate_templateVar, vm, template_var)) {
        LOG4FIMEX(logger, Logger::DEBUG, "verticalInterpolate to template var '" << template_var << "'");
        verticalReader->interpolateByTemplateVariable(template_var);
    } else {
        string level1_text, level2_text;
        if (!getOption(op_verticalInterpolate_level1, vm, level1_text)) {
            LOG4FIMEX(logger, Logger::FATAL, "verticalInterpolate needs level1");
            exit(1);
        }
        vector<double> level1 = tokenizeDotted<double>(level1_text,",");
        if (getOption(op_verticalInterpolate_level2, vm, level2_text)) {
            LOG4FIMEX(logger, Logger::WARN, "verticalInterpolate level2 ignored");
        }
        verticalReader->interpolateToFixed(level1);
    }
    bool ignoreValidityMin;
    if (getOption(op_verticalInterpolate_ignoreValidityMin, vm, ignoreValidityMin)) {
        verticalReader->ignoreValidityMin(ignoreValidityMin);
    }
    bool ignoreValidityMax;
    if (getOption(op_verticalInterpolate_ignoreValidityMax, vm, ignoreValidityMax)) {
        verticalReader->ignoreValidityMax(ignoreValidityMax);
    }
    printReaderStatements("verticalInterpolate", vm, verticalReader);
    return verticalReader;
}

std::shared_ptr<InterpolatorProcess2d> parseProcess(const string& procString, const string& logProcess)
{
    std::smatch what;
    if (std::regex_match(procString, what, std::regex("\\s*fill2d\\(([^, ]+), *([^, ]+), *([^ )]+)\\).*"))) {
        double critx = string2type<double>(what[1]);
        double cor = string2type<double>(what[2]);
        size_t maxLoop = string2type<size_t>(what[3]);
        LOG4FIMEX(logger, Logger::DEBUG, "running interpolate " << logProcess << ": fill2d(" << critx << "," << cor << "," << maxLoop << ")");
        return std::make_shared<InterpolatorFill2d>(critx, cor, maxLoop);
    } else if (std::regex_match(procString, what, std::regex("\\s*creepfill2d\\((.+)\\).*"))) {
        vector<string> vals = tokenize(what[1], ",");
        if (vals.size() == 2) {
            unsigned short repeat = string2type<unsigned short>(vals.at(0));
            char setWeight = string2type<char>(vals.at(1));
            LOG4FIMEX(logger, Logger::DEBUG, "running interpolate " << logProcess << ": creepfill2d(" << repeat << "," << setWeight << ")");
            return std::make_shared<InterpolatorCreepFill2d>(repeat, setWeight);
        } else if (vals.size() == 3) {
            unsigned short repeat = string2type<unsigned short>(vals.at(0));
            char setWeight = string2type<char>(vals.at(1));
            float defVal = string2type<float>(vals.at(2));
            LOG4FIMEX(logger, Logger::DEBUG, "running interpolate " << logProcess << ": creepfillval2d(" << repeat << "," << setWeight << "," << defVal << ")");
            return std::make_shared<InterpolatorCreepFillVal2d>(repeat, setWeight, defVal);
        } else {
            throw CDMException("creepfill requires two or three arguments, got " + what[1].str());
        }
     }
     throw CDMException("undefined interpolate."+logProcess+": " + procString);
}

CDMInterpolator_p createCDMInterpolator(const po::value_set& vm, CDMReader_p dataReader)
{
    CDMInterpolator_p interpolator = std::make_shared<CDMInterpolator>(dataReader);
    string value;
    if (getOption(op_interpolate_latitudeName, vm, value)) {
        interpolator->setLatitudeName(value);
    }
    if (getOption(op_interpolate_longitudeName, vm, value)) {
        interpolator->setLongitudeName(value);
    }

    if (getOption(op_interpolate_preprocess, vm, value)) {
        interpolator->addPreprocess(parseProcess(value, "preprocess"));
    }
    if (getOption(op_interpolate_postprocess, vm, value)) {
        interpolator->addPostprocess(parseProcess(value, "postprocess"));
    }
    return interpolator;
}

CDMReader_p getCDMInterpolator(const po::value_set& vm, CDMReader_p dataReader)
{
    CDMInterpolator_p interpolator;
    int method = getInterpolationMethod(vm, op_interpolate_method);

    string proj_init;
    if (getOption(op_interpolate_projString, vm, proj_init)) {
        string xAxisUnit, yAxisUnit, xAxisValues, yAxisValues;
        if (!(getOption(op_interpolate_xAxisUnit, vm, xAxisUnit) && getOption(op_interpolate_yAxisUnit, vm, yAxisUnit))) {
            LOG4FIMEX(logger, Logger::FATAL, "xAxisUnit and yAxisUnit required");
            exit(1);
        }
        if (!(getOption(op_interpolate_xAxisValues, vm, xAxisValues) && getOption(op_interpolate_yAxisValues, vm, yAxisValues))) {
            LOG4FIMEX(logger, Logger::FATAL, "xAxisValues and yAxisValues required");
            exit(1);
        }
        interpolator = createCDMInterpolator(vm, dataReader);
        if (vm.is_set(op_interpolate_distanceOfInterest)) {
            interpolator->setDistanceOfInterest(string2type<double>(vm.value(op_interpolate_distanceOfInterest)));
        }
        interpolator->changeProjection(method, proj_init, xAxisValues, yAxisValues, xAxisUnit, yAxisUnit, vm.value(op_interpolate_xAxisType),
                                       vm.value(op_interpolate_yAxisType));
    } else if (vm.is_set(op_interpolate_template)) {
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjection(method, vm.value(op_interpolate_template));
    } else if (vm.is_set(op_interpolate_vcrossNames)) {
        if (!vm.is_set(op_interpolate_vcrossNoPoints)) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires vcrossNoPoints, too");
            exit(1);
        }
        if (!vm.is_set(op_interpolate_longitudeValues)) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires longitudeValues, too");
            exit(1);
        }
        if (!vm.is_set(op_interpolate_latitudeValues)) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames requires latitudeValues, too");
            exit(1);
        }
        vector<string> vNames = tokenize(vm.value(op_interpolate_vcrossNames), ",");
        vector<size_t> pointNo = tokenizeDotted<size_t>(vm.value(op_interpolate_vcrossNoPoints));
        vector<double> latVals = tokenizeDotted<double>(vm.value(op_interpolate_latitudeValues));
        vector<double> lonVals = tokenizeDotted<double>(vm.value(op_interpolate_longitudeValues));
        if (vNames.size() != pointNo.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.vcrossNames and vcrossNoPoints need same size: " << vNames.size() << "!=" << pointNo.size());
            exit(1);
        }
        size_t pointSum = std::accumulate(pointNo.begin(),pointNo.end(),0);
        if (pointSum != latVals.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues size does not match the sum of vcrossNoPoints: " << latVals.size() << "!=" << pointSum);
            exit(1);
        }
        if (latVals.size() != lonVals.size()) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues size does not match longitudeVals size: " << latVals.size() << "!=" << lonVals.size());
            exit(1);
        }
        vector<CrossSectionDefinition> csd;
        size_t llPos = 0;
        for (size_t i = 0; i < vNames.size(); i++) {
            vector<pair<double, double> > lonLatVals;
            size_t points = pointNo.at(i);
            for (size_t j = 0; j < points; ++j) {
                lonLatVals.push_back(make_pair(lonVals.at(llPos+j), latVals.at(llPos+j)));
            }
            csd.push_back(CrossSectionDefinition(vNames.at(i), lonLatVals));
            llPos += points;
        }
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjectionToCrossSections(method, csd);
    } else if (vm.is_set(op_interpolate_latitudeValues)) {
        if (!vm.is_set(op_interpolate_longitudeValues)) {
            LOG4FIMEX(logger, Logger::FATAL, "interpolate.latitudeValues requires longitudeValues, too");
            exit(1);
        }
        vector<double> latVals = tokenizeDotted<double>(vm.value(op_interpolate_latitudeValues));
        vector<double> lonVals = tokenizeDotted<double>(vm.value(op_interpolate_longitudeValues));
        interpolator = createCDMInterpolator(vm, dataReader);
        interpolator->changeProjection(method, lonVals, latVals);
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "interpolate.projString, interpolate.template or interpolate.latitudeValues not found, no interpolation used");
        return dataReader;
    }

    printReaderStatements("interpolate", vm, interpolator);
    return interpolator;
}

CDMReader_p getNcmlCDMReader(const po::value_set& vm, CDMReader_p dataReader)
{
    const string config = getConfig("ncml", vm);
    if (!config.empty()) {
        dataReader = std::make_shared<NcmlCDMReader>(dataReader, XMLInputFile(config));
        printReaderStatements("ncml", vm, dataReader);
    }
    return dataReader;
}

CDMReader_p getCDMMerger(const po::value_set& vm, CDMReader_p dataReader)
{
    if (not(vm.is_set(op_merge_inner_file) or vm.is_set(op_merge_inner_type) or vm.is_set(op_merge_inner_config) or vm.is_set(op_merge_smoothing) or
            vm.is_set(op_merge_method)))
        return dataReader;

    CDMReader_p readerI = getCDMFileReader(vm, "merge.inner");
    if( not readerI )
        throw CDMException("could not create reader for inner in merge");
    if (vm.is_set(op_merge_inner_cfg)) {
        po::value_set mvm = po::parse_config_file(vm.value(op_merge_inner_cfg), config_file_options);
        // apply all fimex processing tasks to the merge.inner stream
        readerI = applyFimexStreamTasks(mvm, readerI);
    }

    CDMMerger_p merger = std::make_shared<CDMMerger>(readerI, dataReader);

    if (vm.is_set(op_merge_smoothing)) {
        merger->setSmoothing(createSmoothingFactory(vm.value(op_merge_smoothing)));
    } else {
        LOG4FIMEX(logger, Logger::DEBUG, "no merge.smoothing given, using default as defined by CDMMerger");
    }

    if (vm.is_set(op_merge_keepOuterVariables)) {
        merger->setKeepOuterVariables(true);
    }
    int method = getInterpolationMethod(vm, op_merge_method);
    merger->setGridInterpolationMethod(method);

    if (vm.is_set(op_merge_projString)) {
        if (not(vm.is_set(op_merge_xAxisUnit) && vm.is_set(op_merge_yAxisUnit)))
            throw CDMException("merge.xAxisUnit and merge.yAxisUnit required");

        if (not(vm.is_set(op_merge_xAxisValues) && vm.is_set(op_merge_yAxisValues)))
            throw CDMException("merge.xAxisValues and merge.yAxisValues required");

        merger->setTargetGrid(vm.value(op_merge_projString), vm.value(op_merge_xAxisValues), vm.value(op_merge_yAxisValues), vm.value(op_merge_xAxisUnit),
                              vm.value(op_merge_yAxisUnit), vm.value(op_merge_xAxisType), vm.value(op_merge_yAxisType));
    } else {
        merger->setTargetGridFromInner();
    }

    printReaderStatements("merge", vm, merger);
    return merger;
}

CDMReader_p applyFimexStreamTasks(const po::value_set& vm, CDMReader_p dataReader)
{
    dataReader = getCDMProcessor(vm, dataReader);
    dataReader = getCDMQualityExtractor("", vm, dataReader);
    dataReader = getCDMExtractor(vm, dataReader);
    dataReader = getCDMTimeInterpolator(vm, dataReader);
    dataReader = getCDMInterpolator(vm, dataReader);
    dataReader = getCDMVerticalInterpolator(vm, dataReader);
    dataReader = getCDMMerger(vm, dataReader);
    dataReader = getCDMQualityExtractor("2", vm, dataReader);
    dataReader = getNcmlCDMReader(vm, dataReader);
    return dataReader;
}

void fillWriteCDM(CDMReader_p dataReader, po::value_set& vm)
{
    string fillFile;
    if (!getOption(op_output_fillFile, vm, fillFile))
        return;
    const string type = getType("output", vm);
    const string config = getConfig("output", vm);

    LOG4FIMEX(logger, Logger::DEBUG, "filling file " << fillFile << " without config");
    CDMReaderWriter_p rw = CDMFileReaderFactory::createReaderWriter("nc", fillFile);
    FillWriter(dataReader, rw, config);
}

void writeCDM(CDMReader_p dataReader, const po::value_set& vm)
{
    printReaderStatements("output", vm, dataReader);
    string fileName;
    if (!getOption(op_output_file, vm, fileName)) {
        LOG4FIMEX(logger, Logger::DEBUG, "no output.file selected");
        return;
    }
    const string type = getType("output", vm);
    const string config = getConfig("output", vm);
    try {
      createWriter(dataReader, type, fileName, config);
    } catch (exception& ex) {
      LOG4FIMEX(logger, Logger::FATAL, "exception while writing: " << ex.what());
      exit(1);
    }
}

int run(int argc, char* args[])
{
    config_file_options
        << op_input_file
        << op_input_type
        << op_input_config
        << op_input_optional
        << op_input_printNcML
        << op_input_printCS
        << op_input_printSize
        << op_output_file
        << op_output_fillFile
        << op_output_type
        << op_output_config
        << op_output_printNcML
        << op_output_printCS
        << op_output_printSize
        << op_process_accumulateVariable
        << op_process_deaccumulateVariable
//        << op_process_rotateVectorToLatLonX
//        << op_process_rotateVectorToLatLonY
        << op_process_rotateVector_direction
        << op_process_rotateVector_angle
        << op_process_rotateVector_x
        << op_process_rotateVector_stdNameX
        << op_process_rotateVector_y
        << op_process_rotateVector_stdNameY
        << op_process_rotateVector_all
        << op_process_addVerticalVelocity
        << op_process_printNcML
        << op_process_printCS
        << op_process_printSize
        << op_extract_removeVariable
        << op_extract_selectVariables
        << op_extract_selectVariables_noAuxiliary
        << op_extract_reduceDimension_name
        << op_extract_reduceDimension_start
        << op_extract_reduceDimension_end
        << op_extract_pickDimension_name
        << op_extract_pickDimension_list
        << op_extract_reduceTime_start
        << op_extract_reduceTime_end
        << op_extract_reduceVerticalAxis_unit
        << op_extract_reduceVerticalAxis_start
        << op_extract_reduceVerticalAxis_end
        << op_extract_reduceToBoundingBox_south
        << op_extract_reduceToBoundingBox_north
        << op_extract_reduceToBoundingBox_east
        << op_extract_reduceToBoundingBox_west
        << op_extract_printNcML
        << op_extract_printCS
        << op_extract_printSize
        << op_qualityExtract_autoConfString
        << op_qualityExtract_config
        << op_qualityExtract_printNcML
        << op_qualityExtract_printCS
        << op_qualityExtract_printSize
        << op_interpolate_projString
        << op_interpolate_method
        << op_interpolate_xAxisValues
        << op_interpolate_yAxisValues
        << op_interpolate_xAxisUnit
        << op_interpolate_yAxisUnit
        << op_interpolate_xAxisType
        << op_interpolate_yAxisType
        << op_interpolate_distanceOfInterest
        << op_interpolate_latitudeName
        << op_interpolate_longitudeName
        << op_interpolate_preprocess
        << op_interpolate_postprocess
        << op_interpolate_latitudeValues
        << op_interpolate_longitudeValues
        << op_interpolate_vcrossNames
        << op_interpolate_vcrossNoPoints
        << op_interpolate_template
        << op_interpolate_printNcML
        << op_interpolate_printCS
        << op_interpolate_printSize

        << op_merge_inner_file
        << op_merge_inner_type
        << op_merge_inner_config
        << op_merge_inner_cfg
        << op_merge_smoothing
        << op_merge_keepOuterVariables
        << op_merge_method
        << op_merge_projString
        << op_merge_xAxisValues
        << op_merge_yAxisValues
        << op_merge_xAxisUnit
        << op_merge_yAxisUnit
        << op_merge_xAxisType
        << op_merge_yAxisType
        << op_merge_printNcML
        << op_merge_printCS
        << op_merge_printSize

        << op_verticalInterpolate_config
        << op_verticalInterpolate_type
        << op_verticalInterpolate_ignoreValidityMin
        << op_verticalInterpolate_ignoreValidityMax
        << op_verticalInterpolate_method
        << op_verticalInterpolate_templateVar
        << op_verticalInterpolate_toAxis
        << op_verticalInterpolate_level1
        << op_verticalInterpolate_level2
        << op_verticalInterpolate_dataConversion
        << op_verticalInterpolate_printNcML
        << op_verticalInterpolate_printCS
        << op_verticalInterpolate_printSize
        << op_timeInterpolate_timeSpec
        << op_timeInterpolate_printNcML
        << op_timeInterpolate_printCS
        << op_timeInterpolate_printSize
        << op_qualityExtract2_autoConfString
        << op_qualityExtract2_config
        << op_qualityExtract2_printNcML
        << op_qualityExtract2_printCS
        << op_qualityExtract2_printSize
        << op_ncml_config
        << op_ncml_printNcML
        << op_ncml_printCS
        << op_ncml_printSize
        ;

    po::option_set cmdline_options = config_file_options;
    cmdline_options
        << op_help
        << op_version
        << op_debug
        << op_log4cpp
        << op_print_options
        << op_config
        << op_num_threads
        ;

    std::vector<std::string> positional;
    po::value_set vm = parse_command_line(argc, args, cmdline_options, positional);
    if (vm.is_set(op_config))
        vm.add(parse_config_file(vm.value(op_config), config_file_options));
    if (argc == 1 || vm.is_set(op_help)) {
        writeUsage(cout, cmdline_options);
        return 0;
    }
    if (vm.is_set(op_version)) {
        cout << "fimex version " << fimexVersion() <<" (" << mifi_version_major() << "." << mifi_version_minor() << "." << mifi_version_patch() << "-" << std::hex << mifi_version_status() << ")" << endl;
        return 0;
    }

    const int opt_debug = vm.is_set(op_debug);
    if (opt_debug >= 1) {
        // TODO allow for multiple occurances and use INFO as == 1
        defaultLogLevel(Logger::DEBUG);
    } else {
        defaultLogLevel(Logger::WARN);
    }

    if (vm.is_set(op_log4cpp)) {
#ifdef HAVE_LOG4CPP
        Logger::setClass(Logger::LOG4CPP);
        std::string propFile = vm.value(op_log4cpp);
        if (propFile != "-") {
            log4cpp::PropertyConfigurator::configure(propFile);
            if (opt_debug)
                LOG4FIMEX(logger, Logger::WARN, "--log4cpp config file overrides loglevel from --debug");
        }
#else
        Logger::setClass(Logger::LOG2STDERR);
        LOG4FIMEX(logger, Logger::WARN, "fimex was compiled without log4cpp");
#endif
    } else {
        Logger::setClass(Logger::LOG2STDERR);
    }

    int num_threads = 1;
    if (vm.is_set(op_num_threads))
        num_threads = string2type<int>(vm.value(op_num_threads));
    mifi_setNumThreads(num_threads);

    if (vm.is_set(op_print_options)) {
        cmdline_options.dump(cout, vm);
    } else if (opt_debug) {
        cmdline_options.dump(cerr, vm);
    }

    po::positional_args_consumer pac(vm, positional);
    pac >> op_input_file;
    if (!pac.done())
        pac >> op_output_file;
    if (!pac.done()) {
        cmdline_options.dump(cerr, vm);
        pac.dump(cerr);
        LOG4FIMEX(logger, Logger::FATAL, "too many positional arguments");
        return 1;
    }

    CDMReader_p dataReader = getCDMFileReader(vm);
    dataReader = applyFimexStreamTasks(vm, dataReader);
    fillWriteCDM(dataReader, vm);
    writeCDM(dataReader, vm);

    return 0;
}

#ifdef HAVE_MPI
struct MPI_Init_Finalize {
  MPI_Init_Finalize(int argc, char* args[])
  {
    MPI_Init(&argc, &args);
    mifi_initialize_mpi(MPI_COMM_WORLD, MPI_INFO_NULL);
  }

  ~MPI_Init_Finalize()
  {
    mifi_free_mpi();
    MPI_Finalize();
  }
};
#endif

} // namespace

int main(int argc, char* args[])
{
    int retStatus = 0;

#ifdef HAVE_MPI
    MPI_Init_Finalize mpi(argc, args);
#endif

    // wrapping main-functions in run to catch all exceptions
#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    try {
#else
#warning Not catching exceptions in main method
#endif

        retStatus = run(argc, args);

#ifndef DO_NOT_CATCH_EXCEPTIONS_FROM_MAIN
    } catch (const po::option_error& ex) {
        clog << "invalid options: " << ex.what() << endl;
        retStatus = 1;
    } catch (exception& ex) {
        clog << "exception occured: " << ex.what() << endl;
        retStatus = 1;
    }
#endif

    return retStatus;
}
