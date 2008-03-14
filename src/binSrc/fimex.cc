#include <iostream>
#include <fstream>
#include <cctype>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>
#include <boost/tokenizer.hpp>
#include "../../config.h"
#include "CDMReader.h"
#include "CDMExtractor.h"
#include "CDMInterpolator.h"
#ifdef HAVE_LIBMIC
#include "FeltCDMReader.h"
#endif
#ifdef HAVE_NETCDF
#include "NetCDF_CDMWriter.h"
#include "NetCDF_CF10_CDMReader.h"
#endif

namespace po = boost::program_options;
using namespace std;
using namespace MetNoUtplukk;

static void writeUsage(ostream& out, const po::options_description& generic, const po::options_description& config) {
    out << "usage: utplukk --input.file  FILENAME [--input.type  INPUT_TYPE]" << endl;
    out << "               --output.file FILENAME [--output.type OUTPUT_TYPE]" << endl;
    out << "               [--input.config CFGFILENAME] [--output.config CFGFILENAME]" << endl;
    out << "               [--extract....]" << endl;
    out << "               [--interpolate....]" << endl;
    out << endl;
    out << generic << endl;
    out << config << endl;	
}

template <typename T>
static void writeOption(ostream& out, const string& var, const po::variables_map& vm) {
	if (vm.count(var)) {
		out << var << ": " << vm[var].as<T>() << endl;
	}
}

template <>
static void writeOption<boost::any>(ostream& out, const string& var, const po::variables_map& vm) {
	// variables without real value, just set or unset
	if (vm.count(var)) {
		out << var  << endl;
	}
}

template <typename T>
static void writeVectorOption(ostream& out, const string& var, const po::variables_map& vm) {
	if (vm.count(var)) {
		vector<T> vals = vm[var].as<vector<T> >();
		typedef typename vector<T>::iterator VIT;
		for (VIT it = vals.begin(); it != vals.end(); ++it) {
			out << var << ": " << *it << endl;
		}
	}
}

static void writeOptions(ostream& out, const po::variables_map& vm) {
	out << "Currently active options: " << endl;
	writeOption<boost::any>(out, "help", vm);
	writeOption<boost::any>(out, "version", vm);
	writeOption<boost::any>(out, "debug", vm);
	writeOption<boost::any>(out, "print-options", vm);
	writeOption<string>(out, "config", vm);
	writeOption<string>(out, "input.file", vm);
	writeOption<string>(out, "input.type", vm);
	writeOption<string>(out, "input.config", vm);
	writeOption<string>(out, "output.file", vm);
	writeOption<string>(out, "output.type", vm);
	writeOption<string>(out, "output.config", vm);
	writeVectorOption<string>(out, "extract.removeVariable", vm);
	writeVectorOption<string>(out, "extract.reduceDimension.name", vm);
	writeVectorOption<int>(out, "extract.reduceDimension.start", vm);
	writeVectorOption<int>(out, "extract.reduceDimension.end", vm);
	writeOption<string>(out, "interpolate.projString", vm);
	writeOption<string>(out, "interpolate.method", vm);
	writeOption<string>(out, "interpolate.xAxisValues", vm);
	writeOption<string>(out, "interpolate.yAxisValues", vm);
	writeOption<string>(out, "interpolate.xAxisUnit", vm);
	writeOption<string>(out, "interpolate.yAxisUnit", vm);
}

static string getType(const string& io, po::variables_map& vm) {
	string type;
	if (vm.count(io+".type")) {
		type = vm[io+".type"].as<string>();
	} else {
		boost::smatch what; 
		if (boost::regex_match(vm[io+".file"].as<string>(), what, boost::regex(".*\\.(\\w+)$"))) {
			type = what[1].str();
		}
	}
	std::transform(type.begin(), type.end(), type.begin(), (int(*)(int)) tolower);
	return type;
}

static auto_ptr<CDMReader> getCDMFileReader(po::variables_map& vm) {
	string type = getType("input", vm);
#ifdef HAVE_LIBMIC
	if (type == "flt" || type == "dat" || type == "felt") {
		string config(DATADIR);
		config += "/flt2nc_variables.xml";
		if (vm.count("input.config")) {
			config = vm["input.config"].as<string>();
		}
		if (vm.count("debug"))
			cerr << "reading Felt-File " << vm["input.file"].as<string>() << " with config " << config << endl;
		return auto_ptr<CDMReader>(new FeltCDMReader(vm["input.file"].as<string>(), config));
	}
#endif
#ifdef HAVE_NETCDF
	if (type == "nc" || type == "cdf" || type == "netcdf") {
		if (vm.count("debug"))
			cerr << "reading Felt-File " << vm["input.file"].as<string>() << " without config"<< endl;
		return auto_ptr<CDMReader>(new NetCDF_CF10_CDMReader(vm["input.file"].as<string>()));
	}
#endif
	cerr << "unable to read type: " << type << endl;
	exit(1);
	
	return auto_ptr<CDMReader>();
}

static auto_ptr<CDMReader> getCDMExtractor(po::variables_map& vm, auto_ptr<CDMReader> dataReader) {
	auto_ptr<CDMExtractor> extractor(new CDMExtractor(boost::shared_ptr<CDMReader>(dataReader)));
	if (vm.count("extract.removeVariable")) {
		vector<string> vars = vm["extract.removeVariable"].as<vector<string> >();
		for (vector<string>::iterator it = vars.begin(); it != vars.end(); ++it) {
			extractor->removeVariable(*it);
		}
	}
	if (vm.count("extract.reduceDimension.name")) {
		vector<string> vars = vm["extract.reduceDimension.name"].as<vector<string> >();
		vector<int> startPos;
		vector<int> endPos;
		if (vm.count("extract.reduceDimension.start")) {
			startPos = vm["extract.reduceDimension.start"].as<vector<int> >();
		}
		if (vm.count("extract.reduceDimension.end")) {
			endPos = vm["extract.reduceDimension.end"].as<vector<int> >();
		}
		if (startPos.size() != vars.size()) {
			cerr << "extract.reduceDimension.start has not same no. of elements than extract.reduceDimension.name" << endl;
			cerr << "use start = 0 if you don't want to reduce the start-position" << endl;
		}
		if (endPos.size() != vars.size()) {
			cerr << "extract.reduceDimension.start has not same no. of elements than extract.reduceDimension.name" << endl;
			cerr << "use end = 0 if you don't want to reduce the end-position" << endl;
		}
		for (size_t i = 0; i < vars.size(); ++i) {
			extractor->reduceDimensionStartEnd(vars[i], startPos[i], endPos[i]);
		}
	}
	return auto_ptr<CDMReader>(extractor);
}

static string trim(const string& str) {
	int pos1 = str.find_first_not_of(" ");
	int pos2 = str.find_last_not_of(" ");
	return str.substr(pos1, pos2+1);
}

static double toDouble(const string& str) {
	stringstream ss;
	ss << str;
	double d;
	ss >> d;
	return d;
}
static vector<double> parseDoubleString(const string& str) {
	typedef boost::tokenizer<boost::char_separator<char> > 
	    tokenizer;
	  boost::char_separator<char> sep(",");
	  tokenizer tokens(str, sep);
	  vector<double> vals;
	  bool pricks = false;
	  for (tokenizer::iterator tok = tokens.begin(); tok != tokens.end(); ++tok) {
		  string current = trim(*tok);
		  if (current == "...") {
			  pricks = true;
		  } else {
			  double val = toDouble(current); 
			  if (pricks == true) {
				  pricks = false;
				  size_t end = vals.size();
				  if (end < 2) {
					  cerr << "ERROR: cannot use ... expansion at position " << end-1 <<", need at least two values before" << endl;;
					  exit(1);
				  }
				  double last = vals[end-1];
				  double dist = last - vals[end-2];
				  double curVal = last + dist;
				  while (curVal < val) {
					  vals.push_back(curVal);
					  curVal += dist;
				  }
			  }
			  vals.push_back(val);
		  }
	  }
	  return vals;
}

static auto_ptr<CDMReader> getCDMInterpolator(po::variables_map& vm, auto_ptr<CDMReader> dataReader) {
	if (! vm.count("interpolate.projString")) {
		return dataReader;
	}
	auto_ptr<CDMInterpolator> interpolator(new CDMInterpolator(boost::shared_ptr<CDMReader>(dataReader)));
	
	int method = MIUP_NEAREST_NEIGHBOR;
	if (vm.count("interpolate.method")) {
		string m = vm["interpolate.method"].as<string>();
		if (m == "bilinear") {
			method = MIUP_BILINEAR;
		} else if (m == "nearestneighbor") {
			method = MIUP_NEAREST_NEIGHBOR;
		} else if (m == "bicubic") {
			method = MIUP_BICUBIC;
		} else {
			cerr << "WARNING: unknown interpolate.method: " << m << " using nearestneighbor" << endl;
		}
	}
	
	vector<double> xAxisVals;
	if (vm.count("interpolate.xAxisValues")) {
		xAxisVals = parseDoubleString(vm["interpolate.xAxisValues"].as<string>());
	} else {
		cerr << "ERROR: no xAxisValues given" << endl;
		exit(1);
	}
	vector<double> yAxisVals;
	if (vm.count("interpolate.yAxisValues")) {
		yAxisVals = parseDoubleString(vm["interpolate.yAxisValues"].as<string>());
	} else {
		cerr << "ERROR: no yAxisValues given" << endl;
		exit(1);
	}
	
	if (!(vm.count("interpolate.xAxisUnit") && vm.count("interpolate.yAxisUnit"))) {
		cerr << "ERROR: xAxisUnit and yAxisUnit required" << endl;
		exit(1);
	}
	
	interpolator->changeProjection(method, vm["interpolate.projString"].as<string>(), xAxisVals, yAxisVals, vm["interpolate.xAxisUnit"].as<string>(), vm["interpolate.yAxisUnit"].as<string>());
	return auto_ptr<CDMReader>(interpolator);
}

static void writeCDM(auto_ptr<CDMReader> dataReader, po::variables_map& vm) {
	string type = getType("output", vm);
#ifdef HAVE_NETCDF
	if (type == "nc" || type == "cdf" || type == "netcdf") {
		// no config for netcdf!
		if (vm.count("debug"))
			cerr << "writing NetCDF-file " << vm["output.file"].as<string>() << " without config" << endl;
		NetCDF_CDMWriter(boost::shared_ptr<CDMReader>(dataReader), vm["output.file"].as<string>());
		return;
	}
#endif
	
	cerr << "unable to write type: " << type << endl;
	exit(1);
}

int main(int argc, char* args[])
{
	// Declare the supported options.
	po::options_description generic("Generic options");
	std::string configFile("utplukk.cfg");
	generic.add_options()
	    ("help,h", po::value<bool>(), "help message")
	    ("version", "program version")
	    ("debug", "debug program")
	    ("print-options", "print all options")
	    ("config,c", po::value<string>(&configFile)->default_value(configFile), "configuration file")
	    ;

	// Declare a group of options that will be 
	// allowed both on command line and in
	// config file
	po::options_description config("Configurational options");
	config.add_options()
		("input.file", po::value<string>(), "input file")
		("input.type", po::value<string>(), "filetype of intput file")
		("input.config", po::value<string>(), "non-standard input configuration")
		("output.file", po::value<string>(), "output file")
		("output.type", po::value<string>(), "filetype of output file")
		("output.config", po::value<string>(), "non-standard output configuration")
		("extract.removeVariable", po::value<vector<string> >()->composing(), "remove variables")
		("extract.reduceDimension.name", po::value<vector<string> >()->composing(), "name of a dimension to reduce")
        ("extract.reduceDimension.start", po::value<vector<int> >()->composing(), "start position of the dimension to reduce (>=0)")
        ("extract.reduceDimension.end", po::value<vector<int> >()->composing(), "end position of the dimension to reduce")
        ("interpolate.projString", po::value<string>(), "proj4 input string describing the new projection")
        ("interpolate.method", po::value<string>(), "interpolation method, one of nearestneighbor, bilinear or bicubic")
        ("interpolate.xAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5")
        ("interpolate.yAxisValues", po::value<string>(), "string with values on x-Axis, use ... to continue, i.e. 10.5,11,...,29.5")
        ("interpolate.xAxisUnit", po::value<string>(), "unit of x-Axis given as udunits string, i.e. m or degrees_east")
        ("interpolate.yAxisUnit", po::value<string>(), "unit of y-Axis given as udunits string, i.e. m or degrees_north")
		;
	
	
	po::options_description cmdline_options;
	cmdline_options.add(generic).add(config);

	po::options_description config_file_options;
	config_file_options.add(config);
	
    po::positional_options_description p;
    p.add("input.file", 1);
    p.add("output.file", 1);

	// read first only cmd-line to get the configFile right
	po::variables_map genVm;
	po::store(po::command_line_parser(argc, args).options(cmdline_options).run(), genVm);
    po::notify(genVm); 
    
	po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).positional(p).run(), vm);
    ifstream ifs(configFile.c_str());
    po::store(po::parse_config_file(ifs, config_file_options), vm);
    po::notify(vm);
    if (argc == 1 || vm.count("help")) {
    	writeUsage(cout, generic, config);
        return 0;
    }
    if (vm.count("version")) {
    	cout << "utplukk version " << VERSION << endl;
    	return 0;
    }
    if (vm.count("print-options")) {
    	writeOptions(cout, vm);
    }
    if (vm.count("debug") && !vm.count("print-options")) {
    	writeOptions(cerr, vm);
    }
    if (!(vm.count("input.file") && vm.count("output.file"))) {
    	writeUsage(cerr, generic, config);
    	cerr << "ERROR: input.file and output.file required" << endl;
    	exit(1);
    }
    
    auto_ptr<CDMReader> dataReader = getCDMFileReader(vm);
    dataReader = getCDMExtractor(vm, dataReader);
    dataReader = getCDMInterpolator(vm, dataReader);
    writeCDM(dataReader, vm);
    
	return 0;
}
