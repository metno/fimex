#include <iostream>
#include <fstream>
#include <cctype>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>
#include "../../config.h"
#include "CDMReader.h"
#ifdef HAVE_LIBMIC
#include "FeltCDMReader.h"
#endif
#ifdef HAVE_NETCDF
#include "NetCDF_CDMWriter.h"
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

static auto_ptr<CDMReader> getCDMReader(po::variables_map& vm) {
	string type = getType("input", vm);
#ifdef HAVE_LIBMIC
	if (type == "flt" || type == "dat") {
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

	cerr << "unable to read type: " << type << endl;
	exit(1);
	
	return auto_ptr<CDMReader>();
}

static void writeCDM(auto_ptr<CDMReader> dataReader, po::variables_map& vm) {
	string type = getType("output", vm);
#ifdef HAVE_NETCDF
	if (type == "nc" || type == "cdf") {
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
    
    auto_ptr<CDMReader> dataReader = getCDMReader(vm);
    //dataReader = getCDMExtractor(vm);
    writeCDM(dataReader, vm);
    
	return 0;
}
