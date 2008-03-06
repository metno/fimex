#include <iostream>
#include <fstream>
#include <boost/program_options.hpp>
#include "../config.h"

namespace po = boost::program_options;
using namespace std;

int main(int argc, char* args[])
{
	// Declare the supported options.
	po::options_description generic("Generic options");
	std::string configFile("utplukk.cfg");
	generic.add_options()
	    ("help,h", "help message")
	    ("version,v", "program version")
	    ("config,c", po::value<string>(&configFile)->default_value(configFile), "configuration file")
	    ;

	// read this first to get the configFile right
	po::variables_map genVm;
	po::store(po::command_line_parser(argc, args).options(generic).run(), genVm);
    po::notify(genVm); 
    

	
	// Declare a group of options that will be 
	// allowed both on command line and in
	// config file
	po::options_description config("Configurational options");
	config.add_options()
		("input-file", po::value<string>(), "input file")
		("input-type", "filetype of intput file")
		("input-config", "non-standard input configuration")
		("output-file", "output file")
		("output-type", "filetype of output file")
		("output-config", "non-standard output configuration")
		;
	
	
	po::options_description cmdline_options;
	cmdline_options.add(generic).add(config);

	po::options_description config_file_options;
	config_file_options.add(config);
	
    po::positional_options_description p;
    p.add("input-file", 1);
    p.add("output-file", 1);

	
	po::variables_map vm;
    po::store(po::command_line_parser(argc, args).options(cmdline_options).positional(p).run(), vm);
    ifstream ifs(configFile.c_str());
    po::store(po::parse_config_file(ifs, config_file_options), vm);
    po::notify(vm);
    if (argc == 1 || vm.count("help")) {
        cout << "usage: utplukk --input  FILENAME [--input-type INPUT_TYPE]" << endl;
        cout << "               --output FILENAME [--output-type OUTPUT_TYPE]" << endl;
        cout << "               [--input-config CFGFILENAME] [--output-config CFGFILENAME]" << endl;
        cout << "               [--extract-config EXTRACT_INI_FILENAME]" << endl;
        cout << "               [--interpolate-config INTERPOLATE_INI_FILENAME]" << endl;
        cout << endl;
        cout << generic << endl;
        cout << config << endl;
        return 0;
    }
    if (vm.count("version")) {
    	cout << "utplukk version " << VERSION << endl;
    	return 0;
    }
    
	return 0;
}
