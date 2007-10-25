#include "felt_reader/FeltParameters.h"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <boost/regex.hpp> 
#include <boost/tokenizer.hpp>

FeltParameters::FeltParameters() {
	init();
}

FeltParameters::FeltParameters(std::string filename)
{
	init(filename);
}

void FeltParameters::init(std::string configFile) {
	boost::regex sectionEx("\\s*<([^/].*)>\\s*");
	boost::regex parameterEx("\\s*([^=\\s]+)=(\\S+)");
	boost::smatch what; 
	
	std::ifstream dianaFeltDeclarations(configFile.c_str());
	if (dianaFeltDeclarations.is_open()) {
		std::string line;
		std::string section;
		std::string lastLine("");
		boost::regex endSectionEx;
		while (std::getline(dianaFeltDeclarations, line)) {
			line = boost::regex_replace(line, boost::regex("#.*"), "");
			if (boost::regex_match(line, boost::regex("\\s*"))) {
				continue;
			}

			if (boost::regex_match(line, what, boost::regex("(.*)\\\\.*"))) {
				// continuing line
				lastLine.append(what[1].first, what[1].second);
				continue;
			} else if (lastLine.length() > 0) {
					line.assign(lastLine + " " + line);
					lastLine.assign("");
			}

   			if (boost::regex_match(line, what, sectionEx)) {
   				section = what[1].str();
   				endSectionEx = boost::regex("\\s*</\\Q"+section+"\\E>\\s*");
   			} else if (!endSectionEx.empty() &&  boost::regex_match(line, what, endSectionEx)) {
   				section.erase();
   			} else if (section == "METNOFIELDFILE_PARAMETERS") {
				std::string::const_iterator start, end;
				boost::match_flag_type flags = boost::match_default; 
   				start = line.begin(); 
   				end = line.end();
				while (boost::regex_search(start, end, what, parameterEx)) {
					//std::cerr << "Debug: " << what[1] << "===" << what[2] << std::endl;
					// update start position
					start = what[0].second;
					// update flags to allow for --first as start-position
      				flags |= boost::match_prev_avail; 
      				flags |= boost::match_not_bob;
      				
      				parameterMap[ what[1].str() ] = diana2feltparameters(what[2].str());
	    		}
			}
			
		}
	}	
}
FeltParameters::~FeltParameters()
{
}

boost::array<short, 16> FeltParameters::diana2feltparameters(const std::string& dianaString)
{
	boost::regex equalSeparatedRegex("\\s*(\\w*)=(\\d+)\\s*");
	boost::smatch what;
	boost::array<short, 16> diana2feltParameters;
	for (int i = 0; i < 16; i++) {
		diana2feltParameters[i] = ANY_VALUE();
	}

	boost::char_separator<char> colonSep(":");
	boost::char_separator<char> commaSep(",");
	boost::tokenizer<boost::char_separator<char> > tok(dianaString, colonSep);
	boost::tokenizer<boost::char_separator<char> >::iterator tokIt = tok.begin();
	
	boost::tokenizer<boost::char_separator<char> > tok2(*tokIt, commaSep);
	boost::tokenizer<boost::char_separator<char> >::iterator tok2It = tok2.begin();
	for (int i = 0; tok2It != tok2.end(); ++tok2It, ++i) {
		short value(std::atoi((*tok2It).c_str()));
		switch (i) {
			case 0: diana2feltParameters[11] = value; break; // param
			case 1: diana2feltParameters[10] = value; break; // v.coord
			case 2: diana2feltParameters[12] = value; break; // level
		}
	}
	
	++tokIt;
	for (;tokIt != tok.end(); ++tokIt) {
		if (boost::regex_match(*tokIt, what, equalSeparatedRegex)) {
			short id(std::atoi(what[2].str().c_str()));
			if (what[1].str() == "prod") {
				diana2feltParameters[0] = id;
			} else if (what[1].str() == "grid") {
				diana2feltParameters[1] = id;
			} else if (what[1].str() == "dtype") {
				diana2feltParameters[8] = id;
			} else if (what[1].str() == "level") {
				diana2feltParameters[12] = id;
			} else if (what[1].str() == "idnum") {
				diana2feltParameters[13] = id;
			}
		}	
	}
	return diana2feltParameters;
}

const boost::array<short, 16>& FeltParameters::getParameters(const std::string& input) {
	std::map<std::string, boost::array<short, 16> >::iterator it = parameterMap.find(input);
	if (it == parameterMap.end()) {
		// not found
		return ANY_ARRAY();
	} else {
		return it->second;
	}
}

const std::string& FeltParameters::getParameterName(const boost::array<short, 16>&  feltParams) {
	std::map<std::string, boost::array<short, 16> >::iterator it;
	for	(it = parameterMap.begin(); it != parameterMap.end(); ++it) {
		boost::array<short, 16>  value = it->second;
		int errors = 0;
		for (int i = 0; i < 16; i++) {
			if ((value[i] != ANY_VALUE()) && value[i] != feltParams[i]) {
				errors++;
				break;
			}
		}
		if (errors == 0) {
			return it->first;
		}
	}
	return UNDEFINED();
}
