/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
e
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#ifndef WDBCDMREADERPARSER_H_
#define WDBCDMREADERPARSER_H_

#include "WdbCDMReaderParserInfo.h"

// fimex
//
#include "fimex/XMLDoc.h"

// std
//
#include <string>
#include <vector>
#include <utility>

/**
 * This class will parse command line and extract
 * needed options.
 *
 * One of the options can be wdbcdmreader config
 * file. Parser will work through it as well.
 *
 * Remember, values from command line will
 * override those from config file.
 */
namespace MetNoFimex
{

namespace wdb
{

    class WdbCDMReaderParser
    {
    public:
        WdbCDMReaderParser();
        ~WdbCDMReaderParser();

        /**
         * if bParseConfigFile == false, parse() method
         * will skip to parse the file (even if found
         * within int argc, char* args[] options)
         * this method will work for comman line
         * taken verbatim from main.cpp
         */
        WdbCDMReaderParserInfo parse(int argc, char* args[], bool bParseConfigFile = true);

        /**
         * As Fimex is already parsing cmd line we will use that result
         * we are interested in following options:
         *     1. --input.file (sent as 'source' argument)
         *     2. --input.config (sent as 'configFileName' argument)
         *
         * the source that has format
         * dbHost=<string>;dbName=<string>;dbPort=<string>;dbUser=<string>;wciUser=<string>;provider=<string>;place=<string>;refTime=<string>
         * refTime is  given as iso string "20110210T000000"
         */
        WdbCDMReaderParserInfo parse(const std::string& source, const std::string& configFileName, bool bParseConfigFile = true);
    private:
        /**
          * ATM:
          * to keep things simple
          * take just firs occurence
          * ignore retst
          */
        std::pair<std::string, std::string> getNameValuePairForXmlPath(const XMLDoc& doc, const std::string& path);

        /**
          * or get the vector
          */
        std::vector<std::pair<std::string, std::string> > getNameValuePairsForXmlPath(const XMLDoc& doc, const std::string& path);

        std::vector<std::pair<std::string, std::string> > getAttributesForXmlPath(const XMLDoc& doc, const std::string& path);

        void parseSource(const std::string& source, WdbCDMReaderParserInfo& info);
        void parseCfgFile(const std::string& cfgFile, WdbCDMReaderParserInfo& info);
    };

}
} // MetNoFimex

#endif // WDBCDMREADERPARSER_H_
