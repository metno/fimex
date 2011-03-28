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

/**
 * This class will parse command line and extract
 * needed options.
 *
 * One of the options can be wdbcdmreader config
 * file. Parser will work throuhg it as well.
 *
 * Remember, values from command line will
 * override those from config file.
 */

namespace MetNoFimex
{
    class WdbCDMReaderParserInfo
    {
    public:
        explicit WdbCDMReaderParserInfo();
        ~WdbCDMReaderParserInfo();

        /**
         * ATM we will expose only getters
         *
         * WdbCDMReaderParser will be able
         * to sett values as friend class
         */
        std::string wdbHost() const;
        std::string wdbName() const;
        std::string wdbUser() const;
        std::string wciUser() const;
        std::string configFileName() const;
        unsigned short wdbPort() const;
    private:
        std::string    wdbHost_;
        std::string    wdbName_;
        std::string    wdbUser_;
        std::string    wciUser_;
        std::string    configFileName_;
        std::string    referenceTime_;
        unsigned short wdbPort_;

        friend class WdbCDMReaderParser;
    };

    class WdbCDMReaderParser
    {
    public:
        explicit WdbCDMReaderParser();
        ~WdbCDMReaderParser();

        /**
         * if bParseConfigFile == false, parse() method
         * will skip to parse the file (even if found
         * within cmdLine options)
         */
        WdbCDMReaderParserInfo parse(const std::string& cmdLine, bool bParseConfigFile = true);
    private:
        void parseCmdLine(const std::string& cmdLine, WdbCDMReaderParserInfo& info);
        void parseCfgFile(const std::string& cfgFile, WdbCDMReaderParserInfo& info);
    };

} // MetNoFimex

#endif // WDBCDMREADERPARSER_H_
