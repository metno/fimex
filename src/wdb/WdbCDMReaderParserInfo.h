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

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#ifndef WDBCDMREADERPARSERINFO_H_
#define WDBCDMREADERPARSERINFO_H_

#include <fimex/CDMAttribute.h>
#include <string>


namespace MetNoFimex
{

namespace wdb
{

class WdbCDMReaderParserInfo
{
public:
    WdbCDMReaderParserInfo();

    std::string databaseConnectString() const;

    /**
     * ATM we will expose only getters
     *
     * WdbCDMReaderParser will be able
     * to sett values as friend class
     */
    std::string wdbHost() const;
    std::string wdbName() const;
    std::string wdbUser() const;
    unsigned short wdbPort() const;

    std::string wciUser() const;

    std::string configFileName() const;

    std::string provider() const;
    std::string place() const;
    std::string referenceTime() const;

    const std::vector<CDMAttribute> & globalAttributes() const { return globalAttributes_; }

private:
    std::string    wdbHost_;
    std::string    wdbName_;
    std::string    wdbUser_;
    std::string    wciUser_;
    std::string    configFileName_;
    std::string    provider_;
    std::string    place_;
    std::string    referenceTime_;
    unsigned short wdbPort_;

    std::vector<CDMAttribute> globalAttributes_;

    friend class WdbCDMReaderParser;
};

}

}

#endif /* WDBCDMREADERPARSERINFO_H_ */
