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

#ifndef WDBTRANSLATIONS_H_
#define WDBTRANSLATIONS_H_

#include <fimex/CDMAttribute.h>
#include <boost/filesystem/path.hpp>
#include <string>
#include <map>
#include <vector>


namespace MetNoFimex
{
class XMLDoc;

namespace wdb
{

class GlobalWdbConfiguration
{
public:
	//GlobalWdbConfiguration() {}
	explicit GlobalWdbConfiguration(const boost::filesystem::path & configFile);
	~GlobalWdbConfiguration();

	std::string cfName(const std::string & wdbName) const;

	std::string wdbName(const std::string & cfName) const;

	typedef std::vector<CDMAttribute> AttributeList;
	AttributeList getAttributes(const std::string & wdbParameter, const std::string & defaultUnit = std::string()) const;

	const AttributeList & getGlobalAttributes() const { return globalAttributes_; }

private:
	void initParseGlobalAttributes_(XMLDoc & config);
	void initParseValueParameter_(XMLDoc & config);


	typedef std::map<std::string, std::string> NameTranslation;
	NameTranslation wdb2cf_;
	NameTranslation cf2wdb_;


	typedef std::map<std::string, AttributeList> NameToAttributeMap;
	NameToAttributeMap attributes_;

	AttributeList globalAttributes_;
};

}

}

#endif /* WDBTRANSLATIONS_H_ */
