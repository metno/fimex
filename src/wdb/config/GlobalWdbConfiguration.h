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

/**
 * Holds global configuration which should be valid for all users of
 * wdb/fimex, such as name translations and global attributes.
 *
 * The configuration is read from an xml file, with syntax described in the
 * wdb_config.xsd file, which is distributed with fimex.
 */
class GlobalWdbConfiguration
{
public:

	/**
	 * Initialize object with content from the given configuration file
	 */
	explicit GlobalWdbConfiguration(const boost::filesystem::path & configFile);
	~GlobalWdbConfiguration();

	/**
	 * Translate a wdb parameter name (value- or level-) into a cf standard
	 * name. If no explicit translations are given in configuration - make
	 * a guess.
	 */
	std::string cfName(const std::string & wdbName) const;

	/**
	 * Translate a cf standard name (value- or level-) into a wdb parameter
	 * name. If no explicit translations are given in configuration - make
	 * a guess.
	 */
	std::string wdbName(const std::string & cfName) const;

	/// A collection of attributes
	typedef std::vector<CDMAttribute> AttributeList;

	/**
	 * Get all attributes that config says the given parameter should
	 * have. The returned list is not meant to be exhaustive - other
	 * attributes may be added by other means.
	 */
	AttributeList getAttributes(const std::string & wdbParameter, const std::string & defaultUnit = std::string()) const;

	/**
	 * Get all globla attributes mentioned in config. The returned list is not
	 * meant to be exhaustive - other attributes may be added by other means.
	 */
	const AttributeList & getGlobalAttributes() const { return globalAttributes_; }

private:
	void init_(const boost::filesystem::path & configFile);

	void initParseGlobalAttributes_(XMLDoc & config);
	void initParseValueParameter_(XMLDoc & config);

	static const boost::filesystem::path defaultConfigFile_;

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
