/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#ifndef FELTCDMREADER2_H_
#define FELTCDMREADER2_H_

#include <string>
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>
#include "fimex/CDMReader.h"
#include "fimex/CDMDimension.h"
#include "fimex/Felt_Types.h"
#include "fimex/ReplaceStringObject.h"
#include <boost/date_time/posix_time/posix_time_types.hpp>

namespace MetNoFelt {
    class Felt_File2; // forward decl.
}

namespace MetNoFimex
{
class XMLDoc; // declaration without import

class FeltCDMReader2 : public CDMReader
{
public:
	FeltCDMReader2(std::string filename, std::string configFilename);
	virtual ~FeltCDMReader2();

	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);

private:
	const std::string filename;
	const std::string configFilename;
	boost::shared_ptr<MetNoFelt::Felt_File2> feltfile_;
	CDMDimension xDim;
	CDMDimension yDim;
	std::map<std::string, std::string> varNameFeltIdMap;
	std::vector<boost::posix_time::ptime> timeVec;
	std::map<std::string, std::vector<MetNoFelt::LevelPair> > levelVecMap;
	/**
	 * config attributes may contain template parameters marked with %PARAM%
	 * which should be replaced by dynamic values from the felt-file and stored
	 * temporary in this map
	 *
	 * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
	 */
	std::map<std::string, boost::shared_ptr<ReplaceStringObject> > templateReplacementAttributes;
	void init();
	// the following methods are parts of the init function and should not
	// be called from elsewhere
	std::vector<std::string> initGetKnownFeltIdsFromXML(const XMLDoc& doc);
	void initAddGlobalAttributesFromXML(const XMLDoc& doc);
	/**
	 * read processOptions/option from the xml file
	 */
	std::map<std::string, std::string> initGetOptionsFromXML(const XMLDoc& doc);
	CDMDimension initAddTimeDimensionFromXML(const XMLDoc& doc);
	std::map<short, CDMDimension> initAddLevelDimensionsFromXML(const XMLDoc& doc);
	/**
	 * add additional axis from the xml-file to this cdm
	 *
	 * @param xpathCtx xpath context of the file
	 * @param xpathLevelString xpath-string of the level which might have additional_axis_variable
	 * @param templateReplacements replacements for template parameters
	 */
	void readAdditionalAxisVariablesFromXPath(const XMLDoc& doc, const std::string& xpathLevelString, const std::map<std::string, boost::shared_ptr<ReplaceStringObject> >& templateReplacements);
	std::vector<double> readValuesFromXPath(const XMLDoc& doc, const std::string& variableXPath);
	void initAddProjectionFromXML(const XMLDoc& doc, std::string& projName, std::string& coordinates);
	void initAddVariablesFromXML(const XMLDoc& doc, const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const std::map<short, CDMDimension>& levelDims);


};

/// convenience function to retrieve a reader as shared_ptr instead of calling new
boost::shared_ptr<FeltCDMReader2> getFeltReader(std::string filename, std::string configFilename);

}

#endif /*FELTCDMREADER2_H_*/
