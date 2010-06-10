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

#ifndef FELTCDMREADER_H_
#define FELTCDMREADER_H_

#include <string>
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>
#include "fimex/Felt_File.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMDimension.h"
#include "fimex/ReplaceStringObject.h"
#include "fimex/Utils.h"
#include "fimex/deprecated.h"

namespace MetNoFimex
{
class XMLDoc; // declaration without import

class FeltCDMReader : public CDMReader
{
public:
    /**
     * @deprecated use FeltCDMReader2(), accessing data through the c++ libfelt instead of fortran libmi
     */
	DEPRECATED(FeltCDMReader(std::string filename, std::string configFilename) throw(CDMException));
	virtual ~FeltCDMReader();

	virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException);

private:
	const std::string filename;
	const std::string configFilename;
	MetNoFelt::Felt_File feltFile;
	CDMDimension xDim;
	CDMDimension yDim;
	std::map<std::string, std::string> varNameFeltIdMap;
	std::vector<epoch_seconds> timeVec;
	std::map<std::string, std::vector<short> > levelVecMap;
	/**
	 * config attributes may contain template parameters marked with %PARAM%
	 * which should be replaced by dynamic values from the felt-file and stored
	 * temporary in this map
	 *
	 * Currently implemented parameters are: %MIN_DATETIME%, %MAX_DATETIME%: earliest and latest time in felt-file as ISO string
	 */
	std::map<std::string, boost::shared_ptr<ReplaceStringObject> > templateReplacementAttributes;
	void init() throw(MetNoFelt::Felt_File_Error, CDMException);
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
	void readAdditionalAxisVariablesFromXPath(const XMLDoc& doc, const std::string& xpathLevelString, const std::map<std::string, boost::shared_ptr<ReplaceStringObject> >& templateReplacements) throw(MetNoFelt::Felt_File_Error);
	std::vector<double> readValuesFromXPath(const XMLDoc& doc, const std::string& variableXPath);
	void initAddProjectionFromXML(const XMLDoc& doc, std::string& projName, std::string& coordinates);
	void initAddVariablesFromXML(const XMLDoc& doc, const std::string& projName, const std::string& coordinates, const CDMDimension& timeDim, const std::map<short, CDMDimension>& levelDims);


};

}

#endif /*FELTCDMREADER_H_*/
