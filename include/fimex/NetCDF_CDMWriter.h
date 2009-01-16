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

#ifndef NETCDF_CDMWRITER_H_
#define NETCDF_CDMWRITER_H_

#include "fimex/CDMWriter.h"
#include "fimex/config.h"
#include <map>
#include <string>

/* forward declarations */
class NcFile;
class NcError;
class NcDim;
class NcVar;

namespace MetNoFimex
{
/* forward declarations */
class XMLDoc;

class NetCDF_CDMWriter : public CDMWriter
{
	typedef std::map<std::string, NcDim*> NcDimMap;
	typedef std::map<std::string, NcVar*> NcVarMap;

public:
	NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, int version = 3);
	/**
	 * @param cdmReader dataSource
	 * @param outputFile file-name to write to
	 * @param configFile xml-configuration
	 * @param netcdf version, can be 3 or 4; 4 requires compilation against netcdf-4.0 or higher
	 */
	NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile, int version = 3);
	virtual ~NetCDF_CDMWriter();
	/** @return the new name of a variable, eventually changed by the writers config */
	const std::string& getVariableName(const std::string& varName) const;
	/** @return the new name of a dimension, eventually changed by the writers config */
	const std::string& getDimensionName(const std::string& dimName) const;
	/** @return the new name of an attribute, eventually changed by the writers config */
	const std::string& getAttributeName(const std::string& varName, const std::string& attName) const;
	/**
	 * @param varName original variable name  (before config: newname)
	 * @param attName original attribute name (before config: newname)
	 * @return an attribute contained in the writers attribute, possibly added by config
	 */
	const CDMAttribute& getAttribute(const std::string& varName, const std::string& attName) const throw(CDMException);



private:
	void init() throw(CDMException);
	void initFillRenameDimension(std::auto_ptr<XMLDoc>& doc) throw(CDMException);
	void initFillRenameVariable(std::auto_ptr<XMLDoc>& doc) throw(CDMException);
	void initFillRenameAttribute(std::auto_ptr<XMLDoc>& doc) throw(CDMException);
	/** test if the variable exists in the cdmReader or throw an CDMException */
	void testVariableExists(const std::string& varName) throw(CDMException);

	NcDimMap defineDimensions();
	NcVarMap defineVariables(const NcDimMap& dimMap);
	void writeAttributes(const NcVarMap& varMap);
	void writeData(const NcVarMap& varMap);
	double getOldAttribute(const std::string& varName, const std::string& attName, double defaultValue) const;
	double getNewAttribute(const std::string& varName, const std::string& attName, double defaultValue) const;
	std::auto_ptr<NcFile> ncFile;
	std::auto_ptr<NcError> ncErr;;
	std::map<std::string, std::string> variableNameChanges;
	std::map<std::string, CDMDataType> variableTypeChanges;
	std::map<std::string, std::string> dimensionNameChanges;
	std::map<std::string, std::map<std::string, std::string> > attributeNameChanges;
	CDM::StrAttrVecMap attributes;
};

}

#endif /*NETCDF_CDMWRITER_H_*/
