#ifndef NETCDF_CDMWRITER_H_
#define NETCDF_CDMWRITER_H_

#include "CDMWriter.h"
#include "config.h"
#include <map>
#include <string>
#include NETCDF_CPP_INCLUDE

namespace MetNoFimex
{

class NetCDF_CDMWriter : public CDMWriter
{
	typedef std::map<std::string, NcDim*> NcDimMap;
	typedef std::map<std::string, NcVar*> NcVarMap;

public:
	NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile);
	/**
	 * @param cdmReader dataSource
	 * @param outputFile file-name to write to
	 * @param configFile xml-configuration
	 */
	NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~NetCDF_CDMWriter();
private:
	NcError ncErr;
	NcFile ncFile;
	void init() throw(CDMException);
	NcDimMap defineDimensions();
	NcVarMap defineVariables(const NcDimMap& dimMap);
	void writeAttributes(const NcVarMap& varMap);
	void writeData(const NcVarMap& varMap);
	std::map<std::string, std::string> variableNameChanges;
	const std::string& getVariableName(const std::string& varName) const;
	std::map<std::string, std::string> dimensionNameChanges;
	const std::string& getDimensionName(const std::string& dimName) const;
	std::map<std::string, std::map<std::string, std::string> > attributeNameChanges;
	const std::string& getAttributeName(const std::string& varName, const std::string& attName) const;
};

}

#endif /*NETCDF_CDMWRITER_H_*/
