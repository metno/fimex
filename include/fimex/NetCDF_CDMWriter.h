#ifndef NETCDF_CDMWRITER_H_
#define NETCDF_CDMWRITER_H_

#include "CDMWriter.h"
#include "netcdfcpp.h"

namespace MetNoUtplukk
{

class NetCDF_CDMWriter : public CDMWriter
{
public:
	NetCDF_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile);
	virtual ~NetCDF_CDMWriter();
private:
	NcFile ncFile;
};

}

#endif /*NETCDF_CDMWRITER_H_*/
