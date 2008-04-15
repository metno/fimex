#ifndef NETCDF_CF10_CDMREADER_H_
#define NETCDF_CF10_CDMREADER_H_

#include "../config.h"
#include "CDMReader.h"
#include NETCDF_CPP_INCLUDE

namespace MetNoUtplukk
{

class NetCDF_CF10_CDMReader : public MetNoUtplukk::CDMReader
{
	std::string filename;
	NcFile ncFile;
public:
	NetCDF_CF10_CDMReader(const std::string& fileName);
	virtual ~NetCDF_CF10_CDMReader();
	virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);
private:
	void addAttribute(const std::string& varName, NcAtt* ncAtt);
};

}

#endif /*NETCDF_CF10_CDMREADER_H_*/
