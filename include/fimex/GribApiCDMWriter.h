#ifndef GRIBAPICDMWRITER_H_
#define GRIBAPICDMWRITER_H_

#include "CDMWriter.h"

namespace MetNoFimex
{

class GribApiCDMWriter : public MetNoFimex::CDMWriter
{
public:
	GribApiCDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~GribApiCDMWriter();
};

}

#endif /*GRIBAPICDMWRITER_H_*/
