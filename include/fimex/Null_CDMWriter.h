#ifndef NULL_CDMWRITER_H_
#define NULL_CDMWRITER_H_

#include "CDMWriter.h"

namespace MetNoFimex
{

/**
 * CDMWriter does all operations as the NetCDF_CDMWriter, except writing to the file.
 * This class is useful for performance tests.
 */
class Null_CDMWriter : public CDMWriter
{
public:
	Null_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile);
	virtual ~Null_CDMWriter();
private:
};

}

#endif /*NULL_CDMWRITER_H_*/
