#ifndef CDMWRITER_H_
#define CDMWRITER_H_

#include <string>
#include <boost/shared_ptr.hpp>
#include "CDMReader.h"

namespace MetNoFimex
{

class CDMWriter
{
public:
	CDMWriter(boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile) 
	: cdmReader(cdmReader), outputFile(outputFile) {}
	virtual ~CDMWriter() {}

protected:
	const boost::shared_ptr<CDMReader> cdmReader;
	const std::string outputFile;
};

} // namespace

#endif /*CDMWRITER_H_*/
