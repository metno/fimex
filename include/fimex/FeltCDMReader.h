#ifndef FELTCDMREADER_H_
#define FELTCDMREADER_H_

#include <string>
#include <map>
#include "Felt_File.h"
#include "Felt_File_Error.h"
#include "CDMReader.h"
#include "Time.h"

namespace MetNoUtplukk
{

class FeltCDMReader : public CDMReader
{
public:
	FeltCDMReader(std::string filename, std::string configFilename) throw(MetNoFelt::Felt_File_Error);
	virtual ~FeltCDMReader();
	
	virtual boost::shared_ptr<Data> getDataSlice(const std::string& variableName, const Time& time) const;
	virtual const CDM& getCDM() const {return cdm;}
	
private:
	const std::string filename;
	const std::string configFilename;
	MetNoFelt::Felt_File feltFile;
};

}

#endif /*FELTCDMREADER_H_*/
