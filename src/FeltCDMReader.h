#ifndef FELTCDMREADER_H_
#define FELTCDMREADER_H_

#include <string>
#include <map>
#include "felt_reader/Felt_File.h"
#include "CDMReader.h"

namespace MetNoUtplukk
{

class FeltCDMReader : public CDMReader
{
public:
	FeltCDMReader(std::string filename, std::string configFilename);
	virtual ~FeltCDMReader();
	
	virtual TimeSliceData getDataSlice(int variableId, Time time);
	virtual CDM getCDM() {return cdm;}
	
private:
	const std::string filename;
	const std::string configFilename;
	MetNoFelt::Felt_File feltFile;
	std::map<std::string, std::string> projectionVariables; // proj4-String -> variableName
	CDM cdm;
};

}

#endif /*FELTCDMREADER_H_*/
