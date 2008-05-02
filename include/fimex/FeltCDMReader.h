#ifndef FELTCDMREADER_H_
#define FELTCDMREADER_H_

#include <string>
#include <vector>
#include <map>
#include "Felt_File.h"
#include "CDMReader.h"
#include "CDMDimension.h"

namespace MetNoUtplukk
{

class FeltCDMReader : public CDMReader
{
public:
	FeltCDMReader(std::string filename, std::string configFilename) throw(CDMException);
	virtual ~FeltCDMReader();
	
	virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);
	virtual const CDM& getCDM() const {return cdm;}
	
private:
	const std::string filename;
	const std::string configFilename;
	MetNoFelt::Felt_File feltFile;
	CDMDimension xDim;
	CDMDimension yDim;
	std::map<std::string, std::string> varNameFeltIdMap;
	std::vector<std::time_t> timeVec;
	std::map<std::string, std::vector<short> > levelVecMap;
	void init() throw(MetNoFelt::Felt_File_Error);
};

}

#endif /*FELTCDMREADER_H_*/
