#ifndef CDMREADER_H_
#define CDMREADER_H_

#include "CDM.h"
#include "TimeSliceData.h"

namespace MetNoUtplukk
{

class CDMReader
{
public:
	CDMReader() {}
	virtual ~CDMReader() {}

	virtual TimeSliceData getDataSlice(int variableId, Time time) = 0;
	virtual CDM getCDM() = 0;
};

}

#endif /*CDMREADER_H_*/
