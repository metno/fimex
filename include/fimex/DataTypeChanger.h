#ifndef DATATYPECHANGER_H_
#define DATATYPECHANGER_H_
#include "CDMDataType.h"
#include "boost/shared_ptr.hpp"
#include "CDMException.h"

namespace MetNoFimex
{
class Data; // forward decl.


/** brief wrapper class around data->convertType */
class DataTypeChanger
{
	CDMDataType oldType;
	CDMDataType newType;
	double oldFill;
	double newFill;
	double oldScale;
	double newScale;
	double oldOffset;
	double newOffset;
public:
	/**
	 * initialize data with the oldType
	 * convertData will do nothing in this case
	 */
	explicit DataTypeChanger(CDMDataType oldType);
	/**
	 * initialize with the old and new settings
	 */
	explicit DataTypeChanger(CDMDataType oldType, double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset);
	virtual ~DataTypeChanger();
	/**
	 * convert the data to the new scale/fill/offset
	 */
	boost::shared_ptr<Data> convertData(boost::shared_ptr<Data>) const throw(CDMException);
	/**
	 * return the datatype of the converted data 
	 */
	CDMDataType getDataType() const;
	
};

}

#endif /*DATATYPECHANGER_H_*/
