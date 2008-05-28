#include "DataTypeChanger.h"
#include "Data.h"

namespace MetNoFimex
{

DataTypeChanger::DataTypeChanger(CDMDataType oldType)
: oldType(oldType), newType(CDM_NAT)
{
}

DataTypeChanger::DataTypeChanger(CDMDataType oldType, double oldFill, double oldScale, double oldOffset, CDMDataType newType, double newFill, double newScale, double newOffset)
: oldType(oldType), newType(newType), oldFill(oldFill), newFill(newFill), oldScale(oldScale), newScale(newScale), oldOffset(oldOffset), newOffset(newOffset)
{
}

DataTypeChanger::~DataTypeChanger()
{
}

boost::shared_ptr<Data> DataTypeChanger::convertData(boost::shared_ptr<Data> data) const throw(CDMException)
{
	if (newType == CDM_NAT) {
		return data; // null - initialization, do nothing
	}
	return data->convertDataType(oldFill, oldScale, oldOffset,newType, newFill, newScale, newOffset);
}

CDMDataType DataTypeChanger::getDataType() const
{
	if (newType == CDM_NAT) {
		return oldType;
	} else {
		return newType;
	}
}


}
