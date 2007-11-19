#ifndef TIMESLICEDATA_H_
#define TIMESLICEDATA_H_

#include <boost/shared_ptr.hpp>
#include "Time.h"
#include "Data.h"

namespace MetNoUtplukk {


/**
 * @brief  DataSlice of a certain time for a variable
 * 
 */
class TimeSliceData
{
public:
	explicit TimeSliceData(int variableId, Time time, boost::shared_ptr<Data> data)
	: variableId(variableId), time(time), myData(data) {}
	~TimeSliceData() {}

	/// @brief unique variableId this data-slice belongs to
	int getVariableId() const {return variableId;}
	/// @brief the data
	const boost::shared_ptr<Data>& getData() const {return myData;}
	
private:
	int variableId;
	Time time;
	boost::shared_ptr<Data> myData;
};


} // namespace MetNoUtplukk
#endif /*TIMESLICEDATA_H_*/
