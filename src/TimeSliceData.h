#ifndef TIMESLICEDATA_H_
#define TIMESLICEDATA_H_

#include <boost/shared_ptr.hpp>
#include <string>
#include "Time.h"
#include "Data.h"

namespace MetNoFimex {


/**
 * @brief  DataSlice of a certain time for a variable
 * 
 */
class TimeSliceData
{
public:
	explicit TimeSliceData(const std::string& variableName, const Time& time, boost::shared_ptr<Data> data)
	: variableName(variableName), time(time), myData(data) {}
	~TimeSliceData() {}

	/// @brief unique variableName this data-slice belongs to
	const std::string& getVariableId() const {return variableName;}
	/// @brief time of this slice
	const Time& getTime() const {return time;}
	/// @brief the data
	const boost::shared_ptr<Data>& getData() const {return myData;}
	
private:
	std::string variableName;
	Time time;
	boost::shared_ptr<Data> myData;
};


} // namespace MetNoFimex
#endif /*TIMESLICEDATA_H_*/
