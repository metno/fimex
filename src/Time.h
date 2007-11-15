#ifndef TIME_H_
#define TIME_H_

extern "C" {
#include <udunits.h>
}
#include <boost/shared_ptr.hpp>
#include <string>
#include "TimeException.h"

namespace MetNoUtplukk {

/** Time is a simple encapsulation of udunits time format. 
 * 
 * @warning Anything bad can happen when you call udTerm() anywhere else in the program!
 */  
class Time
{
private:
	std::string format;
	boost::shared_ptr<utUnit> unit;
	double value;

public:
	 
	/**
	 * @brief Initialize time with a udunits time string
	 * @param format e.g. seconds since 1970-01-01 00:00:00 +00:00
	 * @param value time in above units since above start-time, default 0
	 */
	explicit Time(std::string format, double value = 0) throw(TimeException);
	/**
	 * @brief Initialize time with a unit pointer
	 * @param unit unit and startpoint, usually derived from another Time, i.e. Time(t.getUnit())
	 * @param value time in unit since units start-time, default 0
	 */
	explicit Time(boost::shared_ptr<utUnit> unit, double value = 0) throw(TimeException);
	virtual ~Time();
		
	/**
	 * @brief convert the current time format to a new format
	 * @param newFormat format to convert to. The value of the newFormat will not be taken into account
	 */
	void convert(const Time& newFormat);
	/**
	 * @return a value representation in the current format
	 */
	double getValue() const {return value;}
	/**
	 * @return a string representation of the current format, without value
	 */
	const std::string& getFormatString() const {return format;}
	/**
	 * @return a utUnit pointer of the current time-format. Do not modify the 
	 * pointer, or pointer values! This is only exhibited to reduce the number of utUnit pointers.
	 */
	const boost::shared_ptr<utUnit> getUnit() const {return unit;}
	
};
}
#endif /*TIME_H_*/
