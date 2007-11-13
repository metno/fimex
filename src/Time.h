#ifndef TIME_H_
#define TIME_H_

#include <udunits.h>
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
	 * Initialize time with a udunits time string, e.g. 'seconds since 1970-01-01'  
	 * 
	 */
	explicit Time(std::string format, double value = 0) throw(TimeException);
	explicit Time(boost::shared_ptr<utUnit> unit, double value = 0) throw(TimeException);
	virtual ~Time();
		
	// convert the current time format to a new format
	void convert(const Time& newFormat);
	const std::string& getFormatString() const {return format;}
	const boost::shared_ptr<utUnit> getUnit() const {return unit;}
	
};

}
#endif /*TIME_H_*/
