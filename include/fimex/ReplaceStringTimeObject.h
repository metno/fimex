#ifndef REPLACESTRINGTIMEOBJECT_H_
#define REPLACESTRINGTIMEOBJECT_H_

#include "ReplaceStringObject.h"
#include <ctime>

namespace MetNoFimex
{

class ReplaceStringTimeObject : public MetNoFimex::ReplaceStringObject
{
	std::time_t myTime;
	std::string myFormat;
public:
	ReplaceStringTimeObject() {}
	ReplaceStringTimeObject(std::time_t time, std::string format = "%Y-%m-%d %H:%M:%S%F%Q") : myTime(time), myFormat(format) {} 
	virtual ~ReplaceStringTimeObject() {}
	friend std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto);
	virtual std::ostream& put(std::ostream& s) const { s << *this; return s;}
	/**
	 *  set the formatting String for this object
	 * 
	 * @param format: format string of strftime http://www.cplusplus.com/reference/clibrary/ctime/strftime.html
	 */
	virtual void setFormatString(std::string format) {myFormat = format;}

};

}

#endif /*REPLACESTRINGTIMEOBJECT_H_*/
