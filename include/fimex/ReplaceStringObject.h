#ifndef REPLACESTRINGOBJECT_H_
#define REPLACESTRINGOBJECT_H_

#include <iostream>
#include <string>

namespace MetNoFimex {

/**
 * Interface for objects which might be converted to 
 * different strings
 */
class ReplaceStringObject
{
public:
	virtual ~ReplaceStringObject() = 0;
	/**
	 *  put the formatted string to the stream
	 * 
	 * implementors are asked to implement operator<<
	 */
	virtual std::ostream& put(std::ostream& s) const = 0;
	// set the formatting String for this object
	virtual void setFormatString(std::string) = 0;
};

}
#endif /*REPLACESTRINGOBJECT_H_*/
