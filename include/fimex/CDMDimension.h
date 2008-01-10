#ifndef CDMDIMENSION_H_
#define CDMDIMENSION_H_

#include <string>
#include <ostream>

namespace MetNoUtplukk
{

const static int CDM_UNLIMITED_DIMENSION = -1;

class CDMDimension
{
public:
	CDMDimension(); // default null constructor for maps
	CDMDimension(std::string name, long length);
	virtual ~CDMDimension();
	const std::string& getName() const {return name;}
	long getLength() const {return length;}
	/// print xml representation to stream
	void toXMLStream(std::ostream& out) const;
private:
	std::string name;
	long length;

};

}

#endif /*CDMDIMENSION_H_*/
