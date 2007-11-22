#ifndef CDMDIMENSION_H_
#define CDMDIMENSION_H_

#include<string>

namespace MetNoUtplukk
{

const static int CDM_UNLIMITED_DIMENSION = -1;

class CDMDimension
{
public:
	CDMDimension(std::string name, long length);
	virtual ~CDMDimension();
private:
	std::string name;
	long length;

};

}

#endif /*CDMDIMENSION_H_*/
