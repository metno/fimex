#ifndef CDMDIMENSION_H_
#define CDMDIMENSION_H_

#include<string>

namespace MetNoUtplukk
{

class CDMDimension
{
public:
	CDMDimension(string name, long length);
	virtual ~CDMDimension();
private:
	string name;
	long length;

};

}

#endif /*CDMDIMENSION_H_*/
