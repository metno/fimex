#include "ReplaceStringTimeObject.h"


namespace MetNoFimex
{

// implementation before boost::date_time 1.33
std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto)
{
	using namespace std;
	struct tm * timeinfo;
	char buffer [80];

	timeinfo = gmtime( &rsto.myTime);

	strftime(buffer, 80, rsto.myFormat.c_str(), timeinfo);
	s << buffer;
	return s;
}

}
