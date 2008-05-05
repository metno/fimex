#include "ReplaceStringTimeObject.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <locale>


namespace MetNoUtplukk
{

std::ostream& operator<<(std::ostream& s, const ReplaceStringTimeObject& rsto)
{
	using namespace boost::posix_time;
	using namespace boost::gregorian;
	using namespace std;
	time_facet* f = new time_facet();
	f->format(rsto.myFormat.c_str());
	s.imbue(locale(s.getloc(), f));
	// use ptime from_time_t to output a formatted date_time object
	s << from_time_t(rsto.myTime);
	return s;
}

}
