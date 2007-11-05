#include "felt_reader/Felt_Array.h"
#include "felt_reader/FeltParameters.h"
#include <sstream>

#include <algorithm>
#include <iostream>
#include <cassert>

namespace MetNoFelt {

Felt_Array::Felt_Array(const string name, const boost::array<short, 16> idx)
: feltArrayName(name), idx(idx) 
{
}
Felt_Array::Felt_Array()
: feltArrayName(""), idx(ANY_ARRAY()) {
}

Felt_Array::~Felt_Array()
{
}

void Felt_Array::addInformationByIndex(const boost::array<short, 16> idx) {
	for (int i = 0; i < 16; i++) {
		assert((this->idx[i] == ANY_VALUE()) || (this->idx[i] == idx[i]));
	}
	time_t rawtime;
	struct tm * timeinfo;
	time ( &rawtime );
	timeinfo = gmtime ( &rawtime );
	timeinfo->tm_year = idx[2] - 1900;
	timeinfo->tm_mon = idx[3] / 100 - 1;
	timeinfo->tm_mday = idx[3] % 100;
	timeinfo->tm_hour = idx[4] / 100 + idx[9];
	timeinfo->tm_min = idx[4] % 100;
	timeinfo->tm_sec = 0;
	time_t thisTime = mktime(timeinfo);

	// required to be able to go back from tm_hour to idx[4], idx[9]
	boost::array<short, 4> timeIdx= { { idx[2], idx[3], idx[4], idx[9] } };
	times[thisTime] = timeIdx;
	levels.insert(idx[12]);
}

vector<time_t> Felt_Array::getTimes() {
	vector<time_t> vTimes = vector<time_t>(times.size());
	// below follows something similar to STL-copy, but for map-keys
	TIME_MAP::iterator now(times.begin());
	TIME_MAP::iterator last(times.end());
	vector<time_t>::iterator result(vTimes.begin());
	while (now!=last) {
		*result++ = now->first;
		now++;
	}

	sort(vTimes.begin(), vTimes.end());
	return vTimes; 
}

vector<short> Felt_Array::getLevels() {
	vector<short> vLevels = vector<short>(levels.size());
	copy(levels.begin(), levels.end(), vLevels.begin());
	sort(vLevels.begin(), vLevels.end());
	return vLevels;
}

const string& Felt_Array::getName() {
	return feltArrayName;
} 

boost::array<short, 16> const Felt_Array::getIndex(time_t time) throw(Felt_File_Error) {
	boost::array<short, 16> index(idx); // get a copy
	TIME_MAP::iterator it = times.find(time);
	if (it != times.end()) {
		boost::array<short, 4>& timeIdx = it->second;
		index[2] = timeIdx[0];
		index[3] = timeIdx[1];
		index[4] = timeIdx[2];
		index[9] = timeIdx[3];
	} else {
		ostringstream  msg;
		msg << "unknown time: " << time << " for felt_array: " << getName();
		throw Felt_File_Error(msg.str());
	}
	return index;
}

} // end namespace MetNoFelt
