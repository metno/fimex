#include "felt_reader/Felt_Array.h"
#include "felt_reader/FeltParameters.h"
#include <sstream>
#include <algorithm>
#include <iostream>
#include <cassert>

Felt_Array::Felt_Array(const string name, const boost::array<short, 16> idx)
: feltArrayName(name), idx(idx) 
{
}
Felt_Array::Felt_Array()
: feltArrayName(""), idx(FeltParameters::ANY_ARRAY()) {
}

Felt_Array::~Felt_Array()
{
}

void Felt_Array::addInformationByIndex(const boost::array<short, 16> idx) {
	for (int i = 0; i < 16; i++) {
		assert((this->idx[0] == FeltParameters::ANY_VALUE()) || (this->idx[0] == idx[0]));
	}
	time_t rawtime;
	struct tm * timeinfo;
	time ( &rawtime );
	timeinfo = localtime ( &rawtime );
	timeinfo->tm_year = idx[2] - 1900;
	timeinfo->tm_mon = idx[3] / 100 - 1;
	timeinfo->tm_mday = idx[3] % 100;
	timeinfo->tm_hour = idx[4] / 100 + idx[9];
	timeinfo->tm_min = idx[4] % 100;
	timeinfo->tm_sec = 0;
	time_t thisTime = mktime(timeinfo);

	times.insert(thisTime);
	levels.insert(idx[12]);
}

vector<time_t> Felt_Array::getTimes() {
	vector<time_t> vTimes = vector<time_t>(times.size());
	copy(times.begin(), times.end(), vTimes.begin());
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
