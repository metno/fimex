#include "felt_reader/Felt_Array.h"
#include "felt_reader/FeltParameters.h"
#include <sstream>

#include <algorithm>
#include <iostream>
#include <cassert>

namespace MetNoFelt {

Felt_Array::Felt_Array(const string name, const boost::array<short, 16> idx)
: feltArrayName(name),
  nx(ANY_VALUE()),
  ny(ANY_VALUE()),
  scaling_factor(ANY_VALUE()),
  idx(idx) 
{
	// clear time
	this->idx[2] = ANY_VALUE();
	this->idx[3] = ANY_VALUE();
	this->idx[4] = ANY_VALUE();
	this->idx[9] = ANY_VALUE();
	// clear levels
	this->idx[12] = ANY_VALUE();
	// clear internal block information
	this->idx[5] = ANY_VALUE();
	this->idx[6] = ANY_VALUE();
	this->idx[7] = ANY_VALUE();
	this->idx[15] = ANY_VALUE();
	// clear data type - what is that???
	this->idx[8] = ANY_VALUE();
	
}
Felt_Array::Felt_Array()
: feltArrayName(""),
 nx(ANY_VALUE()),
 ny(ANY_VALUE()),
 scaling_factor(ANY_VALUE()),
 idx(ANY_ARRAY()) {
}

Felt_Array::~Felt_Array()
{
}

void Felt_Array::addInformationByIndex(const boost::array<short, 16> idx, int fieldSize) throw(Felt_File_Error) {
	if (fieldSize <= 0) {
		// no data, no field
		return;
	}
	for (int i = 0; i < 16; i++) {
		if (!((this->idx[i] == ANY_VALUE()) || (this->idx[i] == idx[i]))) {
			ostringstream msg;
			msg << "inexact definition of parameter "<< getName() << " at id " << (i+1) << "values: " << this->idx[i] << "<->" << idx[i];
			throw Felt_File_Error(msg.str());
		}
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
	fieldSizeMap[thisTime][idx[12]] = fieldSize;
}

void Felt_Array::setXandY(int nx, int ny) throw(Felt_File_Error) {
	if (!((this->nx == ANY_VALUE()) || (this->nx == nx))) {
		ostringstream msg;
		msg << "nx changed from " << this->nx << " to " << nx << " in parameter "<< getName();
		throw Felt_File_Error(msg.str());
	}
	if (!((this->ny == ANY_VALUE()) || (this->ny == ny))) {
		ostringstream msg;
		msg << "ny changed from " << this->ny << " to " << ny << " in parameter "<< getName();
		throw Felt_File_Error(msg.str());
	}
	this->nx = nx;
	this->ny = ny;
}

void Felt_Array::setScalingFactor(long scalingFactor) throw(Felt_File_Error) {
	if (!((this->scaling_factor == ANY_VALUE()) || (this->scaling_factor == scalingFactor))) {
		ostringstream msg;
		msg << "scaling_factor changed from " << this->scaling_factor << " to " << scalingFactor << " in parameter "<< getName();
		throw Felt_File_Error(msg.str());
	}
	this->scaling_factor = scalingFactor;
}

void Felt_Array::setExtraInformation(vector<short> v) {
	this->extraGridInfo = v;
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

const int Felt_Array::getX() {
	return nx;
}
const int Felt_Array::getY() {
	return ny;
}

const long Felt_Array::getScalingFactor() {
	return scaling_factor;
}

const string& Felt_Array::getName() {
	return feltArrayName;
} 

boost::array<short, 16> const Felt_Array::getIndex(time_t time, short level) throw(Felt_File_Error) {
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
	if (fieldSizeMap[time].find(level) != fieldSizeMap[time].end()) {
		index[12] = level;
	} else {
		ostringstream msg;
		msg << "unknown level: " << level << " for felt array: " << getName();
		throw Felt_File_Error(msg.str());
	}	
	return index;
}

int const Felt_Array::getFieldSize(time_t time, short level) throw(Felt_File_Error) {
	map<time_t, map<short, int> >::iterator timeMap = fieldSizeMap.find(time); 
	if (timeMap != fieldSizeMap.end()) {
		map<short, int>::iterator levelMap = timeMap->second.find(level);
		if (levelMap != timeMap->second.end()) {
			return levelMap->second;
		} else {
			ostringstream msg;
			msg << "unknown level: " << level << " for felt array: " << getName();
			throw Felt_File_Error(msg.str());
		}
	} else {
		ostringstream msg;
		msg << "unknown time: " << time << " for felt array: " << getName();
		throw Felt_File_Error(msg.str());
	}
}
	
} // end namespace MetNoFelt
