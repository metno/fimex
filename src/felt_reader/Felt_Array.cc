#include "Felt_Array.h"
#include "FeltParameters.h"
#include "Utils.h"
#include <sstream>

#include <algorithm>
#include <iostream>
#include <cmath>

namespace MetNoFelt {

Felt_Array::Felt_Array(const string name, const boost::array<short, 16> idx)
: feltArrayName(name),
  idx(idx),
  header(ANY_ARRAY20())
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
 idx(ANY_ARRAY()),
 header(ANY_ARRAY20()) {
}

Felt_Array::~Felt_Array()
{
}

void Felt_Array::addInformationByIndex(const boost::array<short, 16>& idx, int fieldSize) throw(Felt_File_Error) {
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

void Felt_Array::testHeaderElement(short oldVal, short newVal, const std::string& msg) const throw(Felt_File_Error)
{
	if ((oldVal != ANY_VALUE()) && (oldVal != newVal)) {
		throw Felt_File_Error("change in " + msg);
	}
}

void Felt_Array::setDataHeader(boost::array<short, 20> header) throw(Felt_File_Error)
{
	for (int i = 0; i < 20; i++) {
		switch (i) {
			case 9: testHeaderElement(this->header[i], header[i], "dataheader for param " + MetNoUtplukk::type2string(header[5])+": x"); break;
			case 10: testHeaderElement(this->header[i], header[i], "dataheader for param " + MetNoUtplukk::type2string(header[5])+": y"); break;
			case 19: testHeaderElement(this->header[i], header[i], "dataheader for param " + MetNoUtplukk::type2string(header[5])+": scalingFactor"); break;
			case 14:
			case 15:
			case 16:
			case 17: testHeaderElement(this->header[i], header[i], "dataheader for param " + MetNoUtplukk::type2string(header[5])+": grid definition " + MetNoUtplukk::type2string(i)); break;
			default: break;
		}
	}
	this->header = header;
}

vector<time_t> Felt_Array::getTimes() const {
	vector<time_t> vTimes = vector<time_t>(times.size());
	// below follows something similar to STL-copy, but for map-keys
	TIME_MAP::const_iterator now(times.begin());
	TIME_MAP::const_iterator last(times.end());
	vector<time_t>::iterator result(vTimes.begin());
	while (now!=last) {
		*result++ = now->first;
		now++;
	}

	sort(vTimes.begin(), vTimes.end());
	return vTimes; 
}

vector<short> Felt_Array::getLevels() const {
	vector<short> vLevels = vector<short>(levels.size());
	copy(levels.begin(), levels.end(), vLevels.begin());
	sort(vLevels.begin(), vLevels.end());
	return vLevels;
}

double Felt_Array::getScalingFactor() const {
	return std::pow(10,static_cast<double>(header[19]));
}

const string& Felt_Array::getName() const {
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
		msg << "unknown time: " << time << " for felt_array: " << getName() << " (timeIdx: "<< index[2] << " " << index[3] << " " << index[4] << " " << index[9] << ")";
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

int Felt_Array::getFieldSize(time_t time, short level) const throw(Felt_File_Error) {
	map<time_t, map<short, int> >::const_iterator timeMap = fieldSizeMap.find(time); 
	if (timeMap != fieldSizeMap.end()) {
		map<short, int>::const_iterator levelMap = timeMap->second.find(level);
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
