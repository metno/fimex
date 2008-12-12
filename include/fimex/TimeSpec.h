/*
 * Fimex, TimeSpec.h
 *
 * (C) Copyright 2008, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Dec 11, 2008
 *      Author: Heiko Klein
 */

#ifndef TIMESPEC_H_
#define TIMESPEC_H_

#include "fimex/TimeUnit.h"
#include "fimex/CDMException.h"
#include <vector>
#include <string>

namespace MetNoFimex
{

class TimeSpec
{
public:
	TimeSpec(const std::string& timeSpec, const FimexTime& startTime, const FimexTime& endTime) throw(CDMException);
	virtual ~TimeSpec() {};
	const std::vector<FimexTime>& getTimeSteps() const {return timeSteps;}
	const std::string& getUnitString() const {return outputUnit;}

private:
	std::string outputUnit;
	std::vector<FimexTime> timeSteps;
};

} /* MetNoFimex */

#endif /* TIMESPEC_H_ */
