/*
 * Fimex, CDMTimeInterpolator.h
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
 *  Created on: Dec 3, 2008
 *      Author: heikok
 */

#ifndef CDMTIMEINTERPOLATOR_H_
#define CDMTIMEINTERPOLATOR_H_


#include "CDMReader.h"
#include <map>

namespace MetNoFimex
{

class CDMTimeInterpolator: public MetNoFimex::CDMReader
{
public:
	CDMTimeInterpolator(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMTimeInterpolator();
	/**
	 * @brief retrieve data from the underlying dataReader and interpolate the values due to the current projection
	 *
	 * @param varName name of variable
	 * @param size_t unLimDimPos position of the unlimited dimension, most commonly time-position of the output as set in #changeTimeAxis
	 */
	virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);
	/**
	 * change the time-axis from from the one given to a new specification
	 * @param timeSpec string of time-specification
	 * @throws CDMException on unparsable timeSpec
	 * @see @ref secTimeSpec
	 */
	virtual void changeTimeAxis(std::string timeSpec) throw(CDMException);

private:
	typedef std::map<std::string, std::map<int, std::pair<int,int> > > TimeChangeMap;
	boost::shared_ptr<CDMReader> dataReader;
	TimeChangeMap timeChangeMap;
};

} /* MetNoFimex */


#endif /* CDMTIMEINTERPOLATOR_H_ */
