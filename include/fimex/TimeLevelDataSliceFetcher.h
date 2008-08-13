/*
 * Fimex
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
 */

#ifndef TIMELEVELDATASLICEFETCHER_H_
#define TIMELEVELDATASLICEFETCHER_H_

#include <boost/shared_ptr.hpp>
#include "fimex/CDMReader.h"
#include "fimex/Data.h"

namespace MetNoFimex
{

/**
 * @brief read a slice of a given time/level combination from a cdmReader
 */
class TimeLevelDataSliceFetcher
{
	boost::shared_ptr<CDMReader> cdmReader;
	const std::string& varName;
	std::vector<size_t> orgShape;
	int timePos;
	int levelPos;
	int unLimPos;
	boost::shared_ptr<Data> dataCache;
	size_t dataCachePos;
public:
	/**
	 * initialize the Fetcher
	 * @param cdmReader the reader to fetch the original data from
	 * @param varName the variable to read the data from
	 */
	TimeLevelDataSliceFetcher(boost::shared_ptr<CDMReader> cdmReader, const std::string& varName);
	virtual ~TimeLevelDataSliceFetcher();
	/**
	 * get the slice of time at position time and level at position level
	 * join unlimited dimensions if needed, slice data if needed
	 * @param time the position of the time according to the variables level-dimension
	 * @param level the position of the level according to the level-dimension
	 */
	boost::shared_ptr<Data> getTimeLevelSlice(size_t time, size_t level) throw(CDMException);
};

}

#endif /* TIMELEVELDATASLICEFETCHER_H_ */
