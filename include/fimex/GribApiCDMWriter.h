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

#ifndef GRIBAPICDMWRITER_H_
#define GRIBAPICDMWRITER_H_

#include "fimex/CDMWriter.h"

// pre-declaration of parameters
struct grib_handle;
namespace MetNoFimex
{
class TimeUnit;

class GribApiCDMWriter : public MetNoFimex::CDMWriter
{

public:
	GribApiCDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~GribApiCDMWriter();
private:
	std::string configFile;
	void writeData(std::ofstream& gribFile, boost::shared_ptr<grib_handle> gribHandle, boost::shared_ptr<Data> data, std::vector<size_t> orgDims, const std::string& time, const std::string& level, int timePos, int levelPos, size_t currentTime, size_t currentLevel, const boost::shared_array<double>& timeData, const boost::shared_array<double>& levelData, TimeUnit tu);
};

}

#endif /*GRIBAPICDMWRITER_H_*/
