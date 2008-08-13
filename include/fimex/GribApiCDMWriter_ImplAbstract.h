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


#ifndef GRIBAPICDMWRITER_IMPLABSTRACT_H_
#define GRIBAPICDMWRITER_IMPLABSTRACT_H_

#include <vector>
#include <grib_api.h>
#include "fimex/CDMWriter.h"
#include "fimex/XMLDoc.h"
#include "fimex/CDMException.h"
#include "fimex/TimeUnit.h"


namespace MetNoFimex
{

class GribApiCDMWriter_ImplAbstract
{
public:
	GribApiCDMWriter_ImplAbstract(int gribVersion, const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~GribApiCDMWriter_ImplAbstract();
protected:
	virtual void setData(const boost::shared_ptr<Data>& data);
	/**
	 * set the projection parameters, throw an exception if none are available
	 * @param varName
	 * @throw CDMException if parameters cannot be set
	 */
	virtual void setProjection(const std::string& varName) throw(CDMException) = 0;
	virtual void setParameter(const std::string& varName) throw(CDMException) = 0;
	virtual void setTime(size_t timePos, const std::vector<FimexTime>& cdmVarTimes);
	virtual void setLevel(const std::string& varName, size_t levelPos, const std::vector<double>& cdmLevels) = 0;
	/**
	 * get the levels from the cdm scaled to values used in grib (units/scale-factor)
	 * assign at least 1 level, give it a default value if none is found in the cdm
	 *
	 */
	virtual std::vector<double> getLevels(const std::string& varName) throw(CDMException);
	/**
	 * get the times from the cdm as FimexTime (including unit)
	 * assign at least 1 time, give it a default value if none is found in the cdm
	 */
	virtual std::vector<FimexTime> getTimes(const std::string& varName) throw(CDMException);
	virtual void setMissingValue() = 0;

protected:
	int gribVersion;
	const boost::shared_ptr<CDMReader> cdmReader;
	const std::string outputFile;
	const std::string configFile;
	const boost::shared_ptr<XMLDoc> xmlConfig;
	boost::shared_ptr<grib_handle> gribHandle;

};

}

#endif /* GRIBAPICDMWRITER_IMPLABSTRACT_H_ */
