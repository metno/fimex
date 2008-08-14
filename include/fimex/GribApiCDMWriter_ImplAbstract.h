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
#include <fstream>
#include <iostream>
#include "fimex/CDMWriter.h"
#include "fimex/XMLDoc.h"
#include "fimex/CDMException.h"
#include "fimex/TimeUnit.h"


namespace MetNoFimex
{

class GribApiCDMWriter_ImplAbstract
{
public:
	/**
	 * Constructor of the general writer. It should be called during
	 * construction of derived classes.
	 *
	 * @warn remember to call run to actually do something
	 */
	GribApiCDMWriter_ImplAbstract(int gribVersion, const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~GribApiCDMWriter_ImplAbstract();
	/**
	 * @brief actually write the data
	 *
	 * The run function has be to called after construction the object to
	 * actually fetch and write the data.
	 */
	void run() throw(CDMException);
protected:
	virtual void setData(const boost::shared_ptr<Data>& data);
	/**
	 * set the projection parameters, throw an exception if none are available
	 * @param varName
	 * @throw CDMException if parameters cannot be set
	 */
	virtual void setProjection(const std::string& varName) throw(CDMException) = 0;
	virtual void setParameter(const std::string& varName, const FimexTime& fTime, double levelValue) throw(CDMException) = 0;
	virtual void setTime(const std::string& varName, const FimexTime& fTime);
	virtual void setLevel(const std::string& varName, double levelValue) = 0;
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
	/**
	 * add the missing value to the gribHandle
	 * @return value of the missing value
	 */
	virtual double setMissingValue(const std::string& varName, const FimexTime& fTime, double levelValue) = 0;
	virtual void writeGribHandleToFile();

protected:
	int gribVersion;
	const boost::shared_ptr<CDMReader> cdmReader;
	const std::string outputFile;
	const std::string configFile;
	const boost::shared_ptr<XMLDoc> xmlConfig;
	boost::shared_ptr<grib_handle> gribHandle;
private:
	std::ofstream gribFile;

};

}

#endif /* GRIBAPICDMWRITER_IMPLABSTRACT_H_ */
