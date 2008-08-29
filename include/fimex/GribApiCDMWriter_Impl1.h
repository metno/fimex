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

#ifndef GRIBAPICDMWRITER_IMPL1_H_
#define GRIBAPICDMWRITER_IMPL1_H_

#include "fimex/GribApiCDMWriter_ImplAbstract.h"

namespace MetNoFimex
{

/**
 * Implemenationn of a writer using GribApi for grib1
 *
 */
class GribApiCDMWriter_Impl1: public MetNoFimex::GribApiCDMWriter_ImplAbstract
{
public:
	GribApiCDMWriter_Impl1(const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile);
	virtual ~GribApiCDMWriter_Impl1();

	virtual void setParameter(const std::string& varName, const FimexTime& fTime, double levelValue) throw(CDMException);
	virtual void setProjection(const std::string& varName) throw(CDMException);
	virtual void setLevel(const std::string& varName, double levelValue);
	virtual boost::shared_ptr<Data> handleTypeScaleAndMissingData(const std::string& varName, const FimexTime& fTime, double levelValue, boost::shared_ptr<Data> inData);
private:
	xmlNode* getNodePtr(const std::string& varName, const FimexTime& fTime, double levelValue) throw(CDMException);

};

}

#endif /* GRIBAPICDMWRITER_IMPL1_H_ */
