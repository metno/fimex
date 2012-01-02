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

#include "fimex/Null_CDMWriter.h"
#include <boost/shared_array.hpp>
#include "fimex/CDM.h"
#include "fimex/CDMDataType.h"
#include "fimex/Utils.h"
#include "fimex/Data.h"

#include "../config.h"
#ifdef HAVE_OPENMP
#include <omp.h>
#endif


namespace MetNoFimex
{

static bool putRecData(CDMDataType dt, boost::shared_ptr<Data> data, size_t recNum) {
	if (data->size() == 0) return true;

	switch (dt) {
	case CDM_NAT: return false;
	case CDM_CHAR:
	case CDM_STRING: data->asChar().get(); break;
	case CDM_SHORT:  data->asShort().get(); break;
	case CDM_INT:    data->asInt().get(); break;
	case CDM_FLOAT:  data->asFloat().get(); break;
	case CDM_DOUBLE: data->asDouble().get(); break;
	default: return false;
	}
	return true;

}


static bool putVarData(CDMDataType dt, boost::shared_ptr<Data> data) {
	size_t size = data->size();
	if (size == 0) return true;

	int dims = 5;
	int dim_size = 1;
	for (int i = 0; i < dims; i++) {
		dim_size *= 3;
	}

	switch (dt) {
	case CDM_NAT: return false;
	case CDM_CHAR:
	case CDM_STRING: data->asChar().get(); break;
	case CDM_SHORT:  data->asShort().get(); break;
	case CDM_INT:    data->asInt().get(); break;
	case CDM_FLOAT:  data->asFloat().get(); break;
	case CDM_DOUBLE: data->asDouble().get(); break;
	default: return false;
	}
	return true;
}

Null_CDMWriter::Null_CDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile)
: CDMWriter(cdmReader, outputFile)
{
	const CDM& cdm = cdmReader->getCDM();
	const CDM::DimVec& cdmDims = cdm.getDimensions();
	// define dims
	for (CDM::DimVec::const_iterator it = cdmDims.begin(); it != cdmDims.end(); ++it) {
		int length = it->isUnlimited() ? 0 : it->getLength();
		length++;
	}

	// define vars
	const CDM::VarVec& cdmVars = cdm.getVariables();
	for (CDM::VarVec::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
		const CDMVariable& var = *it;
		const std::vector<std::string>& shape = var.getShape();
		for (size_t i = 0; i < shape.size(); i++) {
			// revert order, cdm requires fastest moving first, netcdf-cplusplus requires fastest moving first
		}
		CDMDataType datatype = var.getDataType();
		if (datatype == CDM_NAT && shape.size() == 0) {
			datatype = CDM_INT;
		}
	}

	// write attributes
	const CDM::StrAttrVecMap& cdmAttrs = cdm.getAttributes();
	// using C interface since it offers a combined interface to global and var attributes
	for (CDM::StrAttrVecMap::const_iterator it = cdmAttrs.begin(); it != cdmAttrs.end(); ++it) {
		int varId;
		if (it->first == CDM::globalAttributeNS()) {
			varId = 0;
		} else {
			varId = 1;
		}
		for (CDM::AttrVec::const_iterator ait = it->second.begin(); ait != it->second.end(); ++ait) {
			const CDMAttribute& attr = *ait;
			CDMDataType dt = attr.getDataType();
			switch (dt) {
			case CDM_STRING: ;
			case CDM_CHAR:
				attr.getData()->asChar().get();
				break;
			case CDM_SHORT:
				attr.getData()->asShort().get();
				break;
			case CDM_INT:
				attr.getData()->asInt().get();
				break;
			case CDM_FLOAT:
				attr.getData()->asFloat().get();
				break;
			case CDM_DOUBLE:
				attr.getData()->asDouble().get();
				break;
			case CDM_NAT:
			default: throw CDMException("unknown datatype for attribute " + attr.getName());
			}
		}
	}

	// write data
#ifdef HAVE_OPENMP
#pragma omp parallel default(shared)
    {
    //omp_set_nested(1);
#pragma omp single
    {
#endif
    for (size_t vi = 0; vi < cdmVars.size(); ++vi) {
//	for (CDM::VarVec::const_iterator it = cdmVars.begin(); it != cdmVars.end(); ++it) {
		if (!cdm.hasUnlimitedDim(cdmVars.at(vi))) {
#ifdef HAVE_OPENMP
#pragma omp task firstprivate(vi)
		    {
#endif
		    const CDMVariable& cdmVar = cdmVars.at(vi);
		    //std::stringstream ss;
		    //ss << omp_get_thread_num() << ":" << vi << ":" << cdmVar.getName() << std::endl;
		    //std::cerr << ss.str();
			boost::shared_ptr<Data> data = cdmReader->getData(cdmVar.getName());
#ifdef HAVE_OPENMP
#pragma omp critical (mifi_null_cdmwriter)
			{
#endif

            if (!putVarData(cdmVar.getDataType(), data)) {
                throw CDMException("problems writing data to var " + cdmVar.getName() + ": " + ", datalength: " + type2string(data->size()));
            }
#ifdef HAVE_OPENMP
			} // critical
		    } // task
#endif
		} else {
			// iterate over each unlimited dim (usually time)
			const CDMDimension* unLimDim = cdm.getUnlimitedDim();
			for (size_t i = 0; i < unLimDim->getLength(); ++i) {
#ifdef HAVE_OPENMP
#pragma omp task firstprivate(vi,i)
			    {
#endif
                const CDMVariable& cdmVar = cdmVars.at(vi);
                //std::stringstream ss;
                //ss << omp_get_thread_num() << ":" << i << ":" << cdmVar.getName() << std::endl;
                //std::cerr << ss.str();
				boost::shared_ptr<Data> data = cdmReader->getDataSlice(cdmVar.getName(), i);
#ifdef HAVE_OPENMP
#pragma omp critical (mifi_null_cdmwriter)
                {
#endif
                if (!putRecData(cdmVar.getDataType(), data, i)) {
                    throw CDMException("problems writing datarecord " + type2string(i) + " to var " + cdmVar.getName() + ": " + ", datalength: " + type2string(data->size()));
                }
#ifdef HAVE_OPENMP
                } // critical
			    } // task
#endif
			}

		}
	}
#ifdef HAVE_OPENMP
    } // single
    omp_set_nested(0);
    } // parallel
#endif
}

Null_CDMWriter::~Null_CDMWriter()
{
}

}
