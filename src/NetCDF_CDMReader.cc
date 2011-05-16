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

#include "fimex/NetCDF_CDMReader.h"
#include "fimex/Data.h"
#include "NetCDF_Utils.h"
#include "fimex/CDM.h"
#include "netcdfcpp.h"
#include <numeric>

namespace MetNoFimex
{
using namespace std;

NetCDF_CDMReader::NetCDF_CDMReader(const std::string& filename)
: filename(filename), ncFile(std::auto_ptr<NcFile>(new NcFile(filename.c_str(), NcFile::ReadOnly)))
{
	NcError ncErr(NcError::verbose_nonfatal);
	if (!ncFile->is_valid()) {
	    // ncErr.get_err does not work in new NcFile, try to get the error message manually
	    size_t *bufrsizeptr = new size_t;
	    int the_id;
	    int error = nc__open(filename.c_str(), NC_NOWRITE, bufrsizeptr, &the_id);
        throw CDMException(nc_strerror(error));
		//throw CDMException(nc_strerror(ncErr.get_err()));
	}

	// read metadata to cdm
	// define dimensions
	for (int i = 0; i < ncFile->num_dims(); ++i) {
		NcDim* dim = ncFile->get_dim(i);
		CDMDimension d(dim->name(), dim->size());
		d.setUnlimited(dim->is_unlimited());
		cdm_->addDimension(d);
	}
	// define variables
	for (int i = 0; i < ncFile->num_vars(); ++i) {
		NcVar* var = ncFile->get_var(i);
		CDMDataType type = ncType2cdmDataType(var->type());
		std::vector<std::string> shape;
		// reverse dimensions
		for (int j = var->num_dims()-1; j >= 0; --j) {
			NcDim* dim = var->get_dim(j);
			shape.push_back(dim->name());
		}
		cdm_->addVariable(CDMVariable(var->name(), type, shape));
		// define the attributes of the variable
		for (int j = 0; j < var->num_atts(); ++j) {
			addAttribute(var->name(), var->get_att(j));
		}
	}
	// define global attributes
	for (int i = 0; i < ncFile->num_atts(); ++i) {
		addAttribute(cdm_->globalAttributeNS(), ncFile->get_att(i));
	}
	if (ncErr.get_err() != NC_NOERR) {
	    throw CDMException(nc_strerror(ncErr.get_err()));
	}
}

NetCDF_CDMReader::~NetCDF_CDMReader()
{
}

boost::shared_ptr<Data> NetCDF_CDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
	const CDMVariable& var = cdm_->getVariable(varName);
	if (var.hasData()) {
		return getDataSliceFromMemory(var, unLimDimPos);
	}

	NcVar* ncVar = ncFile->get_var(var.getName().c_str());
	if (cdm_->hasUnlimitedDim(var)) {
		return ncValues2Data(ncVar->get_rec(unLimDimPos), ncVar->type(), ncVar->rec_size());
	} else {
		return ncValues2Data(ncVar->values(), ncVar->type(), ncVar->num_vals());
	}
}

template<typename T>
boost::shared_ptr<Data> getData_(NcVar* ncVar, long* count)
{
    size_t length = accumulate(count, count + ncVar->num_dims(), 1, multiplies<size_t>());
    boost::shared_array<T> vals(new T[length]);
    if (!ncVar->get(&vals[0], &count[0])) throw CDMException("cannot get netcdf dataslice values");
    return createData(length, vals);
}

boost::shared_ptr<Data> NetCDF_CDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
{
    const CDMVariable& var = cdm_->getVariable(varName);
    if (var.hasData()) {
        return var.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionStartPositions());
    }

    NcVar* ncVar = ncFile->get_var(var.getName().c_str());
    vector<long> ncStartPos;
    copy(sb.getDimensionStartPositions().begin(), sb.getDimensionStartPositions().end(), back_inserter(ncStartPos));
    reverse(ncStartPos.begin(), ncStartPos.end()); // netcdf/c++ uses opposite dimension numbering
    if (static_cast<int>(ncStartPos.size()) != ncVar->num_dims()) throw CDMException("dimension mismatch between slicebuilder and netcdf variable "+ varName);
    if (!ncVar->set_cur(&ncStartPos[0])) throw CDMException("cannot set the netcdf dataslice start-position");

    vector<long> count;
    copy(sb.getDimensionSizes().begin(), sb.getDimensionSizes().end(), back_inserter(count));
    reverse(count.begin(), count.end()); // netcdf/c++ uses opposite dimension numbering
    if (static_cast<int>(count.size()) != ncVar->num_dims()) throw CDMException("dimension mismatch between slicebuilder and netcdf size of variable "+ varName);

    boost::shared_ptr<Data> retData;
    switch (var.getDataType()) {
    case CDM_CHAR:
    case CDM_STRING:
        retData = getData_<char>(ncVar, &count[0]); break;
    case CDM_SHORT:
        retData = getData_<short>(ncVar, &count[0]); break;
    case CDM_INT:
        retData = getData_<int>(ncVar, &count[0]); break;
    case CDM_FLOAT:
        retData = getData_<float>(ncVar, &count[0]); break;
    case CDM_DOUBLE:
        retData = getData_<double>(ncVar, &count[0]); break;
    default:
        throw CDMException("Cannot read netcdf-data without know type: "+var.getDataType());
    }
    return retData;
}

void NetCDF_CDMReader::addAttribute(const std::string& varName, NcAtt* ncAtt)
{
	CDMDataType dt(ncType2cdmDataType(ncAtt->type()));
	boost::shared_ptr<Data> attrData = ncValues2Data(ncAtt->values(), ncAtt->type(), ncAtt->num_vals());
	cdm_->addAttribute(varName, CDMAttribute(ncAtt->name(), dt, attrData));
}


}
