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

#include "fimex/CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/interpolation.h"
#include <boost/bind.hpp>
#include "fimex/Data.h"

namespace MetNoFimex {

static void addSizeUnlessUnlimited(const CDM& cdm, const std::string& dimName, size_t& size) {
	const CDMDimension& dim = cdm.getDimension(dimName);
	if (!dim.isUnlimited()) {
		size *= dim.getLength();
	}
}

// retrieve size of data slice
static size_t getSliceSize(const CDM& cdm, const CDMVariable& variable) {
	size_t sliceSize = 1;
	std::vector<std::string> shape = variable.getShape();
	for_each(shape.begin(), shape.end(), boost::bind(addSizeUnlessUnlimited, cdm, _1, sliceSize));
	return sliceSize;
}

CDMReader::CDMReader()
    : cdm_(new CDM())
{
}

const CDM& CDMReader::getCDM() const
{
    return *(cdm_.get());
}

boost::shared_ptr<Data> CDMReader::getData(const std::string& varName) throw(CDMException)
{
	const CDMVariable& variable = cdm_->getVariable(varName);
	if (variable.hasData()) {
		return variable.getData();
	} else {
		if (cdm_->hasUnlimitedDim(variable)) {
			const CDMDimension* udim = cdm_->getUnlimitedDim();
			size_t uDimSize = udim->getLength();
			size_t sliceSize = getSliceSize(getCDM(), variable);
			boost::shared_ptr<Data> data = createData(variable.getDataType(), uDimSize*sliceSize);
			for (size_t i = 0; i < uDimSize; i++) {
				boost::shared_ptr<Data> slice = getDataSlice(varName, i);
				data->setValues(i*sliceSize, *slice, 0, sliceSize);
			}
			return data;
		} else {
			return getDataSlice(varName, 0);
		}
	}
}

// handle data scaling using add_offset, scale_factor and _FillValue from the varName variable
boost::shared_ptr<Data> CDMReader::scaleDataOf(const std::string& varName, boost::shared_ptr<Data> data) throw(CDMException)
{
	// retrieve scale and offset
	CDMAttribute attr;
	double scale = 1.;
	double offset = 0.;
	if (cdm_->getAttribute(varName, "scale_factor", attr)) {
		scale = attr.getData()->asConstDouble()[0];
	}
	if (cdm_->getAttribute(varName, "add_offset", attr)) {
		offset = attr.getData()->asConstDouble()[0];
	}
	// fillValue
	double inFillValue = cdm_->getFillValue(varName);

	return data->convertDataType(inFillValue, scale, offset, CDM_DOUBLE, MIFI_UNDEFINED_D,1,0);
}

boost::shared_ptr<Data> CDMReader::getScaledDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	return scaleDataOf(varName, getDataSlice(varName, unLimDimPos));
}

boost::shared_ptr<Data> CDMReader::getScaledData(const std::string& varName) throw(CDMException)
{
	return scaleDataOf(varName, getData(varName));
}

boost::shared_ptr<Data> CDMReader::getDataSliceFromMemory(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	if (variable.hasData()) {
		if (cdm_->hasUnlimitedDim(variable)) {
			// cut out the unlimited dim data
			size_t sliceSize = getSliceSize(*cdm_.get(), variable);
			return createDataSlice(variable.getDataType(), *(variable.getData()), unLimDimPos*sliceSize, sliceSize);
		} else {
			return variable.getData();
		}
	} else {
		return boost::shared_ptr<Data>();
	}
}

}
