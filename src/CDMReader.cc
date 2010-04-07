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
#include "fimex/Data.h"

namespace MetNoFimex {

// retrieve size of data slice
static size_t getSliceSize(const CDM& cdm, const CDMVariable& variable) {
	size_t sliceSize = 1;
	std::vector<std::string> shape = variable.getShape();
	for (std::vector<std::string>::const_iterator dimIt = shape.begin(); dimIt != shape.end(); ++dimIt) {
	    const CDMDimension& dim = cdm.getDimension(*dimIt);
	    if (!dim.isUnlimited()) {
	        sliceSize *= dim.getLength();
	    }
	}
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

boost::shared_ptr<Data> CDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb) throw(CDMException)
{
    using namespace std;
    boost::shared_ptr<Data> retData;
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        retData = variable.getData()->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
    } else {
        if (cdm_->hasUnlimitedDim(variable)) {
            string unLimDim = cdm_->getUnlimitedDim()->getName();
            vector<string> dimNames = sb.getDimensionNames();
            // get the data along the unlimited dimension and join
            // unlimited dimension must be outer=first dimension!
            size_t unLimDimStart = 0;
            size_t unLimDimSize = 0;
            vector<size_t> dimStart;
            vector<size_t> dimSize;
            vector<size_t> maxDimSize;
            const vector<size_t>& orgDimStart = sb.getDimensionStartPositions();
            const vector<size_t>& orgDimSize = sb.getDimensionSizes();
            const vector<size_t>& orgMaxDimSize = sb.getMaxDimensionSizes();
            size_t unLimSliceSize = 1;
            for (size_t i = 0; i < dimNames.size(); ++i) {
                if (dimNames.at(i) == unLimDim) {
                    unLimDimStart = orgDimStart.at(i);
                    unLimDimSize = orgDimSize.at(i);
                } else {
                    dimStart.push_back(orgDimStart.at(i));
                    dimSize.push_back(orgDimSize.at(i));
                    maxDimSize.push_back(orgMaxDimSize.at(i));
                    unLimSliceSize *= orgDimSize.at(i);
                }
            }
            if (unLimDimSize == 0) {
                return createData(variable.getDataType(), 0);
            }
            // read now each unlimdim-slice
            // slice that slice according to the other dimensions
            // join those slices
            retData = createData(variable.getDataType(), unLimSliceSize*unLimDimSize, cdm_->getFillValue(varName));
            for (size_t i = 0; i < unLimDimSize; ++i) {
                boost::shared_ptr<Data> unLimDimData = getDataSlice(varName, i+unLimDimStart);
                if (unLimDimData->size() != 0) {
                    unLimDimData = unLimDimData->slice(maxDimSize, dimStart, dimSize);
                    assert(unLimDimData->size() == unLimSliceSize);
                    retData->setValues(i*unLimSliceSize, *unLimDimData);
                }
            }
        } else {
            retData = getData(varName)->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
        }
    }
    return retData;
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

boost::shared_ptr<Data> CDMReader::getScaledDataSlice(const std::string& varName, const SliceBuilder& sb) throw(CDMException)
{
    return scaleDataOf(varName, getDataSlice(varName, sb));
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
