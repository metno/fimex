/*
 * Fimex, C_CDMReader.cc
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Oct 19, 2009
 *      Author: Heiko Klein
 */

#include "fimex/mifi_cdm_reader.h"
#include "fimex/C_CDMReader.h"
#include "fimex/Data.h"
#include "fimex/CDMVariable.h"
#include "fimex/CDM.h"
#include "fimex/Utils.h"

namespace MetNoFimex
{

C_CDMReader::C_CDMReader(CDMReader_p dataReader)
: dataReader_(dataReader)
{
    *cdm_ = dataReader_->getCDM();
}

C_CDMReader::~C_CDMReader()
{
}

void C_CDMReader::setDoubleCallbackFunction(const std::string& varName, doubleDatasliceCallbackPtr callback)
{
    CDMVariable& var = cdm_->getVariable(varName);
    var.setDataType(CDM_DOUBLE);
    // remove scale_factor and add_offset attributes
    // data will be auto-scaled
    cdm_->removeAttribute(varName, "scale_factor");
    cdm_->removeAttribute(varName, "add_offset");
    // set the datatype for later processing
    doubleCallbacks_[varName] = callback;
}

void noDealloc(CDMReader* reader)
{
    // do nothing
    // this function is intended to overwrite the standard 'free'
    // used within a shared_ptr<CDMReader>, where the auto-dealloc is not needed
}

DataPtr C_CDMReader::getDataSlice(const std::string & varName, size_t unLimDimPos)
{
    // check if there is a callback-function (currently only double)
    std::map<std::string, doubleDatasliceCallbackPtr>::iterator callbackIt = doubleCallbacks_.find(varName);
    if (callbackIt == doubleCallbacks_.end()) {
        return dataReader_->getDataSlice(varName, unLimDimPos);
    }

    // start modify the data by the existing callback
    doubleDatasliceCallbackPtr callback = callbackIt->second;

    const CDMVariable& variable = cdm_->getVariable(varName);
    // this reader should never have in-memory data, since it is not accessible
    // from C
    assert(variable.hasData() == false);
    DataPtr data = dataReader_->getScaledDataSlice(varName, unLimDimPos);

    // wrap the object as a mifi_cdm_reader for C-usage
    mifi_cdm_reader reader(boost::shared_ptr<C_CDMReader>(this, noDealloc));
    // get a C-array to the scaled data
    boost::shared_array<double> doubleArray = data->asDouble();
    // call the callback-function
    int retVal = (*callback)(&reader, varName.c_str(), unLimDimPos, &doubleArray[0], data->size());
    if (retVal != 0) {
        throw CDMException("c-callback-function for variable "+varName+" gives error-code: "+type2string(retVal));
    }
    return createData(data->size(), doubleArray);
}

}
