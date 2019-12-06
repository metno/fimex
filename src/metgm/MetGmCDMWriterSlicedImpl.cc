/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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

#include "MetGmCDMWriterSlicedImpl.h"

#include "fimex/CDMException.h"
#include "MetGmHandlePtr.h"

namespace MetNoFimex {

MetGmCDMWriterSlicedImpl::MetGmCDMWriterSlicedImpl(CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile)
    : MetGmCDMWriterImpl(cdmReader, outputFile, configFile)
{
}

void MetGmCDMWriterSlicedImpl::init()
{
    for (xml_configuration::const_iterator pIt : sorted_by_pid(xmlConfiguration_)) {
        const MetGmConfigurationMappings& entry = *pIt;

        if (std::find_if(cdmConfiguration_.begin(), cdmConfiguration_.end(), MetGmCDMVariableProfileEqName(entry.cdmName_)) != cdmConfiguration_.end()) {
            throw CDMException("hmmm... the variable should not be found in cdm profile map");
        }

        const CDMVariable* pVariable = &cdmReader->getCDM().getVariable(entry.cdmName_);

        MetGmTagsPtr tags = MetGmTags::createMetGmTagsForSlicedWriting(cdmReader, pVariable, metgmHandle_, entry.p_id_);
        if (!tags) {
            MGM_MESSAGE_POINT(std::string(" MetGmTag null -- not writing variable :").append(entry.cdmName_))
            continue;
        }

        MetGmCDMVariableProfile profile(entry.p_id_, entry.cdmName_, tags);
        // make sure that units are aligned
        profile.units_ = tags->units();
        cdmConfiguration_.push_back(profile);
    }
}

void MetGmCDMWriterSlicedImpl::writeGroup5Data(const MetGmCDMVariableProfile& profile, const CDMVariable* pVar)
{
    size_t total_num_of_slices = 1;
    if (profile.pTags_->tTag()) {
        total_num_of_slices = profile.pTags_->tTag()->nT();
    }

    for (size_t slice_index = 0; slice_index < total_num_of_slices; ++slice_index) {
        size_t cSlicePos = -1;
        DataPtr raw_slice = cdmReader->getScaledDataSliceInUnit(pVar->getName(), profile.units_, slice_index);
        shared_array<float> slice_to_write = raw_slice->asFloat();
        profile.pTags_->sliceToMetGmLayout(slice_to_write);
        MGM_THROW_ON_ERROR(mgm_write_group5_slice(*metgmFileHandle_, *metgmHandle_, slice_to_write.get(), &cSlicePos));
    }
}

} // namespace MetNoFimex
