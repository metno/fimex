/*
 * Fimex
 *
 * (C) Copyright 2011, met.no
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

// private/implementation code
//
#include "MetGmTags.h"
#include "MetGmUtils.h"
#include "MetGmVersion.h"
#include "MetGmHandlePtr.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup2Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmConfigurationMappings.h"

namespace MetNoFimex {
    MetGmCDMWriterSlicedImpl::MetGmCDMWriterSlicedImpl
        (
            boost::shared_ptr<CDMReader> cdmReader,
            const std::string& outputFile,
            const std::string& configFile
        )
        : MetGmCDMWriterImpl(cdmReader, outputFile)
    {
        configFileName_ = configFile;
        std::auto_ptr<XMLDoc> xmlDoc;
        if (configFileName_ == std::string()) {
            xmlDoc = std::auto_ptr<XMLDoc>(0);
        } else {
            xmlDoc = std::auto_ptr<XMLDoc>(new XMLDoc(configFileName_));
        }

        metgmTimeTag_ = MetGmTimeTag::createMetGmTimeTagGlobal(cdmReader);
        metgmVersion_ = MetGmVersion::createMetGmVersion(xmlDoc);
        metgmFileHandle_ = MetGmFileHandlePtr::createMetGmFileHandlePtrForWriting(outputFile);
        metgmHandle_ = MetGmHandlePtr::createMetGmHandleForWriting(metgmFileHandle_, metgmVersion_);

        configure(xmlDoc);

        init();
    }

    MetGmCDMWriterSlicedImpl::~MetGmCDMWriterSlicedImpl() { }

    void MetGmCDMWriterSlicedImpl::init()
    {
        xmlPidView &pidView = xmlConfiguration_.get<xml_pid_index>();
        for(xmlPidView::const_iterator pIt = pidView.begin(); pIt != pidView.end(); ++pIt) {

            MetGmConfigurationMappings entry = *pIt;

//            MGM_MESSAGE_POINT(std::string(" writing parameter with pid = ").append(boost::lexical_cast<std::string>(entry.p_id_)))

            MetGmTagsPtr tags;

            cdmNameView &nameView = cdmConfiguration_.get<cdm_name_index>();
            if(nameView.find(entry.cdmName_) != nameView.end()) {
                throw CDMException("hmmm... the variable should not be fount in cdm profile map");
            }

            const CDMVariable* pVariable = &cdmReader->getCDM().getVariable(entry.cdmName_);

            tags = MetGmTags::createMetGmTagsForSlicedWriting(cdmReader, pVariable, metgmHandle_, entry.p_id_);

            if(!tags.get()) {
                MGM_MESSAGE_POINT(std::string(" MetGmTag null -- not writing variable :").append(entry.cdmName_))
                continue;
            }

            MetGmCDMVariableProfile profile(entry.p_id_, entry.cdmName_, tags);
            // make sure that units are aligned
            profile.units_ = tags->units();
            cdmConfiguration_.insert(profile);
        }

        writeGroup0Data();
        writeGroup1Data();
        writeGroup2Data();

        writeHeader();

        const CDM& cdmRef = cdmReader->getCDM();
        cdm_configuration::const_iterator varIt;
        for(varIt = cdmConfiguration_.begin(); varIt != cdmConfiguration_.end(); ++varIt) {
            const CDMVariable* pVariable = &cdmRef.getVariable(varIt->cdmName_);
            writeGroup3Data(pVariable);
            writeGroup4Data(pVariable);
            writeGroup5Data(pVariable);
        }
    }

    void MetGmCDMWriterSlicedImpl::writeGroup5Data(const CDMVariable* pVar)
    {
        cdmNameView &nameView = cdmConfiguration_.get<cdm_name_index>();

        MetGmCDMVariableProfile profile = *(nameView.find(pVar->getName()));

//        MGM_MESSAGE_POINT(std::string("variable name=").append(pVar->getName()))

        assert(profile.pTags_.get());

        size_t total_num_of_slices = 0;
        if(!profile.pTags_->tTag().get()) {
            total_num_of_slices = 1;
        } else {
            total_num_of_slices = profile.pTags_->tTag()->nT();
        }

        for(size_t slice_index = 0; slice_index < total_num_of_slices; ++slice_index)
        {
            size_t cSlicePos = -1;
            boost::shared_ptr<Data> raw_slice  = cdmReader->getScaledDataSliceInUnit(pVar->getName(), profile.units_, slice_index);
            boost::shared_array<float> slice_to_write = raw_slice->asFloat();
            profile.pTags_->sliceToMetGmLayout(slice_to_write);
            MGM_THROW_ON_ERROR(mgm_write_group5_slice(*metgmFileHandle_, *metgmHandle_, slice_to_write.get(), &cSlicePos));
//            MGM_MESSAGE_POINT(std::string(" slice # written: ").append(boost::lexical_cast<std::string>(cSlicePos)))
        }
    }
}
