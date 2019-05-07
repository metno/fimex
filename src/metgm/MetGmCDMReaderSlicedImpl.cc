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

#include "MetGmCDMReaderSlicedImpl.h"

// private implementation details
//
#include "MetGmUtils.h"
#include "MetGmVersion.h"
#include "MetGmHandlePtr.h"
#include "MetGmGroup1Ptr.h"
#include "MetGmGroup2Ptr.h"
#include "MetGmGroup3Ptr.h"
#include "MetGmGroup5Ptr.h"
#include "MetGmDimensionsTag.h"
#include "MetGmFileHandlePtr.h"

// METGM C lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDM.h"
#include "fimex/CDMAttribute.h"
#include "fimex/CDMDimension.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/Utils.h"
#include "fimex/XMLDoc.h"

// boost
//
#include <boost/lexical_cast.hpp>

#include <regex>
#include <tuple>

// libxml2
//
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

//#define GXDEBUG 1

namespace MetNoFimex {

MetGmCDMReaderSlicedImpl::MetGmCDMReaderSlicedImpl(const std::string& mgmsource, const XMLInput& configXML, const std::shared_ptr<CDM>& cdm)
    : MetGmCDMReaderImpl(cdm)
{
    sourceFileName_ = mgmsource;
    configId_ = configXML.id();

    try {
        init(configXML);
    } catch (std::runtime_error& exp) {
        throw CDMException(std::string("MetGmCDMReaderImpl error: ") + exp.what());
    }
    }

    MetGmCDMReaderSlicedImpl::~MetGmCDMReaderSlicedImpl() { }

    void MetGmCDMReaderSlicedImpl::addVariables()
    {
        cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
        for(cdmNameView::iterator nIt = nameView.begin(); nIt != nameView.end(); ++nIt)
        {
            MetGmCDMVariableProfile profile = *nIt;

            int p_id = profile.p_id_;

            std::string fillValue = profile.fillValue_.empty() ? std::string("9999.0") : profile.fillValue_;

            std::vector<CDMAttribute> attributes;

            CDMAttribute metgmPidAttribute("metgm_p_id", "short", boost::lexical_cast<std::string>(p_id));
            attributes.push_back(metgmPidAttribute);

            CDMAttribute cfNameAttribute("standard_name", "string", profile.standardName_);
            attributes.push_back(cfNameAttribute);

            CDMAttribute varUnitsAttribute("units", "string", profile.units_);
            attributes.push_back(varUnitsAttribute);

            CDMAttribute varFillValueAttribute("_FillValue", "float", fillValue);
            attributes.push_back(varFillValueAttribute);

            if(!profile.addOffset_.empty()) {
                CDMAttribute varAddOffsetAttribute("add_offset", "float", profile.addOffset_);
                attributes.push_back(varAddOffsetAttribute);
            }

            if(!profile.scaleFactor_.empty()) {
                CDMAttribute varScaleFactorAttribute("scale_factor", "float", profile.scaleFactor_);
                attributes.push_back(varScaleFactorAttribute);
            }

            std::vector<std::string> shape;

            shape.push_back(xDim_.getName());
            shape.push_back(yDim_.getName());

            if(p_id != 0) {

                if(!profile.zDimensionName_.empty() && cdm_->hasDimension(profile.zDimensionName_))
                    shape.push_back(profile.zDimensionName_);

                shape.push_back(tDim_.getName());
            }

            CDMVariable var(profile.cdmName_, CDM_FLOAT, shape);

            cdm_->addVariable(var);

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(profile.cdmName_, *attrIt);
            }
        }
    }

    DataPtr MetGmCDMReaderSlicedImpl::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
        CDMVariable& variable = cdm_->getVariable(varName);

        if(!cdm_->hasVariable(varName))
            return DataPtr();

        if(variable.hasData()) {
            return getDataSliceFromMemory(variable, unLimDimPos);
        } else {
            cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
            cdmNameView::iterator it = nameView.find(varName);

            if(it == nameView.end())
                return DataPtr();

            MetGmCDMVariableProfile profile = *it;
            DataPtr data = createData(profile.pTags_->sliceDataSize(), profile.pTags_->readDataSlices(unLimDimPos + 1, 1));

            return data;
        }
    }

#if 0
    // this code does not work and was thought to improve performance
    // disabled until Alexandar fixes it
    DataPtr MetGmCDMReaderSlicedImpl::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
//        MGM_CHECK_POINT()
        using namespace std;
        DataPtr retData;
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

                cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
                cdmNameView::iterator it = nameView.find(varName);

                if(it == nameView.end())
                    return DataPtr();

                MetGmCDMVariableProfile profile = *it;
                retData = createData(unLimSliceSize * unLimDimSize, profile.pTags_->readDataSlices(unLimDimStart + 1, unLimDimSize));
                retData = retData->slice(maxDimSize, dimStart, dimSize);
                assert(retData->size() == unLimSliceSize * unLimDimSize);
//                MGM_CHECK_POINT()
            } else {
                retData = getData(varName)->slice(sb.getMaxDimensionSizes(), sb.getDimensionStartPositions(), sb.getDimensionSizes());
            }
        }
        return retData;
    }
#endif

    void MetGmCDMReaderSlicedImpl::parseMgmFile(const std::string& mgmFileName)
    {
        pHandle_ = MetGmHandlePtr::createMetGmHandleForReading(mgmFileName);
        if(!(*pHandle_))
            throw CDMException(std::string("error opening metgm handle"));

        pGroup1_ = MetGmGroup1Ptr::createMetGmGroup1PtrForReading(pHandle_);

        pGroup2_ = MetGmGroup2Ptr::createMetGmGroup2PtrForReading(pHandle_);

        std::shared_ptr<MetGmVerticalTag> prevZTag;

        for(int index = 0; index < pGroup2_->totalnp(); ++index) {

            std::shared_ptr<MetGmTags> tags = MetGmTags::createMetGmTagsForSlicedReading(pGroup1_, pGroup2_, prevZTag);

            prevZTag = tags->zTag();

            xmlPidView &pidView = xmlConfiguration_.get<xml_pid_index>();

            std::string kildeName;
            std::string standardName;
            std::string addOffset;
            std::string scaleFactor;
            std::string strUnit(mgm_get_param_unit(tags->p_id(), *pHandle_));
            std::string fillValue;
            if(pidView.count(tags->p_id()) == 0) {

            } else if(pidView.count(tags->p_id()) == 1) {
                MetGmConfigurationMappings entry = *(pidView.find(tags->p_id()));
                kildeName = entry.cdmName_;
                standardName = entry.standardName_;
                fillValue = entry.fillValue_;
                addOffset = entry.addOffset_;
                scaleFactor = entry.scaleFactor_;
//                if(!entry.units_.empty())
//                    strUnit = entry.units_;
            } else {
                xmlPidView::iterator ic0, ic1;
                std::tie(ic0, ic1) = pidView.equal_range(tags->p_id());
                for(; ic0 != ic1; ++ic0) {
                    if(!ic0->units_.empty() && ic0->units_ == strUnit) {
                        kildeName = ic0->cdmName_;
//                        strUnit = ic0->units_;
                        standardName = ic0->standardName_;
                        fillValue = ic0->fillValue_;
                        addOffset = ic0->addOffset_;
                        scaleFactor = ic0->scaleFactor_;
                        break;
                    }
                }
            }

            if(!kildeName.empty()) {

                std::string fixedKildeName(kildeName);

                switch(tags->pr()) {
                case 0:
                    if (!ends_with(kildeName, "MSL"))
                        fixedKildeName.append("_MSL");
                    break;
                case 1:
                    if (!ends_with(kildeName, "GND"))
                        fixedKildeName.append("_GND");
                    break;
                case 2:
                    if (!ends_with(kildeName, "Pa"))
                        fixedKildeName.append("_Pa");
                    break;
                }

                MetGmCDMVariableProfile profile(tags->p_id(), fixedKildeName, tags);
                profile.standardName_ = standardName;
                profile.units_ = strUnit;
                profile.fillValue_ = fillValue;
                profile.addOffset_ = addOffset;
                profile.scaleFactor_ = scaleFactor;
                cdmConfiguration_.insert(profile);

            }
        }
    }

}
