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
#include "fimex/Data.h"
#include "fimex/TimeUnit.h"
#include "fimex/XMLDoc.h"
#include "fimex/CDMException.h"

// boost
//
#include <boost/regex.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/scoped_array.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

// libxml2
//
#include <libxml/xinclude.h>
#include <libxml/xpathInternals.h>

//#define GXDEBUG 1

namespace MetNoFimex {

    MetGmCDMReaderSlicedImpl::MetGmCDMReaderSlicedImpl(const std::string& mgmsource, const std::string& configfilename, const boost::shared_ptr<CDM>& cdm)
        : MetGmCDMReaderImpl(cdm)
    {
        sourceFileName_ = mgmsource;
        configFileName_ = configfilename;

        try {
            init();
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

//            boost::shared_ptr<Data> data = createData(profile.pTags_->totalDataSize(), profile.pTags_->data());
//            var.setData(data);

            cdm_->addVariable(var);

            for (std::vector<CDMAttribute>::const_iterator attrIt = attributes.begin(); attrIt != attributes.end(); ++attrIt) {
                cdm_->addAttribute(profile.cdmName_, *attrIt);
            }
        }
    }

    boost::shared_ptr<Data> MetGmCDMReaderSlicedImpl::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
        CDMVariable& variable = cdm_->getVariable(varName);

        if(!cdm_->hasVariable(varName))
            return boost::shared_ptr<Data>();

        if(variable.hasData()) {
            return getDataSliceFromMemory(variable, unLimDimPos);
        } else {
            cdmNameView& nameView = cdmConfiguration_.get<cdm_name_index>();
            cdmNameView::iterator it = nameView.find(varName);

            if(it == nameView.end())
                return boost::shared_ptr<Data>();

            MetGmCDMVariableProfile profile = *it;
            boost::shared_ptr<Data> data = createData(profile.pTags_->totalDataSize(), profile.pTags_->getDataSlice(unLimDimPos + 1));

            return data;
        }
    }

    boost::shared_ptr<Data> MetGmCDMReaderSlicedImpl::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
        return CDMReader::getDataSlice(varName, sb);
    }

    void MetGmCDMReaderSlicedImpl::parseMgmFile(const std::string& mgmFileName)
    {
        pHandle_ = MetGmHandlePtr::createMetGmHandleForReading(mgmFileName);
        if(!(*pHandle_))
            throw CDMException(std::string("error opening metgm handle"));

        pGroup1_ = MetGmGroup1Ptr::createMetGmGroup1PtrForReading(pHandle_);

        pGroup2_ = MetGmGroup2Ptr::createMetGmGroup2PtrForReading(pHandle_);

        boost::shared_ptr<MetGmVerticalTag> prevZTag;

        for(int index = 0; index < pGroup2_->totalnp(); ++index) {

            boost::shared_ptr<MetGmTags> tags = MetGmTags::createMetGmTagsForSlicedReading(pGroup1_, pGroup2_, prevZTag);

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
                boost::tuples::tie(ic0,ic1) = pidView.equal_range(tags->p_id());
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
                    if(!boost::algorithm::ends_with(kildeName, "MSL"))
                        fixedKildeName.append("_MSL");
                    break;
                case 1:
                    if(!boost::algorithm::ends_with(kildeName, "GND"))
                        fixedKildeName.append("_GND");
                    break;
                case 2:
                    if(!boost::algorithm::ends_with(kildeName, "Pa"))
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
