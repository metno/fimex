/*
  Fimex, src/metgm/MetGmCDMReaderImpl.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef METGM_CDM_READERIMPL_H
#define METGM_CDM_READERIMPL_H

// implementation
//
#include "MetGmCDMVariableProfile.h"
#include "MetGmConfigurationMappings.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMDimension.h"
#include "fimex/XMLInput.h"

// boost
//
#include <boost/algorithm/string/replace.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>

// standard
//
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

namespace MetNoFimex {

    class XMLDoc;
    class MetGmVersion;
    class MetGmHandlePtr;
    class MetGmGroup3Ptr;
    class MetGmFileHandlePtr;

    class MetGmCDMReaderImpl : public CDMReader
    {
    public:
        explicit MetGmCDMReaderImpl(const std::string& metgmsource, const XMLInput& configXML, const std::shared_ptr<CDM>& cdm);
        ~MetGmCDMReaderImpl();

        void init(const XMLInput& configXML);
        bool deinit();

        int getPidForMetgmName(const std::string& metgm_name);
        int getPidForCdmName(const std::string& cdm_name);

        virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);
        virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);

    protected:
        explicit MetGmCDMReaderImpl(const std::shared_ptr<CDM>& cdm);

        virtual void configure(const XMLDoc_p& doc);
        virtual void parseMgmFile(const std::string& mgmFileName);
        virtual void addVariables();

        void addGlobalCDMAttributes();
        void addTimeDimension();
        void addHorizontalDimensions();
        void addVerticalDimensions();
        void sanityCheck();

        std::string spaceToUnderscore(const std::string& text);

        std::string                            sourceFileName_;
        std::string                            configId_;
        std::shared_ptr<MetGmHandlePtr> pHandle_;
        std::shared_ptr<MetGmGroup1Ptr> pGroup1_;
        std::shared_ptr<MetGmGroup2Ptr> pGroup2_;

        xml_configuration xmlConfiguration_;
        cdm_configuration cdmConfiguration_;

        /**
          * cache widely dimensions
          */
        CDMDimension xDim_;
        CDMDimension yDim_;
        CDMDimension tDim_;
    };

} // end namespace

#endif // METGM_CDM_READERIMPL_H

