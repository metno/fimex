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

#ifndef METGM_CDMWRITERIMPL_HPP
#define METGM_CDMWRITERIMPL_HPP

// implementation stuff
//
#include "MetGmTimeTag.h"
#include "MetGmCDMVariableProfile.h"
#include "MetGmConfigurationMappings.h"

// fimex
#include "fimex/CDMWriter.h"
#include "fimex/CDM.h"
#include "fimex/XMLDoc.h"

// standard
//
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace MetNoFimex {

    /* forwrd decelarations */
    class MetGmVersion;
    class MetGmHandlePtr;
    class MetGmGroup3Ptr;
    class MetGmFileHandlePtr;
    class MetGmTimeTag;
    class MetGmXTag;
    class MetGmYTag;
    class MetGmTags;

    class MetGmCDMWriterImpl : public CDMWriter
    {
    public:
        /**
         * @param cdmReader dataSource
         * @param outputFile file-name to write to
         * @param configFile xml-configuration
         */
        explicit MetGmCDMWriterImpl
                (
                        CDMReader_p cdmReader,
                        const std::string& outputFile,
                        const std::string& configFile
                );

        virtual ~MetGmCDMWriterImpl();

       /**
         * @warning only public for testing
         * @return the new name of a variable, eventually changed by the writers config
         */
        const std::string& getVariableName(const std::string& varName) const;
        /**
         * @warning only public for testing
         * @return the new name of a dimension, eventually changed by the writers config
         */
        const std::string& getDimensionName(const std::string& dimName) const;
        /**
         * @warning only public for testing
         * @param varName original variable name  (before config: newname)
         * @param attName original attribute name (before config: newname)
         * @return an attribute contained in the writers attribute, possibly added by config
         */
        const CDMAttribute& getAttribute(const std::string& varName, const std::string& attName) const;

    protected:

        explicit MetGmCDMWriterImpl(CDMReader_p cdmReader, const std::string& outputFile);

        void configure(const std::unique_ptr<XMLDoc>& doc);

        void writeGroup0Data();
        void writeGroup1Data();
        void writeGroup2Data();
        void writeHeader();

        void writeGroup3Data(const CDMVariable* pVar);
        void writeGroup3VerticalAxis(const CDMVariable* pVar);
        void writeGroup3TimeAxis(const CDMVariable* pVar);
        void writeGroup3HorizontalAxis(const CDMVariable* pVar);

        void writeGroup4Data(const CDMVariable* pVar);


        virtual void init();
        virtual void writeGroup5Data(const CDMVariable* pVar);

        typedef std::shared_ptr<MetGmTags> MetGmTagsPtr;

        std::string                                 configFileName_;

        std::shared_ptr<MetGmVersion> metgmVersion_;
        std::shared_ptr<MetGmHandlePtr> metgmHandle_;
        std::shared_ptr<MetGmFileHandlePtr> metgmFileHandle_;
        std::shared_ptr<MetGmTimeTag> metgmTimeTag_;

        time_point analysisTime_;

        xml_configuration                           xmlConfiguration_;
        cdm_configuration                           cdmConfiguration_;
    };

}

#endif // METGM_CDMWRITERIMPL_HPP

