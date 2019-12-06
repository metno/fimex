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

#ifndef METGM_CDMWRITERIMPL_HPP
#define METGM_CDMWRITERIMPL_HPP

// implementation stuff
//
#include "MetGmCDMVariableProfile.h"
#include "MetGmConfigurationMappings.h"

// fimex
#include "fimex/CDMWriter.h"

// standard
//
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace MetNoFimex {

class CDMAttribute;
class CDMVariable;
class XMLDoc;

/* forwrd decelarations */
class MetGmVersion;
class MetGmHandlePtr;
class MetGmGroup3Ptr;
class MetGmFileHandlePtr;
class MetGmTags;
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
    explicit MetGmCDMWriterImpl(CDMReader_p cdmReader, const std::string& outputFile, const std::string& configFile);

    ~MetGmCDMWriterImpl();

    virtual void configureAndWrite();

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
    typedef std::shared_ptr<MetGmTags> MetGmTagsPtr;

    explicit MetGmCDMWriterImpl(CDMReader_p cdmReader, const std::string& outputFile);

    virtual void configure(const std::unique_ptr<XMLDoc>& doc);
    virtual void init();
    virtual void write();

    void writeGroup0Data();
    void writeGroup1Data();
    void writeGroup2Data();
    void writeHeader();

    void writeGroup3Data(const MetGmCDMVariableProfile& profile);
    void writeGroup3VerticalAxis(const MetGmTagsPtr& tags);
    void writeGroup3TimeAxis(const MetGmTagsPtr& tags);
    void writeGroup3HorizontalAxis(const MetGmTagsPtr& tags);

    void writeGroup4Data(const MetGmCDMVariableProfile& profile);

    virtual void writeGroup5Data(const MetGmCDMVariableProfile& profile, const CDMVariable* pVar);

protected:
    std::unique_ptr<XMLDoc> xmlConfig_;

    std::shared_ptr<MetGmVersion> metgmVersion_;
    std::shared_ptr<MetGmHandlePtr> metgmHandle_;
    std::shared_ptr<MetGmFileHandlePtr> metgmFileHandle_;
    std::shared_ptr<MetGmTimeTag> metgmTimeTag_;

    xml_configuration xmlConfiguration_;
    cdm_configuration cdmConfiguration_;
};

} // namespace MetNoFimex

#endif // METGM_CDMWRITERIMPL_HPP

