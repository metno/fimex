/*
 * Fimex, GribCDMIndexer.h
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#ifndef FIMEX_GRIBCDMINDEXER_H_
#define FIMEX_GRIBCDMINDEXER_H_

#include "fimex/ChunkReaderFactory.h"
#include "fimex/DataDecl.h"
#include "fimex/GridDefinition.h"
#include "fimex/XMLDoc.h"
#include "fimex/XMLInput.h"

#include <map>
#include <regex>
#include <vector>

namespace MetNoFimex {

// forward decl.
class CDM;
class CDMDimension;

class GribFileMessage;

class GribCDMIndexer
{
public:
    struct grib_index
    {
        size_t file_index;
        size_t message_start;
        size_t message_size; // > 1 for a valid grib message
        bool is_valid() const { return file_index > 0; }
    };
    typedef std::vector<grib_index> grib_index_v;

    struct grib_var
    {
        double scale_factor = 0;
        double add_offset = 0;
        bool has_precision() const { return scale_factor != 0; }

        grib_index_v messages;
    };

    struct grib_indexed
    {
        std::vector<std::string> grib_files;
        std::map<std::string, grib_var> grib_vars;
    };

public:
    GribCDMIndexer(const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members, ChunkReaderFactory_p ca);
    GribCDMIndexer(const XMLInput& configXML, const std::vector<std::pair<std::string, std::regex>>& members, ChunkReaderFactory_p ca);
    ~GribCDMIndexer();

    void load(const std::vector<std::string>& fileNames);
    void load(const std::string& grbmlFileName);

    void build(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi);

    static std::vector<std::pair<std::string, std::regex>> makeMembersRegex(const std::vector<std::pair<std::string, std::string>>& memberStrings);

private:
    struct Init;
    typedef std::shared_ptr<Init> Init_p;
    Init_p init_;

    /**
     * init xmlNodeIdx1 and xmlNodeIdx2, used for faster lookups in xml-tree
     */
    void initXMLNodeIdx();

    /**
     * initialize the configXML document
     * @param configXML
     */
    void initXML(const XMLInput& configXML);

    void initUpdateTemplatedAttributeValues();

    void loadOneMessage(const GribFileMessage& gfm);
    void loadAddProjection(const GridDefinition& gridDef, const std::string& gridType);

    void buildLevels();
    void initAddTimeDimension(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi);
    void initAddGlobalAttributes(std::shared_ptr<CDM> cdm);
    void initCreateVarMessages(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi);
    void initLevels(std::shared_ptr<CDM> cdm);

    void initAddEnsembles(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi);
    void initAddVariables(std::shared_ptr<CDM> cdm, std::shared_ptr<grib_indexed> gi);

    std::vector<double> readValuesFromXPath_(std::shared_ptr<CDM> cdm, xmlNodePtr node, DataPtr levelData, const std::vector<double>& levelPvData,
                                             const std::string& extension);
    void initSpecialLevels_(std::shared_ptr<CDM> cdm, xmlNodePtr node, const std::string& extension, const std::pair<long, long>& levelType,
                            std::size_t levelPos, const std::vector<std::string>& levelShape, DataPtr& levelData);
};

} // namespace MetNoFimex

#endif /* FIMEX_GRIBCDMINDEXER_H_ */
