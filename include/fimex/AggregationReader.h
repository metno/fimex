/*
 * Fimex, AggregationReader.h
 *
 * (C) Copyright 2013-2026, met.no
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
 */

#ifndef FIMEX_AGGREGATIONREADER_H_
#define FIMEX_AGGREGATIONREADER_H_

#include "fimex/CDMReader.h"

#include <map>
#include <memory>
#include <set>
#include <string>

namespace MetNoFimex {

/**
 * A CDM-Reader reading data from several-files.
 */
class AggregationReader : public CDMReader
{
public:
    typedef enum {
        AGG_UNION,
        AGG_JOIN_EXISTING,
        AGG_JOIN_NEW,
    } AggType;

public:
    AggregationReader(const std::string& aggregationType);
    ~AggregationReader();

    AggType aggType() const { return aggType_; }

    void joinNewVars(const std::string& jnd, const std::set<std::string>& jnv) { joinNewDim = jnd; joinVars = jnv; } // jnd=ncml dimName; knv=ncml variableAgg

    void addReader(CDMReader_p reader, const std::string& id, const std::string& coordValue);
    void initAggregation();

    using CDMReader::getDataSlice;
    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

    static AggType aggTypeFromText(const std::string& aggType);

private:
    void extendJoinedUnLimDimBy(size_t len, const std::string& coordValue);

    void addFirstReader(CDMReader_p reader, const std::string& id, const std::string& coordValue);
    void addOtherReader(CDMReader_p reader, const std::string& id, const std::string& coordValue);

    CDMReader_p findJoinReader(size_t& unLimDimPos) const;
    bool checkJoinExistingDims(CDMReader_p reader, const std::string& varName) const;
    bool checkJoinNewDims(CDMReader_p reader, const std::string& varName) const;

    CDMReader_p findUnionReader(const std::string& varName) const;

private:
    AggType aggType_;

    //! the readers as ordered in the aggregation
    std::vector<std::pair<std::string, CDMReader_p>> readers_;

    std::string joinNewDim; // from ncml dimName
    std::vector<std::string> joinCoordValues;
    std::set<std::string> joinVars;

    //! accumulated length of unlimited dimension for readers_
    std::vector<size_t> readerUdimPos_;

    //! varName -> readerId, for "union" variables
    std::map<std::string, size_t> unionReaders_;
};

} // namespace MetNoFimex

#endif /* FIMEX_AGGREGATIONREADER_H_ */
