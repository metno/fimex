/*
 * Fimex, AggregationReader.h
 *
 * (C) Copyright 2013-2019, met.no
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

#ifndef FIMEX_AGGREGATIONREADER_H_
#define FIMEX_AGGREGATIONREADER_H_

#include "fimex/CDMReader.h"

#include <map>
#include <memory>
#include <string>

namespace MetNoFimex {

/**
 * A CDM-Reader reading data from several-files.
 */
class AggregationReader : public CDMReader
{
public:
    AggregationReader(const std::string& aggregationType);
    ~AggregationReader();

    void addReader(CDMReader_p reader, const std::string& id = std::string());
    void initAggregation();

    using CDMReader::getDataSlice;
    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

protected:
    // main data-reader
    CDMReader_p gDataReader_;

    std::string aggType_;

    //! the readers_ as ordered in the aggregation
    std::vector<std::pair<std::string, CDMReader_p>> readers_;

    //! udim -> readerId,udimPos, joinExiting mapping of this udim to the readers_(i) udim (j)
    std::vector<std::pair<std::size_t, std::size_t>> readerUdimPos_;

    //! varName -> readerId, union mapping of varName to readers_(i)
    std::map<std::string, std::size_t> varReader_;
};

} // namespace MetNoFimex

#endif /* FIMEX_AGGREGATIONREADER_H_ */
