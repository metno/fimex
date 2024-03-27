/*
 * Fimex, NcmlAggregationReader.h
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
 *
 *  Created on: Apr 17, 2013
 *      Author: heikok
 */

#ifndef NCMLAGGREGATIONREADER_H_
#define NCMLAGGREGATIONREADER_H_

#include "fimex/CDMReader.h"

namespace MetNoFimex {

class XMLInput;

/**
 * A CDM-Reader reading data from several-files defined by
 * ncml-aggregations.
 *
 * This reader should usually be called from the NcmlCDMReader.
 */
class NcmlAggregationReader : public CDMReader
{
public:
    NcmlAggregationReader(const XMLInput& ncml);
    ~NcmlAggregationReader();

    using CDMReader::getDataSlice;
    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

private:
    CDMReader_p reader_;
};

} // namespace MetNoFimex

#endif /* NCMLAGGREGATIONREADER_H_ */
