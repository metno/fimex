/*
 * Fimex, NcmlAggregationReader.h
 *
 * (C) Copyright 2013, met.no
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
#include "fimex/XMLInput.h"

#include <boost/shared_ptr.hpp>



namespace MetNoFimex
{

/**
 * A CDM-Reader reading data from several-files defined by
 * ncml-aggregations.
 *
 * This reader should usually be called from the NcmlCDMReader.
 *
 * joinNew aggregations are currently not implemented.
 */
class NcmlArregationReader: public MetNoFimex::CDMReader
{
public:
    NcmlArregationReader(const XMLInput& ncml);
    virtual ~NcmlArregationReader();
    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos = 0);
private:
    // main data-reader
    boost::shared_ptr<CDMReader> gDataReader_;
    std::string aggType_;
    // the readers_ as ordered in the aggregation
    std::vector<std::pair<std::string, boost::shared_ptr<CDMReader> > > readers_;
    // udim -> readerId,udimPos, joinExiting mapping of this udim to the readers_(i) udim (j)
    std::vector<std::pair<std::size_t, std::size_t> > readerUdimPos_;
    // varName -> readerId, union mapping of varName to readers_(i)
    std::map<std::string, std::size_t> varReader_;

};

} /* namespace MetNoFimex */
#endif /* NCMLAGGREGATIONREADER_H_ */
