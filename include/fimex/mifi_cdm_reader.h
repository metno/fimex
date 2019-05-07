/*
 * Fimex, mifi_cdm_reader.h
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Oct 28, 2009
 *      Author: Heiko Klein
 */

#ifndef MIFI_CDM_READER_H_
#define MIFI_CDM_READER_H_

#include "fimex/CDMReaderDecl.h"
#include "fimex/SliceBuilder.h"
#include "fimex/coordSys/CoordinateSystem.h"

/**
  * @headerfile fimex/mifi_cdm_reader.h
  */
/**
 * wrapper class for CDMReader_p, mainly for usage by
 * C/C++ wrapper
 */
class mifi_cdm_reader {
public:
    mifi_cdm_reader(MetNoFimex::CDMReader_p reader)
        : reader_(reader)
    {
        csVec_ = MetNoFimex::listCoordinateSystems(reader_);
    }
    MetNoFimex::CDMReader_p reader_;
    MetNoFimex::CoordinateSystem_cp_v csVec_;
};

/**
 * wrapper class for a SliceBuilder, mainly for usage by
 * C/C++ wrapper
 */
class mifi_slicebuilder {
public:
    mifi_slicebuilder(boost::shared_ptr<MetNoFimex::SliceBuilder> sb, MetNoFimex::CoordinateSystem_cp cs)
        : sb_(sb)
        , cs_(cs)
    {
    }
    mifi_slicebuilder(boost::shared_ptr<MetNoFimex::SliceBuilder> sb) : sb_(sb) {}
    boost::shared_ptr<MetNoFimex::SliceBuilder> sb_;
    MetNoFimex::CoordinateSystem_cp cs_;
};


#endif /* MIFI_CDM_READER_H_ */
