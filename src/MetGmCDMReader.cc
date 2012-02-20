/*
 * Fimex, MetGmCDMReader.h
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
 *
 */
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/MetGmCDMReader.h"
#undef MIFI_IO_READER_SUPPRESS_DEPRECATED
#include <fimex/CDM.h>
#include "MutexLock.h"

// private implementation details
#include "./metgm/MetGmCDMReaderImpl.h"
#include "./metgm/MetGmCDMReaderSlicedImpl.h"

namespace MetNoFimex {

    static MutexType mutex;

    MetGmCDMReader::MetGmCDMReader(const std::string& metgmsource, const XMLInput& configXML)
        : CDMReader()
    {
        try {
            d_ptr = boost::shared_ptr<MetGmCDMReaderSlicedImpl>(new MetGmCDMReaderSlicedImpl(metgmsource, configXML, cdm_));
        } catch (std::runtime_error& exp) {
            throw CDMException(std::string("METGM_CDMReader error: ") + exp.what());
        }
    }

    MetGmCDMReader::~MetGmCDMReader() { }

    boost::shared_ptr<Data> MetGmCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
        ScopedCritical lock(mutex);
        return d_ptr->getDataSlice(varName, unLimDimPos);
    }

    boost::shared_ptr<Data> MetGmCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
        ScopedCritical lock(mutex);
        return d_ptr->getDataSlice(varName, sb);
    }

}

