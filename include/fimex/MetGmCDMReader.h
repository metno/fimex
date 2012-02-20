#ifndef METGM_CDM_READER_H
#define METGM_CDM_READER_H
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

#ifndef MIFI_IO_READER_SUPPRESS_DEPRECATED
#warning \
  This header-file is deprecated and \
  may be removed without further notice at a future date. Please use a \
  non-deprecated interface with equivalent functionality instead, i.e. \
  instead of \
    *CDMReader(file,config) \
  use \
    CDMFileReaderFactory::create(MIFI_FILETYPE_*,file,config)
#endif


// fimex
#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"


// boost
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    /* forward declarations */
    class MetGmCDMReaderImpl;

    class MetGmCDMReader : public CDMReader
    {
    public:
        MetGmCDMReader(const std::string& metgmsource, const XMLInput& configXML);
        ~MetGmCDMReader();

        boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
        boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

    private:
        boost::shared_ptr<MetGmCDMReaderImpl> d_ptr;
    };

} // end namespace

#endif // METGM_CDM_READER_H

