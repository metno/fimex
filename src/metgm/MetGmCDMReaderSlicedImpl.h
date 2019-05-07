/*
  Fimex, src/metgm/MetGmCDMReaderSlicedImpl.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://wiki.met.no/fimex/start

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef METGM_CDM_READER_SLICED_IMPL_H
#define METGM_CDM_READER_SLICED_IMPL_H

// implementation
//
#include "MetGmCDMReaderImpl.h"
#include "fimex/XMLInput.h"

// boost
//
#include <memory>

// standard
//
#include <string>

namespace MetNoFimex {

    class MetGmCDMReaderSlicedImpl : public MetGmCDMReaderImpl
    {
    public:
        explicit MetGmCDMReaderSlicedImpl(const std::string& metgmsource, const XMLInput& configXML, const std::shared_ptr<CDM>& cdm);
        virtual ~MetGmCDMReaderSlicedImpl();

        virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);
#if 0
        virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);
#endif

    protected:
        virtual void parseMgmFile(const std::string& mgmFileName);
        virtual void addVariables();
    };

} // end namespace

#endif // METGM_CDM_READER_SLICED_IMPL_H

