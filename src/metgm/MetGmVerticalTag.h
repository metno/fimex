/*
 * Fimex
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
 */

/**
  * Used as private/implementation class
  */

#ifndef METGM_VERTICALTAG_H
#define METGM_VERTICALTAG_H

#include "fimex/CDMReaderDecl.h"

// boost
#include "fimex/DataDecl.h"
#include <boost/shared_array.hpp>
#include <memory>

namespace MetNoFimex {

    class CDMVariable;
    class MetGmFileHandlePtr;
    class MetGmHandlePtr;
    class MetGmGroup3Ptr;
    class MetGmVersion;

    class MetGmVerticalTag {
    public:
        static std::shared_ptr<MetGmVerticalTag> createMetGmVerticalTagForWriting(const CDMReader_p pCdmReader, const CDMVariable* pVar);

        static std::shared_ptr<MetGmVerticalTag> createMetGmVerticalTagForReading(std::shared_ptr<MetGmGroup3Ptr> pGp3, std::shared_ptr<MetGmVerticalTag> vTag);

        inline unsigned int                nz()       { return nz_; }
        inline unsigned int                pr()       { return pr_;}
        inline unsigned int                pz()       { return pz_; }
        inline boost::shared_array<float>& points()   { return points_; }

    protected:

        inline MetGmVerticalTag() : nz_(0), pr_(0), pz_(0) { }

        bool hasNegativePoints();
        void extractVerticalPoints(const DataPtr& data);

        unsigned int                nz_;
        unsigned int                pr_;
        unsigned int                pz_;
        boost::shared_array<float>  points_;
    };

} // eons

#endif // METGM_VERTICALTAG_H
