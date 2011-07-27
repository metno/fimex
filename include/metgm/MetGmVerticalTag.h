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

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

namespace MetNoFimex {

    class Data;
    class CDMReader;
    class CDMVariable;
    class MetGmFileHandlePtr;
    class MetGmHandlePtr;
    class MetGmGroup3Ptr;
    class MetGmVersion;

    class MetGmVerticalTag {
    public:

        static boost::shared_ptr<MetGmVerticalTag> createMetGmVerticalTag(boost::shared_ptr<CDMReader>& pCdmReader, const CDMVariable* pVar);

        static boost::shared_ptr<MetGmVerticalTag> createMetGmVerticalTag(boost::shared_ptr<MetGmGroup3Ptr>& pGp3);

        inline unsigned int               nz()       { return nz_; }
        inline unsigned int               pr()       { return pr_;}
        inline unsigned int               pz()       { return pz_; }
        inline boost::shared_array<float> points()   { return points_; }

        void dump();

    protected:

        inline MetGmVerticalTag() : nz_(0), pr_(0), pz_(0) { }

        bool hasNegativePoints();
        void extractVerticalPoints(const boost::shared_ptr<Data>& data);

        unsigned int                nz_;
        unsigned int                pr_;
        unsigned int                pz_;
        boost::shared_array<float>  points_;
    };

} // eons

#endif // METGM_VERTICALTAG_H
