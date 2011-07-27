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

#ifndef METGM_GROUP5PTR_H
#define METGM_GROUP5PTR_H

// metgm
//
#include "metgm.h"

// internals
//
#include "MetGmGroup3Ptr.h"
#include "MetGmDimensionsTag.h"

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/CDMVariable.h"
#include "fimex/interpolation.h"

// boost
//
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

namespace MetNoFimex {

    class MetGmGroup5Ptr {
    public:
        static boost::shared_ptr<MetGmGroup5Ptr> createMetGmGroup5PtrForWriting(boost::shared_ptr<CDMReader>& pCdmReader,
                                                                                const CDMVariable* pVariable,
                                                                                const boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                                                                                const float* pFillValue = 0);

        static boost::shared_ptr<MetGmGroup5Ptr> createMetGmGroup5PtrForReading(boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                                                                                boost::shared_ptr<MetGmHDTag>&     hdTag);

        void dumpFimexLayout();
        void dumpMetGmLayout();

        std::vector<float>& data() { return vData_; }

        boost::shared_array<float> dataAsFloat();
    private:

        void changeFillValue();
        void toFimexLayout();
        void toMetGmLayout();

        explicit MetGmGroup5Ptr(const boost::shared_ptr<MetGmGroup3Ptr>& gp3,
                                const boost::shared_ptr<MetGmHDTag>&     hdTag,
                                const std::vector<float>&                vData,
                                const float&                             fillValue = 9999.0f);

        // not owning
        const boost::shared_ptr<MetGmGroup3Ptr>& pGp3_;
        const boost::shared_ptr<MetGmHDTag> hdTag_;

        // owning
        std::vector<float>            vData_;
        float                         fillValue_;
    };


}

#endif // METGM_GROUP5PTR_H
