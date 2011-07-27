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

#ifndef METGM_GROUP2PTR_H
#define METGM_GROUP2PTR_H

// boost
//
#include <boost/shared_ptr.hpp>

// standard
//
#include <set>

namespace MetNoFimex {

    class MetGmFileHandlePtr;
    class MetGmHandlePtr;
    class MetGmVersion;

    struct Group2Entry {
        int pid_;
        int repeated_;
        int hd_;
    };

    struct cmp_by_pid : public std::binary_function<Group2Entry, Group2Entry, bool>
    {
       bool operator()(Group2Entry const & lhs, Group2Entry const & rhs) {
          return lhs.pid_ < rhs.pid_;
       }
    };


    class MetGmGroup2Ptr {
    public:
        static boost::shared_ptr<MetGmGroup2Ptr> createMetGmGroup2PtrForReading(boost::shared_ptr<MetGmHandlePtr>& pMgmHandle);

        int totalnp()  { return totalNumberOfParameters_; }
        int ndp() { return numberOfDistinctParameters_; }
    private:

        MetGmGroup2Ptr() : totalNumberOfParameters_(-1), numberOfDistinctParameters_(-1) {}

        boost::shared_ptr<MetGmHandlePtr> pHandle_;
        int totalNumberOfParameters_;
        int numberOfDistinctParameters_;

        std::set<Group2Entry, cmp_by_pid> entries;
    };
}

#endif // METGM_GROUP1PTR_H
