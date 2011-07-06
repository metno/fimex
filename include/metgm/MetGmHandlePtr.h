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

#ifndef METGM_HANDLEPTR_H
#define METGM_HANDLEPTR_H

// METGM C library
//
#include "metgm.h"

// boost
//
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    class MetGmHandlePtr {
    public:

        static boost::shared_ptr<MetGmHandlePtr> createMetGmHandle() {
            return boost::shared_ptr<MetGmHandlePtr>(new MetGmHandlePtr);
        }

        ~MetGmHandlePtr() { mgm_free_handle(handle_); }

        inline int reset() { return mgm_reset_handle(handle_); }

        inline operator mgm_handle* () { return handle_; }

    private:

        explicit MetGmHandlePtr() { handle_ = mgm_new_handle(); }

        mgm_handle* handle_;
    };
}

#endif // METGM_HANDLE_H
