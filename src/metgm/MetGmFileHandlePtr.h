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

#ifndef METGM_FILEHANDLEPTR_H
#define METGM_FILEHANDLEPTR_H

// METGM C lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDMException.h"

// boost
//
#include <boost/shared_ptr.hpp>

// standard
//
#include <cstdio>
#include <cerrno>
#include <cstring>
#include <string>
namespace MetNoFimex {

    class MetGmFileHandlePtr {
    public:

        enum IODirection {
            WRITE,
            READ
        };

        static boost::shared_ptr<MetGmFileHandlePtr> createMetGmFileHandlePtrForReading(const std::string& name) {
            boost::shared_ptr<MetGmFileHandlePtr> handle = boost::shared_ptr<MetGmFileHandlePtr>(new MetGmFileHandlePtr(name, READ));
            return handle;
        }

        static boost::shared_ptr<MetGmFileHandlePtr> createMetGmFileHandlePtrForWriting(const std::string& name) {
            boost::shared_ptr<MetGmFileHandlePtr> handle = boost::shared_ptr<MetGmFileHandlePtr>(new MetGmFileHandlePtr(name, WRITE));
            return handle;
        }

        ~MetGmFileHandlePtr()
        {
            if(handle_)
                fclose(handle_);
        }

        inline void reset()
        {
            assert(handle_);
            if(ftell(handle_) == -1L) {
                throw CDMException("ftell failed!");
            } else {
                if(fsetpos(handle_, &startPos_) != 0) {
                    throw CDMException("fsetpos failed!");
                }
            }
        }

        inline operator FILE* () { return handle_; }
        inline FILE* handle() { return handle_; }
        inline std::string fileName() { return fileName_; }

    private:
        FILE* handle_;
        fpos_t startPos_;
        std::string fileName_;

        explicit MetGmFileHandlePtr(const std::string name, IODirection direction = READ) : handle_(0), fileName_(name)
        {
            if(!fileName_.empty()) {
                switch(direction) {
                    case READ:
                        handle_ = fopen(fileName_.c_str() , "rb");
                        if(handle_ == 0)
                            throw CDMException("can't read file "+fileName_+": "+strerror(errno));
                        break;
                    case WRITE:
                        handle_ = fopen(fileName_.c_str() , "wb");
                        if(handle_ == 0)
                            throw CDMException("can't write file "+fileName_+": "+strerror(errno));
                        break;
                    default:
                        throw CDMException("invalid IODirection");
                }


                fgetpos(handle_, &startPos_);
            }
        }
    };
}

#endif // METGM_FILEHANDLE_H
