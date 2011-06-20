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
  * Used as internal class
  */

#ifndef METGM_VERSION_H
#define METGM_VERSION_H

#include "metgm.h"

#include <string>

namespace MetNoFimex {

    class MetGmVersion {
    public:

        explicit MetGmVersion(mgm_version version) : version_(version) { }

        inline std::string getAsString()
        {
            if(version_ == MGM_Edition1)
                return std::string("STANAG 6022 Edition 1");
            else if(version_ == MGM_Edition2)
                return std::string("STANAG 6022 Edition 2");
            else
                return std::string("Unknown STANAG 6022 Edition");
        }

        inline bool operator == (const MetGmVersion &rh) const
        {
            return version_ == rh.version_;
        }

        inline bool operator == (const mgm_version rh_version) const
        {
            return version_ == rh_version;
        }

        inline operator const  mgm_version ()
        {
            return version_;
        }


    private:
        mgm_version version_;
    };
}


#endif // METGM_VERSION_H
