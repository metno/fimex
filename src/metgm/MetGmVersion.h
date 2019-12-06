/*
 * Fimex
 *
 * (C) Copyright 2011-2019, met.no
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

#ifndef METGM_VERSION_H
#define METGM_VERSION_H

// METGM C library
//
#include "metgm.h"

// standard
//
#include <memory>
#include <string>

namespace MetNoFimex {
class XMLDoc;

class MetGmVersion
{
public:
    static std::shared_ptr<MetGmVersion> createMetGmVersion(mgm_version version);

    static std::shared_ptr<MetGmVersion> createMetGmVersion(const std::unique_ptr<XMLDoc>& doc);

    std::string getAsString() const;

    inline bool operator==(const MetGmVersion& rh) const { return version_ == rh.version_; }

    inline bool operator==(const mgm_version rh_version) const { return version_ == rh_version; }

    inline operator mgm_version() const { return version_; }

    inline mgm_version as_mgm_version() const { return version_; }

private:
    mgm_version version_;

    explicit MetGmVersion(mgm_version version)
        : version_(version)
    {
    }
};

} // namespace MetNoFimex

#endif // METGM_VERSION_H
