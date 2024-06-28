/*
  Fimex, src/GribIoFactory.h

  Copyright (C) 2019-2024 met.no

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

#ifndef FIMEX_GRIBIOFACTORY_H
#define FIMEX_GRIBIOFACTORY_H

#include "fimex/IoFactory.h"

namespace MetNoFimex {

class GribIoFactory : public IoFactory
{
public:
    size_t matchMagicSize() override;
    int matchMagic(const char* magic, size_t count) override;

    int matchFileTypeName(const std::string& type) override;

    CDMReader_p createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                             const std::vector<std::string>& args) override;
    void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config) override;
};

} // namespace MetNoFimex

#endif // FIMEX_GRIBIOFACTORY_H
