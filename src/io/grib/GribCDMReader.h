/*
 * Fimex, GribCDMReader.h
 *
 * (C) Copyright 2009-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 *
 *  Created on: Sep 9, 2009
 *      Author: Heiko Klein
 */

#ifndef GRIBCDMREADER_H_
#define GRIBCDMREADER_H_

#ifndef MIFI_IO_READER_SUPPRESS_DEPRECATED
#warning This header-file is deprecated and \
  may be removed without further notice at a future date. Please use a \
  non-deprecated interface with equivalent functionality instead, i.e. \
  instead of \
    *CDMReader(file,config) \
  use \
    CDMFileReaderFactory::create(MIFI_FILETYPE_*,file,config)
#endif

#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"

#include <memory>

namespace MetNoFimex {

class GribCDMReader : public CDMReader
{
public:
    GribCDMReader(const std::vector<std::string>& fileNames, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members);
    GribCDMReader(const std::string& grbmlFileName, const XMLInput& configXML, const std::vector<std::pair<std::string, std::string>>& members);
    GribCDMReader(const std::string& fiinFileName, const XMLInput& configXML);
    ~GribCDMReader();

    DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos) override;
    DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb) override;

private:
    struct Impl;
    std::unique_ptr<Impl> p_;
};

} // namespace MetNoFimex

#endif /* GRIBCDMREADER_H_ */
