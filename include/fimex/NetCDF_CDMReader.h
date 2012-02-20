/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#ifndef NETCDF_CDMREADER_H_
#define NETCDF_CDMREADER_H_

#ifndef MIFI_IO_READER_SUPPRESS_DEPRECATED
#warning \
  This header-file is deprecated and \
  may be removed without further notice at a future date. Please use a \
  non-deprecated interface with equivalent functionality instead, i.e. \
  instead of \
    *CDMReader(file,config) \
  use \
    CDMFileReaderFactory::create(MIFI_FILETYPE_*,file,config)
#endif

#include "fimex/CDMReader.h"

namespace MetNoFimex
{
// forward decl
class Nc;

/**
 * @headerfile "fimex/NetCDF_CDMReader.h"
 */


class NetCDF_CDMReader : public MetNoFimex::CDMReader
{
    const std::auto_ptr<Nc> ncFile;
public:
    NetCDF_CDMReader(const std::string& fileName);
    virtual ~NetCDF_CDMReader();
    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
    virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);
private:
    void addAttribute(const std::string& varName, int varid, const std::string& attName);
};

}

#endif /*NETCDF_CDMREADER_H_*/
