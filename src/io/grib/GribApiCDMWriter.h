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

#ifndef GRIBAPICDMWRITER_H_
#define GRIBAPICDMWRITER_H_

#include "fimex/CDMWriter.h"

// pre-declaration of parameters
struct grib_handle;
namespace MetNoFimex {
class TimeUnit;

class GribApiCDMWriter : public CDMWriter
{

public:
    GribApiCDMWriter(CDMReader_p cdmReader, const std::string& outputFile, const int gribVersion, const std::string& configFile);
     ~GribApiCDMWriter();

private:
};

} // namespace MetNoFimex

#endif /*GRIBAPICDMWRITER_H_*/
