/*
 * Fimex
 *
 * (C) Copyright 2026, met.no
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
 */

#ifndef FIMEX_PROTOBUFUTILS_H_
#define FIMEX_PROTOBUFUTILS_H_

#include <string>

namespace MetNoFimex {

class XMLInput;

/**
 * Determine the root path used to resolve source filenames stored in a
 * protobuf index.
 *
 * The logic is shared by all protobuf index readers (GRIB, NetCDF, …):
 *  1. If the XML config contains a @c /cdm_fimex_index_reader_config/root_path
 *     element, its text content is returned.
 *  2. Otherwise the directory part of @p indexFileName is returned, so that
 *     relative filenames in the index are resolved next to the index file.
 *
 * @param indexFileName  Path to the protobuf index file.
 * @param configXML      Reader configuration (may be empty / null document).
 * @return               Root path (always non-empty unless @p indexFileName
 *                       has no directory component).
 */
std::string protobufIndexRootPath(const std::string& indexFileName, const XMLInput& configXML);

} // namespace MetNoFimex

#endif /* FIMEX_PROTOBUFUTILS_H_ */
