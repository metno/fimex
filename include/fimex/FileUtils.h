/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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

#ifndef FIMEX_FILEUTILS_H_
#define FIMEX_FILEUTILS_H_

#include <regex>
#include <string>
#include <vector>

namespace MetNoFimex {

/**
 * Scan the filesystem for files matching the regexp. Can be used similar to 'glob'
 * or 'find' commands. The files will be sorted alphabetically.
 *
 * @param files output list of files
 * @param dir the input directory
 * @param depth the maximum number of directories to search (-1 is indefinite)
 * @param regexp the regular expression to match the file or complete path
 * @param matchFileOnly if true, the regexp will match the file-part only, if false,
 *        the complete path (behind dir) will be matched.
 */
void scanFiles(std::vector<std::string>& files, const std::string& dir, int depth, const std::regex& regexp, bool matchFileOnly);

/**
 * Similar to scanFiles, but uses glob instead, with * matches everything within a file or directory-name, ? matches exactly one character (not /),
 * and ** match everything even across multiple directories.
 *
 * @param files output list of files
 * @param glob the file/directory glob to match, glob-wildcards are *, ** and ?
 */
void globFiles(std::vector<std::string>& files, const std::string& glob);

/// Expand a @p fileName, adding to @p files:
/// - if it starts with 'glob:', use the remainder as glob pattern
/// - if it starts with 'many:', use the first char after as separator and split
///   the remainder on this separator (e.g. main:|a.grb|b.grb|c.grb)
/// - if it starts with 'list:', use the remainder as path to a list of files,
///   one per line
/// - otherweise just use fileName as is
void  expand_files(std::vector<std::string>& files, const std::string& fileName);

std::string getExtension(const std::string& fileName);

std::string extractFilename(const std::string& path);
std::string removeFilename(const std::string& path);
std::string replaceFilename(const std::string& path, const std::string& filename);
std::string joinFilename(const std::string& path, const std::string& filename);

/**
 * Replace the file extension in @p path with @p newExtension.
 * The extension is the suffix after the last '.' in the filename part of the
 * path (i.e. dots in directory components are ignored).  If the filename has
 * no extension the new extension is appended.
 *
 * @param path         File path (may include directory components).
 * @param newExtension New extension without leading dot (e.g. "ncfp").
 * @return             Path with the extension replaced.
 *
 * Examples:
 *   replaceExtension("data.nc",         "ncfp") == "data.ncfp"
 *   replaceExtension("/path/data.nc",   "ncfp") == "/path/data.ncfp"
 *   replaceExtension("data.tar.gz",     "ncfp") == "data.tar.ncfp"
 *   replaceExtension("/p.q/data",       "ncfp") == "/p.q/data.ncfp"
 *   replaceExtension("data",            "ncfp") == "data.ncfp"
 */
std::string replaceExtension(const std::string& path, const std::string& newExtension);

} // namespace MetNoFimex

#endif /*FIMEX_FILEUTILS_H_*/
