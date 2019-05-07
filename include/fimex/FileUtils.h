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

std::string getExtension(const std::string& fileName);

} // namespace MetNoFimex

#endif /*FIMEX_FILEUTILS_H_*/
