/*
 wdb

 Copyright (C) 2007-2019 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: wdb@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#ifndef FELTFILE_H_
#define FELTFILE_H_

#include "FeltConstants.h"
#include "FeltTypes.h"

#include "fimex/TimeUtils.h"

#include <boost/shared_array.hpp>

#include <iosfwd>
#include <memory>
#include <vector>

namespace felt
{

class FeltLogger
{
public:
    virtual ~FeltLogger();
    virtual void log(const std::string& message) = 0;
};

class FeltFile
{
public:
    explicit FeltFile(const std::string& file);
    FeltFile(const FeltFile&) = delete;
    FeltFile& operator=(const FeltFile&) = delete;

    ~FeltFile();

    typedef size_t size_type;
    size_type size() const;
    bool empty() const { return size() == 0; }

    const std::string& fileName() const { return fileName_; }

    std::string information() const;

    MetNoFimex::FimexTime lastUpdateTime() const;
    MetNoFimex::FimexTime referenceTime() const;
    MetNoFimex::FimexTime firstTime() const;
    MetNoFimex::FimexTime lastTime() const;

    typedef std::shared_ptr<FeltField> FeltFieldPtr;

    typedef std::vector<FeltFieldPtr>::const_iterator iterator;

    iterator begin();
    iterator end();

    typedef iterator const_iterator;
    const_iterator begin() const;
    const_iterator end() const;

    /// throws std::out_of_range if idx is too large.
    const FeltField & at(size_t idx) const;

    // simple log facility
    static void log(const std::string& msg);
    static void setLogStream(std::unique_ptr<FeltLogger>&& l);
    static bool isLogging();

private:

    /// Is the "not ready yet" flag set to false?
    bool complete() const;

    typedef boost::shared_array<word> Block;

    Block getBlock_(size_type blockNo) const;

    /**
     * Read data from file
     *
     * @param out storage for returned data
     * @param fromWord index of the word to be read, as a number of words (not bytes)
     * @param noOfWords how much to read
     */
    void get_(std::vector<word> & out, size_type fromWord, size_type noOfWords) const;

    const std::string fileName_;

    /**
     * Should we swap the two bytes that make up a word before trying to
     * interpret them?
     */
    bool changeEndianness_;

    Block block1_;

    typedef std::vector<FeltFieldPtr> Fields;
    mutable Fields fields_;

    std::unique_ptr<std::istream> feltFile_;

    friend class FeltField;
};

}
#endif /*FELTFILE_H_*/
