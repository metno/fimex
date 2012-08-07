/*
 wdb

 Copyright (C) 2007 met.no

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

#define BOOST_FILESYSTEM_VERSION 2

#include "FeltConstants.h"
#include "FeltTypes.h"
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/shared_array.hpp>
#include <boost/noncopyable.hpp>


#include <iterator>
#include <vector>
#include <iosfwd>

namespace felt
{

class FeltFile : boost::noncopyable
{
public:
    explicit FeltFile(const boost::filesystem::path & file);
    ~FeltFile();

    typedef size_t size_type;
    size_type size() const;
    bool empty() const { return size() == 0; }

    const boost::filesystem::path & fileName() const { return fileName_; }

    std::string information() const;

    boost::posix_time::ptime lastUpdateTime() const;
    boost::posix_time::ptime referenceTime() const;
    boost::posix_time::ptime firstTime() const;
    boost::posix_time::ptime lastTime() const;

    typedef boost::shared_ptr<FeltField> FeltFieldPtr;

    typedef std::vector<FeltFieldPtr>::const_iterator iterator;
//	class iterator;
    iterator begin();
    iterator end();

    typedef iterator const_iterator;
    const_iterator begin() const;
    const_iterator end() const;

    /// throws std::out_of_range if idx is too large.
    const FeltField & at(size_t idx) const;



    // simple log facility
    static void log(const std::string& msg);
    static void setLogStream(std::ostream& o);
    static void setLogging(bool enableLogging);
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

    const boost::filesystem::path fileName_;

    /**
     * Should we swap the two bytes that make up a word before trying to
     * interpret them?
     */
    bool changeEndianness_;

    Block block1_;

    typedef std::vector<FeltFieldPtr> Fields;
    mutable Fields fields_;

    mutable std::istream * feltFile_;

    friend class FeltField;
//	friend class iterator;
};



//class FeltFile::iterator : public std::iterator<std::input_iterator_tag, FeltField>
//{
//public:
//	iterator & operator ++ ();
//	iterator   operator ++ (int);
//	const reference operator * () const;
//	const pointer operator -> () const;
//
//	bool operator == ( const iterator & i ) const;
//	bool operator != ( const iterator & i ) const;
//
//private:
//	value_type & data_;
//	const FeltFile * ff_;
//	int index_;
//	iterator(const FeltFile & ff);
//	iterator();
//	friend class FeltFile;
//};


}
#endif /*FELTFILE_H_*/
