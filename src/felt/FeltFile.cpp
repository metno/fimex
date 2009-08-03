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

#include "felt/FeltFile.h"
#include "felt/FeltTypeConversion.h"
#include "felt/FeltField.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <stdexcept>
#include <iostream>
#include <iterator>
#include <sstream>

using namespace std;
using namespace boost::filesystem;


namespace felt
{

FeltFile::FeltFile(const path & file)
	: fileName_(file), changeEndianness_(false)
{
	if ( ! exists(file) )
		throw runtime_error("Cannot find file " + file.native_file_string() );
	if ( is_directory(file) )
		throw runtime_error(file.native_directory_string() + " is a directory, not a file");

	string fileName = file.native_file_string();
	feltFile_ = new boost::filesystem::ifstream(file);

	word head;
	feltFile_->read(reinterpret_cast<char*>(& head), sizeof(word));
	if ( head < 997 or 999 < head )
		changeEndianness_ = true;

	// Block 1
	block1_ = getBlock_(0);

	if ( not complete() )
		throw std::runtime_error("File update is not complete yet");

	const size_t fieldCount = block1_[9];
	fields_.reserve(fieldCount);
	int i = 0;
	while ( fields_.size() < fieldCount )
	{
		boost::shared_ptr<FeltField> f(new FeltField(*this, i ++));
		if ( f->valid() )
			fields_.push_back(f);
	}
}

FeltFile::~FeltFile()
{
	delete feltFile_;
}

// simple logging
std::ostream* logStream = &std::cerr;
bool isLogging_ = false;
void FeltFile::setLogStream(std::ostream& o)
{
    logStream = &o;
}
void FeltFile::setLogging(bool enableLogging)
{
    isLogging_ = enableLogging;
}
bool FeltFile::isLogging()
{
    return isLogging_;
}
void FeltFile::log(const std::string& message)
{
    if (isLogging_ && logStream->good()) {
        *logStream << message;
    }
}


FeltFile::size_type FeltFile::size() const
{
	return fields_.size();
}


std::string FeltFile::information() const
{
	std::ostringstream cont;
	cont << "File type\t\t" << block1_[0] << "\n";
	cont << "Time a\t\t\t" << lastUpdateTime() << "\n";
	cont << "Time b\t\t\t" << referenceTime() << "\n";
	cont << "M\t\t\t" << block1_[7] << "\n";
	cont << "N\t\t\t" << block1_[8] << "\n";
	cont << "K\t\t\t" << block1_[9] << "\n";
	cont << "L\t\t\t" << block1_[10] << "\n";
	cont << "MX\t\t\t" << block1_[11] << "\n";
	cont << "Last word last block\t" << block1_[12] << "\n";
	cont << "storage type\t\t" << block1_[13] << "\n";
	cont << "Update status\t\t" << block1_[14] << "\n";
	cont << "Arch time a\t\t" << firstTime() << "\n";
	cont << "Arch time b\t\t" << lastTime() << "\n";
	cont << "Termins\t\t\t" << block1_[25] << "\n";
	cont << "Indexes/term\t\t" << block1_[26] << "\n";
	cont << "Producer\t\t" << block1_[27] << "\n";
	cont << "Time unit\t\t" << block1_[28] << "\n";
	cont << "Time resolution\t\t" << block1_[29] << "\n";

	return cont.str();
}

boost::posix_time::ptime FeltFile::lastUpdateTime() const
{
	return parseTimeNoThrow(block1_.get() +1);
}

boost::posix_time::ptime FeltFile::referenceTime() const
{
	return parseTimeNoThrow(block1_.get() +4);
}

boost::posix_time::ptime FeltFile::firstTime() const
{
	return parseTimeNoThrow(block1_.get() +19);
}

boost::posix_time::ptime FeltFile::lastTime() const
{
	return parseTimeNoThrow(block1_.get() +22);
}

FeltFile::iterator FeltFile::begin()
{
	return fields_.begin();
}

FeltFile::iterator FeltFile::end()
{
	return fields_.end();
}

FeltFile::const_iterator FeltFile::begin() const
{
	return fields_.begin();
}

FeltFile::const_iterator FeltFile::end() const
{
	return fields_.end();
}

bool FeltFile::complete() const
{
	felt::word updateInProgress = block1_[14];
	// expected value is 0 or 1
	return not updateInProgress;
}

namespace
{
void swapByteOrder(word & w)
{
	char * wBytes = (char*) & w;
	std::swap(wBytes[0], wBytes[1]);
}

}

FeltFile::Block FeltFile::getBlock_(size_type blockNo) const
{
	Block ret(new word[blockWords]);

	feltFile_->seekg(blockNo * blockWords * sizeof(word), ios_base::beg);
	feltFile_->read((char*) ret.get(), blockWords * sizeof(word));
	if ( changeEndianness_ )
		for_each(ret.get(), ret.get() + blockWords, swapByteOrder);

	return ret;

}

void FeltFile::get_(std::vector<word> & out, size_type fromWord, size_type noOfWords) const
{
	out.resize(noOfWords);

	feltFile_->seekg(fromWord * sizeof(word), ios_base::beg);
	feltFile_->read((char*) & out[0], noOfWords * sizeof(word));
	if ( changeEndianness_ )
		for_each(out.begin(), out.end(), swapByteOrder);
}


const FeltField & FeltFile::at(size_t idx) const
{
	return * fields_.at(idx);
}

}
