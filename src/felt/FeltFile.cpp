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

#include "felt/FeltFile.h"
#include "felt/FeltTypeConversion.h"
#include "felt/FeltField.h"

#include <algorithm>
#include <fstream>
#include <mutex>
#include <sstream>

using namespace std;

namespace felt
{

FeltFile::FeltFile(const std::string& file)
    : fileName_(file)
    , changeEndianness_(false)
    , feltFile_(new std::ifstream(file, std::ios::binary))
{
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
        std::shared_ptr<FeltField> f(new FeltField(*this, i++));
        if ( f->valid() )
            fields_.push_back(f);
    }
}

FeltFile::~FeltFile()
{
}

// simple logging
FeltLogger::~FeltLogger() {}

static std::mutex logger_mutex;
static std::unique_ptr<FeltLogger> logStream;

void FeltFile::setLogStream(std::unique_ptr<FeltLogger>&& l)
{
    std::lock_guard<std::mutex> lock(logger_mutex);
    logStream = std::move(l);
}

bool FeltFile::isLogging()
{
    return static_cast<bool>(logStream);
}

void FeltFile::log(const std::string& message)
{
    std::lock_guard<std::mutex> lock(logger_mutex);
    if (logStream)
        logStream->log(message);
}

FeltFile::size_type FeltFile::size() const
{
    return fields_.size();
}


std::string FeltFile::information() const
{
    std::ostringstream cont;
    cont << "File type\t\t" << block1_[0] << "\n";
    //    cont << "Time a\t\t\t" << lastUpdateTime() << "\n";
    //    cont << "Time b\t\t\t" << referenceTime() << "\n";
    cont << "M\t\t\t" << block1_[7] << "\n";
    cont << "N\t\t\t" << block1_[8] << "\n";
    cont << "K\t\t\t" << block1_[9] << "\n";
    cont << "L\t\t\t" << block1_[10] << "\n";
    cont << "MX\t\t\t" << block1_[11] << "\n";
    cont << "Last word last block\t" << block1_[12] << "\n";
    cont << "storage type\t\t" << block1_[13] << "\n";
    cont << "Update status\t\t" << block1_[14] << "\n";
    //    cont << "Arch time a\t\t" << firstTime() << "\n";
    //    cont << "Arch time b\t\t" << lastTime() << "\n";
    cont << "Termins\t\t\t" << block1_[25] << "\n";
    cont << "Indexes/term\t\t" << block1_[26] << "\n";
    cont << "Producer\t\t" << block1_[27] << "\n";
    cont << "Time unit\t\t" << block1_[28] << "\n";
    cont << "Time resolution\t\t" << block1_[29] << "\n";

    return cont.str();
}

MetNoFimex::FimexTime FeltFile::lastUpdateTime() const
{
    return parseTimeNoThrow(block1_.get() +1);
}

MetNoFimex::FimexTime FeltFile::referenceTime() const
{
    return parseTimeNoThrow(block1_.get() +4);
}

MetNoFimex::FimexTime FeltFile::firstTime() const
{
    return parseTimeNoThrow(block1_.get() +19);
}

MetNoFimex::FimexTime FeltFile::lastTime() const
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

    long long pos = static_cast<long long>(blockNo) * blockWords * sizeof(word);
    feltFile_->seekg(pos, ios_base::beg);
    feltFile_->read((char*) ret.get(), blockWords * sizeof(word));
    if ( changeEndianness_ )
        for_each(ret.get(), ret.get() + blockWords, swapByteOrder);

    return ret;

}

void FeltFile::get_(std::vector<word> & out, size_type fromWord, size_type noOfWords) const
{
    out.resize(noOfWords);
    // this will allow up to 8.4GB (size_t = 4.2G * word=2)
    unsigned long long pos = static_cast<unsigned long long>(fromWord) * sizeof(word);
    feltFile_->seekg(pos, ios_base::beg);
    feltFile_->read((char*) & out[0], noOfWords * sizeof(word));
    if ( changeEndianness_ )
        for_each(out.begin(), out.end(), swapByteOrder);
}


const FeltField & FeltFile::at(size_t idx) const
{
    return * fields_.at(idx);
}

}
