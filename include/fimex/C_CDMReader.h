/*
 * Fimex, C_CDMReader.h
 *
 * (C) Copyright 2009, met.no
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
 *
 *  Created on: Oct 19, 2009
 *      Author: Heiko Klein
 */

#ifndef C_CDMREADER_H_
#define C_CDMREADER_H_


#include "fimex/CDMReader.h"
#include "fimex/c_fimex.h"
#include <map>

namespace MetNoFimex
{

/**
 * @headerfile fimex/C_CDMReader.h
 */
/**
 * This class should be used by people who want write an implementation of a CDMReader in C. They should set
 * a callback-function to retrieve a variable with the getDataSlice functions.
 *
 */
class C_CDMReader: public CDMReader
{
public:
    C_CDMReader(boost::shared_ptr<CDMReader> dataReader);
    virtual ~C_CDMReader();
    using CDMReader::getDataSlice;
    virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);
    virtual void setDoubleCallbackFunction(const std::string& varName, doubleDatasliceCallbackPtr callback);
private:
    boost::shared_ptr<CDMReader> dataReader_;
    std::map<std::string, doubleDatasliceCallbackPtr> doubleCallbacks_;

};

}

#endif /* C_CDMREADER_H_ */
