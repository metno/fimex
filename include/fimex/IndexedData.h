/*
 * Fimex, IndexedData.h
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Jul 18, 2012
 *      Author: Heiko Klein
 */

#ifndef INDEXEDDATA_H_
#define INDEXEDDATA_H_

#include <vector>
#include "fimex/DataDecl.h"
#include "fimex/DataIndex.h"

namespace MetNoFimex
{

// forward decl
class IndexedDataImpl;

/**
 * @headerfile fimex/IndexedData.h
 */
class IndexedData
{
public:
    IndexedData();
    /**
     * Create a indexed data. See setDims() for more information.
     * @param data
     * @param dimSizes
     */
    explicit IndexedData(DataPtr data, std::vector<std::size_t> dimSizes);
    ~IndexedData();
    /**
     *  @brief set the dimension sizes of data
     *
     *  The dimension sizes should be set with the fastest moving index first, e.g. x,y,z.
     *  It handles gracefully the case when the data is not defined.
     *
     *  @throw runtime_error if mult(dims) != size()
     */
    void setDims(std::vector<std::size_t> dims);
    /**
     *  @brief get the index belonging to the data
     *
     *  @return a Index belonging to the data
     */
    const DataIndex& idx() const;
    /**
     * @return the internal data
     */
    DataPtr getDataPtr() const;
    /**
     * simple data accessor, should be used with MetNoFimex::Index
     *
     * @code
     * IndexedData idata(data, dims);
     * double val = id.getDouble(id.getIdx().getPos(3, 5, 7));
     * @endcode
     *
     * @param pos data position, should be < size (not assured)
     */
    double getDouble(size_t pos) const;
    /// shortcut for 2d
    double getDouble(size_t a, size_t b) const {return getDouble(idx().getPos(a,b));}
    /// shortcut for 3d
    double getDouble(size_t a, size_t b, size_t c) const {return getDouble(idx().getPos(a,b,c));}
    /// shortcut for 4d
    double getDouble(size_t a, size_t b, size_t c, size_t d) const {return getDouble(idx().getPos(a,b,c,d));}
    /**
     * simple data accessor, should be used with MetNoFimex::Index
     * @param pos data position, should be < size (not assured)
     */
    long long getLongLong(size_t pos) const;
    /// shortcut for 2d
    double getLongLong(size_t a, size_t b) const {return getLongLong(idx().getPos(a,b));}
    /// shortcut for 3d
    double getLongLong(size_t a, size_t b, size_t c) const {return getLongLong(idx().getPos(a,b,c));}
    /// shortcut for 4d
    double getLongLong(size_t a, size_t b, size_t c, size_t d) const {return getLongLong(idx().getPos(a,b,c,d));}
private:
    boost::shared_ptr<IndexedDataImpl> p_;
    void init(DataPtr data, std::vector<std::size_t> dimSizes);

};

} /* namespace MetNoFimex */
#endif /* INDEXEDDATA_H_ */
