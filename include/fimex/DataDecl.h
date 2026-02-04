/*
 * Fimex, DataDecl.h
 *
 * (C) Copyright 2012-2026, met.no
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
 *
 *  Created on: Jul 18, 2012
 *      Author: Heiko Klein
 */

#ifndef DATADECL_H_
#define DATADECL_H_

#include <memory>

namespace MetNoFimex
{
    /**
     * @headerfile fimex/DataDecl.h
     */

    // forward decl
    class Data;
    /**
     * Pointer to Data, this is the preferred way to access Data
     */
    typedef std::shared_ptr<Data> DataPtr;
}


#endif /* DATADECL_H_ */
