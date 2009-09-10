/*
 * Fimex, binaryConstants.h
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
 *  Created on: Sep 10, 2009
 *      Author: Heiko Klein
 */

#ifndef BINARYCONSTANTS_H_
#define BINARYCONSTANTS_H_


/**
 *  use binary<01001001>::value as constant, works with up to 10 bits
 *  use binary<01001001ULL>::value as constant, works with up to 22 bits
 *  @warning always start with leading 0, since all values have to be octals!!!
 */

template< unsigned long long N >
struct binary
{
    enum { value = (N % 8) + (binary<N/8>::value << 1) } ;
};

template<>
struct binary< 0 >
{
    enum { value = 0 } ;
};


#endif /* BINARYCONSTANTS_H_ */
