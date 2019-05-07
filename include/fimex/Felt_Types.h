/*
 * Fimex, Felt_Types.h
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
 *  Created on: Jul 29, 2009
 *      Author: Heiko Klein
 */

#ifndef FELT_TYPES_H_
#define FELT_TYPES_H_

#include <functional>
#include <utility>

namespace MetNoFelt {
/**
 * a pair with two level values
 */
typedef std::pair<short, short> LevelPair;
/**
 * comparison operator for pair<short, short> used for LevelPairs
 *
 */
struct LevelPairLess : public std::binary_function<const LevelPair, const LevelPair, bool>
{
    bool operator()(const LevelPair& p1, const LevelPair& p2) const {
        if (p1.first == p2.first) return p1.second < p2.second;
        return p1.first < p2.first;
    }
};

} // end namespace MetNoFelt

#endif /* FELT_TYPES_H_ */
