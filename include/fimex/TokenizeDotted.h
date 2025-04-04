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

#ifndef FIMEX_TOKENIZEDOTTED_H
#define FIMEX_TOKENIZEDOTTED_H

#include "fimex/CDMException.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"

#include <vector>

namespace MetNoFimex {

/**
 * convert a string with dots to a vector with type T
 * @param str f.e. 3.5,4.5,...,17.5
 * @param delimiter optional delimiter, defaults to ,
 */
template <typename T>
std::vector<T> tokenizeDotted(const std::string& str, const std::string& delimiter = ",")
{
    std::vector<std::string> tokens = tokenize(str, delimiter);
    std::vector<T> vals;
    vals.reserve(tokens.size());
    for (std::vector<std::string>::iterator tok = tokens.begin(); tok != tokens.end(); ++tok) {
        std::string current = trim(*tok);
        if (current == "...") {
            size_t currentPos = vals.size();
            if (currentPos < 2) {
                throw CDMException("tokenizeDotted: cannot use ... expansion at position " + type2string(currentPos - 1) + ", need at least two values before");
            }
            T last = vals[currentPos - 1];
            T dist = last - vals[currentPos - 2];
            T curVal = last + dist;
            // positive if values get larger, negative if curVal gets samller
            double direction = (dist > 0) ? 1 : -1;
            if (++tok != tokens.end()) {
                T afterDotVal = string2type<T>(*tok);
                // expand the dots until before the afterDotVal, compare against rounding error
                double roundError = direction * dist * -1.e-5;
                while ((curVal - afterDotVal) * direction < roundError) {
                    vals.push_back(curVal);
                    curVal += dist;
                }
                // add the afterDotVal
                vals.push_back(afterDotVal);
            }
        } else {
            T val = string2type<T>(current);
            vals.push_back(val);
        }
    }
    return vals;
}

} // namespace MetNoFimex

#endif /*FIMEX_TOKENIZEDOTTED_H*/
