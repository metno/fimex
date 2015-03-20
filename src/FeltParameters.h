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

#ifndef FELTPARAMETERS_H_
#define FELTPARAMETERS_H_

#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <boost/array.hpp>
#include "Felt_File_Error.h"

namespace MetNoFelt {

class FeltParameters
{
    std::map<std::string, boost::array<short,16> > parameterMap;
    std::map<std::string, std::string> parameterDatatypeMap;
    std::map<std::string, double> parameterFillValueMap;

public:
    FeltParameters();
    /**
     * initialize all known felt parameters from a diana-setup file
     *
     * @param filename diana setup file
     *
     */
    explicit FeltParameters(std::string filename);
    /**
     * initialize parameters from a list of parameters in diana format, e.g. 17,2,1000:prod=74
     */
    explicit FeltParameters(const std::vector<std::string>& feltParams, const std::string& globalRestrictions);
    virtual ~FeltParameters();
    const boost::array<short, 16>& getParameters(const std::string&);
    const std::string& getParameterName(const boost::array<short, 16>&);
    std::string getParameterDatatype(const std::string& parameterName) const;
    double getParameterFillValue(const std::string& parameterName) const;
    // local static objects
    const static std::string& DEFAULT_CONFIG() {
        const static std::string DEFAULT_CONFIG("/metno/local/diana/etc/diana.setup-COMMON");
        return DEFAULT_CONFIG;
    }
    friend std::ostream & operator<<(std::ostream &os, const FeltParameters& p);
private:
    void init(std::string filename=DEFAULT_CONFIG());
    boost::array<short, 16> diana2feltparameters(const std::string&);
};

std::string getProjString(int gridType, const boost::array<float, 6>& gridParameters);

inline int ANY_VALUE() { return -32767;}
const std::string& UNDEFINED();
const boost::array<short, 16>& ANY_ARRAY();
const boost::array<short, 20>& ANY_ARRAY20();

} // end namespace MetNoFelt

#endif /*FELTPARAMETERS_H_*/
