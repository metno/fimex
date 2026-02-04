/*
 * Fimex, CoordinateAxis.h
 *
 * (C) Copyright 2009-2026, met.no
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
 *  Created on: Mar 16, 2010
 *      Author: Heiko Klein
 */

#ifndef COORDINATEAXIS_H_
#define COORDINATEAXIS_H_

#include "fimex/CDMVariable.h"

namespace MetNoFimex
{
/**
 * @headerfile fimex/coordSys/CoordinateAxis.h
 */

class CoordinateAxis: public CDMVariable
{
public:
    enum AxisType {
        Undefined = 0,
        GeoX,
        GeoY,
        GeoZ,
        Time,
        Lon,
        Lat,
        Pressure,
        Height,
        Depth,
        ReferenceTime,
        Realization
    };
    static std::string type2string(AxisType type) {
        switch (type) {
        case GeoX: return "GeoX";
        case GeoY: return "GeoY";
        case GeoZ: return "GeoZ";
        case Time: return "Time";
        case Lon: return "Lon";
        case Lat: return "Lat";
        case Pressure: return "Pressure";
        case Height: return "Height";
        case Depth: return "Depth";
        case ReferenceTime: return "ReferenceTime";
        case Realization: return "Realization";
        default: return "Undefined";
        }
    }
    static AxisType int2type(int type) {
        return static_cast<AxisType>(type);
    }
    static int type2int(AxisType type) {
        return static_cast<int>(type);
    }
    explicit CoordinateAxis(const CDMVariable& var) : CDMVariable(var), type_(Undefined) {}
    virtual ~CoordinateAxis() {}

    bool operator<(const CoordinateAxis& ca) const {
        return this->getName() < ca.getName();
    }

    AxisType getAxisType() const {return type_;}
    std::string getAxisTypeStr() const {return type2string(type_);}
    void setAxisType(AxisType t) {type_ = t;}
    bool isAxisType(AxisType t) const {return t == type_;}
    /**
     * Check if this axis is a explicitly netcdf-dimension, too.
     * @return false if this is a implicit 'coordinates' variable, rather than a netcdf-dimension
     */
    bool isExplicit() const {return explicit_;}
    void setExplicit(bool isExplicit) {explicit_ = isExplicit;}

private:
    bool explicit_;
    AxisType type_;

};

std::ostream& operator<<(std::ostream& out, CoordinateAxis ca);
std::ostream& operator<<(std::ostream& out, CoordinateAxis::AxisType t);

}


#endif /* COORDINATEAXIS_H_ */
