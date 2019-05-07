/*
 * Fimex, Depth.h
 *
 * (C) Copyright 2018, met.no
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

#ifndef DEPTH_H_
#define DEPTH_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex
{

/**
 * @headerfile fimex/coordSys/verticalTransform/Depth.h
 */
class Depth : public VerticalTransformation
{
public:
    /// name of the depth variable
    const std::string depth;
    Depth(const std::string& depth) : depth(depth) {}
    virtual ~Depth() {}
    /**
     * static NAME constant
     * @return depth
     */
    static const std::string NAME() {return "depth";}
    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
    virtual int getPreferredVerticalType() const { return MIFI_VINT_DEPTH; }
    virtual std::string getParameterString() const { return "d="+depth; }
    virtual bool isComplete() const {return !depth.empty();}

protected:
    VerticalConverter_p getPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p getHeightConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p getAltitudeConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
    VerticalConverter_p getDepthConverter(CDMReader_p reader, CoordinateSystem_cp cs) const;
};

} /* namespace MetNoFimex */

#endif /* DEPTH_H_ */
