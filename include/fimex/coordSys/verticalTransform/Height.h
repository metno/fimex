/*
 * Fimex, Height.h
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Aug 6, 2013
 *      Author: heikok
 */

#ifndef HEIGHT_H_
#define HEIGHT_H_

#include "fimex/coordSys/verticalTransform/VerticalTransformation.h"

namespace MetNoFimex
{

/**
 * @headerfile fimex/coordSys/verticalTransform/Height.h
 */
class Height : public VerticalTransformation
{
public:
    /// name of the height variable
    const std::string height;
    Height(const std::string& height) : height(height) {}
    virtual ~Height() {}
    /**
     * static NAME constant
     * @return height
     */
    static const std::string NAME() {return "height";}
    /**
     * @return same as static NAME()
     */
    virtual std::string getName() const { return NAME(); }
    virtual int getPreferredVerticalType() const { return MIFI_VINT_HEIGHT; }
    virtual std::string getParameterString() const { return "h="+height; }
    virtual bool isComplete() const {return !height.empty();}

protected:
    VerticalConverterPtr getPressureConverter(CDMReader_p reader, CoordSysPtr cs) const;
    VerticalConverterPtr getHeightConverter(CDMReader_p reader, CoordSysPtr cs) const;
    VerticalConverterPtr getAltitudeConverter(CDMReader_p reader, CoordSysPtr cs) const;
};

} /* namespace MetNoFimex */

#endif /* HEIGHT_H_ */
