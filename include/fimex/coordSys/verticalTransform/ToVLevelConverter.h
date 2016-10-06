/*
 * Fimex, ToVLevelConverter.h
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Aug 9, 2011
 *      Author: Heiko Klein
 */

#ifndef TO_VLEVEL_CONVERTER_H_
#define TO_VLEVEL_CONVERTER_H_

#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>

#include "fimex/deprecated.h"

#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"
#include "fimex/coordSys/verticalTransform/AltitudeStandardToPressureConverter.h"
#include "fimex/coordSys/verticalTransform/GeopotentialToAltitudeConverter.h"
#include "fimex/coordSys/verticalTransform/HybridSigmaApToPressureConverter.h"
#include "fimex/coordSys/verticalTransform/HybridSigmaToPressureConverter.h"
#include "fimex/coordSys/verticalTransform/LnPressureToPressureConverter.h"
#include "fimex/coordSys/verticalTransform/OceanSCoordinateGToDepthConverter.h"
#include "fimex/coordSys/verticalTransform/PressureIntegrationToAltitudeConverter.h"
#include "fimex/coordSys/verticalTransform/PressureToStandardAltitudeConverter.h"
#include "fimex/coordSys/verticalTransform/SigmaToPressureConverter.h"

namespace MetNoFimex {

/** Deprecated */
class ToVLevelConverter : boost::noncopyable {
public:
    virtual ~ToVLevelConverter();

    /** Deprecated */
    virtual std::vector<double> operator()(size_t x, size_t y, size_t t) = 0;

    //! Deprecated.
   inline std::vector<double> values(size_t x, size_t y, size_t t)
        { return operator()(x, y, t); }

    /** Deprecated */
    virtual bool isValid(double val, size_t x, size_t y, size_t t) = 0;
};

} // namespace

#endif /* TO_VLEVEL_CONVERTER_H_ */
