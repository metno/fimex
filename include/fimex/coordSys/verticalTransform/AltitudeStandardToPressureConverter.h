/*
  Fimex, include/fimex/coordSys/verticalTransform/AltitudeStandardToPressureConverter.h

  Copyright (C) 2019 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef ALTITUDESTANDARDTOPRESSURECONVERTER_H
#define ALTITUDESTANDARDTOPRESSURECONVERTER_H

#include "fimex/coordSys/verticalTransform/VerticalConverter.h"

namespace MetNoFimex {

/**
 * using the international standard atmosphere to convert altitude to pressure
 */
class AltitudeStandardToPressureConverter : public BasicVerticalConverter {
public:
    static VerticalConverter_p createConverter(CDMReader_p reader, CoordinateSystem_cp cs, VerticalConverter_p altitude);

    /**
     * @param altitude given in m
     */
    AltitudeStandardToPressureConverter(CDMReader_p reader, CoordinateSystem_cp cs, const VerticalConverter_p& altitude);

    std::vector<std::string> getShape() const;
    DataPtr getDataSlice(const SliceBuilder& sb) const;

private:
    VerticalConverter_p altitude_;
};

} // namespace MetNoFimex

#endif // ALTITUDESTANDARDTOPRESSURECONVERTER_H
