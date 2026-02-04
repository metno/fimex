/*
  Fimex, src/CDMMergeUtils.h

  Copyright (C) 2019-2026 met.no

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

#ifndef fimex_CDMMergeUtils_h
#define fimex_CDMMergeUtils_h

#include "fimex/CDMBorderSmoothing.h"
#include "fimex/CDMReaderDecl.h"
#include "fimex/Data.h"
#include "fimex/SharedArray.h"
#include "fimex/coordSys/CoordSysDecl.h"

#include <memory>

#include <algorithm>
#include <cmath>
#include <set>
#include <sstream>
#include <vector>

#define THROW(x) do { std::ostringstream t; t << x; throw CDMException(t.str()); } while(false)

namespace MetNoFimex {

class CDM;

class CDMInterpolator;
typedef std::shared_ptr<CDMInterpolator> CDMInterpolator_p;

typedef std::vector<double> values_v;
typedef values_v::iterator values_it;
typedef values_v::const_iterator values_cit;

inline bool equal(double a, double b)
{
    return std::abs(a - b) < 1e-6;
}

struct equal_float
{
    float value;
    equal_float(float v)
        : value(v) { }
    bool operator()(float v) const
        { return equal(value, v); }
};

void addAuxiliary(std::set<std::string>& variables, const CDM& cdm, const CoordinateSystem_cp_v& coordSys);

bool is_compatible(CDMReader_p readerB, CDMReader_p readerT, const CoordinateSystem_cp_v& allCsB, const CoordinateSystem_cp_v& allCsT,
                   const std::string& varName);

values_v getAxisValues(const CDMReader_p reader, CoordinateAxis_cp axis, const std::string& unit);

CDM makeMergedCDM(CDMReader_p readerI, CDMReader_p& readerO, int gridInterpolationMethod, CDMInterpolator_p& interpolatedO, std::string& nameX,
                  std::string& nameY, bool keepAllOuter = false);

template <class T>
shared_array<T> dataAs(DataPtr data);

template <>
inline shared_array<float> dataAs<float>(DataPtr data) { return data->asFloat(); }

template <>
inline shared_array<double> dataAs<double>(DataPtr data) { return data->asDouble(); }

} // namespace MetNoFimex

#endif // fimex_CDMMergeUtils_h
