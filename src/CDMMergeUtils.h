
#ifndef fimex_CDMMergeUtils_h
#define fimex_CDMMergeUtils_h

#include "fimex/CDM.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/CoordinateSystem.h"

#include <boost/shared_ptr.hpp>

#include <algorithm>
#include <sstream>
#include <vector>

#define THROW(x) do { std::ostringstream t; t << x; throw CDMException(t.str()); } while(false)

namespace MetNoFimex {

//namespace CDMMergeUtils {

typedef boost::shared_ptr<CDMInterpolator> CDMInterpolatorPtr;
typedef boost::shared_ptr<Data> DataPtr;
typedef boost::shared_ptr<CDMReader> CDMReaderPtr;
typedef boost::shared_ptr<const CoordinateSystem> CoordinateSystemPtr;
typedef boost::shared_ptr<const Projection> ProjectionPtr;

typedef std::vector<CoordinateSystemPtr> CoordinateSystemPtr_v;
typedef CoordinateSystemPtr_v::iterator CoordinateSystemPtr_it;
typedef CoordinateSystemPtr_v::const_iterator CoordinateSystemPtr_cit;

typedef std::vector<double> values_v;
typedef values_v::iterator values_it;
typedef values_v::const_iterator values_cit;

inline bool equal(double a, double b)
{
    return fabs(a-b) < 1e-6;
}

struct equal_float : public std::unary_function<bool, float>
{
    float value;
    equal_float(float v)
        : value(v) { }
    bool operator()(float v) const
        { return equal(value, v); }
};

void addAuxiliary(std::set<std::string>& variables, const CDM& cdm, std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys);

bool is_compatible(CDMReaderPtr readerB, CDMReaderPtr readerT,
        const CoordinateSystemPtr_v& allCsB, const CoordinateSystemPtr_v& allCsT,
        const std::string& varName);

values_v getAxisValues(const CDMReaderPtr reader, CoordinateSystem::ConstAxisPtr axis, const std::string& unit);

CDM makeMergedCDM(CDMReaderPtr readerI, CDMReaderPtr& readerO, int gridInterpolationMethod,
        CDMInterpolatorPtr& interpolatedO, std::string& nameX, std::string& nameY, bool keepAllOuter = false);

//} // namespace CDMMergeUtils

} // namespace MetNoFimex

#endif // fimex_CDMMergeUtils_h
