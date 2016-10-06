
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"

namespace MetNoFimex {

DataPtr BasicVerticalConverter::getValiditySlice(const SliceBuilder& sb, const std::vector<double>& verticalValues) const
{
    return DataPtr();
}

std::vector<std::string> BasicVerticalConverter::getValidityShape(const std::string& verticalDim) const
{
    return std::vector<std::string>(1, verticalDim);
}

} // namespace MetNoFimex
