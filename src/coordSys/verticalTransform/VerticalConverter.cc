
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"

namespace MetNoFimex {

VerticalConverter::~VerticalConverter()
{
}

DataPtr BasicVerticalConverter::getValidityMax(const SliceBuilder&) const
{
    return DataPtr();
}

DataPtr BasicVerticalConverter::getValidityMin(const SliceBuilder&) const
{
    return DataPtr();
}

std::vector<std::string> BasicVerticalConverter::getValidityMaxShape() const
{
    return std::vector<std::string>();
}

std::vector<std::string> BasicVerticalConverter::getValidityMinShape() const
{
    return std::vector<std::string>();
}

} // namespace MetNoFimex
