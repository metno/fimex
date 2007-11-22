#include "CDMVariable.h"

namespace MetNoUtplukk
{

CDMVariable::CDMVariable(std::string name, CDMDataType datatype, std::vector<CDMDimension> shape)
: name(name), datatype(datatype), shape(shape)
{
}

CDMVariable::~CDMVariable()
{
}

}
