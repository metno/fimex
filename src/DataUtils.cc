
#include "fimex/DataUtils.h"

#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"

namespace MetNoFimex {

/* init data arrays for all types */
template <typename T>
DataPtr initDataArray(const std::vector<std::string>& values)
{
    shared_array<T> array(new T[values.size()]);
    std::transform(values.begin(), values.end(), &array[0], &string2type<T>);
    return createData(values.size(), array);
}

DataPtr initDataByArray(CDMDataType datatype, const std::vector<std::string>& values)
{
    switch (datatype) {
    case CDM_FLOAT:
        return initDataArray<float>(values);
    case CDM_DOUBLE:
        return initDataArray<double>(values);
    case CDM_INT64:
        return initDataArray<long long>(values);
    case CDM_UINT64:
        return initDataArray<unsigned long long>(values);
    case CDM_INT:
        return initDataArray<int>(values);
    case CDM_UINT:
        return initDataArray<unsigned int>(values);
    case CDM_SHORT:
        return initDataArray<short>(values);
    case CDM_USHORT:
        return initDataArray<unsigned short>(values);
    case CDM_CHAR:
        return initDataArray<char>(values);
    case CDM_UCHAR:
        return initDataArray<unsigned char>(values);
    case CDM_STRING: {
        /* string may only have one dimension */
        return createData(join(values.begin(), values.end(), " "));
    }
    case CDM_STRINGS: {
        shared_array<std::string> array(new std::string[values.size()]);
        std::copy(values.begin(), values.end(), &array[0]);
        return createData(values.size(), array);
    }
    default:
        throw CDMException("Unknown type '" + datatype2string(datatype) + "' to generate data array");
    }
}

} // namespace MetNoFimex
