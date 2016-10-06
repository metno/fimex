#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalConverter.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Utils.h"

#include "ArrayLoop.h"

#include <map>

namespace MetNoFimex {

using std::string;

namespace {

static LoggerPtr logger = getLogger("fimex.VerticalTransformationUtils");

typedef std::vector<string> string_v;
typedef std::vector<size_t> size_v;

struct CompleteCSForVariable : public std::unary_function<std::string, bool>
{
public:
    CompleteCSForVariable(boost::shared_ptr<const CoordinateSystem> cs) : cs_(cs) {}
    bool operator()(const std::string& varName) const
      { return cs_->isCSAndCompleteFor(varName); }
private:
    CoordSysPtr cs_;
};

} // anonymous namespace

void removeDimsWithLength1(const CDM& cdm, std::vector<std::string>& dims)
{
    std::vector<string>::iterator it = dims.begin();
    while (it != dims.end()) {
        if (!cdm.hasDimension(*it) || cdm.getDimension(*it).getLength() == 1)
            it = dims.erase(it);
        else
            ++it;
    }
}

std::string findVariableWithDims(const CDM& cdm, const string& standard_name, const std::vector<string>& dims)
{
    std::map<string, string> attrs;
    attrs["standard_name"] = standard_name;
    const std::vector<string> vars = cdm.findVariables(attrs, dims);
    if (!vars.empty())
        return vars.front();
    // maybe try removing axes of undefined type ... ?
    return string();
}

std::string findVariableWithCS(const CDM& cdm, CoordSysPtr cs, const string& standard_name)
{
    std::map<string, string> attrs;
    attrs["standard_name"] = standard_name;
    const std::vector<string> vars = cdm.findVariables(attrs, std::vector<string>());
    std::vector<string>::const_iterator it = std::find_if(vars.begin(), vars.end(), CompleteCSForVariable(cs));
    if (it != vars.end())
        return *it;
    // maybe try removing axes of undefined type ... ?
    return string();
}

size_t getSliceSize(const SliceBuilder& sb, const string& dimName)
{
    size_t start, size;
    sb.getStartAndSize(dimName, start, size);
    return size;
}

SliceBuilder adaptSliceBuilder(const CDM& cdm, const std::string& varName, const SliceBuilder& sb)
{
    SliceBuilder sbVar(cdm, varName);
    const string_v& orgDimNames = sb.getDimensionNames();
    const string_v& varShape = cdm.getVariable(varName).getShape();
    for (string_v::const_iterator itD = varShape.begin(); itD != varShape.end(); ++itD) {
        const string_v::const_iterator itS = std::find(orgDimNames.begin(), orgDimNames.end(), *itD);
        if (itS != orgDimNames.end()) {
            size_t start, size;
            sb.getStartAndSize(*itD, start, size);
            sbVar.setStartAndSize(*itD, start, size);
        }
    }
    return sbVar;
}

DataPtr getSliceData(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    const SliceBuilder sbVar = adaptSliceBuilder(reader->getCDM(), varName, sbOrig);
    return reader->getScaledDataSliceInUnit(varName, unit, sbVar);
}

boost::shared_array<float> getSliceFloats(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    return getSliceData(reader, sbOrig, varName, unit)->asFloat();
}

boost::shared_array<double> getSliceDoubles(CDMReaderPtr reader, const SliceBuilder& sbOrig, const std::string& varName, const std::string& unit)
{
    return getSliceData(reader, sbOrig, varName, unit)->asDouble();
}

std::vector<size_t> getDimSizes(const CDM& cdm, const std::vector<std::string>& dimNames)
{
    std::vector<size_t> dimSizes(dimNames.size(), 1);
    for (size_t i=0; i<dimNames.size(); ++i) {
        if (cdm.hasDimension(dimNames[i]))
            dimSizes[i] = cdm.getDimension(dimNames[i]).getLength();
    }
    return dimSizes;
}

SliceBuilder createSliceBuilder(const CDM& cdm, const std::vector<std::string>& dimNames)
{
    const std::vector<size_t> dimSizes = getDimSizes(cdm, dimNames);
    return SliceBuilder(dimNames, dimSizes);
}

SliceBuilder createSliceBuilder(const CDM& cdm, boost::shared_ptr<VerticalConverter> converter)
{
    return createSliceBuilder(cdm, converter->getShape());
}

void setUnLimDimPos(const CDM& cdm, SliceBuilder& sb, size_t unLimDimPos)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim()) {
        LOG4FIMEX(logger, Logger::DEBUG, "cdm has unlimited dim '" << uld->getName() << "'");
        const std::vector<std::string> dimNames = sb.getDimensionNames();
        if (std::find(dimNames.begin(), dimNames.end(), uld->getName()) != dimNames.end())
            sb.setStartAndSize(uld->getName(), unLimDimPos, 1);
    }
}

size_t setUnLimDimPos(const CDM& cdm, ArrayDims& s, size_t unLimDimPos)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim()) {
        s.set_not_shared(uld->getName());
        return unLimDimPos * s.delta(uld->getName());
    }
    return 0;
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3, s4);
}

void forceUnLimDimLength1(const CDM& cdm, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5)
{
    if (const CDMDimension* uld = cdm.getUnlimitedDim())
        set_length(uld->getName(), 1, s1, s2, s3, s4, s5);
}

ArrayDims makeArrayDims(const CDM& cdm, const std::vector<std::string>& dimNames)
{
    const std::vector<size_t> dimSizes = getDimSizes(cdm, dimNames);
    return ArrayDims(dimNames, dimSizes);
}

ArrayDims makeArrayDims(const CDM& cdm, const std::string& varName)
{
    return makeArrayDims(cdm, cdm.getVariable(varName).getShape());
}

ArrayDims makeArrayDims(const CDM& cdm, boost::shared_ptr<VerticalConverter> converter)
{
    const std::vector<std::string> dimNames = converter->getShape();
    const std::vector<size_t> dimSizes = getDimSizes(cdm, dimNames);
    return ArrayDims(dimNames, dimSizes);
}

ArrayDims makeArrayDims(const SliceBuilder& sb)
{
    const std::vector<std::string> dimNames = sb.getDimensionNames();
    const std::vector<size_t> dimSizes = sb.getDimensionSizes();
    return ArrayDims(dimNames, dimSizes);
}

VerticalConverterPtr verticalConverter(boost::shared_ptr<const CoordinateSystem> cs, boost::shared_ptr<CDMReader> reader, int verticalType)
{
    if (VerticalConverterPtr converter = cs->getVerticalTransformation()->getConverter(reader, cs, verticalType))
        return converter;
    else
        throw CDMException("no vertical transformation found");
}

DataPtr verticalData4D(VerticalConverterPtr converter, const CDM& cdm, size_t unLimDimPos)
{
    SliceBuilder sb = createSliceBuilder(cdm, converter);
    setUnLimDimPos(cdm, sb, unLimDimPos);
    return converter->getDataSlice(sb);
}

DataPtr verticalData4D(boost::shared_ptr<const CoordinateSystem> cs, boost::shared_ptr<CDMReader> reader, size_t unLimDimPos, int verticalType)
{
    return verticalData4D(verticalConverter(cs, reader, verticalType), reader->getCDM(), unLimDimPos);
}

DataPtr checkSize(DataPtr data, size_t expected, const std::string& what)
{
    if (data && data->size() != expected)
        throw CDMException("data for '" + what + "' have size " + type2string(data->size()) + ", expected " + type2string(expected));
    return data;
}

DataPtr checkData(DataPtr data, size_t expected, const std::string& what)
{
    if (!data.get())
        throw CDMException("no data for '" + what + "'");
    return checkSize(data, expected, what);
}

} // namespace MetNoFimex
