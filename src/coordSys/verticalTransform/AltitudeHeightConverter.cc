#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"

#include <boost/make_shared.hpp>

#include <map>
#include <vector>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.AltitudeHeightConverter");

VerticalConverterPtr AltitudeHeightConverter::createConverter(CDMReaderPtr reader,
        CoordSysPtr cs, VerticalConverterPtr altitudeOrHeight, bool addTopography)
{
    using namespace std;

    const CoordinateSystem::ConstAxisPtr xAxis = cs->getGeoXAxis();
    const CoordinateSystem::ConstAxisPtr yAxis = cs->getGeoYAxis();
    if (!xAxis || !yAxis)
        return boost::shared_ptr<AltitudeHeightConverter>();

    vector<string> dims;
    dims.push_back(xAxis->getShape()[0]);
    dims.push_back(yAxis->getShape()[0]);
    map<string, string> attrs;
    attrs["standard_name"] = "(surface_geopotential|altitude|geopotential_height|geopotential)";

    vector<string> topoVars = reader->getCDM().findVariables(attrs, dims);
    if (topoVars.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "no topography/altitude found to retrieve height");
        return boost::shared_ptr<AltitudeHeightConverter>();
    }

    LOG4FIMEX(logger, Logger::INFO, "using altitude "<<topoVars[0]<<" to retrieve height");
    return boost::make_shared<AltitudeHeightConverter>(reader, cs, altitudeOrHeight, topoVars[0], addTopography);
}

AltitudeHeightConverter::AltitudeHeightConverter(CDMReaderPtr reader, CoordSysPtr cs,
        VerticalConverterPtr altitudeOrHeight, const std::string& topography, bool addTopography)
    : BasicVerticalConverter(reader, cs)
    , altitudeOrHeight_(altitudeOrHeight)
    , topography_(topography)
    , topographyFactor_(addTopography ? 1 : -1)
{
    // FIXME next lines copied from CDMProcessor::addVerticalVelocity()
    const std::string stdName = reader->getCDM().getAttribute(topography, "standard_name").getStringValue();
    if (stdName == "altitude" || stdName == "geopotential_height") {
        topographyUnit_ = "m";
    } else {
        topographyFactor_ /= 9.81;
        topographyUnit_ = "m^2/s^2"; // division of gepotential by gravity
    }
}

std::vector<std::string> AltitudeHeightConverter::getShape() const
{
    return altitudeOrHeight_->getShape();
}

DataPtr AltitudeHeightConverter::getDataSlice(const SliceBuilder& sb) const
{
    DataPtr ahData = altitudeOrHeight_->getDataSlice(sb);

    const SliceBuilder sbTopo = adaptSliceBuilder(reader_->getCDM(), topography_, sb);
    DataPtr topoData = reader_->getScaledDataSliceInUnit(topography_, topographyUnit_, sbTopo);

    const size_t nx = getSliceSize(sb, cs_->getGeoXAxis()->getName());
    const size_t ny = getSliceSize(sb, cs_->getGeoYAxis()->getName());
    const size_t sizeXY = nx*ny;
    const size_t size = ahData->size();
    const size_t sizeOther = size / sizeXY;

    checkSize(topoData, sizeXY, topography_);

    boost::shared_array<float> ahVal = ahData->asFloat();
    boost::shared_array<float> topoVal = topoData->asFloat();
    boost::shared_array<float> haVal(new float[size]);

    size_t offset = 0;
    for (size_t i=0; i<sizeOther; ++i, offset += sizeXY) {
        for (size_t xy=0; xy < sizeXY; ++xy) {
            const size_t idx = offset + xy;
            haVal[idx] = ahVal[idx] + topographyFactor_ * topoVal[xy];
        }
    }
    return createData(size, haVal);
}

} // namespace MetNoFimex
