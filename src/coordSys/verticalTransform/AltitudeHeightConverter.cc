#include "fimex/coordSys/verticalTransform/AltitudeHeightConverter.h"

#include "fimex/CDM.h"
#include "fimex/CDMReader.h"
#include "fimex/coordSys/CoordinateSystem.h"
#include "fimex/coordSys/verticalTransform/VerticalTransformationUtils.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"

#include "fimex/ArrayLoop.h"

#include <boost/make_shared.hpp>

#include <map>
#include <vector>

namespace MetNoFimex {

static LoggerPtr logger = getLogger("fimex.AltitudeHeightConverter");

VerticalConverterPtr AltitudeHeightConverter::createConverter(CDMReader_p reader,
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
    attrs["standard_name"] = "(surface_geopotential|surface_altitude|altitude|geopotential_height|geopotential)";

    vector<string> topoVars = reader->getCDM().findVariables(attrs, dims);
    if (topoVars.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "no topography/altitude found to retrieve height");
        return boost::shared_ptr<AltitudeHeightConverter>();
    }

    LOG4FIMEX(logger, Logger::INFO, "using altitude "<<topoVars[0]<<" to retrieve height");
    return boost::make_shared<AltitudeHeightConverter>(reader, cs, altitudeOrHeight, topoVars[0], addTopography);
}

AltitudeHeightConverter::AltitudeHeightConverter(CDMReader_p reader, CoordSysPtr cs,
        VerticalConverterPtr altitudeOrHeight, const std::string& topography, bool addTopography)
    : BasicVerticalConverter(reader, cs)
    , altitudeOrHeight_(altitudeOrHeight)
    , topography_(topography)
    , topographyFactor_(addTopography ? 1 : -1)
{
    // FIXME next lines copied from CDMProcessor::addVerticalVelocity()
    const std::string stdName = reader->getCDM().getAttribute(topography, "standard_name").getStringValue();
    if (stdName == "surface_altitude" || stdName == "altitude" || stdName == "geopotential_height") {
        topographyUnit_ = "m";
    } else {
        topographyFactor_ /= MIFI_EARTH_GRAVITY;
        topographyUnit_ = "m^2/s^2"; // division of gepotential by gravity
    }
}

std::vector<std::string> AltitudeHeightConverter::getShape() const
{
    return ShapeMerger(reader_->getCDM(), cs_)
            .merge(topography_, true)
            .merge(altitudeOrHeight_)
            .shape();
}

DataPtr AltitudeHeightConverter::getDataSlice(const SliceBuilder& sb) const
{
    VarDouble topo(reader_, topography_, topographyUnit_, sb);
    VarDouble altiOrHeight(reader_, altitudeOrHeight_, sb);

    ArrayDims out_dims = makeArrayDims(sb);
    boost::shared_array<double> out_values(new double[out_dims.volume()]);

    enum { TOPO, IN_ALTI, OUT_HEIGHT };
    ArrayGroup group = ArrayGroup().add(topo.dims).add(altiOrHeight.dims).add(out_dims);
    group.minimizeShared(0); // FIXME do not treat each value separately

    Loop loop(group);
    do { // sharedVolume() == 1 because we called minimizeShared before
        out_values[loop[OUT_HEIGHT]] = altiOrHeight.values[loop[IN_ALTI]]
                + topographyFactor_ * topo.values[loop[TOPO]];
    } while (loop.next());
    return createData(out_dims.volume(), out_values);
}

} // namespace MetNoFimex
