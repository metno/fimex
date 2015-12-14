
#include "CDMMergeUtils.h"

#include "fimex/Logger.h"

#include <boost/foreach.hpp>

namespace MetNoFimex {

static LoggerPtr logger(getLogger("fimex.CDMMergeUtils"));

//namespace CDMMergeUtils {

using namespace std;

values_v getAxisValues(const CDMReaderPtr reader, CoordinateSystem::ConstAxisPtr axis, const string& unit)
{
    const CDM& cdm = reader->getCDM();
    const string &name = axis->getName();
    if (not (cdm.hasDimension(name)))
        THROW("axis '" << name << "' is not a dimension");

    const CDMDimension& dim = cdm.getDimension(name);
    if (dim.isUnlimited())
        THROW("axis '" << name << "' is unlimited, cannot get value list");

    DataPtr axisData = reader->getScaledDataInUnit(name, unit);
    if (not axisData or axisData->size() < 2)
        THROW("no data for axis '" << name << "'");

    boost::shared_array<double> array = axisData->asDouble();
    return values_v(array.get(), array.get() + axisData->size());
}

static int axisLength(CDMReaderPtr reader, CoordinateSystem::ConstAxisPtr ax)
{
    if (ax.get() == 0)
        return -2;
    const CDMDimension& dim = reader->getCDM().getDimension(ax->getName());
    // if (dim.isUnlimited())
    //    return -1;
    return dim.getLength();
}

bool checkSingleAxisCompatibility(CDMReaderPtr readerB, CDMReaderPtr readerT,
        CoordinateSystem::ConstAxisPtr axB, CoordinateSystem::ConstAxisPtr axT,
        const char* messageLabel)
{
    const int axisLengthB = axisLength(readerB, axB), axisLengthT = axisLength(readerT, axT);
    LOG4FIMEX(logger, Logger::DEBUG, "axis " << messageLabel << " length B=" << axisLengthB << " T= " << axisLengthT);

    // both a variable, different length >= bad
    if (axisLengthB >= -1 and axisLengthT >= -1 and axisLengthB != axisLengthT)
        return false;

    // both not a variable >= ok
    if (axisLengthB == -2 and axisLengthT == -2)
        return true;

    // one not a variable and the other length 1 => bad
    if ((axisLengthB == -2 and axisLengthT != 1) or (axisLengthB != 1 and axisLengthT == -2))
        return false;

    return true;
}

bool checkAxesCompatibility(CDMReaderPtr readerB, CDMReaderPtr readerT, CoordinateSystemPtr csB, CoordinateSystemPtr csT)
{
    const int N = 5;
    const CoordinateAxis::AxisType types[N] = {
        CoordinateAxis::GeoZ,
        CoordinateAxis::Time,
        CoordinateAxis::Pressure,
        CoordinateAxis::Height,
        CoordinateAxis::ReferenceTime
    };
    const char* labels[N] = {
        "z", "time", "pressure", "height", "reference time"
    };
    for(int i=0; i<N; ++i) {
        if (not checkSingleAxisCompatibility(readerB, readerT, csB->findAxisOfType(types[i]), csT->findAxisOfType(types[i]), labels[i])) {
            LOG4FIMEX(logger, Logger::DEBUG, "axis " << labels[i] << " incompatible");
            return false;
        }
    }
    return true;
}

void addAuxiliary(std::set<std::string>& variables, const CDM& cdm, std::vector<boost::shared_ptr<const CoordinateSystem> > coordSys)
{
    using namespace std;
    // add all dimension variables
    set<string> dimsVars;
    for (set<string>::iterator sit = variables.begin(); sit != variables.end(); ++sit) {
        if (cdm.hasVariable(*sit)) {
            vector<string> shape = cdm.getVariable(*sit).getShape();
            for (vector<string>::iterator shapeIt = shape.begin(); shapeIt != shape.end(); ++shapeIt) {
                if (cdm.hasVariable(*shapeIt)) {
                    dimsVars.insert(*shapeIt);
                }
            }
        }
    }
    size_t count = 0;
    for (set<string>::iterator dIt = dimsVars.begin(); dIt != dimsVars.end(); ++dIt) {
        if (variables.find(*dIt) == variables.end()) {
            variables.insert(*dIt);
            count++;
        }
    }


    // add coordinate-system variables
    set<string> varsCopy = variables;
    for (set<string>::iterator sit = varsCopy.begin(); sit != varsCopy.end(); ++sit) {
        vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
                find_if(coordSys.begin(), coordSys.end(), CompleteCoordinateSystemForComparator(*sit));
        if (varSysIt != coordSys.end()) {
            set<string> csDepVars = (*varSysIt)->getDependencyVariables();
            for (set<string>::iterator dIt = csDepVars.begin(); dIt != csDepVars.end(); ++dIt) {
                if (variables.find(*dIt) == variables.end()) {
                    variables.insert(*dIt);
                    count++;
                }
            }
        }
    }
    if (count > 0) {
        addAuxiliary(variables, cdm, coordSys);
    }
}

#define LOG_INCOMPATIBLE(message) \
    do {                                                                \
        LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' incompatible: " << message); \
        return false;                                                   \
    } while(false)

bool is_compatible(CDMReaderPtr readerB, CDMReaderPtr readerT,
        const CoordinateSystemPtr_v& allCsB, const CoordinateSystemPtr_v& allCsT,
        const string& varName)
{
    LOG4FIMEX(logger, Logger::DEBUG, "checking compatibility for variable '" << varName << "' ...");

    const CDM& cdmB = readerB->getCDM(), &cdmT = readerT->getCDM();
    if (not (cdmT.hasVariable(varName) and cdmB.hasVariable(varName)))
        LOG_INCOMPATIBLE("not found in top and base");

    const CoordinateSystemPtr_cit itCsB = findCS(allCsB, varName);
    const CoordinateSystemPtr_cit itCsT = findCS(allCsT, varName);
    const bool hasCsB = (itCsB != allCsB.end()), hasCsT = (itCsT != allCsT.end());
    if (hasCsB != hasCsT)
        LOG_INCOMPATIBLE("CS in base but not in top, or vice versa");
    if (not hasCsB) {
        LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' has no CS, defined as compatible");
        return true;
    }

    // FIXME the next checks are actually checking what CDMInterpolator can do
    const CoordinateSystemPtr csB = *itCsB, csT = *itCsT;

//    const bool hasSimpleGridB = csB->isSimpleSpatialGridded(), hasSimpleGridT = csT->isSimpleSpatialGridded();
//    if (not (hasSimpleGridB and hasSimpleGridT))
//        LOG_INCOMPATIBLE("no simple spatial grid in base and top");
//
//    if (not (csB->hasProjection() and csT->hasProjection()))
//        LOG_INCOMPATIBLE("no projection in base or top");

    // ProjectionPtr projB = csB->getProjection(), projT = csT->getProjection();
    // if (projB->isDegree() != projT->isDegree())
    //     LOG_INCOMPATIBLE("CS projections  are not both in degrees");

    if (not checkAxesCompatibility(readerB, readerT, csB, csT))
        LOG_INCOMPATIBLE("CS have incompatible axes");

    const vector<string> &shapeB = cdmB.getVariable(varName).getShape(),
            &shapeT = cdmT.getVariable(varName).getShape();
    if (shapeB != shapeT) {
        for (size_t idxB=0, idxT=0; idxB < shapeB.size() and idxT < shapeT.size(); ++idxB, ++idxT) {
            // hop over dimensions with length 1
            while (idxB < shapeB.size() and cdmB.getDimension(shapeB[idxB]).getLength() == 1) {
                LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' hopping over length-1 dimension '" << shapeB[idxB] << "' in base");
                idxB += 1;
            }
            while (idxT < shapeT.size() and cdmT.getDimension(shapeT[idxT]).getLength() == 1) {
                LOG4FIMEX(logger, Logger::DEBUG, "variable '" << varName << "' hopping over length-1 dimension '" << shapeT[idxT] << "' in top");
                idxT += 1;
            }

            // both must reach end of shape vector at the same time
            if (idxT == shapeT.size() and idxB == shapeB.size())
                break;
            if ((idxB < shapeB.size()) != (idxT < shapeT.size()))
                LOG_INCOMPATIBLE("mismatch in dimensions with length!=1");

            // must reach x/y axis at the same time
            const bool isGeoXB = (shapeB[idxB] == csB->getGeoXAxis()->getName()),
                    isGeoXT = (shapeT[idxT] == csT->getGeoXAxis()->getName()),
                    isGeoYB = (shapeB[idxB] == csB->getGeoYAxis()->getName()),
                    isGeoYT = (shapeT[idxT] == csT->getGeoYAxis()->getName());
            if (isGeoXB != isGeoXT or isGeoYB != isGeoYT)
                LOG_INCOMPATIBLE("geo x/y mismatch");

            // if not geox or geoy, must be same dimension length and values
            if (not (isGeoXB or isGeoYB)) {
                const size_t lengthB = cdmB.getDimension(shapeB[idxB]).getLength(),
                        lengthT = cdmT.getDimension(shapeT[idxT]).getLength();
                if (lengthB != lengthT)
                    LOG_INCOMPATIBLE("length mismatch for dimension '" << shapeB[idxB]
                            << "' in base and '" << shapeT[idxT] << "' in top");

                // TODO compare values
            }
        }
    }

    return true;
}

CDM makeMergedCDM(CDMReaderPtr readerI, CDMReaderPtr& readerO, int gridInterpolationMethod,
        CDMInterpolatorPtr& interpolatedO, string& nameX, string& nameY, bool keepAllOuter)
{
    const CDM& cdmIC = readerI->getCDM();
    CDM cdmI = readerI->getCDM();                  // copy, as we modify cdmI
    const CDM::VarVec varsI = cdmI.getVariables(); // copy, as we modify cdmI
    const vector<CoordinateSystemPtr> allCsI = listCoordinateSystems(readerI);

    BOOST_FOREACH(const CDMVariable& varI, varsI) {
        const string& varName = varI.getName();
        if (not readerO->getCDM().hasVariable(varName))
            continue;

        const CoordinateSystemPtr_cit itCsI = findCS(allCsI, varName);
        if (itCsI == allCsI.end())
            continue;
        const CoordinateSystemPtr csI = *itCsI;
        if (not (csI->hasProjection() and csI->isSimpleSpatialGridded()))
            continue;

        ProjectionPtr projI = csI->getProjection();
        interpolatedO = CDMInterpolatorPtr(new CDMInterpolator(readerO));
        nameX = csI->getGeoXAxis()->getName();
        nameY = csI->getGeoYAxis()->getName();
        const string& unitIX = cdmI.getUnits(nameX),
                unitIY = cdmI.getUnits(nameY);
        const values_v valuesX = getAxisValues(readerI, csI->getGeoXAxis(), unitIX),
                valuesY = getAxisValues(readerI, csI->getGeoYAxis(), unitIY);
        interpolatedO->changeProjection(gridInterpolationMethod, projI->getProj4String(),
                valuesX, valuesY, unitIX, unitIX, CDM_DOUBLE, CDM_DOUBLE);

        LOG4FIMEX(logger, Logger::INFO, "interpolating top grid");
        break;
    }
    if (not interpolatedO)
        THROW("CDMBorderSmoothing: could not find CS for interpolation");

    const vector<CoordinateSystemPtr> allCsO = listCoordinateSystems(interpolatedO);
    std::set<string> variablesToKeep;
    BOOST_FOREACH(const CDMVariable& varI, varsI) {
        const string& varName = varI.getName();
        if (is_compatible(readerI, interpolatedO, allCsI, allCsO, varName))
            variablesToKeep.insert(varName);
    }
    addAuxiliary(variablesToKeep, cdmI, allCsI);

    BOOST_FOREACH(const CDMVariable& varI, varsI) {
        const string& varName = varI.getName();
        if (variablesToKeep.find(varName) == variablesToKeep.end()) {
            LOG4FIMEX(logger, Logger::INFO, "variable '" << varName << "' removed");
            cdmI.removeVariable(varName);
        } else {
            LOG4FIMEX(logger, Logger::INFO, "variable '" << varName << "' kept");
        }
    }

    if (keepAllOuter) {
        const CDM::DimVec& dims = interpolatedO->getCDM().getDimensions();
        BOOST_FOREACH(const CDMDimension& dim, dims) {
            if (!cdmI.hasDimension(dim.getName())) {
                LOG4FIMEX(logger, Logger::DEBUG, "dimension '" << dim.getName() << "' added from outer");
                cdmI.addDimension(dim);
            }
        }
        const CDM::VarVec& vars = interpolatedO->getCDM().getVariables();
        BOOST_FOREACH(const CDMVariable& var, vars) {
            if (!cdmIC.hasVariable(var.getName())) {
                LOG4FIMEX(logger, Logger::DEBUG, "variable '" << var.getName() << "' added from outer");
                cdmI.addVariable(var);
            }
        }
    }

    return cdmI;
}

//} // namespace CDMMergeUtils

} // namespace MetNoFimex
