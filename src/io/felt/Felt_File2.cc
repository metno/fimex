/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include "Felt_File2.h"

#include "Felt_Array2.h"

#include "FeltField.h"
#include "FeltFile.h"
#include "FeltGridDefinition.h"

#include "fimex/CDMDataType.h"
#include "fimex/CDMconstants.h"
#include "fimex/Data.h"
#include "fimex/DataUtils.h"
#include "fimex/Logger.h"
#include "fimex/String2Type.h"
#include "fimex/StringUtils.h"

#include <algorithm>
#include <cassert>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <fstream>
#include <set>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

namespace MetNoFelt {

using namespace MetNoFimex;

static Logger_p logger = getLogger("fimex.Felt_File2");

static MetNoFimex::FimexTime toFimexTime(const felt::FeltTime& time)
{
    return MetNoFimex::FimexTime(time.year, time.month, time.day, time.hour, time.minute, time.second);
}

namespace {

struct FimexFeltLogger : public felt::FeltLogger
{
    FimexFeltLogger(Logger_p fl)
        : fl_(fl)
    {
    }
    void log(const std::string& message) override { LOG4FIMEX(fl_, Logger::DEBUG, message); }
    Logger_p fl_;
};

} // namespace

Felt_File2::Felt_File2(const string& filename)
    : filename_(filename)
{
    int pos = filename.rfind("/");
    std::string dianaSetup = filename.substr(0, pos + 1) + "diana.setup";
    std::ifstream setupFile(dianaSetup.c_str());
    if (setupFile.is_open()) {
        setupFile.close();
        feltParameters_ = FeltParameters(dianaSetup);
    }
    // else default constructor
    init();
}

Felt_File2::Felt_File2(const string& filename, const string& filenameSetup)
    : filename_(filename)
{
    std::ifstream setupFile(filenameSetup.c_str());
    if (setupFile.is_open()) {
        setupFile.close();
        feltParameters_ = FeltParameters(filenameSetup);
    }
    // else default constructor
    init();
}

Felt_File2::Felt_File2(const std::string& filename, const std::vector<std::string>& dianaParamList, const std::map<std::string, std::string>& options)
    : filename_(filename)
{
    setOptions(options);
    feltParameters_ = FeltParameters(dianaParamList, globalParameterOptions_);
    init();
}

void Felt_File2::setOptions(const std::map<std::string, std::string>& options)
{
    // set gridParameterDelta from string ' ' splitted string of max 6 double values
    std::set<std::string> knownOptions;

    for (int i = 0; i < 6; i++)
        gridParameterDelta_[i] = 0;

    std::string optName = "gridParameterDelta";
    knownOptions.insert(optName);
    std::map<std::string, std::string>::const_iterator gridParOpt = options.find("gridParameterDelta");
    if (gridParOpt != options.end()) {
        std::vector<std::string> tokens = tokenize(gridParOpt->second);
        int end = tokens.size() < gridParameterDelta_.size() ? tokens.size() : gridParameterDelta_.size();
        for (int i = 0; i < end; ++i) {
            gridParameterDelta_.at(i) = string2type<double>(tokens[i]);
        }
        LOG4FIMEX(logger, Logger::DEBUG, "adding " << optName << " processing-option: " << gridParOpt->second);
    }

    optName = "globalParameterRestrictions";
    knownOptions.insert(optName);
    if (options.find(optName) != options.end()) {
        globalParameterOptions_ = options.find(optName)->second;
    }

    // test for unknown options
    for (std::map<std::string, std::string>::const_iterator oit = options.begin(); oit != options.end(); ++oit) {
        if (knownOptions.find(oit->first) == knownOptions.end()) {
            LOG4FIMEX(logger, Logger::WARN, "unknown processing options: " << oit->first);
        }
    }
}

void Felt_File2::init()
{
    try {
        feltFile_ = std::make_shared<felt::FeltFile>(filename_);
        if (logger->isEnabledFor(Logger::DEBUG))
            feltFile_->setLogStream(std::unique_ptr<FimexFeltLogger>(new FimexFeltLogger(logger)));
        else
            feltFile_->setLogStream(0);
        LOG4FIMEX(logger, Logger::DEBUG, "FeltParameters: " << feltParameters_);
        LOG4FIMEX(logger, Logger::DEBUG, feltFile_->information());
        for (felt::FeltFile::const_iterator ffit = feltFile_->begin(); ffit != feltFile_->end(); ++ffit) {
            felt::FeltFile::FeltFieldPtr field = *ffit;
            const felt::FeltField::Header& header = field->getHeader();
            std::string name = feltParameters_.getParameterName(header);
            if (name != UNDEFINED()) {
                findOrCreateFeltArray(field);
                if (field->verticalCoordinate() == 10) {
                    // hybrid levels, read all level-parameters once for each level-pairs
                    LevelPair lp = make_pair(field->level1(), field->level2());
                    if (hybridLevels_.find(lp) == hybridLevels_.end()) {
                        hybridLevels_[lp] = field->miscField();
                    }
                }
            } else {
                LOG4FIMEX(logger, Logger::DEBUG, "no definition for '" << join(header.begin(), header.end(), ",") << "'");
            }
        }
    } catch (runtime_error& re) {
        throw Felt_File_Error(re.what());
    }
}

Felt_File2::~Felt_File2() {}

// true = find, false = create
bool Felt_File2::findOrCreateFeltArray(std::shared_ptr<felt::FeltField> field)
{
    string name = feltParameters_.getParameterName(field->getHeader());
    string dataType = feltParameters_.getParameterDatatype(name);
    map<string, std::shared_ptr<Felt_Array2>>::iterator it = feltArrayMap_.find(name);
    if (it == feltArrayMap_.end()) {
        LOG4FIMEX(logger, Logger::DEBUG,
                  "new FeltArray " << name << ": " << dataType << " " << feltParameters_.getParameterFillValue(name)
                                   << " vTime: " << make_time_string(toFimexTime(field->validTime())));
        std::shared_ptr<Felt_Array2> fa(new Felt_Array2(name, field, dataType, feltParameters_.getParameterFillValue(name)));
        feltArrayMap_[name] = fa; // copy to map
        return false;             // reference from map
    } else {
        it->second->addInformationByField(field);
        return true;
    }
}

const std::shared_ptr<Felt_Array2> Felt_File2::getFeltArray(const string& arrayName) const
{
    map<string, std::shared_ptr<Felt_Array2>>::const_iterator it = feltArrayMap_.find(arrayName);
    if (it == feltArrayMap_.end()) {
        throw Felt_File_Error("unknown parameter: " + arrayName);
    }
    return it->second;
}

std::vector<std::shared_ptr<Felt_Array2>> Felt_File2::listFeltArrays() const
{
    vector<std::shared_ptr<Felt_Array2>> li;
    for (map<string, std::shared_ptr<Felt_Array2>>::const_iterator it = feltArrayMap_.begin(); it != feltArrayMap_.end(); ++it) {
        li.push_back(it->second);
    }
    return li;
}

template <typename T>
class Scale : public unary_function<short, T>
{
public:
    Scale(T newFillValue, double scalingFactor)
        : newFill(newFillValue)
        , scalingFactor(scalingFactor)
    {
    }
    T operator()(short val) { return (val == ANY_VALUE() ? newFill : data_caster<T, double>()(val * scalingFactor)); }

private:
    const T newFill;
    const double scalingFactor;
};

// convert felt short to a scaled Data
template <typename T>
std::shared_ptr<MetNoFimex::Data> createScaledData(const vector<short>& indata, double newFillValue, double scalingFactor)
{
    auto data = make_shared_array<T>(indata.size());
    Scale<T> scale(newFillValue, scalingFactor);
    std::transform(&indata[0], &indata[0] + indata.size(), &data[0], scale);
    return MetNoFimex::createData(indata.size(), data);
}

std::shared_ptr<MetNoFimex::Data> Felt_File2::getScaledDataSlice(std::shared_ptr<Felt_Array2> feltArray, const MetNoFimex::FimexTime& time,
                                                                 const LevelPair level)
{
    size_t dataSize = feltArray->getX() * feltArray->getY();
    vector<short> data;
    data.reserve(dataSize);
    int fieldScaleFactor = feltArray->getGridAllowDelta(time, level, data, gridParameterDelta_);

    std::shared_ptr<MetNoFimex::Data> returnData;
    if (feltArray->getDatatype() == "short") {
        if (fieldScaleFactor != feltArray->scaleFactor()) {
            throw Felt_File_Error("change in scaling factor for parameter: " + feltArray->getName() + " consider using float or double datatpye");
        }
        returnData = createScaledData<short>(data, feltArray->getFillValue(), 1.);
    } else if (feltArray->getDatatype() == "float") {
        returnData = createScaledData<float>(data, feltArray->getFillValue(), std::pow(10, static_cast<double>(fieldScaleFactor)));
    } else if (feltArray->getDatatype() == "double") {
        returnData = createScaledData<double>(data, feltArray->getFillValue(), std::pow(10, static_cast<double>(fieldScaleFactor)));
    } else {
        throw Felt_File_Error("unknown datatype for feltArray " + feltArray->getName() + ": " + feltArray->getDatatype());
    }
    return returnData;
}

std::map<short, std::vector<LevelPair>> Felt_File2::getFeltLevelPairs() const
{
    // put level values of each id into the levelSet (sort and unique)
    std::map<short, set<LevelPair, LevelPairLess>> typeLevelSet;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        vector<LevelPair> levels = fait->second->getLevelPairs();
        typeLevelSet[fait->second->getLevelType()].insert(levels.begin(), levels.end());
    }
    // convert the set into a vector
    std::map<short, std::vector<LevelPair>> typeLevelVector;
    for (std::map<short, set<LevelPair, LevelPairLess>>::iterator it = typeLevelSet.begin(); it != typeLevelSet.end(); ++it) {
        typeLevelVector[it->first] = std::vector<LevelPair>(it->second.begin(), it->second.end());
    }
    return typeLevelVector;
}

std::vector<short> Felt_File2::getEnsembleMembers() const
{
    std::set<short> ensembles;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        vector<short> em = fait->second->getEnsembleMembers();
        ensembles.insert(em.begin(), em.end());
    }
    return std::vector<short>(ensembles.begin(), ensembles.end());
}

FimexTime Felt_File2::getUniqueReferenceTime() const
{
    std::set<FimexTime> refTimes;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        std::vector<FimexTime> feltRefTimes = fait->second->getReferenceTimes();
        refTimes.insert(feltRefTimes.begin(), feltRefTimes.end());
    }
    if (refTimes.size() != 1) {
        LOG4FIMEX(logger, Logger::DEBUG, "no unique reference time found, found " << refTimes.size() << " reference times");
        throw Felt_File_Error("no unique reference time found");
    }
    // unique element, return shared ptr of it
    return *refTimes.begin();
}

std::vector<MetNoFimex::FimexTime> Felt_File2::getFeltTimes() const
{
    std::set<MetNoFimex::FimexTime> times;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        vector<MetNoFimex::FimexTime> fa_times = fait->second->getTimes();
        times.insert(fa_times.begin(), fa_times.end());
    } // times automatically unique and sorted due to set
    std::vector<MetNoFimex::FimexTime> sortedTimes(times.begin(), times.end());
    return sortedTimes;
}

int Felt_File2::getNX() const
{
    int nx = 0;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        nx = std::max(fait->second->getX(), nx);
    }
    return nx;
}

int Felt_File2::getNY() const
{
    int ny = 0;
    for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin(); fait != feltArrayMap_.end(); ++fait) {
        ny = std::max(fait->second->getY(), ny);
    }
    return ny;
}

DataPtr Felt_File2::getXData() const
{
    DataPtr xData = createData(CDM_FLOAT, getNX());
    std::shared_ptr<felt::FeltGridDefinition> gridDef = getGridDefinition();
    for (int i = 0; i < getNX(); i++) {
        float value = gridDef->startX() + i * gridDef->getXIncrement();
        xData->setValue(i, value);
    }
    return xData;
}

DataPtr Felt_File2::getYData() const
{
    DataPtr yData = createData(CDM_FLOAT, getNY());
    std::shared_ptr<felt::FeltGridDefinition> gridDef = getGridDefinition();
    float start = gridDef->startY();
    float incr = gridDef->getYIncrement();
    if (gridDef->getScanMode() == felt::FeltGridDefinition::LeftUpperHorizontal) {
        incr *= -1;
        float tmp = start;
        start = tmp - (getNY() - 1) * incr;
    }
    for (int i = 0; i < getNY(); i++) {
        float value = start + i * incr;
        yData->setValue(i, value);
    }
    return yData;
}

int Felt_File2::getGridType() const
{
    if (feltArrayMap_.size() > 0) {
        int gridType = feltArrayMap_.begin()->second->getGridType();
        for (std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator it = feltArrayMap_.begin(); it != feltArrayMap_.end(); ++it) {
            // check for changes of projection
            int otherGridType = it->second->getGridType();
            if (otherGridType != gridType) {
                ostringstream oss;
                oss << "gridType changes from: " << gridType << " to " << otherGridType << " between parameter " << feltArrayMap_.begin()->first << " and "
                    << it->first;
                throw Felt_File_Error(oss.str());
            }
        }

        return gridType;
    }
    return -1; // default
}

std::shared_ptr<felt::FeltGridDefinition> Felt_File2::getGridDefinition() const
{
    std::map<std::string, std::shared_ptr<Felt_Array2>>::const_iterator fait = feltArrayMap_.begin();
    if (feltArrayMap_.size() > 0) {
        return fait->second->getGridDefinition();
    } else {
        throw Felt_File_Error("cannot read gridParameters: no Felt_Array2 available");
    }
}

} // end namespace MetNoFelt
