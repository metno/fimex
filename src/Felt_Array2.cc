/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#include "Felt_Array2.h"
#include "FeltParameters.h"
#include "fimex/Utils.h"
#include "fimex/Logger.h"

#include "felt/FeltField.h"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <sstream>

namespace MetNoFelt {

using MetNoFimex::type2string;

static MetNoFimex::Logger_p logger = MetNoFimex::getLogger("fimex.Felt_Array2");

Felt_Array2::Felt_Array2(const string name, const std::shared_ptr<felt::FeltField> feltField, const string& dataType, double fillValue)
    : feltArrayName_(name)
    , defaultField_(feltField)
    , dataType_(dataType)
    , fillValue_(fillValue)
{
    addField_(defaultField_);
}

Felt_Array2::~Felt_Array2()
{
}

// add field to feltFields, check first for non-existence
void Felt_Array2::addField_(const std::shared_ptr<felt::FeltField> field)
{
    MetNoFimex::FimexTime time = field->validTime();
    LevelPair level;
    if (field->isEpsRunParameter()) {
        level = make_pair(field->level1(), field->dataVersion());
    } else {
        level = make_pair(field->level1(), field->level2());
    }
    LOG4FIMEX(logger, MetNoFimex::Logger::DEBUG,
              "adding field to param " << getName() << " vtime: " << MetNoFimex::make_time_string(time) << " level: " << level.first << "," << level.second
                                       << "," << field->miscField());

    TimeLevelFieldMap::iterator timeSliceIt = feltFields_.find(time);
    if (timeSliceIt != feltFields_.end()) {
        LevelFieldMap::iterator timeLevelIt = timeSliceIt->second.find(level);
        if (timeLevelIt != timeSliceIt->second.end()) {
            if (timeLevelIt->second->dataType() == field->dataType()) {
                ostringstream msg;
                msg << "level " << level.first << "," << level.second << " and time " << MetNoFimex::make_time_string(time) << " already exists for "
                    << getName();
                throw Felt_File_Error(msg.str());
            } else {
                // overwrite existing field with the new datatype field
                timeSliceIt->second[level] = field;
                LOG4FIMEX(logger, MetNoFimex::Logger::INFO,
                          "overwriting field of param " << getName() << " vtime: " << MetNoFimex::make_time_string(time) << " level: " << level.first << ","
                                                        << level.second << " with data of datatype: " << field->dataType());
            }
        } else {
            LevelFieldMap& lfm = timeSliceIt->second;
            lfm.insert(make_pair(level, field));
        }
    } else {
        LevelFieldMap lfm;
        lfm.insert(make_pair(level, field));
        feltFields_.insert(make_pair(time, lfm));
    }
}

void Felt_Array2::addInformationByField(const std::shared_ptr<felt::FeltField> field)
{
    if (!field->valid()) {
        // no data, no field
        return;
    }
    if (!((defaultField_->parameter() == field->parameter()) && (defaultField_->verticalCoordinate() == field->verticalCoordinate()))) {
        ostringstream msg;
        msg << "inexact definition of parameter "<< getName()<< ":";
        if (field->parameter() != defaultField_->parameter()) {
            msg << " parameter-id changed from " << defaultField_->parameter() << " to " << field->parameter();
        }
        if (field->verticalCoordinate() != defaultField_->verticalCoordinate()) {
            msg << " verticalCoordinate-id changed from " << defaultField_->verticalCoordinate() << " to " << field->verticalCoordinate();
        }
        throw Felt_File_Error(msg.str());
    }
    addField_(field);
}

vector<MetNoFimex::FimexTime> Felt_Array2::getReferenceTimes() const
{
    vector<MetNoFimex::FimexTime> refTimes;
    refTimes.reserve(feltFields_.size());
    for (TimeLevelFieldMap::const_iterator tlm = feltFields_.begin(); tlm != feltFields_.end(); ++tlm) {
        for (LevelFieldMap::const_iterator lm = tlm->second.begin(); lm != tlm->second.end(); ++lm) {
            std::shared_ptr<felt::FeltField> field = lm->second;
            refTimes.push_back(field->referenceTime());
        }
    }
    return refTimes;
}

vector<MetNoFimex::FimexTime> Felt_Array2::getTimes() const
{
    vector<MetNoFimex::FimexTime> vTimes;
    if (!hasTime()) return vTimes;

    vTimes.reserve(feltFields_.size());
    for (TimeLevelFieldMap::const_iterator tlm = feltFields_.begin(); tlm != feltFields_.end(); ++tlm) {
        vTimes.push_back(tlm->first);
    }
    return vTimes;
}

vector<LevelPair> Felt_Array2::getLevelPairs() const {
    set<LevelPair, LevelPairLess> lset; // unique, sorted set of levels
    for (TimeLevelFieldMap::const_iterator tlm = feltFields_.begin(); tlm != feltFields_.end(); ++tlm) {
        for (LevelFieldMap::const_iterator lm = tlm->second.begin(); lm != tlm->second.end(); ++lm) {
            if (defaultField_->isEpsRunParameter()) {
                // make sure, level2 is dataVersion 0 for all pairs
                LevelPair lp = lm->first;
                lp.second = 0;
                lset.insert(lp);
            } else {
                lset.insert(lm->first);
            }
        }
    }
    vector<LevelPair> retVal(lset.begin(), lset.end());
    // retVal is sorted since the set is sorted
    return retVal;
}

vector<short> Felt_Array2::getEnsembleMembers() const {
    set<short> ensembleMembers;
    if (defaultField_->isEpsRunParameter()) {
        for (TimeLevelFieldMap::const_iterator tlm = feltFields_.begin(); tlm != feltFields_.end(); ++tlm) {
            for (LevelFieldMap::const_iterator lm = tlm->second.begin(); lm != tlm->second.end(); ++lm) {
                ensembleMembers.insert(lm->first.second);
            }
        }
    }
    return vector<short>(ensembleMembers.begin(), ensembleMembers.end());
}

const std::shared_ptr<felt::FeltField> Felt_Array2::getField(const MetNoFimex::FimexTime& time, LevelPair levelPair) const
{
    TimeLevelFieldMap::const_iterator tlm;
    if (hasTime()) {
        tlm = feltFields_.find(time);
    } else {
        tlm = feltFields_.begin();
    }
    if (tlm != feltFields_.end()) {
        LevelFieldMap::const_iterator lm = tlm->second.find(levelPair);
        if (lm != tlm->second.end()) {
            return lm->second;
        }
    }
    throw NoSuchField_Felt_File_Error("time/pair value not found in field");
}

int Felt_Array2::getIdent19(const MetNoFimex::FimexTime& time, LevelPair levelPair) const
{
    return getField(time, levelPair)->miscField();
}

int Felt_Array2::scaleFactor() const
{
    return defaultField_->scaleFactor();
}

double Felt_Array2::getScalingFactor() const
{
    return (dataType_ == "short") ? std::pow(10,static_cast<double>(defaultField_->scaleFactor())) : 1;
}

const string& Felt_Array2::getName() const
{
    return feltArrayName_;
}

int Felt_Array2::getLevelType() const
{
    return defaultField_->verticalCoordinate();
}
/** return x/longitude size */
int Felt_Array2::getX() const
{
    return defaultField_->xNum();
}

/** return y/latitude size */
int Felt_Array2::getY() const
{
    return defaultField_->yNum();
}
int Felt_Array2::getGrid(const MetNoFimex::FimexTime& time, LevelPair levelPair, vector<short>& gridOut)
{
    std::array<float, 6> nullDelta;
    for (int i = 0; i < 6; i++) nullDelta[i] = 0;
    return getGridAllowDelta(time, levelPair, gridOut, nullDelta);
}

int Felt_Array2::getGridAllowDelta(const MetNoFimex::FimexTime& time, LevelPair levelPair, vector<short>& gridOut,
                                   const std::array<float, 6>& gridParameterDelta)
{
    const std::shared_ptr<felt::FeltField>& field = getField(time, levelPair);

    // make consistency checks
     int fieldGridType = field->gridType();
     fieldGridType = (fieldGridType >= 1000) ? (fieldGridType / 1000) : fieldGridType;
     if (getGridType() != fieldGridType)
         throw Felt_File_Error("gridType changes from "+type2string(getGridType()) +" to " + type2string(fieldGridType) + " in parameter " + getName());

    // set the output data
    field->grid(gridOut);

     // check parameters against delta
    const std::array<float, 6> newParams = field->projectionInformation()->getGridParameters();
    const std::array<float, 6> defaultParams = defaultField_->projectionInformation()->getGridParameters();
    for (int i = 0; i < 6; i++) {
        // allow params to differ by a delta (optional)
        if (newParams[i] != defaultParams[i] && std::fabs(newParams[i]-defaultParams[i]) > gridParameterDelta[i]) {
            throw Felt_File_Error("cannot change gridParameters within a file for " + getName() + " gridParameter (c-counting) " + type2string(i) + ": " + type2string(defaultParams[i]) + " != " + type2string(newParams[i]) + "("+type2string(newParams[i]-defaultParams[i])+")");
        }
    }
    return field->scaleFactor();
}

int Felt_Array2::getGridType() const
{
    int gridType = defaultField_->gridType();
    gridType = (gridType >= 1000) ? (gridType / 1000) : gridType;
    return gridType;
}
/** @return true if grid has a time-axis, i.e. not a parameter field **/
bool Felt_Array2::hasTime() const
{
    if (feltFields_.size() == 1 &&
            defaultField_->dataType() == 4) {
        // felt time-datatype == parameter-field, no time dimension
        return false;
    }
    return true;
}

std::shared_ptr<felt::FeltGridDefinition> Felt_Array2::getGridDefinition() const
{
    return defaultField_->projectionInformation();
}


} // end namespace MetNoFelt
