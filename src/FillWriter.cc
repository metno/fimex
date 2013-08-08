/*
 * Fimex, FillWriter.cc
 *
 * (C) Copyright 2013, met.no
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
 *
 *  Created on: Mar 19, 2013
 *      Author: heikok
 */

#include "fimex/FillWriter.h"
#include "fimex/Logger.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

namespace MetNoFimex
{

FillWriter::FillWriter(boost::shared_ptr<CDMReader> in, boost::shared_ptr<CDMReaderWriter> io)
{
    using namespace std;
    LoggerPtr logger = getLogger("fimex.FillWriter");

    const CDM& iCdm = in->getCDM();
    const CDM& oCdm = io->getCDM();

    // find the dimensions which are different
    // [dimensionName][inputPos] = outputPos
    map<string, map<size_t, size_t> > dimSlices;
    const CDM::DimVec iDims = iCdm.getDimensions();
    const CDM::DimVec oDims = oCdm.getDimensions();
    for (CDM::DimVec::const_iterator iDimsIt = iDims.begin(); iDimsIt != iDims.end(); ++iDimsIt) {
        CDM::DimVec::const_iterator oDimsIt = find_if(oDims.begin(), oDims.end(), CDMNameEqual(iDimsIt->getName()));
        if (oDimsIt == oDims.end()) {
            throw CDMException("Cannot find name '" + iDimsIt->getName() + "' in output, can't fill");
        }
        if (iCdm.hasVariable(iDimsIt->getName()) && oCdm.hasVariable(oDimsIt->getName())) {
            boost::shared_array<double> iDimData = in->getScaledData(iDimsIt->getName())->asDouble();
            boost::shared_array<double> oDimData = io->getScaledData(iDimsIt->getName())->asDouble();
            size_t changes = 0;
            size_t unLimCount = 0;
            map<size_t, size_t> currentSlices;
            for (size_t i = 0; i < iDimsIt->getLength(); ++i) {
                double* found = find(&oDimData[0], &oDimData[0] + oDimsIt->getLength(), iDimData[i]);
                size_t oPos = distance(&oDimData[0], found);
                if (oPos == oDimsIt->getLength()) {
                    // not found, put at first position after output
                    if (oDimsIt->isUnlimited()) {
                        oPos = oDimsIt->getLength() + unLimCount++;
                    } else {
                        throw CDMException("Could  not find value '" + type2string(iDimData[i])+"' in fill-output of dimension " + oDimsIt->getName());
                    }
                }
                currentSlices[i] = oPos;
                if (i != oPos) {
                    ++changes;
                    LOG4FIMEX(logger, Logger::DEBUG, "found slice of " <<iDimsIt->getName() << ", value '" << iDimData[i] << "' in output at pos: " << oPos);
                }
            }
            if (changes > 0 || (iDimsIt->getLength() != oDimsIt->getLength())) {
                LOG4FIMEX(logger, Logger::DEBUG, "dimensions changed for  " << iDimsIt->getName());
                dimSlices[iDimsIt->getName()] = currentSlices;
            }
        } else {
            if (oDimsIt->isUnlimited()) {
                LOG4FIMEX(logger, Logger::INFO, "no dimension-variable for '" << iDimsIt->getName() <<"': appending as unlimited dimension'");
                for (size_t i = 0; i < iDimsIt->getLength(); ++i) {
                    dimSlices[oDimsIt->getName()][i] = oDimsIt->getLength()+i;
                }
            } else if (iDimsIt->getLength() == oDimsIt->getLength()){
                LOG4FIMEX(logger, Logger::INFO, "no dimension-variable for '" << iDimsIt->getName() <<"': keeping as is'");
            } else {
                throw CDMException("no dimension-variable for '" + iDimsIt->getName() + "' and neither unlimited nor equal");
            }
        }

        if (iDimsIt->isUnlimited() && (dimSlices.find(iDimsIt->getName()) == dimSlices.end())) {
            // iDims = oDims, but slice anyway since it is unLimited (save memory!)
            LOG4FIMEX(logger, Logger::DEBUG, "dimensions changed for  " << iDimsIt->getName() << " since it is unlimited");
            for (size_t i = 0; i < iDimsIt->getLength(); ++i) {
                dimSlices[oDimsIt->getName()][i] = i;
            }
        }
    }


    // process variables
    CDM::VarVec iVars = iCdm.getVariables();
    for (CDM::VarVec::iterator iv = iVars.begin(); iv != iVars.end(); ++iv) {
        LOG4FIMEX(logger, Logger::DEBUG, "processing variable  " << iv->getName());
        if (!oCdm.hasVariable(iv->getName())) {
            LOG4FIMEX(logger, Logger::WARN, "new variable " << iv->getName() << ": omitting");
            continue;
        }
        const CDMVariable& iVar = *iv;
        const vector<string>& iShape = iVar.getShape();
        const CDMVariable& oVar = oCdm.getVariable(iv->getName());
        const vector<string>& oShape = oVar.getShape();

        if (!equal(iShape.begin(), iShape.end(), oShape.begin())) {
            LOG4FIMEX(logger, Logger::WARN, "variable " << iv->getName() << "has different shape: omitting");
            continue;
        }

        typedef vector<pair<SliceBuilder, SliceBuilder> > SlicePairs;
        SlicePairs slices;
        slices.push_back(make_pair(SliceBuilder(iCdm, iv->getName()), SliceBuilder(oCdm, iv->getName(), true)));
        for (vector<string>::const_iterator dimIt = iShape.begin(); dimIt != iShape.end(); ++dimIt) {
            if (dimSlices.find(*dimIt) != dimSlices.end()) {
                SlicePairs newSlices;
                map<size_t, size_t> currentDimSlices = dimSlices.find(*dimIt)->second;
                for (SlicePairs::iterator oldSliceIt = slices.begin(); oldSliceIt != slices.end(); ++oldSliceIt) {
                    for (map<size_t, size_t>::iterator slicePosIt = currentDimSlices.begin(); slicePosIt != currentDimSlices.end(); ++slicePosIt) {
                        SliceBuilder isb = oldSliceIt->first; // create a copy of the existing one
                        SliceBuilder osb = oldSliceIt->second; // create a copy
                        isb.setStartAndSize(*dimIt, slicePosIt->first, 1);
                        osb.setStartAndSize(*dimIt, slicePosIt->second, 1);
                        newSlices.push_back(make_pair(isb, osb));
                    }
                }
                slices = newSlices;
            }
        }

        LOG4FIMEX(logger, Logger::DEBUG, "processing "<< iv->getName() << " with " << slices.size() << " slices");
        for (SlicePairs::iterator sliceIt = slices.begin(); sliceIt != slices.end(); ++sliceIt) {
            LOG4FIMEX(logger, Logger::DEBUG, "processing "<< iv->getName() << " with:  " << endl << sliceIt->first << endl << sliceIt->second);
            io->putDataSlice(iv->getName(), sliceIt->second, in->getDataSlice(iv->getName(), sliceIt->first));
        }
    }
}

FillWriter::~FillWriter()
{
}

} /* namespace MetNoFimex */
