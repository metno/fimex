/*
 * Fimex, FillWriter.cc
 *
 * (C) Copyright 2013-2022, met.no
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

#include "fimex/CDM.h"
#include "fimex/CDMException.h"
#include "fimex/Data.h"
#include "fimex/Logger.h"
#include "fimex/SliceBuilder.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/XMLDoc.h"

#include <algorithm>
#include <map>
#include <set>
#include <utility>
#include <vector>

#include <libxml/tree.h>
#include <libxml/xpath.h>

namespace MetNoFimex {

namespace {
Logger_p logger = getLogger("fimex.FillWriter");

struct FillWriterTranslation {
    std::string inputDim;
    std::string outputDim;
    std::vector<std::size_t> inSlices;
    std::vector<std::size_t> outSlices;
};

std::vector<FillWriterTranslation> readConfigFile(const std::string& fileName)
{
    std::vector<FillWriterTranslation> fwts;
    if (fileName.empty())
        return fwts;

    XMLDoc doc(fileName);
    doc.registerNamespace("c", "http://www.met.no/schema/fimex/cdmFillWriterConfig");
    xmlXPathObject_p xpathObj = doc.getXPathObject("/c:cdmFillWriter");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    if (nodes->nodeNr != 1) {
        throw CDMException("file '" + fileName + "' is not a cdmFillWriterConfig with root /cdmFillWriter");
    }
    // translate
    xpathObj = doc.getXPathObject("/c:cdmFillWriter/c:translate");
    nodes = xpathObj->nodesetval;
    const int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        FillWriterTranslation fwt;
        fwt.inputDim = getXmlProp(nodes->nodeTab[i], "inputDimension");
        fwt.outputDim = getXmlProp(nodes->nodeTab[i], "outputDimension");
        const auto iSlices = getXmlProp(nodes->nodeTab[i], "inputSlices");
        const auto oSlices = getXmlProp(nodes->nodeTab[i], "outputSlices");
        fwt.inSlices = tokenizeDotted<size_t>(iSlices);
        fwt.outSlices = tokenizeDotted<size_t>(oSlices);
        if (fwt.outSlices.size() > 1) {
            if (fwt.inSlices.size() > 0) {
                if (fwt.outSlices.size() != fwt.inSlices.size()) {
                    throw CDMException("fillWriterConfig: inputSlices != outputSlices for " + fwt.inputDim);
                }
            }
        }
        if (fwt.inSlices.size() > 1) {
            if (fwt.outSlices.size() > 0) {
                if (fwt.outSlices.size() != fwt.inSlices.size()) {
                    throw CDMException("fillWriterConfig: inputSlices != outputSlices for " + fwt.inputDim);
                }
            }
        }
        fwts.push_back(fwt);
    }
    return fwts;
}

/**
 * find the first shape-element wich is in the unusableDims set
 * @param unusableDims set with dimensions which should not be used
 * @param shape dimensions of a variable
 * @return first string in shape which maches unusableDims
 */
std::string findFirstUnusableDimension(const std::set<std::string>& unusableDims, const std::vector<std::string>& shape)
{
    for (const auto shp : shape) {
        if (unusableDims.find(shp) != unusableDims.end()) {
            return shp;
        }
    }
    return std::string();
}
} // namespace

FillWriter::FillWriter(CDMReader_p in, CDMReaderWriter_p io, std::string configFileName)
{
    using namespace std;

    const auto fwts = readConfigFile(configFileName);

    const CDM& iCdm = in->getCDM();
    const CDM& oCdm = io->getCDM();

    // find the dimensions which are different
    // [dimensionName][inputPos] = outputPos
    map<string, map<size_t, size_t> > dimSlices;
    const auto& iDims = iCdm.getDimensions();
    const auto& oDims = oCdm.getDimensions();
    std::set<std::string> unusableIDims;
    for (const auto& iDim : iDims) {
        const auto oDimsIt = find_if(oDims.begin(), oDims.end(), CDMNameEqual(iDim.getName()));
        if (oDimsIt == oDims.end()) {
            LOG4FIMEX(logger, Logger::INFO, "dimension '" + iDim.getName() + "' not found in output-file, ignoring");
            continue;
        }
        if (iCdm.hasVariable(iDim.getName()) && oCdm.hasVariable(oDimsIt->getName())) {
            // rewrite coordinate-variables
            auto iDimData = in->getScaledData(iDim.getName())->asDouble();
            auto oDimData = io->getScaledData(iDim.getName())->asDouble();
            size_t changes = 0;
            size_t unLimCount = 0;
            map<size_t, size_t> currentSlices;
            for (size_t i = 0; i < iDim.getLength(); ++i) {
                double* found = find(&oDimData[0], &oDimData[0] + oDimsIt->getLength(), iDimData[i]);
                size_t oPos = distance(&oDimData[0], found);
                if (oPos == oDimsIt->getLength()) {
                    // not found, put at first position after output
                    if (oDimsIt->isUnlimited()) {
                        oPos = oDimsIt->getLength() + unLimCount++;
                    } else {
                        unusableIDims.insert(iDim.getName());
                        LOG4FIMEX(logger, Logger::ERROR, "Dimension-value error for '" << oDimsIt->getName() << "': value '" << iDimData[i] << "' not found");
                        continue;
                    }
                }
                currentSlices[i] = oPos;
                if (i != oPos) {
                    ++changes;
                    LOG4FIMEX(logger, Logger::DEBUG, "found slice of " << iDim.getName() << ", value '" << iDimData[i] << "' in output at pos: " << oPos);
                }
            }
            if (changes > 0 || (iDim.getLength() != oDimsIt->getLength())) {
                LOG4FIMEX(logger, Logger::DEBUG, "dimensions changed for  " << iDim.getName());
                dimSlices[iDim.getName()] = currentSlices;
            }
        } else {
            if (oDimsIt->isUnlimited()) {
                LOG4FIMEX(logger, Logger::INFO, "no dimension-variable for '" << iDim.getName() << "': appending as unlimited dimension'");
                for (size_t i = 0; i < iDim.getLength(); ++i) {
                    dimSlices[oDimsIt->getName()][i] = oDimsIt->getLength()+i;
                }
            } else if (iDim.getLength() == oDimsIt->getLength()) {
                LOG4FIMEX(logger, Logger::INFO, "no dimension-variable for '" << iDim.getName() << "': keeping as is'");
            } else {
                unusableIDims.insert(iDim.getName());
                LOG4FIMEX(logger, Logger::ERROR, "Dimension-missing in fill-output: '" << iDim.getName());
            }
        }
        if (unusableIDims.find(iDim.getName()) == unusableIDims.end()) {
            continue;
        }
        if (iDim.isUnlimited() && (dimSlices.find(iDim.getName()) == dimSlices.end())) {
            // iDims = oDims, but slice anyway since it is unLimited (save memory!)
            LOG4FIMEX(logger, Logger::DEBUG, "dimensions changed for  " << iDim.getName() << " since it is unlimited");
            for (size_t i = 0; i < iDim.getLength(); ++i) {
                dimSlices[oDimsIt->getName()][i] = i;
            }
        }
    }

    // process variables
    const auto iVars = iCdm.getVariables();
    const int varVecSize = iVars.size();
#ifdef _OPENMP
#pragma omp parallel for default(shared)
#endif
    for (int iVar = 0; iVar < varVecSize; iVar++) {
        const auto& iv = iVars.at(iVar);
        LOG4FIMEX(logger, Logger::DEBUG, "processing variable  '" << iv.getName() << "'");
        if (!oCdm.hasVariable(iv.getName())) {
            LOG4FIMEX(logger, Logger::WARN, "new variable '" << iv.getName() << "': omitting");
            continue;
        }
        const auto& iShape = iv.getShape();
        const auto badDim = findFirstUnusableDimension(unusableIDims, iShape);
        if (!badDim.empty()) {
            LOG4FIMEX(logger, Logger::ERROR, "Cannot fill-write '" << iv.getName() << "' due to bad dimension: '" << badDim << "'");
            continue;
        }
        const auto& oVar = oCdm.getVariable(iv.getName());
        const auto& oShape = oVar.getShape();

        vector<string> iTestShape(iShape.begin(), iShape.end());
        vector<string> oTestShape(oShape.begin(), oShape.end());
        for (const auto& fit : fwts) {
            // translated dimensions are handled different from normal dimension, so omitting from regular test
            iTestShape.erase(std::remove(iTestShape.begin(), iTestShape.end(), fit.inputDim), iTestShape.end());
            oTestShape.erase(std::remove(oTestShape.begin(), oTestShape.end(), fit.outputDim), oTestShape.end());
        }
        // simple test of equal shapes (omitting translated dims)
        if (!equal(iTestShape.begin(), iTestShape.end(), oTestShape.begin())) {
            LOG4FIMEX(logger, Logger::WARN, "variable '" << iv.getName() << "' has different shape: omitting");
            continue;
        }

        typedef vector<pair<SliceBuilder, SliceBuilder> > SlicePairs;
        SliceBuilder inputSb(iCdm, iv.getName());
        SliceBuilder outputSb(oCdm, iv.getName(), true); // extension along unlimited possible for output
        // translate dimensions of input and output slice from config-file
        for (const auto& fit : fwts) {
            // reduce the input slicebuilder
            if (std::find(iShape.begin(), iShape.end(), fit.inputDim) != iShape.end()) {
                if (fit.inSlices.size() == 1) { // 0 = do nothing
                    // cerr << "reducing input dim: " << fit->outputDim;
                    inputSb.setStartAndSize(fit.inputDim, fit.inSlices.at(0), 1);
                } else {
                    // TODO
                    throw CDMException("FillWriter translation to with multiple elements in slice not implemented yet");
                }
            }
            // reduce the output slicebuilder
            if (std::find(oShape.begin(), oShape.end(), fit.outputDim) != oShape.end()) {
                if (fit.outSlices.size() == 1) { // 0 = do nothing
                    // cerr << "reducing output dim: " << fit->outputDim;
                    outputSb.setStartAndSize(fit.outputDim, fit.outSlices.at(0), 1);
                } else {
                    // TODO
                    throw CDMException("FillWriter translation to with multiple elements in slice not implemented yet");
                }
            }
        }

        SlicePairs slices = {{inputSb, outputSb}};
        // loop over the untranslated slices
        for (const auto& dimIt : iTestShape) {
            const auto dsIt = dimSlices.find(dimIt);
            if (dsIt != dimSlices.end()) {
                SlicePairs newSlices;
                const auto& currentDimSlices = dsIt->second;
                for (const auto& oldSlice : slices) {
                    for (const auto& slicePos : currentDimSlices) {
                        SliceBuilder isb = oldSlice.first;  // create a copy of the existing one
                        SliceBuilder osb = oldSlice.second; // create a copy
                        isb.setStartAndSize(dimIt, slicePos.first, 1);
                        osb.setStartAndSize(dimIt, slicePos.second, 1);
                        newSlices.push_back(make_pair(isb, osb));
                    }
                }
                slices = newSlices;
            }
        }

        LOG4FIMEX(logger, Logger::DEBUG, "processing " << iv.getName() << " with " << slices.size() << " slices");
        for (const auto& slice : slices) {
            LOG4FIMEX(logger, Logger::DEBUG, "processing " << iv.getName() << " with:  " << endl << slice.first << endl << slice.second);
            DataPtr inData = in->getDataSlice(iv.getName(), slice.first);
            io->putDataSlice(iv.getName(), slice.second, inData);
        }
    }
    if (!unusableIDims.empty())
        throw CDMException("FillWriter finished with errors in dimension-mapping");
}

FillWriter::~FillWriter()
{
}

} /* namespace MetNoFimex */
