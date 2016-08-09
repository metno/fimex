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
#include "fimex/Utils.h"
#include "fimex/XMLDoc.h"
#include <vector>
#include <map>
#include <utility>
#include <algorithm>
#include <libxml/tree.h>
#include <libxml/xpath.h>

namespace MetNoFimex
{
static LoggerPtr logger = getLogger("fimex.FillWriter");

struct FillWriterTranslation {
    std::string inputDim;
    std::string outputDim;
    std::vector<std::size_t> inSlices;
    std::vector<std::size_t> outSlices;
};

std::vector<FillWriterTranslation> readConfigFile(const std::string& fileName);

/**
 * find the first shape-element wich is in the unusableDims set
 * @param unusableDims set with dimensions which should not be used
 * @param shape dimensions of a variable
 * @return first string in shape which maches unusableDims
 */
static std::string findFirstUnusableDimension(const std::set<std::string>& unusableDims, const std::vector<std::string>& shape)
{
    for (std::vector<std::string>::const_iterator shapeIt = shape.begin(); shapeIt != shape.end(); ++shapeIt) {
        if (unusableDims.find(*shapeIt) != unusableDims.end()) {
            return *shapeIt;
        }
    }
    return "";
}

FillWriter::FillWriter(boost::shared_ptr<CDMReader> in, boost::shared_ptr<CDMReaderWriter> io, std::string configFileName)
{
    using namespace std;

    std::vector<FillWriterTranslation> fwts = readConfigFile(configFileName);

    const CDM& iCdm = in->getCDM();
    const CDM& oCdm = io->getCDM();

    // find the dimensions which are different
    // [dimensionName][inputPos] = outputPos
    map<string, map<size_t, size_t> > dimSlices;
    const CDM::DimVec iDims = iCdm.getDimensions();
    const CDM::DimVec oDims = oCdm.getDimensions();
    std::set<std::string> unusableIDims;
    for (CDM::DimVec::const_iterator iDimsIt = iDims.begin(); iDimsIt != iDims.end(); ++iDimsIt) {
        CDM::DimVec::const_iterator oDimsIt = find_if(oDims.begin(), oDims.end(), CDMNameEqual(iDimsIt->getName()));
        if (oDimsIt == oDims.end()) {
            LOG4FIMEX(logger, Logger::INFO, "dimension '"+ iDimsIt->getName() + "' not found in output-file, ignoring");
            continue;
        }
        if (iCdm.hasVariable(iDimsIt->getName()) && oCdm.hasVariable(oDimsIt->getName())) {
            // rewrite coordinate-variables
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
                        unusableIDims.insert(iDimsIt->getName());
                        LOG4FIMEX(logger, Logger::ERROR, "Dimension-value error for '" << oDimsIt->getName() << "': value '" << iDimData[i] << "' not found");
                        continue;
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
                unusableIDims.insert(iDimsIt->getName());
                LOG4FIMEX(logger, Logger::ERROR, "Dimension-missing in fill-output: '" << iDimsIt->getName());
            }
        }
        if (unusableIDims.find(iDimsIt->getName()) == unusableIDims.end()) {
            continue;
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
    int varVecSize = iVars.size();
#ifdef _OPENMP
#pragma omp parallel for default(shared)
#endif
    for (int iVar = 0; iVar < varVecSize; iVar++) {
    //for (CDM::VarVec::iterator iv = iVars.begin(); iv != iVars.end(); ++iv) {
        CDMVariable* iv = &iVars.at(iVar);
        LOG4FIMEX(logger, Logger::DEBUG, "processing variable  '" << iv->getName()<< "'");
        if (!oCdm.hasVariable(iv->getName())) {
            LOG4FIMEX(logger, Logger::WARN, "new variable '" << iv->getName() << "': omitting");
            continue;
        }
        const vector<string>& iShape = iv->getShape();
        string badDim = findFirstUnusableDimension(unusableIDims, iShape);
        if (badDim != "") {
            LOG4FIMEX(logger, Logger::ERROR, "Cannot fill-write '" << iv->getName() << "' due to bad dimension: '" << badDim << "'");
            continue;
        }
        const CDMVariable& oVar = oCdm.getVariable(iv->getName());
        const vector<string>& oShape = oVar.getShape();

        vector<string> iTestShape(iShape.begin(), iShape.end());
        vector<string> oTestShape(oShape.begin(), oShape.end());
        for (vector<FillWriterTranslation>::iterator fit = fwts.begin(); fit != fwts.end(); ++fit) {
            // translated dimensions are handled different from normal dimension, so omitting from regular test
            iTestShape.erase( std::remove( iTestShape.begin(), iTestShape.end(), fit->inputDim ), iTestShape.end() );
            oTestShape.erase( std::remove( oTestShape.begin(), oTestShape.end(), fit->outputDim ), oTestShape.end() );
        }
        // simple test of equal shapes (omitting translated dims
        if (!equal(iTestShape.begin(), iTestShape.end(), oTestShape.begin())) {
            LOG4FIMEX(logger, Logger::WARN, "variable '" << iv->getName() << "' has different shape: omitting");
            continue;
        }

        typedef vector<pair<SliceBuilder, SliceBuilder> > SlicePairs;
        SliceBuilder inputSb(iCdm, iv->getName());
        SliceBuilder outputSb(oCdm, iv->getName(), true); // extension along unlimited possible for output
        // translate dimensions of input and output slice from config-file
        for (vector<FillWriterTranslation>::iterator fit = fwts.begin(); fit != fwts.end(); ++fit) {
            // reduce the input slicebuilder
            if (std::find(iShape.begin(), iShape.end(), fit->inputDim) != iShape.end()) {
                if (fit->inSlices.size() == 1) { // 0 = do nothing
                    //cerr << "reducing input dim: " << fit->outputDim;
                    inputSb.setStartAndSize(fit->inputDim, fit->inSlices.at(0), 1);
                } else {
                    // TODO
                    throw CDMException("FillWriter translation to with multiple elements in slice not implemented yet");
                }
            }
            // reduce the output slicebuilder
            if (std::find(oShape.begin(), oShape.end(), fit->outputDim) != oShape.end()) {
                if (fit->outSlices.size() == 1) { // 0 = do nothing
                    //cerr << "reducing output dim: " << fit->outputDim;
                    outputSb.setStartAndSize(fit->outputDim, fit->outSlices.at(0), 1);
                } else {
                    // TODO
                    throw CDMException("FillWriter translation to with multiple elements in slice not implemented yet");
                }
            }
        }

        SlicePairs slices;
        slices.push_back(make_pair(inputSb, outputSb));
        // loop over the untranslated slices
        for (vector<string>::const_iterator dimIt = iTestShape.begin(); dimIt != iTestShape.end(); ++dimIt) {
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
            DataPtr inData = in->getDataSlice(iv->getName(), sliceIt->first);
            io->putDataSlice(iv->getName(), sliceIt->second, inData);
        }
    }
    if (unusableIDims.size() > 0) throw CDMException("FillWriter finished with errors in dimension-mapping");
}

FillWriter::~FillWriter()
{
}

std::vector<FillWriterTranslation> readConfigFile(const std::string& fileName)
{
    std::vector<FillWriterTranslation> fwts;
    if (fileName == "") return fwts;

    XMLDoc doc(fileName);
    doc.registerNamespace("c", "http://www.met.no/schema/fimex/cdmFillWriterConfig");
    XPathObjPtr xpathObj = doc.getXPathObject("/c:cdmFillWriter");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    if (nodes->nodeNr != 1) {
        throw CDMException("file '"+fileName+"' is not a cdmFillWriterConfig with root /cdmFillWriter");
    }
    // translate
    xpathObj = doc.getXPathObject("/c:cdmFillWriter/c:translate");
    nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    for (int i = 0; i < size; i++) {
        FillWriterTranslation fwt;
        fwt.inputDim = getXmlProp(nodes->nodeTab[i], "inputDimension");
        fwt.outputDim = getXmlProp(nodes->nodeTab[i], "outputDimension");
        std::string iSlices = getXmlProp(nodes->nodeTab[i], "inputSlices");
        std::string oSlices = getXmlProp(nodes->nodeTab[i], "outputSlices");
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

} /* namespace MetNoFimex */
