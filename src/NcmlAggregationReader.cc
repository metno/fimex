/*
 * Fimex, NcmlAggregationReader.cc
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
 *  Created on: Apr 17, 2013
 *      Author: heikok
 */

#include "NcmlAggregationReader.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/XMLDoc.h"
#include "fimex/Logger.h"
#include "fimex/Utils.h"
#include "fimex/CDMFileReaderFactory.h"
#define MIFI_IO_READER_SUPPRESS_DEPRECATED
#include "fimex/NetCDF_CDMReader.h"
#include "fimex/CDM.h"
#include "fimex/Data.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <boost/regex.hpp>
#include <boost/filesystem.hpp>

namespace MetNoFimex
{

using namespace std;

void getFileTypeConfig(const string& location, string& file, string& type, string& config)
{
    vector<string> locations = tokenize(location, " ");
    file = (locations.size() > 0) ? locations.at(0) : "";
    type = (locations.size() > 1) ? locations.at(1) : "";
    config = (locations.size() > 2) ? locations.at(2) : "";
    return;
}

void scanFiles(vector<string>& files, const boost::filesystem::path& dir, int depth, const boost::regex& regexp, int depthCount = 0)
{
    LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "scanning directory " + dir.string());
    if (depthCount > 1000) {
        throw CDMException("possible circular reference: more than 1000 subdirectories found at " + dir.string());
    }
    using namespace boost::filesystem;
    vector<path> entries;
    copy(directory_iterator(dir), directory_iterator(), back_inserter(entries));
    sort(entries.begin(), entries.end());

    for (vector<path>::iterator e = entries.begin(); e != entries.end(); ++e) {
        file_status lstat(symlink_status(*e));
        if (lstat.type() == directory_file) {
            if (depth != 0) {
                scanFiles(files, *e, depth-1, regexp, depthCount+1);
            }
        } else if (lstat.type() == regular_file) {
            string filename = e->filename().string();
            if (boost::regex_match(filename, regexp)) {
                files.push_back(e->string());
            }
        }
    }
}

NcmlArregationReader::NcmlArregationReader(const XMLInput& ncml)
{
    boost::shared_ptr<XMLDoc> doc;
    if (!ncml.isEmpty()) {
        doc = ncml.getXMLDoc();
        doc->registerNamespace("nc", "http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2");
        XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        if (nodes->nodeNr != 1) {
            throw CDMException("config "+ncml.id()+" is not a ncml document with root /nc:netcdf");
        }
    }

    // warn if multiple aggregations, joinNew aggregations
    // and get a global file if no aggregations
    XPathObjPtr xpathObj = doc->getXPathObject("/nc:netcdf/nc:aggregation");
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    int size = (nodes) ? nodes->nodeNr : 0;
    if (size == 0) {
        LOG4FIMEX(getLogger("fimex/NcmlCDMReader"), Logger::DEBUG, "found no ncml-aggregations in "<< ncml.id());
        XPathObjPtr xpathObjL = doc->getXPathObject("/nc:netcdf[@location]");
        xmlNodeSetPtr nodesL = xpathObjL->nodesetval;
        if (nodesL->nodeNr != 1) {
            LOG4FIMEX(getLogger("fimex/NcmlCDMReader"), Logger::INFO, "config " << ncml.id() << " does not contain location-attribute, only ncml initialization");
        } else {
            string file, type, config;
            getFileTypeConfig(getXmlProp(nodesL->nodeTab[0], "location"), file, type, config);
            if (type == "" || type == "netcdf" || type == "nc" || type == "nc4") {
                // remove file: URL-prefix
                file = boost::regex_replace(file, boost::regex("^file:"), "", boost::format_first_only);
                // java-netcdf allows dods: prefix for dods-files while netcdf-C requires http:
                file = boost::regex_replace(file, boost::regex("^dods:"), "http:", boost::format_first_only);
                LOG4FIMEX(getLogger("fimex/NcmlCDMReader"), Logger::DEBUG, "reading netcdf-file:  ");
                gDataReader_ = boost::shared_ptr<CDMReader>(new NetCDF_CDMReader(file));
            } else {
                LOG4FIMEX(getLogger("fimex/NcmlCDMReader"), Logger::DEBUG, "reading file:  "<< file << ", " << type << ", " << config);
                gDataReader_ = CDMFileReaderFactory::create(type, file, config);
            }
            *(this->cdm_) = gDataReader_->getCDM();
        }
    } else {
        if (size > 1) {
            LOG4FIMEX(getLogger("fimex/NcmlCDMReader"), Logger::WARN, "found several ncml-aggregations in " << ncml.id() << ", using 1st");
        }
        // find sources from location, scan, ...
        XPathObjPtr xpathObjNc = doc->getXPathObject("./nc:netcdf", nodes->nodeTab[0]);
        xmlNodeSetPtr nodesNc = xpathObjNc->nodesetval;
        int sizeNc = (nodesNc) ? nodesNc->nodeNr : 0;
        for (int i = 0; i < sizeNc; i++) {
            // open <netcdf /> tags recursively
            string id = ncml.id() +":netcdf:" + type2string(i);
            string current = doc->toString(nodesNc->nodeTab[i]);
            readers_.push_back(make_pair(id, boost::shared_ptr<CDMReader>(new NcmlCDMReader(XMLInputString(current, id)))));
        }

        aggType_ = getXmlProp(nodes->nodeTab[0], "type");
        // open reader by scan
        XPathObjPtr xpathObjScan = doc->getXPathObject("./nc:scan", nodes->nodeTab[0]);
        xmlNodeSetPtr nodesScan = xpathObjScan->nodesetval;
        int sizeScan = (nodesScan) ? nodesScan->nodeNr : 0;
        for (int i = 0; i < sizeScan; i++) {
            string dir, type, config;
            getFileTypeConfig(getXmlProp(nodesScan->nodeTab[i], "location"), dir, type, config);
            if (type == "") type = "netcdf";
            string suffix = getXmlProp(nodesScan->nodeTab[i], "suffix");
            string regExp;
            if (suffix != "") {
                regExp = ".*\\Q"+suffix+"\\E$";
            } else {
                regExp = getXmlProp(nodesScan->nodeTab[i], "regExp");
            }
            string subdirs = getXmlProp(nodesScan->nodeTab[i], "subdirs");
            int depth = -1; //negative depth == unlimited
            if (subdirs == "false" || subdirs == "FALSE" || subdirs == "False") {
                depth = 0;
            }
            vector<string> files;
            scanFiles(files, boost::filesystem::path(dir), depth, boost::regex(regExp));
            for (size_t i = 0; i < files.size(); ++i) {
                LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "scanned file: " << files.at(i));
                try {
                    readers_.push_back(make_pair(files.at(i), CDMFileReaderFactory::create(type, files.at(i), config)));
                } catch (CDMException& ex) {
                    LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::ERROR, "cannot read scanned file '" << files.at(i) << "' type: " << type << ", config: " << files.at(i) );

                }
            }
        }

        // find the reference-file, choose penultimate, no readers also possible
        if (readers_.size() == 1) {
            gDataReader_ = readers_.at(0).second;
        } else if (readers_.size() > 1) {
            gDataReader_ = readers_.at(readers_.size() - 2).second;
        }

        if (readers_.size() > 1) { // aggregation only with more than 1 reader
            if (aggType_ == "joinExisting") {
                // join unlimited from joinExisting, remember unlimdim->datasource map
                *(this->cdm_) = gDataReader_->getCDM();
                const CDMDimension* uDim = cdm_->getUnlimitedDim();
                if (uDim == 0) {
                    throw CDMException("cannot aggregate files with joinExisting without unlimited dimension");
                }
                string uDimName = uDim->getName();
                for (size_t i = 0; i < readers_.size(); ++i) {
                    const CDMDimension* readerUdim = readers_.at(i).second->getCDM().getUnlimitedDim();
                    if (readerUdim == 0 || (readerUdim->getName() != uDimName)) {
                        LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::INFO, "file '" << readers_.at(i).first << "' does not have matching unlimited dimension: " << uDimName);
                        readers_.at(i).second.reset(); // no longer needed-
                    } else {
                        for (size_t j = 0; j < readerUdim->getLength(); ++j) {
                            readerUdimPos_.push_back(make_pair(i,j));
                        }
                    }
                 }
                // change size of unlimited dimension
                CDMDimension& ulimDim = cdm_->getDimension(uDim->getName());
                ulimDim.setLength(readerUdimPos_.size());
            } else if (aggType_ == "union") {
                // join variables/dimensions from union, remember variable->datasource map
                *(this->cdm_) = gDataReader_->getCDM();
                for (size_t ir = 0; ir < readers_.size(); ++ir) {
                    const CDM::VarVec& knownVars = cdm_->getVariables();
                    const CDM& rCdm = readers_.at(ir).second->getCDM();
                    const CDM::VarVec& rVars = rCdm.getVariables();
                    for (CDM::VarVec::const_iterator rv = rVars.begin(); rv != rVars.end(); ++rv) {
                        if (find_if(knownVars.begin(), knownVars.end(), CDMNameEqual(rv->getName())) == knownVars.end()) {
                            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::INFO, "found new variable '" << rv->getName() << " in " << readers_.at(ir).first);
                            // check dimensions
                            vector<string> rshape = rv->getShape();
                            bool dimsOk = true;
                            for (size_t is = 0; is < rshape.size(); ++is) {
                                const CDMDimension& rdim = rCdm.getDimension(rshape.at(is));
                                if (cdm_->hasDimension(rshape.at(is))) {
                                    CDMDimension& dim = cdm_->getDimension(rshape.at(is));
                                    if (dim.isUnlimited()) {
                                        if (!rdim.isUnlimited()) {
                                            dimsOk = false;
                                            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::WARN, rv->getName() << " not unlimited in " << readers_.at(is).first);
                                        } else {
                                            if (rdim.getLength() > dim.getLength()) {
                                                dim.setLength(rdim.getLength());
                                            }
                                        }
                                    } else {
                                        if (rdim.isUnlimited()) {
                                            dimsOk = false;
                                            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::WARN, rv->getName() << " unlimited in " << readers_.at(is).first);
                                        } else {
                                            if (rdim.getLength() != dim.getLength()) {
                                                dimsOk = false;
                                                LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::WARN, rv->getName() << " changes size in dim "<< rdim.getName() << " at " << readers_.at(is).first);
                                            }
                                        }
                                    }
                                } else {
                                    if (rdim.isUnlimited()) {
                                        if (cdm_->getUnlimitedDim() == 0) {
                                            cdm_->addDimension(rdim);
                                        } else {
                                            dimsOk = false;
                                            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::WARN, "two unlimited dimensions: "<< rdim.getName() << " at " << readers_.at(is).first);
                                        }
                                    } else {
                                        cdm_->addDimension(CDMDimension(rdim));
                                    }
                                }
                            }
                            if (dimsOk) {
                                varReader_[rv->getName()] = ir;
                                cdm_->addVariable(CDMVariable(*rv));
                                vector<CDMAttribute> atts = rCdm.getAttributes(rv->getName());
                                for (vector<CDMAttribute>::iterator attIt = atts.begin(); attIt != atts.end(); ++attIt) {
                                    cdm_->addAttribute(rv->getName(), *attIt);
                                }
                            }
                        }
                    }
                }
            } else {
                throw CDMException("aggregation type " + aggType_ + " found in " + ncml.id() + ", but currently only union and joinExisting supported");
            }
        }
    }

}

NcmlArregationReader::~NcmlArregationReader()
{
}

DataPtr NcmlArregationReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
{
    LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "getDataSlice(var,uDim): (" << varName << "," << unLimDimPos << ")");
    // return unchanged data from this CDM
    const CDMVariable& variable = cdm_->getVariable(varName);
    if (variable.hasData()) {
        LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "fetching data from memory");
        DataPtr data = variable.getData();
        if (data->size() == 0) {
            return data;
        } else {
            return variable.getData();
        }
    }

    if (gDataReader_.get() == 0) {
        // no datasource, return empty
        return createData(CDM_NAT, 0);
    }

    if (aggType_ == "joinExisting") {
        cdm_->hasUnlimitedDim(variable);
        if (cdm_->hasUnlimitedDim(variable) && (unLimDimPos < readerUdimPos_.size())) {
            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "fetching data from " << readers_.at(readerUdimPos_.at(unLimDimPos).first).first << " at uDimPos " << readerUdimPos_.at(unLimDimPos).second);
            return readers_.at(readerUdimPos_.at(unLimDimPos).first).second->getDataSlice(varName, readerUdimPos_.at(unLimDimPos).second);
        }
        LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "fetching data from default reader");
        return gDataReader_->getDataSlice(varName, unLimDimPos);
    } else if (aggType_ == "union") {
        if (varReader_.find(varName) == varReader_.end()) {
            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "fetching data from default reader");
            return gDataReader_->getDataSlice(varName, unLimDimPos);
        } else {
            pair<string, boost::shared_ptr<CDMReader> >& r = readers_.at(varReader_[varName]);
            LOG4FIMEX(getLogger("fimex.NcmlCDMReader"), Logger::DEBUG, "fetching data of " << varName << " from " << r.first);
            return r.second->getDataSlice(varName, unLimDimPos);
        }
    }
    return gDataReader_->getDataSlice(varName, unLimDimPos);
}


} /* namespace MetNoFimex */
