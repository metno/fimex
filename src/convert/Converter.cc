/*
 * Fimex, Converter.cc
 *
 * (C) Copyright 2015, met.no
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
 *  Created on: May 15, 2015
 *      Author: heikok
 */
#include "Converter.h"
#include "CDMReaderVar.h"
#include "RelativeToSpecificHumidityConverter.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMReaderUtils.h"
#include "fimex/CDM.h"
#include "fimex/Logger.h"

namespace MetNoFimex
{

using namespace std;

typedef boost::shared_ptr<const Converter> ConstConverterPtr;
typedef std::vector<ConstConverterPtr> ConstConverterList;

static ConstConverterList knownConverters = Varargs<boost::shared_ptr<const Converter> >(ConstConverterPtr(new RelativeToSpecificHumidityConverter())).args;
static LoggerPtr logger = getLogger("fimex.Converter");

std::vector<ConverterPtr> Converter::findByName(std::string conversionName, Varargs<ConverterSrcPtr> srcs)
{
    if (srcs.args.size() == 0) return vector<ConverterPtr>();
    ConstConverterList::iterator ci = find_if(knownConverters.begin(), knownConverters.end(),CDMNameEqualPtr(conversionName));
    if (ci == knownConverters.end()) return vector<ConverterPtr>();

    vector<string> stdNames = (*ci)->getInputStandardNames();
    vector<vector<CDMReaderVar> > varGroups;
    for (size_t stdPos = 0; stdPos < stdNames.size(); stdPos++) {
        // TODO: work with several sources, currently only using the first
        {
            ConverterSrcPtr src = srcs.args.at(0);
            const CDM& cdm = src->getReader()->getCDM();
            vector<string> vars = cdm.findVariables("standard_name", stdNames.at(stdPos) + "(\\s.*)?");
            // TODO: remove vars not matching src-restrictions, something like
            //    vars = src->applyRestrictions(vars);
            if (stdPos == 0) {
                for (vector<string>::iterator vi = vars.begin(); vi != vars.end(); ++vi) {
                    varGroups.push_back(vector<CDMReaderVar>(1,CDMReaderVar(src->getReader(), *vi)));
                }
            } else {
                vector<vector<CDMReaderVar> > newVarGroups;
                for (vector<vector<CDMReaderVar> >::iterator vgi = varGroups.begin(); vgi != varGroups.end(); ++vgi) {
                    for (vector<string>::iterator vi = vars.begin(); vi != vars.end(); ++vi) {
                        // check that new variable is compatible with first variable in *vgi, e.g. axes have same size
                        if (compareCDMVarShapes(vgi->at(0).reader->getCDM(), vgi->at(0).varName, cdm, *vi)) {
                            vector<CDMReaderVar> vg = *vgi;
                            vg.push_back(CDMReaderVar(src->getReader(), *vi));
                            newVarGroups.push_back(vg);
                        } else {
                            LOG4FIMEX(logger, Logger::DEBUG, "variable '" << *vi << "' incompatible with '" << vgi->at(0).varName << "'");
                        }
                    }
                }
                varGroups = newVarGroups;
            }
        }
    }

    vector<ConverterPtr> vcp;
    for (vector<vector<CDMReaderVar> >::iterator vgi = varGroups.begin(); vgi != varGroups.end(); ++vgi) {
        ConverterPtr cp = (*ci)->clone();
        if (! cp->charge(*vgi) ) {
            throw CDMException("unable to charge converter " + cp->getName());
        }
        vcp.push_back(cp);
    }

    return vcp;
}

} /* namespace MetNoFimex */
