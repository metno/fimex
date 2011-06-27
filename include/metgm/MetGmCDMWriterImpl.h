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

#ifndef METGM_CDMWRITERIMPL_HPP
#define METGM_CDMWRITERIMPL_HPP

// metlib
// TODO: remove even this dependancy in future
#include "metgm.h"

// fimex
#include "fimex/CDMWriter.h"
#include "fimex/CDM.h"
#include "fimex/XMLDoc.h"

// boost
//
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/unordered_set.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

// standard
//
#include <string>
#include <set>
#include <vector>
#include <map>

namespace MetNoFimex {

    /* forwrd decelarations */
    class MetGmVersion;

class MetGmCDMWriterImpl : public CDMWriter
{
public:
        /**
         * @param cdmReader dataSource
         * @param outputFile file-name to write to
         * @param configFile xml-configuration
         * @param METGM version, can be Edition 1 or Edition 2;
         */
        explicit MetGmCDMWriterImpl
                (
                        const boost::shared_ptr<CDMReader> cdmReader,
                        const std::string& outputFile,
                        const std::string& configFile = std::string()
                );

        virtual ~MetGmCDMWriterImpl();

       /**
         * @warning only public for testing
         * @return the new name of a variable, eventually changed by the writers config
         */
        const std::string& getVariableName(const std::string& varName) const;
        /**
         * @warning only public for testing
         * @return the new name of a dimension, eventually changed by the writers config
         */
        const std::string& getDimensionName(const std::string& dimName) const;
        /**
         * @warning only public for testing
         * @param varName original variable name  (before config: newname)
         * @param attName original attribute name (before config: newname)
         * @return an attribute contained in the writers attribute, possibly added by config
         */
        const CDMAttribute& getAttribute(const std::string& varName, const std::string& attName) const throw(CDMException);

private:

        void mapKildeVariablesToMetgmPids(const std::auto_ptr<XMLDoc>& doc);
        void mapMetgmPidToMetgmHDs(const std::auto_ptr<XMLDoc>& doc);
        void mapStandardNamesToMetgmPids(const std::auto_ptr<XMLDoc>& doc);
        void mapKildeNamesToFillValues(const std::auto_ptr<XMLDoc>& doc);

        void allocateMgmHandle();
        void freeMgmHandle();

        void openMgmFileHandle();
        void closeMgmFileHandle();
        void loadInternalCDMObject();
        void detectCDMVariables();
        void detectCDMVariablesByPid();
        void detectCDMVariablesByName();
        void detectCDMVariablesByStandardName();
        void writeGroup0Data();
        void writeGroup1Data();
        void writeGroup2Data();
        void writeHeader();

        void writeGroup3Data(mgm_group3* gp3, const CDMVariable* pVar);
        void writeGroup3VerticalAxis(mgm_group3* gp3, const CDMVariable* pVar);
        void writeGroup3TimeAxis(mgm_group3* gp3, const CDMVariable* pVar);
        void writeGroup3HorizontalAxis(mgm_group3* gp3, const CDMVariable* pVar);

        void writeGroup4Data(const mgm_group3* gp3, const CDMVariable* pVar);
        void writeGroup5Data(const mgm_group3* gp3, const CDMVariable* pVar);

        void init();

        enum MetgmPr {
            REF_MSL,
            REF_GND,
            REF_hPa // reference in Pa
        };

        typedef std::map<const MetgmPr, const CDMVariable*> MetgmPrToCDMVariableMap;


        std::multimap<short, const CDMVariable*>    pid2CdmVariablesMMap_;
        std::map<std::string, float>                kildeName2FillValueMap_;
        std::multimap<short, std::string>           pid2kildemap_;
        std::multimap<short, std::string>           pid2StandardNamesMMap_;
        std::map<short, short>                      pid2hdmap_;
        boost::shared_ptr<MetGmVersion>             metgmVersion_;
        std::string                                 configFileName_;
        FILE*                                       metgmFileHandle_;
        mgm_handle*                                 metgmHandle_;

        boost::posix_time::ptime                    analysisTime_;
        boost::posix_time::ptime                    startTime_;
        time_t                                      dTimeStep_;
        CDM cdmInternal;
};

}

#endif // METGM_CDMWRITERIMPL_HPP

