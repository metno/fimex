#include "fimex/MetGmCDMReader.h"
#include <fimex/CDM.h>

// private implementation details
#include "../include/metgm/MetGmCDMReaderImpl.h"

namespace MetNoFimex {

    MetGmCDMReader::MetGmCDMReader(const std::string& metgmsource, const std::string& configfilename)
        : CDMReader()
    {
        try {
            d_ptr = boost::shared_ptr<MetGmCDMReaderImpl>(new MetGmCDMReaderImpl(metgmsource, configfilename, cdm_));
        } catch (std::runtime_error& exp) {
            throw CDMException(std::string("METGM_CDMReader error: ") + exp.what());
        }
    }

    MetGmCDMReader::~MetGmCDMReader() { }

    boost::shared_ptr<Data> MetGmCDMReader::getDataSlice(const std::string& varName, size_t unLimDimPos)
    {
        return d_ptr->getDataSlice(varName, unLimDimPos);
    }

    boost::shared_ptr<Data> MetGmCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
        return d_ptr->getDataSlice(varName, sb);
    }

}

