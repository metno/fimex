#include "fimex/MetGmCDMReader.h"
#include <fimex/CDM.h>

// private implementation details
#include "./metgm/MetGmCDMReaderImpl.h"
#include "./metgm/MetGmCDMReaderSlicedImpl.h"

namespace MetNoFimex {

    MetGmCDMReader::MetGmCDMReader(const std::string& metgmsource, const XMLInput& configXML)
        : CDMReader()
    {
        try {

            d_ptr = boost::shared_ptr<MetGmCDMReaderSlicedImpl>(new MetGmCDMReaderSlicedImpl(metgmsource, configXML, cdm_));

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

