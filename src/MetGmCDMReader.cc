#include "fimex/MetGmCDMReader.h"
#include <fimex/CDM.h>

#include "../config.h"
#ifdef HAVE_OPENMP
#include <omp.h>
#endif

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
        boost::shared_ptr<Data> data;
#ifdef HAVE_OPENMP
#pragma omp critical (mifi_metgmcdmreader)
    {
#endif
        data = d_ptr->getDataSlice(varName, unLimDimPos);
#ifdef HAVE_OPENMP
    }
#endif
        return data;
    }

    boost::shared_ptr<Data> MetGmCDMReader::getDataSlice(const std::string& varName, const SliceBuilder& sb)
    {
        boost::shared_ptr<Data> data;
#ifdef HAVE_OPENMP
#pragma omp critical (mifi_metgmcdmreader)
        {
#endif
        data = d_ptr->getDataSlice(varName, sb);
#ifdef HAVE_OPENMP
        }
#endif
        return data;
    }

}

