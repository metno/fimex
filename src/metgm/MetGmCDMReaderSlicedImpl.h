#ifndef METGM_CDM_READER_SLICED_IMPL_H
#define METGM_CDM_READER_SLICED_IMPL_H

// implementation
//
#include "MetGmCDMReaderImpl.h"
#include "fimex/XMLInput.h"

// boost
//
#include <boost/shared_ptr.hpp>

// standard
//
#include <string>

namespace MetNoFimex {

    class MetGmCDMReaderSlicedImpl : public MetGmCDMReaderImpl
    {
    public:
        explicit MetGmCDMReaderSlicedImpl(const std::string& metgmsource, const XMLInput& configXML, const boost::shared_ptr<CDM>& cdm);
        virtual ~MetGmCDMReaderSlicedImpl();

        virtual DataPtr getDataSlice(const std::string& varName, size_t unLimDimPos);
        virtual DataPtr getDataSlice(const std::string& varName, const SliceBuilder& sb);

    protected:
        virtual void parseMgmFile(const std::string& mgmFileName);
        virtual void addVariables();
    };

} // end namespace

#endif // METGM_CDM_READER_SLICED_IMPL_H

