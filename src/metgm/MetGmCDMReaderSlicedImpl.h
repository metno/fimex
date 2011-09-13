#ifndef METGM_CDM_READER_SLICED_IMPL_H
#define METGM_CDM_READER_SLICED_IMPL_H

// implementation
//
#include "MetGmCDMReaderImpl.h"

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
        explicit MetGmCDMReaderSlicedImpl(const std::string& metgmsource, const std::string& configfilename, const boost::shared_ptr<CDM>& cdm);
        ~MetGmCDMReaderSlicedImpl();

        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
        virtual boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

    protected:
        virtual void parseMgmFile(const std::string& mgmFileName);
        virtual void addVariables();
    };

} // end namespace

#endif // METGM_CDM_READER_SLICED_IMPL_H

