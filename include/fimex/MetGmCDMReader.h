#ifndef METGM_CDM_READER_H
#define METGM_CDM_READER_H

// fimex
//
#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"


// boost
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    /* forward declarations */
    class MetGmCDMReaderImpl;

    class MetGmCDMReader : public CDMReader
    {
    public:
        MetGmCDMReader(const std::string& metgmsource, const XMLInput& configXML);
        ~MetGmCDMReader();

        boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
        boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

    private:
        boost::shared_ptr<MetGmCDMReaderImpl> d_ptr;
    };

} // end namespace

#endif // METGM_CDM_READER_H

