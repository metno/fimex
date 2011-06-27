#ifndef METGM_CDM_READER_H
#define METGM_CDM_READER_H

// fimex
//
#include "fimex/CDMReader.h"


// boost
#include <boost/shared_ptr.hpp>

namespace MetNoFimex {

    /* forward declarations */
    class MetGmCDMReaderImpl;

    class METGM_CDMReader : public CDMReader
    {
    public:
        METGM_CDMReader(const std::string& metgmsource, const std::string& configfilename);
        ~METGM_CDMReader();

        boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos);
        boost::shared_ptr<Data> getDataSlice(const std::string& varName, const SliceBuilder& sb);

    private:
        boost::shared_ptr<MetGmCDMReaderImpl> d_ptr;
    };

} // end namespace

#endif // METGM_CDM_READER_H

