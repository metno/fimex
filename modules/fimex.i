/* 
 * PERL
 * swig -I/usr/include -Wall -c++ -module Geo::Fimex -outdir lib/Geo -perl fimex.i
 */

/* R
 * cd R
 * swig -I/usr/include -c++ -r --module=RFimex -o fimex_wrap.cpp ../fimex.i
 * R CMD SHLIB -o fimex.so fimex_wrap.cpp -lfimex
 */
%module fimex
%{
/* Put header files here or function declarations below */
#include "boost/shared_ptr.hpp"
#include "fimex/Data.h"
#include "fimex/CDMReader.h"
#include "fimex/CDMExtractor.h"
#include "fimex/CDMQualityExtractor.h"
#include "fimex/CDMInterpolator.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NcmlCDMReader.h"
#include "fimex/SliceBuilder.h"
#include "fimex/Logger.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMException.h"
#include "fimex/c_fimex.h"
#include "fimex/mifi_cdm_reader.h"


%}


%include "typemaps.i"
%include "std_string.i"
%include "std_vector.i"
namespace std {
    %template(IntVector) vector<size_t>;
    %template(DoubleVector) vector<double>;
    %template(StringVector) vector<string>;
};

%include "fimex/mifi_cdm_reader.h"

size_t mifi_get_variable_number(mifi_cdm_reader* reader);
const char* mifi_get_variable_name(mifi_cdm_reader* reader, size_t pos);

%include "fimex/SliceBuilder.h"

namespace boost {
template<class T>
class shared_ptr
{
  public:
    T * operator-> () const;
};
}

%nodefaultctor;

namespace MetNoFimex {
class CDM {
};
}


namespace MetNoFimex {

class CDMReader {
public:
    const MetNoFimex::CDM& getCDM() const;
    %extend {
      // need a vector<double>* (ptr) back, otherwise, R-Swig fails on compiled-in libarary with type_info<Type>() with segfault
      std::vector<double>* getSliceVecInUnit(std::string varName, SliceBuilder sb, std::string units = "") throw(MetNoFimex::CDMException) {
         MetNoFimex::DataPtr d;
         if (units != "") {
           d = $self->getScaledDataSliceInUnit(varName, units, sb);
         } else {
           d = $self->getScaledDataSlice(varName, sb);
         }
         std::vector<double>* out = new std::vector<double>();
         if (d.get() != 0) {
            boost::shared_array<double> dAry = d->asDouble();
            out->assign(&dAry[0], &dAry[0]+d->size());
         }
         return out;
      }
    }
};

}

%template(boost__shared_ptrCDMReader) boost::shared_ptr<MetNoFimex::CDMReader>;

namespace MetNoFimex {

class CDMFileReaderFactory {
  public:
    static boost::shared_ptr<MetNoFimex::CDMReader> create(std::string fileType, std::string filename, std::string configFile, const std::vector<std::string>& args = std::vector<std::string>()) throw(MetNoFimex::CDMException);
};

class NetCDF_CDMWriter {
  public:
    NetCDF_CDMWriter(boost::shared_ptr<MetNoFimex::CDMReader> reader, const std::string& filename, std::string configFile = "", int version = 3) throw(MetNoFimex::CDMException); 
};

}
