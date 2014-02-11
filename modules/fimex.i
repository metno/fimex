/*
 * PERL
 * swig -I/usr/include -Wall -c++ -module Geo::Fimex -outdir lib/Geo -perl fimex.i
 */

/* R
 * cd RFimex
 * swig -Isrc -I/usr/include -c++ -r -module RFimex -o fimex_wrap.cpp ../fimex.i
 * mv src/RFimex.R R
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
#include "fimex/CDMProcessor.h"
#include "fimex/CDMTimeInterpolator.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/NetCDF_CDMWriter.h"
#include "fimex/NcmlCDMReader.h"
#include "rfimexSliceBuilder.h"
#include "fimex/Logger.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMException.h"
#include "fimex/c_fimex.h"
#include "fimex/mifi_cdm_reader.h"
#include "fimex/coordSys/CoordinateSystem.h"

namespace MetNoFimex {
/*
 * lists (in that order) time, x, y, z, forecastRefTime, other1, other2, ... dimensions
 * empty string means missing; doesn't allow for compound axes
 */
std::vector<std::string> listCoordinates(boost::shared_ptr<MetNoFimex::CDMReader> reader, std::vector<boost::shared_ptr<const MetNoFimex::CoordinateSystem> >* csList, std::string varName)
{
    std::vector<std::string> coords;
    std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(csList->begin(), csList->end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt != csList->end()) {
        if ((*varSysIt)->isSimpleSpatialGridded()) {
            std::vector<CoordinateSystem::ConstAxisPtr> axes;
            axes.push_back((*varSysIt)->getTimeAxis());
            axes.push_back((*varSysIt)->getGeoXAxis());
            axes.push_back((*varSysIt)->getGeoYAxis());
            axes.push_back((*varSysIt)->getGeoZAxis());
            axes.push_back((*varSysIt)->findAxisOfType(CoordinateAxis::ReferenceTime));

            std::vector<std::string> shape = reader->getCDM().getVariable(varName).getShape();
            std::set<std::string> shapeSet(shape.begin(), shape.end());
            for (int i = 0; i < axes.size(); i++) {
                if (axes.at(i).get() != 0) {
                    std::string dimName = axes.at(i)->getName();
                    size_t hasAxis = shapeSet.erase(dimName);
                    if (hasAxis) {
                        coords.push_back(dimName);
                    } else {
                        coords.push_back("");
                    }
                } else {
                    coords.push_back("");
                }
            }
            for (std::set<std::string>::iterator shapeIt = shapeSet.begin(); shapeIt != shapeSet.end(); ++shapeIt) {
                coords.push_back(*shapeIt);
            }
        }
    }
    return coords;
}

/*
 * get the proj4-string of the variable
 * empty string means missing
 */
std::string getProj4(boost::shared_ptr<MetNoFimex::CDMReader> reader, std::vector<boost::shared_ptr<const MetNoFimex::CoordinateSystem> >* csList, std::string varName)
{
    std::vector<std::string> coords;
    std::vector<boost::shared_ptr<const CoordinateSystem> >::iterator varSysIt =
            find_if(csList->begin(), csList->end(), CompleteCoordinateSystemForComparator(varName));
    if (varSysIt != csList->end()) {
        if ((*varSysIt)->hasProjection()) {
            return (*varSysIt)->getProjection()->getProj4String();
        }
    }
    return "";
}

boost::shared_ptr<CDMReader> latLonInterpolatedReader(boost::shared_ptr<CDMReader> in, int method, const std::vector<double>& lonVals, const std::vector<double>& latVals) {
    CDMInterpolator* read = new CDMInterpolator(in);
    boost::shared_ptr<CDMReader> r = boost::shared_ptr<CDMReader>(read);
    read->changeProjection(method, lonVals, latVals);
    return r;
}

boost::shared_ptr<CDMReader> vectorAutoRotatedReader(boost::shared_ptr<CDMReader> in, int toLatLon) {
    CDMProcessor* read = new CDMProcessor(in);
    boost::shared_ptr<CDMReader> r = boost::shared_ptr<CDMReader>(read);
    read->rotateAllVectorsToLatLon(toLatLon != 0);
    return r;
}


double mifi_get_unique_forecast_reference_time(boost::shared_ptr<MetNoFimex::CDMReader> reader, const char* units)
{
    mifi_cdm_reader r(reader);
    return mifi_get_unique_forecast_reference_time(&r, units);
}


}  // namespace MetNoFimex

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
double mifi_get_unique_forecast_reference_time(boost::shared_ptr<MetNoFimex::CDMReader> reader, const char* units);

// SliceBuilder.h broken in fimex-0.48, using therefore own Slicebuilder setup ... %include "fimex/SliceBuilder.h"
%include "rfimexSliceBuilder.h"

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
std::vector<boost::shared_ptr<const MetNoFimex::CoordinateSystem> > listCoordinateSystems(boost::shared_ptr<MetNoFimex::CDMReader> reader);
std::vector<std::string> listCoordinates(boost::shared_ptr<MetNoFimex::CDMReader> reader, std::vector<boost::shared_ptr<const MetNoFimex::CoordinateSystem> >* csList, std::string varName);
std::string getProj4(boost::shared_ptr<MetNoFimex::CDMReader> reader, std::vector<boost::shared_ptr<const MetNoFimex::CoordinateSystem> >* csList, std::string varName);
boost::shared_ptr<MetNoFimex::CDMReader> latLonInterpolatedReader(boost::shared_ptr<MetNoFimex::CDMReader> in, int method, const std::vector<double>& lonVals, const std::vector<double>& latVals) throw(MetNoFimex::CDMException);
boost::shared_ptr<MetNoFimex::CDMReader> vectorAutoRotatedReader(boost::shared_ptr<MetNoFimex::CDMReader> in, int toLatLon) throw(MetNoFimex::CDMException);

class CDM {
};
}


namespace MetNoFimex {

class CDMReader {
public:
    const MetNoFimex::CDM& getCDM() const;
    %extend {
      // need a vector<double>* (ptr) back, otherwise, R-Swig fails on compiled-in libarary with type_info<Type>() with segfault
      // could now switch back to non-pointer version?
      std::vector<double> getSliceVecInUnit(std::string varName, SliceBuilder sb, std::string units = "") throw(MetNoFimex::CDMException) {
         MetNoFimex::DataPtr d;
         if (units != "") {
           d = $self->getScaledDataSliceInUnit(varName, units, sb);
         } else {
           d = $self->getScaledDataSlice(varName, sb);
         }
         std::vector<double> out; // = new std::vector<double>();
         if (d.get() != 0) {
            boost::shared_array<double> dAry = d->asDouble();
            out.assign(&dAry[0], &dAry[0]+d->size());
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
