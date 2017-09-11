#ifndef TOVLEVELCONVERTERADAPTER_H
#define TOVLEVELCONVERTERADAPTER_H

#include "ToVLevelConverter.h"

namespace MetNoFimex {

// forward decl
class CDMReader;
class CoordinateSystem;
class SliceBuilder;
class VerticalConverter;

class ToVLevelConverterAdapter : public ToVLevelConverter {
public:
    ToVLevelConverterAdapter(CDMReader_p reader, boost::shared_ptr<const CoordinateSystem> cs,
                             VerticalConverterPtr converter, size_t unLimDimPos);
    virtual std::vector<double> operator()(size_t x, size_t y, size_t t);
    virtual bool isValid(double val, size_t x, size_t y, size_t t);

private:
    SliceBuilder prepareSliceBuilder(size_t x, size_t y, size_t t) const;

private:
    CDMReader_p reader_;
    VerticalConverterPtr converter_;
    std::string varGeoX_, varGeoY_, varTime_;
    size_t unlimitedTimePos_;
};

} // namespace MetNoFimex

#endif // TOVLEVELCONVERTERADAPTER_H
