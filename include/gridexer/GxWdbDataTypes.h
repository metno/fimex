#ifndef GX_WDBDATATYPES_HPP_
#define GX_WDBDATATYPES_HPP_

// std
#include <string>
#include <vector>
#include <limits>

// boost
//
#include <boost/shared_ptr.hpp>

// postgresql
#include <postgresql/libpq-fe.h>

class GxWdbRow
{
public:
    GxWdbRow() : numberOfTuples_(std::numeric_limits<unsigned int>::quiet_NaN()) {}
    virtual ~GxWdbRow() {}

    static std::string getBrowseReturnType() { return "NULL::NULL"; }

    void setNumberOfTuples(const unsigned int numberOfTuples) {
        numberOfTuples_ = numberOfTuples;
    }
    unsigned int numberOfTuples() { return numberOfTuples_; }

protected:
    unsigned int numberOfTuples_;
};

class GxNamedWdbRow : public GxWdbRow
{
public:
    GxNamedWdbRow() : GxWdbRow(), name_(), cfName_() {}
    ~GxNamedWdbRow() {}

    void setName(const std::string& name) {
        name_ = name;
    }
    std::string name() const { return name_; }

    void setCFName(const std::string& cfName) {
        cfName_ = cfName;
    }
    std::string cfName() const { return cfName_; }

protected:
    std::string name_;
    std::string cfName_;
};

// ############################################################################

class GxDataProviderRow : public GxNamedWdbRow
{
public:
    GxDataProviderRow() : GxNamedWdbRow() {}
    ~GxDataProviderRow() {}

    static std::string getReturnType() { return "NULL::wci.browsedataprovider"; }

//    void dataProviderName(const std::string& dataProviderName);
//    std::string dataProviderName() const;

//    std::string toString() const;

//protected:
//    std::string dataProviderName_;
};

class GxPlaceRow : public GxNamedWdbRow
{
public:
    GxPlaceRow() : GxNamedWdbRow() {}
    ~GxPlaceRow() {}

    static std::string getReturnType() { return "NULL::wci.browseplace"; }

//    void placeName(const std::string& placeName);
//    std::string placeName() const;

//    std::string toString() const;
//    void fill(const pqxx::result::const_iterator& cit);

//protected:
//    std::string placeName_;
};

// ############################################################################

class GxParameterRow : public GxNamedWdbRow
{
public:
    GxParameterRow() : GxNamedWdbRow(), unitName_() {}
    virtual ~GxParameterRow() {}

    void setUnitName(const std::string& unitName) {
        unitName_ = unitName;
    }
    std::string unitName() const { return unitName_; }

protected:
    std::string unitName_;
};

class GxValueParameterRow : public GxParameterRow
{
public:
    GxValueParameterRow()
        : GxParameterRow(), fillValue_(std::numeric_limits<float>::quiet_NaN()) {}
    ~GxValueParameterRow() {}

    static std::string getReturnType() { return "NULL::wci.browsevalueparameter"; }

    void setFillValue(const double fillValue);
    float fillValue() const;

protected:
    float fillValue_;
};

class GxLevelParameterRow : public GxParameterRow
{
public:
    GxLevelParameterRow() : GxParameterRow(),
        from_(std::numeric_limits<float>::quiet_NaN()),
        to_(std::numeric_limits<float>::quiet_NaN()) {}
    ~GxLevelParameterRow() {}

    static std::string getReturnType() { return "NULL::wci.browselevelparameter"; }

    void setFrom(const double from);
    void setTo(const double to);
    float from() const;
    float to() const;

protected:
    float from_;
    float to_;
};

// ############################################################################

class GxTimeRow : public GxParameterRow
{
public:
    GxTimeRow() : GxParameterRow(), sinceEpochInSeconds_(0) {}
    virtual ~GxTimeRow() {}

    void setSinceEpochInSeconds(const long long sinceEpochInSeconds) {
        sinceEpochInSeconds_ = sinceEpochInSeconds;
    }
    long long sinceEpochInSeconds() const {
        return sinceEpochInSeconds_;
    }

protected:
    long long sinceEpochInSeconds_;
};

class GxReferenceTimeRow : public GxTimeRow
{
public:
    GxReferenceTimeRow() : GxTimeRow() {}
    ~GxReferenceTimeRow() {}

    static std::string getReturnType() { return "NULL::wci.browsereferencetime"; }
};

class GxValidTimeRow : public GxParameterRow
{
public:
    GxValidTimeRow() : GxParameterRow(), from_(), to_() {}
    ~GxValidTimeRow() {}

    static std::string getReturnType() { return "NULL::wci.browsevalidtime"; }

    void setFrom(const GxTimeRow& from);
    void setTo(const GxTimeRow& to);
    GxTimeRow from() const;
    GxTimeRow to() const;

protected:
    GxTimeRow from_;
    GxTimeRow to_;
};

// ############################################################################

class GxGeoReferencedWdbRow : public GxNamedWdbRow
{
public:
    GxGeoReferencedWdbRow()
        : GxNamedWdbRow(), startX_(0), startY_(0),
          projectionDefinition_() {}
    ~GxGeoReferencedWdbRow() {}

    void setStartX(const float startX) {
        startX_ = startX;
    }
    float startX() const {
        return startX_;
    }
    void setStartY(const float startY) {
        startY_ = startY;
    }
    float startY() const {
        return startY_;
    }
    void setProjectionDefinition(const std::string& projectionDefinition) {
        projectionDefinition_ = projectionDefinition;
    }
    std::string projectionDefinition() const {
        return projectionDefinition_;
    }

protected:
    float startX_;
    float startY_;
    std::string projectionDefinition_;
};

class GxPlaceRegularGridRow : public GxGeoReferencedWdbRow
{
public:
    GxPlaceRegularGridRow()
        : GxGeoReferencedWdbRow(), place_(),
          numberX_(0), numberY_(0),
          incrementX_(0), incrementY_(0){}

    ~GxPlaceRegularGridRow() {}

    void setPlace(const GxPlaceRow& place);
    GxPlaceRow place();
    void setNumberX(const float X);
    float numberX() const;
    void setNumberY(const float Y);
    float numberY() const;
    void setIncrementX(const float X);
    float incrementX() const;
    void setIncrementY(const float Y);
    float incrementY() const;

protected:
    GxPlaceRow place_;
    float numberX_;
    float numberY_;
    float incrementX_;
    float incrementY_;
};

// ############################################################################

class GxGidRow : GxWdbRow
{
public:
    GxGidRow() : GxWdbRow() {}
    ~GxGidRow() {}
    static std::string getReturnType() { return "NULL::wci.returngid";}

    typedef long long gid;

    void setValue(gid value);
    gid value() const;
    void setValueId(long long valueId);
    long long valueId() const;
    std::string valueType() const;
    void setProvider(const GxDataProviderRow& provider);
    GxDataProviderRow provider() const;
    void setPlace(const GxPlaceRow& place);
    GxPlaceRow place() const;
    void setReferenceTime(const GxReferenceTimeRow& referenceTime);
    GxReferenceTimeRow referenceTime() const;
    void setValidTime(const GxValidTimeRow& validTime);
    GxValidTimeRow validTime() const;
    void setStoreTime(const GxTimeRow& storeTime);
    GxTimeRow storeTime() const;

    void setLevelParameter(const GxLevelParameterRow& level);
    GxLevelParameterRow levelParameter() const;
    void setValueParameter(const GxValueParameterRow& value);
    GxValueParameterRow valueParameter() const;


protected:
    unsigned long long value_;
    unsigned long long valueID_;
    GxDataProviderRow provider_;
    GxPlaceRow place_;
    GxLevelParameterRow levelParameter_;
    GxValueParameterRow valueParameter_;
    GxValidTimeRow validTime_;
    GxReferenceTimeRow referenceTime_;
    GxTimeRow storeTime_;
};

class GxGridDataRow : public GxGeoReferencedWdbRow
{
public:
    GxGridDataRow() : GxGeoReferencedWdbRow() {}
    ~GxGridDataRow() { }

    static std::string getReturnType() { return "NULL::wci.grid"; }

    void setData(const boost::shared_ptr<std::vector<float> >& data) {
        data_ = data;
    }

    boost::shared_ptr<std::vector<float> > data() {
        return data_;
    }

    void setNumberX(const float X) {
        numberX_ = X;
    }

    float numberX() const {
        return numberX_;
    }

    void setNumberY(const float Y) {
        numberY_ = Y;
    }

    float numberY() const {
        return numberY_;
    }

protected:
    boost::shared_ptr<std::vector<float> > data_;
    float numberX_;
    float numberY_;
};

#endif // GX_WDBDATATYPES_HPP_
