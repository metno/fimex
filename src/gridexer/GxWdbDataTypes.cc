#include "gridexer/GxWdbDataTypes.h"


// ############################################################################

void GxLevelParameterRow::setFrom(const double from)
{
    from_ = from;
}

void GxLevelParameterRow::setTo(const double to)
{
    to_ = to;
}

float GxLevelParameterRow::from() const
{
    return from_;
}

float GxLevelParameterRow::to() const
{
    return to_;
}

// ############################################################################

void GxValidTimeRow::setFrom(const GxTimeRow& from)
{
    from_ = from;
}

void GxValidTimeRow::setTo(const GxTimeRow& to)
{
    to_ = to;
}

GxTimeRow GxValidTimeRow::from() const
{
    return from_;
}
GxTimeRow GxValidTimeRow::to() const
{
    return to_;
}

// ############################################################################

void GxPlaceRegularGridRow::setPlace(const GxPlaceRow& place)
{
    place_ = place;
}

GxPlaceRow GxPlaceRegularGridRow::place()
{
    return place_;
}

void GxPlaceRegularGridRow::setNumberX(const float X)
{
    numberX_ = X;
}

float GxPlaceRegularGridRow::numberX() const
{
    return numberX_;
}

void GxPlaceRegularGridRow::setNumberY(const float Y)
{
    numberY_ = Y;
}

float GxPlaceRegularGridRow::numberY() const
{
    return numberY_;
}

void GxPlaceRegularGridRow::setIncrementX(const float X)
{
    incrementX_ = X;
}

float GxPlaceRegularGridRow::incrementX() const
{
    return incrementX_;
}
void GxPlaceRegularGridRow::setIncrementY(const float Y)
{
    incrementY_ = Y;
}

float GxPlaceRegularGridRow::incrementY() const
{
    return incrementY_;
}

// ############################################################################

void GxGidRow::setValue(GxGidRow::gid value)
{
    value_ = value;
}

GxGidRow::gid GxGidRow::value() const
{
    return value_;
}

void GxGidRow::setValueId(GxGidRow::gid valueId)
{
    valueID_ = valueId;
}

GxGidRow::gid GxGidRow::valueId() const
{
    return valueID_;
}

std::string GxGidRow::valueType() const
{
    return "float";
}

void GxGidRow::setProvider(const GxDataProviderRow& provider)
{
    provider_ = provider;
}

GxDataProviderRow GxGidRow::provider() const
{
    return provider_;
}

void GxGidRow::setPlace(const GxPlaceRow& place)
{
    place_ = place;
}

GxPlaceRow GxGidRow::place() const
{
    return place_;
}

void GxGidRow::setReferenceTime(const GxReferenceTimeRow& referenceTime)
{
    referenceTime_ = referenceTime;
}

GxReferenceTimeRow GxGidRow::referenceTime() const
{
    return referenceTime_;
}

void GxGidRow::setValidTime(const GxValidTimeRow& validTime)
{
    validTime_ = validTime;
}

GxValidTimeRow GxGidRow::validTime() const
{
    return validTime_;
}

void GxGidRow::setStoreTime(const GxTimeRow& storeTime)
{
    storeTime_ = storeTime;
}

GxTimeRow GxGidRow::storeTime() const
{
    return storeTime_;
}

void GxGidRow::setLevelParameter(const GxLevelParameterRow& level)
{
    levelParameter_ = level;
}

GxLevelParameterRow GxGidRow::levelParameter() const
{
    return levelParameter_;
}

void GxGidRow::setValueParameter(const GxValueParameterRow& value)
{
    valueParameter_ = value;
}

GxValueParameterRow GxGidRow::valueParameter() const
{
    return valueParameter_;
}
