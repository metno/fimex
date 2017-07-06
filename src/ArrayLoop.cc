// -*- c++ -*-

#include "fimex/ArrayLoop.h"

#include <algorithm>
#include <numeric>
#include <stdexcept>

namespace MetNoFimex {

static const size_t INVALID_SIZE = ~(0ul);

/*
  not sure what to do with samedims and loop dims -- could samedims for (t,p,x,y) and (p) with loop p be (x,y)?

  fix() not possible after add(), use loop()

  out may have different z length than in

  time / unlimited must be last
*/

// ========================================================================

static size_t check_non0_length(const std::string& name, size_t length)
{
    if (length == 0)
        throw std::runtime_error("dim '" + name + " has 0 length");
    return length;
}

ArrayDims::ArrayDims()
{
}

ArrayDims::ArrayDims(const string_v& names, const size_v& lengths)
    : name_(names)
    , length_(lengths)
    , is_shared_(length_.size(), true)
{
    if (name_.size() != length_.size())
        throw std::runtime_error("size name != length");
    if (!length_.empty()) {
        check_non0_length(name_[0], length_[0]);
        delta_.reserve(length_.size()+1);
        delta_.push_back(1);
        for (size_t i=1; i<length_.size(); ++i) {
            const size_t l = check_non0_length(name_[i-1], length_[i-1]);
            delta_.push_back(delta_.back() * l);
        }
    }
}

ArrayDims& ArrayDims::add(const std::string& name, size_t length)
{
    check_non0_length(name, length);
    name_.push_back(name);
    if (length_.empty())
        delta_.push_back(1);
    else
        delta_.push_back(delta_.back() * length_.back());
    length_.push_back(length);
    is_shared_.push_back(true);
    return *this;
}

void ArrayDims::set_length(size_t dim_idx, size_t length)
{
    if (validindex(dim_idx))
        length_[dim_idx] = check_non0_length(name_[dim_idx], length);
}

size_t ArrayDims::dim_index(const std::string& dim_name) const
{
    string_v::const_iterator it = std::find(name_.begin(), name_.end(), dim_name);
    if (it != name_.end())
        return std::distance(name_.begin(), it);
    else
        return INVALID_SIZE;
}

void ArrayDims::set_not_shared(size_t dim_idx)
{
    if (validindex(dim_idx))
        is_shared_[dim_idx] = false;
}

bool ArrayDims::is_shared(size_t dim_idx) const
{
    if (validindex(dim_idx))
        return is_shared_[dim_idx];
    else
        return false;
}

size_t ArrayDims::volume() const
{
    if (length_.empty())
        return 1;
    else
        return delta_.back() * length_.back();
}

// ========================================================================

void set_not_shared(const std::string& dimname, ArrayDims& s1)
{
    s1.set_not_shared(s1.dim_index(dimname));
}

void set_not_shared(const std::string& dimname, ArrayDims& s1, ArrayDims& s2)
{
    set_not_shared(dimname, s1);
    set_not_shared(dimname, s2);
}

void set_not_shared(const std::string& dimname, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3)
{
    set_not_shared(dimname, s1);
    set_not_shared(dimname, s2, s3);
}

void set_not_shared(const std::string& dimname, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4)
{
    set_not_shared(dimname, s1, s2);
    set_not_shared(dimname, s3, s4);
}

void set_not_shared(const std::string& dimname, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5)
{
    set_not_shared(dimname, s1, s2);
    set_not_shared(dimname, s3, s4, s5);
}

// ========================================================================

void set_length(const std::string& dimname, size_t length, ArrayDims& s1)
{
    s1.set_length(s1.dim_index(dimname), length);
}

void set_length(const std::string& dimname, size_t length, ArrayDims& s1, ArrayDims& s2)
{
    set_length(dimname, length, s1);
    set_length(dimname, length, s2);
}

void set_length(const std::string& dimname, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3)
{
    set_length(dimname, length, s1);
    set_length(dimname, length, s2, s3);
}

void set_length(const std::string& dimname, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4)
{
    set_length(dimname, length, s1, s2);
    set_length(dimname, length, s3, s4);
}

void set_length(const std::string& dimname, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5)
{
    set_length(dimname, length, s1, s2);
    set_length(dimname, length, s3, s4, s5);
}

// ========================================================================

ArrayGroup::ArrayMap::ArrayMap(const ArrayDims& s, size_t rank)
    : array(&s)
    , dim_index_map(rank, INVALID_SIZE)
{
}

// ========================================================================

ArrayGroup::ArrayGroup()
    : sharedDims_(INVALID_SIZE)
    , sharedVolume_(0)
{
}

void ArrayGroup::minimizeShared(size_t notsame)
{
    if (sharedDims_ == INVALID_SIZE || sharedDims_ > notsame) {
        sharedDims_ = notsame;
        sharedVolume_ = std::accumulate(lengths.begin(), lengths.begin() + sharedDims_, 1, std::multiplies<size_t>());
    }
}

size_t ArrayGroup::volume() const
{
    return std::accumulate(lengths.begin(), lengths.end(), 1, std::multiplies<size_t>());
}

size_t ArrayGroup::position(const std::string& dimname) const
{
    string_v::const_iterator it = std::find(dims.begin(), dims.end(), dimname);
    if (it != dims.end())
        return std::distance(dims.begin(), it);
    else
        return INVALID_SIZE;
}

ArrayGroup& ArrayGroup::add(const ArrayDims& a)
{
    arrays.push_back(ArrayMap(a, rank()));
    ArrayMap& am = arrays.back();

    for (size_t sdimidx = 0; sdimidx < am.array->rank(); ++sdimidx) {
        if (!am.array->is_shared(sdimidx)) {
            minimizeShared(sdimidx);
            continue;
        }

        const size_t slength = am.array->length(sdimidx);
        const std::string& sdimname = am.array->dim_name(sdimidx);
        const size_t dimidx = position(sdimname);

        if (dimidx != INVALID_SIZE) {
            // known dimension, just add to sd.map
            if (slength != lengths[dimidx])
                throw std::runtime_error("dim length mismatch for '" + sdimname + "'");
            am.dim_index_map[dimidx] = sdimidx;
            if (dimidx != sdimidx)
                minimizeShared(std::min(sdimidx, dimidx));
        } else {
            // unknown dimension, add dims, lengths, extend all singles's maps
            dims.push_back(sdimname);
            lengths.push_back(slength);
            am.dim_index_map.push_back(sdimidx);

            if (arrays.size() > 1) {
                minimizeShared(sdimidx);

                // all other singles do not have this dimension
                for (size_t o=0; o<arrays.size() - 1; ++o)
                    arrays[o].dim_index_map.push_back(INVALID_SIZE);
            }
        }
    }
    minimizeShared(am.array->rank());
    return *this;
}

} // namespace MetNoFimex
