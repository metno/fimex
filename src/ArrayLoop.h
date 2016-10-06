// -*- c++ -*-

#ifndef MIFI_ARRAYLOOP_H
#define MIFI_ARRAYLOOP_H 1

#include <string>
#include <vector>

namespace MetNoFimex {

typedef std::vector<std::size_t> size_v;
typedef std::vector<std::string> string_v;
typedef std::vector<bool> bool_v;

// ========================================================================

class ArrayDims {
public:
    ArrayDims();

    ArrayDims(const string_v& names, const size_v& lengths);

    ArrayDims& add(const std::string& name, size_t length);

    void set_length(const std::string& dim_name, size_t length)
        { set_length(dim_index(dim_name), length); }

    void set_length(size_t dimidx, size_t length);

    void set_not_shared(const std::string& dim_name)
        { set_not_shared(dim_index(dim_name)); }

    void set_not_shared(size_t dim_idx);

    bool is_shared(size_t dim_idx) const;

    size_t delta(const std::string& dimname) const
        { return delta(dim_index(dimname)); }

    size_t delta(size_t dim_idx) const
        { return validindex(dim_idx) ? delta_[dim_idx] : 0; }

    size_t length(const std::string& dim_name) const
        { return length(dim_index(dim_name)); }

    size_t length(size_t dim_idx) const
        { return validindex(dim_idx) ? length_[dim_idx] : 1; }

    const std::string& dim_name(size_t dim_idx) const
        { return name_[dim_idx]; }

    size_t dim_index(const std::string& dim_name) const;

    const string_v& dim_names() const
        { return name_; }

    size_t rank() const
        { return length_.size(); }

    size_t volume() const;

    void step(size_t& base, size_t dim_idx, size_t n) const
        { base += n*delta(dim_idx); }

private:
    bool validindex(size_t dim_idx) const
        { return (dim_idx < rank()); }

private:
    string_v name_;
    size_v length_;
    size_v delta_;
    bool_v is_shared_;
};

// ========================================================================

void set_not_shared(const std::string& dim_name, ArrayDims& s1);
void set_not_shared(const std::string& dim_name, ArrayDims& s1, ArrayDims& s2);
void set_not_shared(const std::string& dim_name, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3);
void set_not_shared(const std::string& dim_name, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4);
void set_not_shared(const std::string& dim_name, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5);

void set_length(const std::string& dim_name, size_t length, ArrayDims& s1);
void set_length(const std::string& dim_name, size_t length, ArrayDims& s1, ArrayDims& s2);
void set_length(const std::string& dim_name, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3);
void set_length(const std::string& dim_name, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4);
void set_length(const std::string& dim_name, size_t length, ArrayDims& s1, ArrayDims& s2, ArrayDims& s3, ArrayDims& s4, ArrayDims& s5);

// ========================================================================

class ArrayGroup {
private:
    struct ArrayMap {
        ArrayMap(const ArrayDims& s, size_t rank);

        size_t delta(size_t group_dim) const
            { return array->delta(dim_index_map[group_dim]); }

        const ArrayDims* array;

        // translate dim indexes: dim_index_map[group dim index] = array dim index
        size_v dim_index_map;
    };
    typedef std::vector<ArrayMap> ArrayMap_v;

public:
    ArrayGroup();

    ArrayGroup& add(const ArrayDims& array);

    //! number of identical dims
    size_t sharedDims() const
        { return sharedDims_; }

    //! volume of identical dims
    size_t sharedVolume() const
        { return sharedVolume_; }

    size_t volume() const;

    size_t rank() const
        { return lengths.size(); }

    size_t length(size_t dim_idx) const
        { return lengths[dim_idx]; }

    size_t arrayCount() const
        { return arrays.size(); }

    size_t delta(size_t a, size_t dim_idx) const
        { return arrays[a].delta(dim_idx); }

    size_t position(const std::string& dim_name) const;

    void minimizeShared(size_t notShared);

private:
    size_t sharedDims_; //! number of identical dims
    size_t sharedVolume_; //! volume of dims in sameDims
    string_v dims;
    size_v lengths;
    ArrayMap_v arrays;
};

// ========================================================================

class Loop {
public:
    Loop(const ArrayGroup& s)
        : group(s)
        , dimPositions(group.rank(), 0)
        , arrayIndexes(group.arrayCount(), 0)
        { }

    //! step by group.sharedDims(); return false iff past end
    inline bool next();

    size_t operator[](size_t s) const
        { return arrayIndexes[s]; }

private:
    const ArrayGroup& group;

    //! position for each of the shared dimension in group
    size_v dimPositions;

    //! position inside each of the arrays in group
    size_v arrayIndexes;
};

inline bool Loop::next()
{
    bool carry = true;
    for (size_t group_dim = group.sharedDims(); carry && group_dim < group.rank(); ++group_dim) {
        const size_t length = group.length(group_dim);
        int singlestep = 1;
        dimPositions[group_dim] += 1;
        if (dimPositions[group_dim] == length) {
            dimPositions[group_dim] = 0;
            singlestep = -(length-1);
        } else {
            carry = false;
        }
        for (size_t s = 0; s < group.arrayCount(); ++s) {
            const size_t d = group.delta(s, group_dim);
            arrayIndexes[s] += d*singlestep;
        }
    }
    return !carry;
}


} // namespace MetNoFimex

#endif // MIFI_ARRAYLOOP_H
