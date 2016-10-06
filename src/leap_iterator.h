#ifndef LEAP_ITERATOR_HH
#define LEAP_ITERATOR_HH 1

#include <boost/iterator/iterator_adaptor.hpp>

/**
 * Iterator adaptor that leaps forward/backward:
 * one step in the leap_iterator are n steps in the adapted iterator.
 */
template <class I>
class leap_iterator
    : public boost::iterator_adaptor< leap_iterator<I>, I>
{
    typedef boost::iterator_adaptor< leap_iterator<I>, I> super_t;
    friend class boost::iterator_core_access;

public:
    leap_iterator() {}

    explicit leap_iterator(I x, typename super_t::difference_type step)
        : super_t(x), step_(step) { }

    template<class O>
    leap_iterator(leap_iterator<O> const& o, typename boost::enable_if_convertible<O, I>::type* = 0)
        : super_t(o.base()), step_(o.step_) { }

private:
    typename super_t::reference dereference() const
        { return *this->base(); }

    void increment() { this->base_reference() += step_; }
    void decrement() { this->base_reference() -= step_; }

    void advance(typename super_t::difference_type n)
        { this->base_reference() += n*step_; }

    template<class O>
    typename super_t::difference_type
    distance_to(leap_iterator<O> const& o) const
        { return (o.base() - this->base_reference()) / step_; }

    typename super_t::difference_type step_;
};

template <class I>
inline leap_iterator<I> make_leap_iterator(I x, typename std::iterator_traits<I>::difference_type step)
{
    return leap_iterator<I>(x, step);
}

#endif // LEAP_ITERATOR_HH
