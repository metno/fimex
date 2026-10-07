// From comment on "C++ 20" version on https://www.reedbeta.com/blog/python-like-enumerate-in-cpp17/
// Written by Nathan Reed and Tin Švagelj

#ifndef FIMEX_ENUMERATE_H
#define FIMEX_ENUMERATE_H 1

#include <concepts>
#include <ranges>

template <std::ranges::range T>
constexpr auto enumerate(T&& iterable)
{
    class iterator
    {
        std::ranges::range_difference_t<T> i{};
        std::ranges::iterator_t<T> iter;

    public:
        iterator(std::ranges::iterator_t<T> it)
            : iter(it)
        {
        }

        bool operator!=(const iterator& other) const { return iter != other.iter; }

        void operator++()
        {
            ++i;
            ++iter;
        }

        auto operator*() const { return std::tie(i, *iter); }
    };
    struct iterable_wrapper
    {
        T iterable;
        auto begin() { return iterator(std::begin(iterable)); }
        auto end() { return iterator(std::end(iterable)); }
    };
    return iterable_wrapper{std::forward<T>(iterable)};
}

#endif // FIMEX_ENUMERATE_H
