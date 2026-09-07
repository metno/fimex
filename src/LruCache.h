/*
  Fimex, src/LruCache.h

  Copyright (C) 2026 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: fimx@met.no

  Project Info:  https://github.com/metno/fimex/wiki

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
  License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/

#ifndef FIMEX_LRUCACHE_H
#define FIMEX_LRUCACHE_H 1

#include <cstddef>
#include <functional>
#include <list>
#include <unordered_map>
#include <utility>

namespace MetNoFimex {

template <typename K, typename V>
class LruCache
{
public:
    explicit LruCache(size_t capacity = 0) : capacity_(capacity) {}

    // Return the cached value if present, otherwise create it with creator
    // and insert into the cache (unless capacity_ == 0, which disables caching).
    // The creator is only invoked when necessary.
    V getOrInsert(const K& key, const std::function<V()>& creator)
    {
        if (capacity_ == 0) {
            return creator();
        }

        auto it = map_.find(key);
        if (it != map_.end()) {
            // move to front
            lru_.splice(lru_.begin(), lru_, it->second.second);
            it->second.second = lru_.begin();
            return it->second.first;
        }

        V value = creator();
        lru_.push_front(key);
        map_.emplace(key, std::make_pair(value, lru_.begin()));

        if (map_.size() > capacity_) {
            const K& last = lru_.back();
            map_.erase(last);
            lru_.pop_back();
        }

        return value;
    }

    void erase(const K& key)
    {
        auto it = map_.find(key);
        if (it == map_.end())
            return;
        lru_.erase(it->second.second);
        map_.erase(it);
    }

    void clear()
    {
        map_.clear();
        lru_.clear();
    }

    size_t capacity() const { return capacity_; }
    void setCapacity(size_t c) { capacity_ = c; if (capacity_ == 0) clear(); }
    size_t size() const { return map_.size(); }

private:
    size_t capacity_;
    std::list<K> lru_;
    std::unordered_map<K, std::pair<V, typename std::list<K>::iterator>> map_;
};

} // namespace MetNoFimex

#endif // FIMEX_LRUCACHE_H
