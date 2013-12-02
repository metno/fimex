/*
 * Fimex, MutexLock.h
 *
 * (C) Copyright 2012, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Jan 3, 2012
 *      Author: Heiko Klein
 */

#ifndef MUTEXLOCK_H_
#define MUTEXLOCK_H_

#include <boost/noncopyable.hpp>

namespace MetNoFimex
{


#ifdef _OPENMP
# include <omp.h>
struct MutexType : boost::noncopyable
{
    MutexType() {omp_init_lock(&lock_);}
    ~MutexType() {omp_destroy_lock(&lock_);}
    void lock() {omp_set_lock(&lock_);}
    void unlock() {omp_unset_lock(&lock_);}

public:
    omp_lock_t lock_;
};
#else
/* A dummy mutex that doesn't actually exclude anything,
 * but as there is no parallelism either, no worries. */
struct MutexType : boost::noncopyable
{
    void lock() {}
    void unlock() {}
};
#endif

/* An exception-safe scoped lock-keeper. */
class ScopedCritical : boost::noncopyable
{
public:
    explicit ScopedCritical(MutexType& m) : mut(m) {
        mut.lock();
#ifdef _OPENMP
#pragma omp flush
#endif
    }
    ~ScopedCritical() {
#ifdef _OPENMP
#pragma omp flush
#endif
        mut.unlock();
    }
private: MutexType & mut;
};

} // namespace
#endif /* MUTEXLOCK_H_ */
