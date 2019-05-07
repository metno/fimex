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

#ifdef _OPENMP
#include <omp.h>
#define IF_OPENMP(x) x
#else
#define IF_OPENMP(x)
#endif

namespace MetNoFimex
{

struct MutexType
{
    MutexType(const MutexType&) = delete;
    MutexType& operator=(const MutexType&) = delete;

    MutexType() { IF_OPENMP(omp_init_lock(&lock_)); }
    ~MutexType() { IF_OPENMP(omp_destroy_lock(&lock_)); }
    void lock() { IF_OPENMP(omp_set_lock(&lock_)); }
    void unlock() { IF_OPENMP(omp_unset_lock(&lock_)); }

public:
#ifdef _OPENMP
    omp_lock_t lock_;
#endif
};

/* An exception-safe scoped lock-keeper. */
class ScopedCritical
{
public:
    explicit ScopedCritical(MutexType& m) : mut(m) {
        mut.lock();
#ifdef _OPENMP
#pragma omp flush
#endif
    }
    ScopedCritical(const ScopedCritical&) = delete;
    ScopedCritical& operator=(const ScopedCritical&) = delete;

    ~ScopedCritical() {
#ifdef _OPENMP
#pragma omp flush
#endif
        mut.unlock();
    }

private:
    MutexType& mut;
};

#undef IF_OPENMP

} // namespace
#endif /* MUTEXLOCK_H_ */
