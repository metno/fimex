/*
 * Fimex, MutexLock.h
 *
 * (C) Copyright 2012-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
#endif

namespace MetNoFimex {

class OmpMutex
{
public:
    OmpMutex()
    {
#ifdef _OPENMP
        omp_init_lock(&lock_);
#endif
    }

    ~OmpMutex()
    {
#ifdef _OPENMP
        omp_destroy_lock(&lock_);
#endif
    }

    OmpMutex(const OmpMutex&) = delete;
    OmpMutex& operator=(const OmpMutex&) = delete;

    void lock()
    {
#ifdef _OPENMP
        omp_set_lock(&lock_);
#pragma omp flush
#endif
    }

    void unlock()
    {
#ifdef _OPENMP
#pragma omp flush
        omp_unset_lock(&lock_);
#endif
    }

private:
#ifdef _OPENMP
    omp_lock_t lock_;
#endif
};

/* An exception-safe scoped lock-keeper. */
class OmpScopedLock
{
public:
    OmpScopedLock(OmpMutex& m)
        : mut(m)
    {
        mut.lock();
    }
    ~OmpScopedLock() { mut.unlock(); }

    OmpScopedLock(const OmpScopedLock&) = delete;
    OmpScopedLock& operator=(const OmpScopedLock&) = delete;

private:
    OmpMutex& mut;
};

/* An exception-safe scoped lock-unlocker. */
class OmpScopedUnlock
{
public:
    OmpScopedUnlock(OmpMutex& m)
        : mut(m)
    {
        mut.unlock();
    }
    ~OmpScopedUnlock() { mut.lock(); }

    OmpScopedUnlock(const OmpScopedUnlock&) = delete;
    OmpScopedUnlock& operator=(const OmpScopedUnlock&) = delete;

private:
    OmpMutex& mut;
};

} // namespace MetNoFimex

#endif /* MUTEXLOCK_H_ */
