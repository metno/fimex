/*
 * Fimex, ThreadPool.cc
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
 *  Created on: Jan 4, 2012
 *      Author: Heiko Klein
 */

#include "fimex/ThreadPool.h"
#ifdef _OPENMP
#include <omp.h>
#endif
#include "fimex/mifi_constants.h"

int mifi_setNumThreads(int n)
{
#ifdef _OPENMP
    if (omp_in_parallel() != 0) {
        return MIFI_ERROR;
    }
    if (n > 0) {
        omp_set_num_threads(n);
    } else {
        omp_set_num_threads(omp_get_num_procs());
    }
#endif
    return MIFI_OK;
}

