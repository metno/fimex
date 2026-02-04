/*
 * Fimex, ThreadPool.h
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
 *  Created on: Jan 4, 2012
 *      Author: Heiko Klein
 */

#ifndef THREADPOOL_H_
#define THREADPOOL_H_

#ifdef __cplusplus
extern "C" {
#endif

/**
  * @brief Set the number of threads.
  *
  * This function may not be set from a parallel region.
  *
  * @param n the number of threads, if 0, use the number of processors
  * @return MIFI_OK or MIFI_ERROR
  */
extern int mifi_setNumThreads(int n);

#ifdef __cplusplus
}
#endif

#endif /* THREADPOOL_H_ */
