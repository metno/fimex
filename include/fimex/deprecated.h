/*
 * Fimex, deprecated.h
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Apr 28, 2010
 *      Author: Heiko Klein
 */

#ifndef MIFI_DEPRECATED_H_
#define MIFI_DEPRECATED_H_

#ifdef __GNUC__
#define MIFI_DEPRECATED(func) func __attribute__ ((deprecated))
#elif defined(_MSC_VER)
#define MIFI_DEPRECATED(func) __declspec(deprecated) func
#elif defined(__IBMC__) || defined(__IBMCPP__)
/* "DEPRECATED not implemented for IBM compilers" */
#define MIFI_DEPRECATED(func) func
#else
#pragma message("WARNING: You need to implement DEPRECATED for this compiler")
#define MIFI_DEPRECATED(func) func
#endif

#endif /* MIFI_DEPRECATED_H_ */
