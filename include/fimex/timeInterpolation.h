/*
 * Fimex, timeInterpolation.h
 *
 * (C) Copyright 2008, met.no
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
 *  Created on: Dec 3, 2008
 *      Author: Heiko Klein
 */

#ifndef TIMEINTERPOLATION_H_
#define TIMEINTERPOLATION_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Linear interpolation/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 * with o(x) = in(a) + x * (in(a) - in(b)) / (a - b)
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 *
 */
extern void mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);



#ifdef __cplusplus
}
#endif

#endif /* TIMEINTERPOLATION_H_ */
