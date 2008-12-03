/*
 * Fimex, timeInterpolation.c
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
 *      Author: heikok
 */

#include "timeInterpolation.h"

void mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
	double f = x / (a - b);
	int i = 0;
	while (n > i++) {
		const float* iA = infieldA++;
		const float* iB = infieldB++;
		float* o = outfield++;
		*o = *iA + f * (*iA - *iB);
	}
	return;
}
