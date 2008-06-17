/*
 * Fimex
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
 */

#include "fimex/CachedVectorReprojection.h"
#include "fimex/CDMException.h"
#include "fimex/DataImpl.h"
#include <iostream>

namespace MetNoFimex
{

void CachedVectorReprojection::reprojectValues(boost::shared_ptr<Data>& uValues, boost::shared_ptr<Data>& vValues, float uBadValue, float vBadValue) const throw(CDMException)
{
	if (ox == 0 || oy == 0 || matrix.get() == 0) {
		throw CDMException("CachedVectorReprojection not initialized"); 
	}
	size_t oz = uValues->size() / (ox*oy);
	boost::shared_array<float> u = uValues->asFloat();
	boost::shared_array<float> v = uValues->asFloat();
	mifi_bad2nanf(&u[0], &u[uValues->size()], uBadValue);
	mifi_bad2nanf(&v[0], &v[vValues->size()], vBadValue);
	int errcode = mifi_vector_reproject_values_by_matrix_f(method, matrix.get(), &u[0], &v[0], ox, oy, oz);
	if (errcode != MIFI_OK)	throw CDMException("Error during reprojection of vector-values");
	mifi_nanf2bad(&u[0], &u[uValues->size()], uBadValue);
	mifi_nanf2bad(&v[0], &v[vValues->size()], vBadValue);
	size_t size = ox*oy*oz;
	uValues->setValues(0, DataImpl<float>(u,size), 0, size);
	vValues->setValues(0, DataImpl<float>(v,size), 0, size);
}

}
