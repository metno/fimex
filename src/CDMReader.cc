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

#include "fimex/CDMReader.h"
#include <boost/bind.hpp>

namespace MetNoFimex {

static void addSizeUnlessUnlimited(const CDM& cdm, const std::string& dimName, size_t& size) {
	const CDMDimension& dim = cdm.getDimension(dimName);
	if (!dim.isUnlimited()) {
		size *= dim.getLength();
	}
}

// retrieve size of data slice
static size_t getSliceSize(const CDM& cdm, const CDMVariable& variable) {
	size_t sliceSize = 1;
	std::vector<std::string> shape = variable.getShape();
	for_each(shape.begin(), shape.end(), boost::bind(addSizeUnlessUnlimited, cdm, _1, sliceSize));
	return sliceSize;
}

const boost::shared_ptr<Data> CDMReader::getDataFromMemory(const CDMVariable& variable, size_t unLimDimPos) throw(CDMException)
{
	if (variable.hasData()) {
		if (cdm.hasUnlimitedDim(variable)) {
			// cut out the unlimited dim data
			size_t sliceSize = getSliceSize(cdm, variable);
			return createDataSlice(variable.getDataType(), *(variable.getData()), unLimDimPos*sliceSize, sliceSize);
		} else {
			return variable.getData();
		}
	} else {
		return boost::shared_ptr<Data>();
	}
}

}
