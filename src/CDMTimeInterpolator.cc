/*
 * Fimex, CDMTimeInterpolator.cpp
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

#include "../include/fimex/CDMTimeInterpolator.h"

namespace MetNoFimex
{

CDMTimeInterpolator::CDMTimeInterpolator(boost::shared_ptr<CDMReader> dataReader)
   : dataReader(dataReader)
{
	cdm = dataReader->getCDM();
}

CDMTimeInterpolator::~CDMTimeInterpolator()
{
}

const boost::shared_ptr<Data> CDMTimeInterpolator::getDataSlice(const std::string& varName, size_t unLimDimPos) throw(CDMException)
{
	// TODO
}

void CDMTimeInterpolator::changeTimeAxis(std::string timeSpec) throw(CDMException)
{
	// TODO
}


} /* MetNoFimex */
