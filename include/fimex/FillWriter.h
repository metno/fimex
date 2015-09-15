/*
 * Fimex, FillWriter.h
 *
 * (C) Copyright 2013, met.no
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
 *  Created on: Mar 19, 2013
 *      Author: heikok
 */

#ifndef FILLWRITER_H_
#define FILLWRITER_H_

#include <fimex/CDMReaderWriter.h>

namespace MetNoFimex
{

/**
 * @headerfile fimex/FillWriter.h
 */


/**
 * The FillWriter will insert the data from the reader to an existing file. This is useful
 * when combining NWP model output, coming slice by slice (e.g. timestep or level), into one
 * large file. In contrast to NCO's ncrcat http://nco.sourceforge.net/nco.html#Concatenation
 * the FillWriter will not append the data at the end of the unlimited-dimension, but
 * will check the dimension's values and put the data to the right position.
 *
 * The output-file should contain already all dimensions in the right size and with the
 * right dimension-values, e.g. created by ncgen or similar.
 *
 * The FillWriter will not try to write new variables. It might try to append new
 * dimension-values to the end of exiting dimensions if allowed by the CDMReaderWriter.
 *
 * No attributes are changed in the output-file.
 */
class FillWriter
{
public:
    FillWriter(boost::shared_ptr<CDMReader> in, boost::shared_ptr<CDMReaderWriter> io, std::string configFileName = "");
    ~FillWriter();

};

} /* namespace MetNoFimex */
#endif /* FILLWRITER_H_ */
