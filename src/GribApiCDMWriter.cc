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

#include "fimex/GribApiCDMWriter.h"
#include <fcntl.h>
#include <grib_api.h>
#include <cstdio>
#include <boost/shared_ptr.hpp>

namespace MetNoFimex
{

GribApiCDMWriter::GribApiCDMWriter(const boost::shared_ptr<CDMReader> cdmReader, const std::string& outputFile, const std::string& configFile)
: CDMWriter(cdmReader, outputFile), configFile(configFile)
{	
	// open the file
	int fd = open(outputFile.c_str(), O_CREAT|O_WRONLY|O_LARGEFILE);
	if (fd == -1) throw CDMException("Cannot write grib-file: "+outputFile);
	boost::shared_ptr<FILE> file(fdopen(fd, "w"), fclose);
	if (file.get() == 0) throw CDMException("Cannot write grib-file with filedescriptor: "+outputFile);

	// get the major grib-handle, including projection and x/y axes
	boost::shared_ptr<grib_handle> mainGH(grib_handle_new_from_template(0, "GRIB1"), grib_handle_delete);
	if (mainGH.get() == 0) throw CDMException("unable to open grib_handle_from_template");
	
	const CDM& cdm = cdmReader->getCDM();
	const CDM::VarVec& vars = cdm.getVariables();
	// iterator over all variables
	for (CDM::VarVec::const_iterator vi = vars.begin(); vi != vars.end(); ++vi) {
		// TODO: detect projection, axes and write data
	}
}

GribApiCDMWriter::~GribApiCDMWriter()
{
}

}
