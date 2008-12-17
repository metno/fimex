/*
 * Fimex, testGribWriter.cc
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
 *  Created on: Dec 17, 2008
 *      Author: Heiko Klein
 */

#include "fimex/config.h"
#ifdef HAVE_BOOST_UNIT_TEST_FRAMEWORK

#include <iostream>
#include <fstream>
#include <boost/shared_ptr.hpp>
#include "fimex/FeltCDMReader.h"
#include "fimex/GribApiCDMWriter.h"

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

using namespace std;
using namespace MetNoFelt;
using namespace MetNoFimex;


BOOST_AUTO_TEST_CASE( test_feltGrib1Write )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	string outputFile("test.grb1");
	GribApiCDMWriter(feltReader, outputFile, 1, topSrcDir+"/share/etc/cdmGribWriterConfig.xml");
	BOOST_CHECK((ifstream(outputFile.c_str()) != 0));
}
BOOST_AUTO_TEST_CASE( test_feltGrib2Write )
{
	string topSrcDir(TOP_SRCDIR);
	string fileName(topSrcDir+"/test/flth00.dat");
	if (!ifstream(fileName.c_str())) {
		// no testfile, skip test
		return;
	}
	boost::shared_ptr<CDMReader> feltReader(new FeltCDMReader(fileName, topSrcDir+"/share/etc/felt2nc_variables.xml"));
	string outputFile("test.grb2");
	GribApiCDMWriter(feltReader, outputFile, 2, topSrcDir+"/share/etc/cdmGribWriterConfig.xml");
	BOOST_CHECK((ifstream(outputFile.c_str()) != 0));
}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
