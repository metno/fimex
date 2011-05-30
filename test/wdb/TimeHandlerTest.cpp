/*
    fimex

    Copyright (C) 2011 met.no

    Contact information:
    Norwegian Meteorological Institute
    Box 43 Blindern
    0313 OSLO
    NORWAY
    E-mail: post@met.no

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA  02110-1301, USA
*/

#include "../config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#include <boost/test/unit_test.hpp>

#include "GridDataFactory.h"
#include <wdb/TimeHandler.h>
#include <wdb/config/GlobalWdbConfiguration.h>
#include <wdb/Wdb2CdmBuilder.h>
#include <wdb/WdbIndex.h>
#include <fimex/CDM.h>

BOOST_AUTO_TEST_SUITE(TimeHandlerTest)

using namespace MetNoFimex::wdb;

class TimeHandlerFixture : public GridDataFactory
{
public:
	TimeHandlerFixture() : tr(TEST_DIR"/wdb_config.xml") {}

	GlobalWdbConfiguration tr;
	MetNoFimex::CDM cdm;
};

BOOST_FIXTURE_TEST_CASE(timeIndexLoopkup, TimeHandlerFixture)
{
	add("2011-03-31 06:00:00");
	add("2011-03-31 18:00:00");
	add("2011-03-31 12:00:00");
	add("2011-04-01 06:00:00");

	const Wdb2CdmBuilder di(gridData(), tr);
	di.populate(cdm);

	Wdb2CdmBuilder::Time t = boost::posix_time::time_from_string("2011-03-31 06:00:00");
	boost::posix_time::time_duration offset(6, 0, 0);
//	BOOST_CHECK_EQUAL(t, di.timeFromIndex(1));
//	BOOST_CHECK_EQUAL(t + offset, di.timeFromIndex(2));
//	BOOST_CHECK_EQUAL(t + (offset * 2), di.timeFromIndex(3));
//	BOOST_CHECK_EQUAL(t + (offset * 4), di.timeFromIndex(4));
}

BOOST_FIXTURE_TEST_CASE(throwOnInvalidTimeIndexLoopkup, TimeHandlerFixture)
{
	add("2011-03-31 06:00:00");
	add("2011-03-31 18:00:00");

	const Wdb2CdmBuilder di(gridData(), tr);
	di.populate(cdm);

	//BOOST_CHECK_THROW(di.timeFromIndex(0), CDMException);
//	di.timeFromIndex(1);
//	di.timeFromIndex(2);
//	BOOST_CHECK_THROW(di.timeFromIndex(3), CDMException);
}

BOOST_AUTO_TEST_SUITE_END()

#endif
