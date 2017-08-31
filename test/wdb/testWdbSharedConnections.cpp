/*
 * Fimex, testWdbSharedConnections.cpp
 *
 * (C) Copyright 2011, met.no
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
 *  Created on: Dec 1, 2011
 *      Author: Heiko Klein
 */

/**
 * This is not a unit test, since wdb cannot be guarranteed to be set up on fixed port.
 *
 * Compile with:
 *       gcc -L../../src/.libs -I../../include -lfimex -o testWdbSharedConnections testWdbSharedConnections.cpp
 *
 * Run: fix the include wdb.wdbml file and run
 *       LD_LIBRARY_PATH=../../src/.libs ./testWdbSharedConnections
 *
 * check number of connections with i.e.
 *       lsof | grep postgres
 */

#include "fimex/CDMFileReaderFactory.h"
#include "fimex/CDMconstants.h"
#include "fimex/CDMReader.h"
#include "fimex/XMLInput.h"
#include "fimex/Data.h"

#include <iostream>
#include <boost/shared_ptr.hpp>
#include <vector>
#include <ctime>

using namespace std;
using namespace MetNoFimex;

int main(int argc, char* argv[])
{
    boost::shared_ptr<CDMReader> r1 = CDMFileReaderFactory::create(MIFI_FILETYPE_WDB, "wdb.wdbml", XMLInputFile(TOP_SRCDIR "/share/etc/wdb_config.xml"));
    DataPtr x = r1->getData("time");
    if (x->size() > 0) {
        clog << "1. connection successfully established" << endl;
    } else {
        cerr << "1. connection not working" << endl;
        exit(1);
    }

    boost::shared_ptr<CDMReader> r2 = CDMFileReaderFactory::create(MIFI_FILETYPE_WDB, "wdb.wdbml", XMLInputFile(TOP_SRCDIR "/share/etc/wdb_config.xml"));
    x = r2->getData("time");
    if (x->size() > 0) {
        clog << "2. connection successfully established" << endl;
    } else {
        cerr << "2. connection not working" << endl;
        exit(1);
    }

    r1.reset();
    x = r2->getData("time");
    if (x->size() > 0) {
        clog << "1. connection disabled. 2. connection still working" << endl;
    } else {
        cerr << "2. connection fails after disconnecting 1." << endl;
        exit(1);
    }


    const size_t MAX_CONN = 1000;
    vector<boost::shared_ptr<CDMReader> > rv;
    long long startTime = time(0);
    size_t startI = 0;
    for (size_t i = 0; i < MAX_CONN; ++i) {
        boost::shared_ptr<CDMReader> r = CDMFileReaderFactory::create(MIFI_FILETYPE_WDB, "wdb.wdbml", XMLInputFile(TOP_SRCDIR "/share/etc/wdb_config.xml"));
        DataPtr x = r->getData("time");
        if (x->size() == 0) {
            cerr << "error after " << i << ". connection";
            exit(1);
        }
        rv.push_back(r);
        if ((rv.size() % 100) == 0) {
            float elaps = static_cast<float>(time(0) - startTime) / (i - startI);
            clog << "successfully created " << rv.size() << " simultaneous connections: " << elaps << "s/CDM" << endl;
            startI = i;
            startTime = time(0);
        }
    }

    return 0;
}
