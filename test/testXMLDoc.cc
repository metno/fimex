/*
 * Fimex
 *
 * (C) Copyright 2009, met.no
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

#include "fimex/config.h"
#include <boost/version.hpp>
#if defined(HAVE_BOOST_UNIT_TEST_FRAMEWORK) && (BOOST_VERSION >= 103400)

#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include <iostream>
#include "fimex/XMLDoc.h"
#include <libxml/tree.h>
#include <libxml/xpath.h>

using namespace std;
using namespace MetNoFimex;

BOOST_AUTO_TEST_CASE( test_xmlDoc )
{
    string topSrcDir(TOP_SRCDIR);
    XMLDoc doc(topSrcDir+"/test/testXMLDoc.xml");
    {
        XPathObjPtr xpathObj = doc.getXPathObject("/cdmQualityConfig");
        xmlNodeSetPtr nodes = xpathObj->nodesetval;
        int size = (nodes) ? nodes->nodeNr : 0;
        BOOST_CHECK(size == 1);
        // check a subnode
        xpathObj = doc.getXPathObject("variable/status_flag_variable[@name=\"blub\"]", nodes->nodeTab[0]);
        xmlNodeSetPtr flagNodes = xpathObj->nodesetval;
        int flagNodesSize = (flagNodes) ? flagNodes->nodeNr : 0;
        BOOST_CHECK(flagNodesSize >= 1);
        // check a subnode with full path
        xpathObj = doc.getXPathObject("/cdmQualityConfig/variable/status_flag_variable[@name=\"blub\"]");
        xmlNodeSetPtr flagNodes2 = xpathObj->nodesetval;
        int flagNodesSize2 = (flagNodes2) ? flagNodes2->nodeNr : 0;
        BOOST_CHECK(flagNodesSize == flagNodesSize2);
    }

}

#else
// no boost testframework
int main(int argc, char* args[]) {
}
#endif
