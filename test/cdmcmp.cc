/*
 * Fimex, fiIndexGribs.cc
 *
 * (C) Copyright 2024-2026, met.no
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

#include "fimex/CDMReaderDecl.h"
#include "fimex/DataDecl.h"
#include "fimex/SharedArray.h"
#include <algorithm>
#include <fimex/CDM.h>
#include <fimex/CDMDataType.h>
#include <fimex/CDMFileReaderFactory.h>
#include <fimex/CDMReader.h>
#include <fimex/Data.h>
#include <fimex/String2Type.h>

#include <mi_programoptions.h>

#include <iostream>
#include <set>
#include <stdexcept>

using namespace MetNoFimex;
namespace po = miutil::program_options;

namespace {

void writeUsage(std::ostream& out, const po::option_set& options)
{
    out << "cdmcmp is used to compare datasets" << std::endl;
    out << std::endl;
    options.help(out);
}

bool is_nat(CDMDataType dt)
{
    return dt == CDM_NAT;
}

const std::set<std::string> getDimensionNames(const CDM& cdm)
{
    std::set<std::string> names;
    for (const auto& dim : cdm.getDimensions()) {
        names.insert(dim.getName());
    }
    return names;
}

const std::set<std::string> getVariableNames(const CDM& cdm)
{
    std::set<std::string> names;
    for (const auto& var : cdm.getVariables()) {
        names.insert(var.getName());
    }
    return names;
}

const std::set<std::string> getAttributeNames(const CDM& cdm, const std::string& varName)
{
    std::set<std::string> names;
    for (const auto& attr : cdm.getAttributes(varName)) {
        names.insert(attr.getName());
    }
    return names;
}

// from mi_cpptest
template <class C>
bool is_close(const C &a, const C &b, const C &tol)
{
  if (a == b)
    return true;
  const C d = std::abs(a - b), aa = std::abs(a), bb = std::abs(b);
  return (d <= aa * tol) && (d <= bb * tol);
}

template<typename T>
bool compareFloatValues(size_t count, shared_array<T> v1, shared_array<T> v2, T tol)
{
  for (size_t i = 0; i < count; ++i) {
    if (!is_close(v1[i], v2[i], tol))
      return false;
  }
  return true;
}

template<typename T>
bool compareValues(size_t count, shared_array<T> v1, shared_array<T> v2)
{
  return std::equal(&v1[0], &v1[0] + count, &v2[0]);
}

bool compareData(DataPtr d1, DataPtr d2, float tol)
{
    if (d1->size() != d2->size())
        return false;

    if (d1->getDataType() != d2->getDataType())
        return false;

    switch (d1->getDataType()) {
    case CDM_NAT: { return true; }
    case CDM_CHAR: { return compareValues(d1->size(), d1->asChar(), d2->asChar()); break; }
    case CDM_SHORT: { return compareValues(d1->size(), d1->asShort(), d2->asShort()); break; }
    case CDM_INT: { return compareValues(d1->size(), d1->asInt(), d2->asInt()); break; }
    case CDM_FLOAT: { return compareFloatValues(d1->size(), d1->asFloat(), d2->asFloat(), tol); break; }
    case CDM_DOUBLE: { return compareFloatValues(d1->size(), d1->asDouble(), d2->asDouble(), (double)tol); break; }
    case CDM_STRING: { return d1->asString() == d2->asString(); break; }
    case CDM_UCHAR: { return compareValues(d1->size(), d1->asUChar(), d2->asUChar()); break; }
    case CDM_USHORT: { return compareValues(d1->size(), d1->asUShort(), d2->asUShort()); break; }
    case CDM_UINT: { return compareValues(d1->size(), d1->asUInt(), d2->asUInt()); break; }
    case CDM_INT64: { return compareValues(d1->size(), d1->asInt64(), d2->asInt64()); break; }
    case CDM_UINT64: { return compareValues(d1->size(), d1->asUInt64(), d2->asUInt64()); break; }
    case CDM_STRINGS: { return compareValues(d1->size(), d1->asStrings(), d2->asStrings()); break; }
    }
    return false;
}

bool compareReaders(bool ignore_nat, bool silent, CDMReader_p r1, CDMReader_p r2, float tol)
{
    const auto& cdm1 = r1->getCDM();
    const auto& cdm2 = r2->getCDM();
    bool equal = true;

    const auto dims1 = getDimensionNames(cdm1);
    const auto dims2 = getDimensionNames(cdm2);
    if (dims1 != dims2) {
        if (!silent) {
            std::cerr << "dims1 != dims2" << std::endl;
        }
        return false;
    }

    for (const auto& dimname : dims1) {
        const auto& dim1 = cdm1.getDimension(dimname);
        const auto& dim2 = cdm2.getDimension(dimname);

        const auto l1 = dim1.getLength();
        const auto l2 = dim2.getLength();
        if (l1 != l2) {
            if (!silent) {
                std::cerr << "length difference for dim '" << dimname << "': " << l1 << " != " << l2 << std::endl;
            }
            equal = false;
        }

        if (dim1.isUnlimited() != dim2.isUnlimited()) {
            if (!silent) {
                std::cerr << "unlimited-ness difference for dim '" << dimname << "'" << std::endl;
            }
            equal = false;
        }
    }

    const auto vars1 = getVariableNames(cdm1);
    const auto vars2 = getVariableNames(cdm2);
    if (vars1 != vars2) {
        if (!silent) {
            std::cerr << "vars1 != vars2" << std::endl;
            return 1;
        }
    }

    for (const auto varname : vars1) {
        const auto& var1 = cdm1.getVariable(varname);
        const auto& var2 = cdm2.getVariable(varname);

        const auto& shape1 = var1.getShape();
        const auto& shape2 = var2.getShape();
        if (shape1 != shape2) {
            if (!silent) {
                std::cerr << "shape difference for var '" << varname << "'" << std::endl;
            }
            equal = false;
        }

        const auto a1 = getAttributeNames(cdm1, varname);
        const auto a2 = getAttributeNames(cdm2, varname);
        if (a1 != a2) {
            if (!silent) {
                std::cerr << "attribute names differ for var '" << varname << "'" << std::endl;
            }
            equal = false;
        }

        for (const auto& attname : a1) {
            const auto& att1 = cdm1.getAttribute(varname, attname);
            const auto& att2 = cdm2.getAttribute(varname, attname);

            const auto adt1 = att1.getDataType();
            const auto adt2 = att2.getDataType();
            if (adt1 != adt2) {
                if (!silent) {
                    std::cerr << "datatype difference for var '" << varname << "' attr '"
                              << attname << "': " << adt1 << " vs " << adt2 << std::endl;
                }
                if (ignore_nat && (is_nat(adt1) || is_nat(adt2))) {
                    if (!silent) {
                        std::cerr << "  ignored as one of the datatypes is NAT" << std::endl;
                    }
                } else {
                    equal = false;
                }
            } else if (!compareData(att1.getData(), att2.getData(), tol)) {
                if (!silent) {
                    std::cerr << "data difference for var '" << varname << "' attr '" << attname << "'" << std::endl;
                }
                equal = false;
            }
        }

        const auto dt1 = var1.getDataType();
        const auto dt2 = var2.getDataType();
        if (dt1 != dt2) {
            if (!silent) {
                std::cerr << "datatype difference for var '" << varname << "': " << dt1 << " vs " << dt2 << std::endl;
            }
            if (ignore_nat && (is_nat(dt1) || is_nat(dt2))) {
                if (!silent) {
                    std::cerr << "  ignored as one of the datatypes is NAT" << std::endl;
                }
            } else {
                equal = false;
            }
        } else if (!compareData(r1->getData(varname), r2->getData(varname), tol)) {
            if (!silent) {
                std::cerr << "data difference for var '" << varname << "'" << std::endl;
            }
            equal = false;
        }
    }
    return equal;
}

} // namespace

int main(int argc, char* args[])
{
    const po::option op_help = po::option("help", "help message").set_shortkey("h").set_narg(0);
    const po::option op_silent = po::option("silent", "do not write messages").set_narg(0);
    const po::option op_refuse_nat = po::option("no-ignore-nat", "refuse different datatypes even if one of them is NAT").set_narg(0);
    const po::option op_tol = po::option("tolerance", "tolerance for float/double equality").set_shortkey("tol").set_default_value("0.001");
    const po::option op_t1 = po::option("type-1", "filetype for first dataset").set_shortkey("t1").set_default_value("");
    const po::option op_c1 = po::option("config-1", "config for first dataset").set_shortkey("c1").set_default_value("");
    const po::option op_t2 = po::option("type-2", "filetype for second dataset").set_shortkey("t2").set_default_value("");
    const po::option op_c2 = po::option("config-2", "config for second dataset").set_shortkey("c2").set_default_value("");

    po::option_set options;
    options
        << op_help
        << op_silent
        << op_refuse_nat
        << op_tol
        << op_t1
        << op_c1
        << op_t1
        << op_c1
        ;

    // read the options
    po::string_v positional;
    po::value_set vm = po::parse_command_line(argc, args, options, positional);

    if (argc == 1 || vm.is_set(op_help)) {
        writeUsage(std::cout, options);
        return 0;
    }

    const bool silent = vm.is_set(op_silent);
    if (positional.size() != 2) {
        if (!silent) {
            writeUsage(std::cout, options);
        }
        return 1;
    }

    const bool ignore_nat = !vm.is_set(op_refuse_nat);
    const float tol = string2type<float>(vm.value(op_tol));

    const std::string& t1 = vm.value(op_t1);
    const std::string& f1 = positional[0];
    const std::string& c1 = vm.value(op_c1);

    const std::string& t2 = vm.value(op_t2);
    const std::string& f2 = positional[1];
    const std::string& c2 = vm.value(op_c2);

    if (!silent) {
        std::cout << "comparing '" << f1 << "' ..." << std::endl
                  << "     with '" << f2 << "' ..." << std::endl;
    }

    try {
        const auto r1 = CDMFileReaderFactory::create(t1, f1, c1);
        const auto r2 = CDMFileReaderFactory::create(t2, f2, c2);

        if (!compareReaders(ignore_nat, silent, r1, r2, tol)) {
            return 1;
        }
    } catch (std::runtime_error& ex) {
        std::cerr << "exception during comparison: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}
