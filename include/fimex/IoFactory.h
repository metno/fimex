/*
 * Fimex, IoFactory.h
 *
 * (C) Copyright 2019-2024, met.no
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

#ifndef FIMEX_IOFACTORY_H
#define FIMEX_IOFACTORY_H

#include "fimex/CDMReaderDecl.h"

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace MetNoFimex {

class XMLInput;

class IoFactory;
typedef std::shared_ptr<IoFactory> IoFactory_p;
typedef std::map<std::string, IoFactory_p> IoFactory_pm;

class IoFactory
{
public:
    virtual ~IoFactory() = 0;

    virtual size_t matchMagicSize();
    virtual int matchMagic(const char* magic, size_t count);

    virtual int matchFileTypeName(const std::string& type) = 0;
    virtual int matchFileName(const std::string& fileName);

    virtual CDMReader_p createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                     const std::vector<std::string>& args) = 0;
    virtual CDMReaderWriter_p createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                                 const std::vector<std::string>& args);
    virtual void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config);

    static bool install(const std::string& id, IoFactory_p iof);
    static IoFactory_pm& factories();
};

} // namespace MetNoFimex

#endif // FIMEX_IOFACTORY_H
