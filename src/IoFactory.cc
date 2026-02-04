/*
 * Fimex, IoFactory.h
 *
 * (C) Copyright 2019-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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

#include "fimex/IoFactory.h"

#include "fimex/CDMException.h"
#include "fimex/FileUtils.h"

namespace MetNoFimex {

IoFactory::~IoFactory() {}

size_t IoFactory::matchMagicSize()
{
    return 0;
}

int IoFactory::matchMagic(const char*, size_t)
{
    return 0;
}

int IoFactory::matchFileName(const std::string& fileName)
{
    return matchFileTypeName(getExtension(fileName));
}

CDMReaderWriter_p IoFactory::createReaderWriter(const std::string& fileTypeName, const std::string&, const XMLInput&, const std::vector<std::string>&)
{
    throw CDMException("unable to create reader-writer for file type '" + fileTypeName + "'");
}

void IoFactory::createWriter(CDMReader_p, const std::string& fileTypeName, const std::string&, const XMLInput&)
{
    throw CDMException("unable to write file type: '" + fileTypeName + "'");
}

// static
IoFactory_pm& IoFactory::factories()
{
    static IoFactory_pm factories;
    return factories;
}

// static
bool IoFactory::install(const std::string& id, IoFactory_p iof)
{
    if (iof) {
        factories()[id] = iof;
        return true;
    } else {
        factories().erase(id);
        return false;
    }
}

} // namespace MetNoFimex
