/*
 * Fimex, IoFactory.h
 *
 * (C) Copyright 2019-2022, met.no
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

#ifndef FIMEX_DLIOFACTORY_H
#define FIMEX_DLIOFACTORY_H

#include "fimex/IoFactory.h"

extern "C" {
struct IoPluginInfo
{
    int plugin_api_version;
    const char* name;
    void* (*create)();
    void (*destroy)(void*);
};

typedef const IoPluginInfo*(ioplugin_info_t)();
}

#define CURRENT_IO_PLUGIN_API_VERSION 0
#define DEFINE_IO_PLUGIN(name, iofactoryclass) \
namespace { \
void* plugin_create() { return new iofactoryclass; } \
void plugin_destroy(void* factory) { delete static_cast<iofactoryclass*>(factory); } \
const IoPluginInfo plugin{CURRENT_IO_PLUGIN_API_VERSION, name, plugin_create, plugin_destroy}; \
} /* namespace */ \
extern "C" { \
const IoPluginInfo* fimex_ioplugin_info() { return &plugin; } \
} /* extern "C" */


namespace MetNoFimex {

class IoPlugin : public IoFactory
{
public:
    IoPlugin(const std::string& lib_path);
    ~IoPlugin();

    std::string name() const;

    size_t matchMagicSize() override;
    int matchMagic(const char* magic, size_t count) override;

    int matchFileTypeName(const std::string& type) override;
    int matchFileName(const std::string& fileName) override;

    CDMReader_p createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                             const std::vector<std::string>& args) override;
    CDMReaderWriter_p createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                         const std::vector<std::string>& args) override;
    void createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const std::string& config) override;

private:
    void* lib_;
    const IoPluginInfo* plugin_;
    IoFactory_p wrapped_;
};

} // namespace MetNoFimex

#endif // FIMEX_DLIOFACTORY_H
