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

#include "fimex/IoPlugin.h"

#include "fimex/CDMException.h"
#include "fimex/IoFactory.h"

#include <memory>
#include <sstream>

#include <dlfcn.h>

#define STRINGIFY(x) #x

// see https://tldp.org/HOWTO/html_single/C++-dlopen/

namespace MetNoFimex {

namespace {

void* load_sym(void* lib, const char* sym)
{
    void* lib_sym = dlsym(lib, sym);
    const char* dlsym_error = dlerror();
    if (dlsym_error) {
        std::ostringstream msg;
        msg << "Cannot load symbol '" << sym << "':" << dlsym_error;
        throw CDMException(msg.str());
    }
    return lib_sym;
}

} // namespace

IoPlugin::IoPlugin(const std::string& lib_path)
    : lib_(nullptr)
    , plugin_(nullptr)
{
    lib_ = dlopen(lib_path.c_str(), RTLD_LAZY);
    if (!lib_) {
        std::ostringstream msg;
        msg << "Cannot load library '" << lib_path << "': " << dlerror();
        throw CDMException(msg.str());
    }
    dlerror();

    ioplugin_info_t* iop_info = (ioplugin_info_t*)load_sym(lib_, "fimex_ioplugin_info");
    if (!iop_info) {
        std::ostringstream msg;
        msg << "Could not resolve io plugin info function from '" << lib_path << "'";
        throw CDMException(msg.str());
    }

    plugin_ = iop_info();
    if (plugin_->plugin_api_version != CURRENT_IO_PLUGIN_API_VERSION) {
        std::ostringstream msg;
        msg << "Unsupported plugin api version " << plugin_->plugin_api_version << " in '" << lib_path << "'";
        throw CDMException(msg.str());
    }
    IoFactory* iof = static_cast<IoFactory*>((*plugin_->create)());
    if (!iof) {
        std::ostringstream msg;
        msg << "Cannot create io factory from '" << lib_path << "'";
        throw CDMException(msg.str());
    }

    wrapped_ = std::shared_ptr<IoFactory>(iof, *plugin_->destroy);
}

IoPlugin::~IoPlugin()
{
    wrapped_ = nullptr;
    dlclose(lib_);
}

std::string IoPlugin::name() const
{
    return plugin_->name;
}

size_t IoPlugin::matchMagicSize()
{
    return wrapped_->matchMagicSize();
}

int IoPlugin::matchMagic(const char* magic, size_t count)
{
    return wrapped_->matchMagic(magic, count);
}

int IoPlugin::matchFileTypeName(const std::string& type)
{
    return wrapped_->matchFileTypeName(type);
}

int IoPlugin::matchFileName(const std::string& fileName)
{
    return wrapped_->matchFileName(fileName);
}

CDMReader_p IoPlugin::createReader(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config, const std::vector<std::string>& args)
{
    return wrapped_->createReader(fileTypeName, fileName, config, args);
}

CDMReaderWriter_p IoPlugin::createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                               const std::vector<std::string>& args)
{
    return wrapped_->createReaderWriter(fileTypeName, fileName, config, args);
}

void IoPlugin::createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& config)
{
    return wrapped_->createWriter(input, fileTypeName, fileName, config);
}

} // namespace MetNoFimex
