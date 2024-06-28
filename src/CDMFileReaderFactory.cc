/*
 * Fimex, CDMFileReaderFactory.cc
 *
 * (C) Copyright 2010-2024, met.no
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
 *  Created on: May 5, 2010
 *      Author: Heiko Klein
 */

#include "fimex/CDMFileReaderFactory.h"

#include "fimex/CDMException.h"
#include "fimex/CDMWriter.h"
#include "fimex/CDMconstants.h"
#include "fimex/FileUtils.h"
#include "fimex/IoFactory.h"
#include "fimex/IoPlugin.h"
#include "fimex/Logger.h"
#include "fimex/StringUtils.h"
#include "fimex/Type2String.h"
#include "fimex/XMLUtils.h"
#include "fimex/min_max.h"

#include <algorithm>
#include <fstream>
#include <memory>
#include <regex>

#include "fimex_config.h"

namespace MetNoFimex {

#if 0 // FIXME detect xml types, look at this header and xmlns:??="..."
static bool detectXML(const char* magic) {
    return std::regex_match(magic, std::regex("\\s*<\\?xml\\s.*"));
}
#endif

// ========================================================================

namespace {

Logger_p logger = getLogger("fimex.CDMFileReaderFactory");

static bool haveScannedForIoPlugins = false;

std::vector<std::string> getIoPluginsDirs()
{
    const char* iopp = iopp = getenv("FIMEX_IO_PLUGINS_PATH");
    if (!iopp) {
        // conda fills the placeholder with 0 bytes after the installation prefix,
        // and these 0 bytes must not end up in the path
        iopp = FIMEX_IO_PLUGINS_PATH;
    }
    // construct std::string from "iopp" which is a "const char*"
    return tokenize(iopp, ":");
}

// static
void scanForIoPlugins()
{
    if (haveScannedForIoPlugins)
        return;
    haveScannedForIoPlugins = true;

    static const std::regex re_plugin_so("libfimex-io-([a-z0-9]+)" FIMEX_IO_PLUGINS_VERSION "\\.so");
    static const auto plugin_dirs = getIoPluginsDirs();
    for (const auto& pd : plugin_dirs) {
        LOG4FIMEX(logger, Logger::DEBUG, "searching for io plugins in '" << pd << "' ...");
        std::vector<std::string> plugins;
        scanFiles(plugins, pd, 0, re_plugin_so, true);
        for (const auto& plugin_file : plugins) {
            try {
                auto plugin = std::make_shared<IoPlugin>(plugin_file);
                const auto name = plugin->name();
                if (IoFactory::factories().count(name)) {
                    LOG4FIMEX(logger, Logger::INFO,
                              "not installing plugin '" << plugin_file << "' because another plugin with name '" << name << "' is already loaded");
                } else {
                    IoFactory::install(name, plugin);
                    LOG4FIMEX(logger, Logger::DEBUG, "installed io plugin '" << plugin_file << "' ...");
                }
            } catch (std::exception& ex) {
                LOG4FIMEX(logger, Logger::ERROR, "failed to load io plugin '" << plugin_file << "': " << ex.what());
            } catch (...) {
                LOG4FIMEX(logger, Logger::ERROR, "failed to load io plugin '" << plugin_file << "' with an unknown error");
            }
        }
    }
}

IoFactory_pm& ioFactories()
{
    return IoFactory::factories();
}

IoFactory_p findFactoryFromFileType(const std::string& fileTypeName)
{
    if (fileTypeName.empty())
        return IoFactory_p();

    IoFactory_p factory;
    int bestType = 0;
    for (const auto& id_f : ioFactories()) {
        if (maximize(bestType, id_f.second->matchFileTypeName(fileTypeName)))
            factory = id_f.second;
    }
    return factory;
}

IoFactory_p findFactoryFromMagic(const std::string& fileName)
{
    size_t magicSize = 0;
    for (const auto& id_f : ioFactories())
        maximize(magicSize, id_f.second->matchMagicSize());

    std::ifstream fs(fileName.c_str());
    if (!fs.is_open())
        return nullptr; // acceptable, fileName might be a url

    std::unique_ptr<char[]> magic(new char[magicSize]);
    fs.read(magic.get(), magicSize);
    const size_t actualMagicSize = fs.gcount();
    fs.close();

    int bestType = 0;
    IoFactory_p factory;
    for (const auto& id_f : ioFactories()) {
        if (maximize(bestType, id_f.second->matchMagic(magic.get(), actualMagicSize)))
            factory = id_f.second;
    }
    return factory;
}

IoFactory_p findFactoryFromFileName(const std::string& fileName)
{
    int bestType = 0;
    IoFactory_p factory;
    for (const auto& id_f : ioFactories()) {
        if (maximize(bestType, id_f.second->matchFileName(fileName)))
            factory = id_f.second;
    }
    return factory;
}

IoFactory_p findFactory(const std::string& fileTypeName, const std::string& fileName, bool write)
{
    scanForIoPlugins();

    if (IoFactory_p f = findFactoryFromFileType(fileTypeName))
        return f;

    if (!write) {
        if (IoFactory_p f = findFactoryFromMagic(fileName))
            return f;
    }

    return findFactoryFromFileName(fileName);
}

} // namespace

// static
CDMReaderWriter_p CDMFileReaderFactory::createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const XMLInput& config,
                                                           const std::vector<std::string>& args)
{
    if (IoFactory_p factory = findFactory(fileTypeName, fileName, false))
        return factory->createReaderWriter(fileTypeName, fileName, config, args);
    throw CDMException("cannot create reader-writer for type '" + fileTypeName + "' and file '" + fileName + "'");
}

// static
CDMReaderWriter_p CDMFileReaderFactory::createReaderWriter(const std::string& fileTypeName, const std::string& fileName, const std::string& configFile,
                                                           const std::vector<std::string>& args)
{
    XMLInputDoc configXML = createXMLInput(configFile);
    return createReaderWriter(fileTypeName, fileName, configXML, args);
}

// static
CDMReader_p CDMFileReaderFactory::create(const std::string& fileTypeName, const std::string& fileName, const XMLInput& configXML,
                                         const std::vector<std::string>& args)
{
    if (IoFactory_p factory = findFactory(fileTypeName, fileName, false))
        return factory->createReader(fileTypeName, fileName, configXML, args);
    throw CDMException("cannot create reader for type '" + fileTypeName + "' and file '" + fileName + "'");
}

// static
CDMReader_p CDMFileReaderFactory::create(const std::string& fileTypeName, const std::string & fileName, const std::string & configFile, const std::vector<std::string> & args)
{
    XMLInputDoc configXML = createXMLInput(configFile);
    return create(fileTypeName, fileName, configXML, args);
}

// static
void CDMFileReaderFactory::createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const XMLInput& configXML)
{
    if (IoFactory_p factory = findFactory(fileTypeName, fileName, true))
        return factory->createWriter(input, fileTypeName, fileName, configXML);
    throw CDMException("cannot create writer for type '" + fileTypeName + "' and file '" + fileName + "'");
}

// static
void CDMFileReaderFactory::createWriter(CDMReader_p input, const std::string& fileTypeName, const std::string& fileName, const std::string& configFile)
{
    XMLInputDoc configXML = createXMLInput(configFile);
    return createWriter(input, fileTypeName, fileName, configXML);
}

} // namespace MetNoFimex
