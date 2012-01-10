/*
 * Fimex
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
 */

#include "fimex/Logger.h"
#include <iostream>

namespace MetNoFimex
{

Logger::LogLevel defaultLL = Logger::WARN;
Logger::LogLevel defaultLogLevel() {
    return defaultLL;
}
void defaultLogLevel(Logger::LogLevel logLevel) {
    defaultLL = logLevel;
}


LoggerPtr getLogger(const std::string& className)
{
    // TODO implement a logger based upon log4cxx
    return boost::shared_ptr<Logger>(new Logger(className));
}

Logger::Logger(const std::string& className)
{
    // default, nothing to do
}

Logger::~Logger()
{
}

bool Logger::isEnabledFor(LogLevel level) {
    if (level >= defaultLogLevel()) {
        return true;
    }
    return false;
}
void Logger::forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) {
    std::string levelName;
    switch (level) {
    case Logger::FATAL: levelName = "FATAL: "; break;
    case Logger::ERROR: levelName = "ERROR: "; break;
    case Logger::WARN : levelName = "WARN: "; break;
    case Logger::INFO:  levelName = "INFO: "; break;
    case Logger::DEBUG: levelName = "DEBUG: "; break;
    default: levelName = "LOG: "; break;
    }
    std::cerr << levelName << message << " in " << filename << " at line " << lineNumber << std::endl;
}

}
