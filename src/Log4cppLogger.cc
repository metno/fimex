/*
 * Fimex, Log4cppLogger.cc
 *
 * (C) Copyright 2012-2026, met.no
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
 *
 *  Created on: Dec 13, 2012
 *      Author: heikok
 */

#include "Log4cppLogger.h"
#ifdef HAVE_LOG4CPP

#include "fimex/MutexLock.h"

#include "log4cpp/CategoryStream.hh"

namespace MetNoFimex {

using namespace log4cpp;

static OmpMutex log4cppMutex;

Priority::Value logLevel2cppPriority(Logger::LogLevel level)
{
    switch (level) {
    case Logger::FATAL: return Priority::FATAL;
    case Logger::ERROR: return Priority::ERROR;
    case Logger::WARN: return Priority::WARN;
    case Logger::INFO: return Priority::INFO;
    case Logger::DEBUG: return Priority::DEBUG;
    case Logger::OFF:
    default:
        return Priority::NOTSET;
    }
}

inline log4cpp::Category& getlog(const std::string& className)
{
    OmpScopedLock lock(log4cppMutex);
    return log4cpp::Category::getInstance(className);
}

Log4cppLogger::Log4cppLogger(const std::string& className)
    : log_(log4cpp::Category::getInstance(className))
{
}

bool Log4cppLogger::isEnabledFor(Logger::LogLevel level)
{
    Priority::Value prio = logLevel2cppPriority(level);
    OmpScopedLock lock(log4cppMutex);
    return log_.isPriorityEnabled(prio);
}

void Log4cppLogger::log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber)
{
    Priority::Value prio = logLevel2cppPriority(level);
    OmpScopedLock lock(log4cppMutex);
    log_.getStream(prio)
            << message
            << " in " << filename
            << " at line " << lineNumber;
}

LoggerImpl* Log4cppClass::loggerFor(Logger* logger, const std::string& className)
{
    remember(logger);
    return new Log4cppLogger(className);
}

// static
void Log4cppClass::configureMinimal(Logger::LogLevel logLevel)
{
    OmpScopedLock lock(log4cppMutex);
    log4cpp::Category& root = log4cpp::Category::getRoot();
    log4cpp::Category& fimex = log4cpp::Category::getInstance("fimex");
    if (root.getAllAppenders().empty() && fimex.getAllAppenders().empty()) {
        log4cpp::Appender *appender1 = new log4cpp::OstreamAppender("console", &std::cout);
        appender1->setLayout(new log4cpp::BasicLayout());
        root.addAppender(appender1);
    }
    const log4cpp::Priority::Value log4cppLevel = logLevel2cppPriority(logLevel);
    if (fimex.getChainedPriority() < log4cppLevel)
        fimex.setPriority(log4cppLevel);
}

} /* namespace MetNoFimex */
#endif /* HAVE_LOG4CPP */
