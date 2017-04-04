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
#include "Log4cppLogger.h"

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif // HAVE_CONFIG_H

#include <boost/make_shared.hpp>
#include <iostream>

namespace MetNoFimex {

// the standard implementation of Logger
class Log2StderrLogger : public LoggerImpl {
public:
    Log2StderrLogger(const std::string& className)
        : className_(className) {}

    bool isEnabledFor(Logger::LogLevel level) /* override */;
    void log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) /* override */;

private:
    std::string className_;
};

bool Log2StderrLogger::isEnabledFor(Logger::LogLevel level)
{
    return (level >= defaultLogLevel());
}

void Log2StderrLogger::log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber)
{
    const char* levelName = "LOG";
    switch (level) {
    case Logger::FATAL: levelName = "FATAL: "; break;
    case Logger::ERROR: levelName = "ERROR: "; break;
    case Logger::WARN : levelName = "WARN: "; break;
    case Logger::INFO:  levelName = "INFO: "; break;
    case Logger::DEBUG: levelName = "DEBUG: "; break;
    case Logger::OFF:   levelName = "OFF"; break;
    }
    std::cerr << levelName << message << " in " << filename << " at line " << lineNumber << std::endl;
}

class Log2StderrClass : public LoggerClass {
public:
    LoggerImpl* loggerFor(Logger* logger, const std::string& className) /* override */;
};

LoggerImpl* Log2StderrClass::loggerFor(Logger* logger, const std::string& className)
{
    remember(logger);
    return new Log2StderrLogger(className);
}

// ========================================================================

static Logger::LogLevel defaultLL = Logger::WARN;

Logger::LogLevel defaultLogLevel()
{
    return defaultLL;
}

void defaultLogLevel(Logger::LogLevel logLevel)
{
    defaultLL = logLevel;
}

// ========================================================================

static LoggerClass* loggerClass_ = 0;

bool Logger::setClass(LoggerClass* lc)
{
    delete loggerClass_;
    loggerClass_ = lc;
    return (lc != 0);
}

bool Logger::setClass(LogClass logClass)
{
#ifdef HAVE_LOG4CPP
    if (logClass == LOG4CPP) {
        Log4cppClass::configureMinimal(defaultLogLevel());
        return setClass(new Log4cppClass);
    }
#endif
    bool ok = setClass(new Log2StderrClass);
    return ok && (logClass == LOG2STDERR);
}

LoggerClass* getLoggerClass()
{
    if (!loggerClass_)
        Logger::setClass(Logger::LOG4CPP);
    return loggerClass_;
}

// ========================================================================

LoggerImpl::~LoggerImpl()
{
}

LoggerClass::~LoggerClass()
{
    reset();
}

void LoggerClass::remember(Logger* logger)
{
    loggers_.push_back(logger);
}

void LoggerClass::reset()
{
#ifdef _OPENMP
#pragma omp critical (MIFI_LOGGER)
    {
#endif
        for (loggers_t::iterator it = loggers_.begin(); it != loggers_.end(); ++it)
            (*it)->reset();
#ifdef _OPENMP
    }
#endif
}

// ========================================================================

Logger::Logger(const std::string& className)
    : className_(className)
{
}

Logger::~Logger()
{
}

LoggerImpl* Logger::impl()
{
    LoggerImpl* i;
#ifdef _OPENMP
#pragma omp critical (MIFI_LOGGER)
    {
#endif
    if (pimpl_.get() == 0) {
        pimpl_.reset(getLoggerClass()->loggerFor(this, className_));
    }
    i = pimpl_.get();
#ifdef _OPENMP
    }
#endif
    return i;
}

void Logger::reset()
{
    pimpl_.reset(0);
}

bool Logger::isEnabledFor(LogLevel level)
{
    if (LoggerImpl* i = impl())
        return i->isEnabledFor(level);
    else
        return false;
}

void Logger::forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber)
{
    if (LoggerImpl* i = impl())
        i->log(level, message, filename, lineNumber);
}

// ========================================================================

LoggerPtr getLogger(const std::string& className)
{
    return boost::make_shared<Logger>(className);
}

} // namespace MetNoFimex
