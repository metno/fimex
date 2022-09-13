/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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
#include "fimex/MutexLock.h"

#include "fimex_config.h"

#include <algorithm>
#include <iostream>
#include <memory>

namespace MetNoFimex {

static OmpMutex loggerMutex;

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
    const bool hasLogger = (lc != 0);
    {
        OmpScopedLock lock(loggerMutex);
        std::swap(lc, loggerClass_);
    }
    delete lc; // calls reset() -> calls LoggerImpl::reset() -> calls forget() -> locks
    return hasLogger;
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
    // FIXME should lock here ...
    if (!loggerClass_)
        // ... and unlock here
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
    OmpScopedLock lock(loggerMutex);
    loggers_.push_back(logger);
}

void LoggerClass::forget(Logger* logger)
{
    OmpScopedLock lock(loggerMutex);
    loggers_.erase(std::remove(loggers_.begin(), loggers_.end(), logger), loggers_.end());
}

void LoggerClass::reset()
{
    loggers_t oldloggers;
    {
        OmpScopedLock lock(loggerMutex);
        std::swap(loggers_, oldloggers);
    }
    for (loggers_t::iterator it = oldloggers.begin(); it != oldloggers.end(); ++it)
        (*it)->reset();
}

// ========================================================================

Logger::Logger(const std::string& className)
    : className_(className)
    , pimpl_(0)
{
}

Logger::~Logger()
{
    reset();
}

LoggerImpl* Logger::impl()
{
    OmpScopedLock lock(loggerMutex);
    if (!pimpl_) {
        LoggerImpl* i;
        {
            OmpScopedUnlock unlock(loggerMutex);
            i = getLoggerClass()->loggerFor(this, className_);
        }
        if (!pimpl_)
            pimpl_ = i;
    }
    return pimpl_;
}

void Logger::reset()
{
    OmpScopedLock lock(loggerMutex);
    if (pimpl_) {
        {
            OmpScopedUnlock unlock(loggerMutex);
            getLoggerClass()->forget(this);
        }
        delete pimpl_;
        pimpl_ = 0;
    }
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

Logger_p getLogger(const std::string& className)
{
    return std::make_shared<Logger>(className);
}

} // namespace MetNoFimex
