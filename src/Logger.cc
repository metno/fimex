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
#include "../config.h"
#include <iostream>

namespace MetNoFimex
{

// the standard implementation of Logger
class Log2Stderr : public Logger
{
public:
    Log2Stderr(const std::string& className) : Logger(className) {}
    virtual ~Log2Stderr() {}
    virtual bool isEnabledFor(LogLevel level) {
        if (level >= defaultLogLevel()) {
            return true;
        }
        return false;
    }
    virtual void forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) {
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


};

Logger::LogLevel defaultLL = Logger::WARN;
Logger::LogLevel defaultLogLevel() {
    return defaultLL;
}
void defaultLogLevel(Logger::LogLevel logLevel) {
    defaultLL = logLevel;
#ifdef HAVE_LOG4CPP
    log4cpp::Appender *appender1 = new log4cpp::OstreamAppender("console", &std::cout);
    appender1->setLayout(new log4cpp::BasicLayout());

    log4cpp::Category& root = log4cpp::Category::getRoot();
    root.setPriority(logLevel2cppPriority(logLevel));
    root.addAppender(appender1);
#endif
}

static Logger::LogClass logClass_ = Logger::LOG2STDERR;
bool Logger::setClass(LogClass logClass) {
    switch (logClass) {
    case LOG2STDERR: logClass_ = logClass; return true;
    case LOG4CPP: {
#ifdef HAVE_LOG4CPP
        logClass_ = logClass;
        return true;
#endif
    }
    }

    logClass_ = LOG2STDERR;
    return false;
}

LoggerPtr getLogger(const std::string& className)
{
    return boost::shared_ptr<Logger>(new Logger(className));
}

Logger::Logger(const std::string& className)
: className_(className) {
    // default, nothing to do
}

Logger::~Logger()
{
}

Logger* Logger::getImpl() {
    if (pimpl_.get() == 0) {
        switch (logClass_) {
        case LOG4CPP:
#ifdef HAVE_LOG4CPP
            pimpl_ = std::auto_ptr<Logger>(new Log4cppLogger(className_)); break;
#endif
        case LOG2STDERR:
        default: pimpl_ = std::auto_ptr<Logger>(new Log2Stderr(className_)); break;
        }
    }
    return pimpl_.get();
}

bool Logger::isEnabledFor(LogLevel level) {
    return getImpl()->isEnabledFor(level);
}
void Logger::forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) {
    return getImpl()->forcedLog(level, message, filename, lineNumber);
}

}
