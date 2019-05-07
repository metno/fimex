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

#ifndef LOGGER_H_
#define LOGGER_H_

#include <list>
#include <memory>
#include <sstream>
#include <string>

namespace MetNoFimex {

class LoggerClass;
class LoggerImpl;

/**
 * @headerfile fimex/Logger.h
 */
/**
 * Interface and default (dummy) implementation for a logger.
 * Don't use this class directly, but retrieve a pointer to it
 * via the #getLogger function and log with the #LOG4FIMEX macro.
 *
 * To switch to another logger-implementation, e.g. log4cpp, use setClass()
 * and eventually implement the Logger-initialization (e.g. for log4cpp via filename)
 *
 */
class Logger
{
private:
    std::string className_;
    LoggerImpl* pimpl_; // lazy initialized member
    LoggerImpl* impl();

public:
    /**
     * different log levels
     */
    enum LogLevel {
        OFF = 1000,
        FATAL = 900,
        ERROR = 800,
        WARN = 700,
        INFO = 600,
        DEBUG = 500
    };

    Logger(const std::string& className);
    ~Logger();

    /**
     * check if the loglevel of this logger is active
     */
    bool isEnabledFor(LogLevel level);

    /** Delete the present logger implementation. */
    void reset();

    /**
     * log for this loglevel
     * @param level log-level to log
     * @param message log-message
     * @param filename best retrieved with __FILE__
     * @param lineNumber best retrieved with __LINE__
     */
    void forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber);

    /**
     * different logger eventually enabled in fimex
     */
    enum LogClass {
        LOG2STDERR = 0,
        LOG4CPP = 1
    };
    /**
     * choose a loggerclass in fimex (default is STDERR)
     *
     * @param logClass
     * @return false if the logClass is not compiled into fimex
     */
    static bool setClass(LogClass logClass);

    /**
     * Choose a loggerclass in fimex
     *
     * Changing LoggerClass after invoking any functions in the
     * library (more precisely, after any creation of LoggerImpl
     * objects), is experimental.
     *
     * Ownership of lc is transferred. To destroy the LoggerClass,
     * e.g. before exit, it is necessary to call setClass(0).
     *
     * @param lc, ownership is transferred; deletes the present loggerclass instance
     * @return false if lc is null
     */
    static bool setClass(LoggerClass* lc);
};


class LoggerImpl {
public:
    virtual ~LoggerImpl();
    virtual bool isEnabledFor(Logger::LogLevel level) = 0;
    virtual void log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) = 0;
};


class LoggerClass {
public:
    virtual ~LoggerClass();
    virtual LoggerImpl* loggerFor(Logger* logger, const std::string& className) = 0;
    void forget(Logger* logger);

protected:
    void remember(Logger* logger);
    void reset();

protected:
    typedef std::list<Logger*> loggers_t;
    loggers_t loggers_;
};


/**
 * the defaultLogLevel can be used by the implemented logger to determine
 * the minimum LogLevel. This value might be ignored/overwritten by a configuration
 * within the implementation. It should be initialized in the main class.
 */
extern Logger::LogLevel defaultLogLevel();
extern void defaultLogLevel(Logger::LogLevel);

/**
 * use this pragma to log a message of a level
 * @param logger a logger as retrieved with getLogger("com.bar")
 * @param level a fimex LogLevel, i.e. OFF, FATAL, ERROR, WARN, INFO, DEBUG
 * @param message the message to log
 */
#define LOG4FIMEX(logger, level, message) \
    do {                                  \
      if (logger->isEnabledFor(level)) {  \
        std::ostringstream buffer;        \
        buffer << message;                \
        logger->forcedLog(level, buffer.str(), __FILE__, __LINE__); \
      }                                   \
    } while(0)

typedef std::shared_ptr<Logger> Logger_p;

/**
 * Retrieve a logger for Fimex. It will use loggers in the following order, skipping
 * to the next one if the current one is not available:
 * 1) log4cxx
 * 2) no/dummy logger
 */
extern Logger_p getLogger(const std::string& className);

}  // namespace MetNoFimex


#endif /* LOGGER_H_ */
