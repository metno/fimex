/*
 * Fimex, Log4cppLogger.h
 *
 * (C) Copyright 2012, met.no
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
 *  Created on: Dec 13, 2012
 *      Author: Heiko Klein
 */

#ifndef LOG4CPPLOGGER_H_
#define LOG4CPPLOGGER_H_

#include "fimex/Logger.h"
#include "../config.h"
#ifdef HAVE_LOG4CPP
#include "log4cpp/Appender.hh"
#include "log4cpp/OstreamAppender.hh"
#include "log4cpp/Layout.hh"
#include "log4cpp/BasicLayout.hh"
#include "log4cpp/Priority.hh"

#include <boost/shared_ptr.hpp>
#include "log4cpp/Category.hh"

namespace MetNoFimex
{

log4cpp::Priority::Value logLevel2cppPriority(Logger::LogLevel level);

class Log4cppLogger: public MetNoFimex::Logger
{
public:
    Log4cppLogger(const std::string& className) : Logger(className), log_(log4cpp::Category::getInstance(className)) {};
    virtual ~Log4cppLogger() {};
    /**
     * check if the loglevel of this logger is active
     */
    virtual bool isEnabledFor(LogLevel level);
    /**
     * log (without checking) for this loglevel
     * @param level log-level to log
     * @param message log-message
     * @param filename best retrieved with __FILE__
     * @param lineNumber best retrieved with __LINE__
     */
    virtual void forcedLog(LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber);
private:
    log4cpp::Category& log_;
};

} /* namespace MetNoFimex */
#endif /* HAVE_LOG4PP */
#endif /* LOG4CPPLOGGER_H_ */
