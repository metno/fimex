/*
 * Fimex, pyfimex0_logging.cc
 *
 * (C) Copyright 2017-2019, met.no
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
 *  Created on: Aug 1, 2017
 *      Author: Alexander Bürger
 */

#include "fimex/Logger.h"

#include <pybind11/pybind11.h>

using namespace MetNoFimex;
namespace py = pybind11;

#define PY_GIL_ACQUIRE py::gil_scoped_acquire acquire

// ignore exeptions from logging
#define PY_GIL_IGNORE_EXCEPTIONS(x) \
    do { try { \
        PY_GIL_ACQUIRE; \
        x; \
    } catch (...) { \
    } } while (false)

// rethrow exeptions from logging, treating StopIteration specially
#define PY_GIL_RETHROW_EXCEPTIONS(x) \
    do { try { \
        PY_GIL_ACQUIRE; \
        x; \
    } catch (py::error_already_set& pex) { \
        if (pex.matches(PyExc_StopIteration)) { \
            throw py::stop_iteration(); \
        } else { \
            throw; \
        } \
    } } while (false)

#define PY_GIL_EXCEPTIONS(x) \
    PY_GIL_IGNORE_EXCEPTIONS(x)

namespace {

// see https://docs.python.org/3.5/library/logging.html#levels
enum PythonLoggingLevel {
    PY_NOTSET = 0,
    PY_DEBUG = 10,
    PY_INFO = 20,
    PY_WARNING = 30,
    PY_ERROR = 40,
    PY_CRITICAL = 50
};

int toPythonLevel(Logger::LogLevel level)
{
    switch (level) {
    case Logger::FATAL: return PY_CRITICAL;
    case Logger::ERROR: return PY_ERROR;
    case Logger::WARN: return PY_WARNING;
    case Logger::INFO: return PY_INFO;
    case Logger::DEBUG: return PY_DEBUG;
    case Logger::OFF: return PY_NOTSET;
    }
    return PY_NOTSET;
}

class PythonLoggingImpl : public LoggerImpl {
public:
    PythonLoggingImpl(const py::object& log);
    ~PythonLoggingImpl();
    bool isEnabledFor(Logger::LogLevel level) /* override */;
    void log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber) /* override */;

private:
    py::object log_;
};

PythonLoggingImpl::PythonLoggingImpl(const py::object& log)
{
    if (!log.is_none())
        PY_GIL_EXCEPTIONS(log_ = log);
}

PythonLoggingImpl::~PythonLoggingImpl()
{
    if (!log_.is_none())
        PY_GIL_IGNORE_EXCEPTIONS(log_ = py::none());
}

bool PythonLoggingImpl::isEnabledFor(Logger::LogLevel level)
{
    const int pylevel = toPythonLevel(level);

    if (!log_.is_none())
        PY_GIL_EXCEPTIONS(return log_.attr("isEnabledFor")(pylevel).cast<bool>());
    return false;
}

void PythonLoggingImpl::log(Logger::LogLevel level, const std::string& message, const char* filename, unsigned int lineNumber)
{
    if (log_.is_none())
        return;

    std::ostringstream py_message;
    py_message << message
           << " in " << filename
           << " at line " << lineNumber;
    const std::string py_msg = py_message.str();
    const int pylevel = toPythonLevel(level);

    PY_GIL_EXCEPTIONS(log_.attr("log")(pylevel, py_msg));
}

class PythonLoggingClass : public LoggerClass {
public:
    PythonLoggingClass();
    LoggerImpl* loggerFor(Logger* logger, const std::string& className) /* override */;
    py::module python_logging;
};

PythonLoggingClass::PythonLoggingClass()
{
    PY_GIL_ACQUIRE;
    python_logging = py::module::import("logging");
}

LoggerImpl* PythonLoggingClass::loggerFor(Logger* logger, const std::string& className)
{
    remember(logger);
    py::object py_logger;
    PY_GIL_EXCEPTIONS(py_logger = python_logging.attr("getLogger")(className));
    return new PythonLoggingImpl(py_logger);
}

} // namespace

void pyfimex0_logging(py::module m)
{
    if (char* disable = getenv("FIMEX_PYTHON_NO_LOGGING")) {
        Logger::setClass(nullptr);
    } else {
        // see https://pybind11.readthedocs.io/en/stable/advanced/misc.html#module-destructors
        auto atexit = py::module::import("atexit");
        atexit.attr("register")(py::cpp_function([]() { Logger::setClass(nullptr); }));

        Logger::setClass(new PythonLoggingClass);
    }
}
