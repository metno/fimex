/*
 * Fimex
 *
 * (C) Copyright 2011, met.no
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

// METGM C Lib
//
#include "metgm.h"

// fimex
//
#include "fimex/CDMException.h"

// boost
//
#include <boost/lexical_cast.hpp>

// standard
//
#include <sstream>
#include <iostream>
#include <time.h>

#ifndef METGM_UTILS_H
#define METGM_UTILS_H

class MetGmProfilingTimer
{
public:
    inline MetGmProfilingTimer()
    {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_);
    }

    virtual inline ~MetGmProfilingTimer()
    {
    }

    inline void restart()
    {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_);
        end_ = start_;
    }

    timespec elapsed()
    {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_);
        timespec delta = diff(start_, end_);
        start_ = end_;
        return delta;
    }

    std::string elapsedToString()
    {
        timespec delta = elapsed();
        std::string ss; ss.clear();
        ss.append("[sec = ").append(boost::lexical_cast<std::string>(delta.tv_sec)).append(":").append("msec = ").append(boost::lexical_cast<std::string>(delta.tv_nsec / 1000000)).append("]");
        return ss;
    }

protected:
    inline timespec diff(timespec start, timespec end)
    {
        timespec temp;
        if ((end.tv_nsec-start.tv_nsec)<0) {
            temp.tv_sec = end.tv_sec-start.tv_sec-1;
            temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
        } else {
            temp.tv_sec = end.tv_sec-start.tv_sec;
            temp.tv_nsec = end.tv_nsec-start.tv_nsec;
        }
        return temp;
    }

    timespec start_;
    timespec end_;
};


class MetGmProfilingTimerOnDestruction : public MetGmProfilingTimer
{
public:
    inline MetGmProfilingTimerOnDestruction() : MetGmProfilingTimer()
    {

    }

    inline ~MetGmProfilingTimerOnDestruction()
    {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_);
        timespec elapsed = diff(start_, end_);
        std::cerr << "[sec = " << elapsed.tv_sec
                  << ":"
                  << "msec = " << elapsed.tv_nsec / 1000000 << "]"
                  << std::endl;
    }
};

#define MGM_THROW_ON_ERROR(expression)                            \
        {                                                         \
            short callResult = expression;                        \
            if(callResult != MGM_OK)                              \
                throw CDMException(mgm_string_error(callResult)); \
        };                                                        \

#define MGM_RETURN_RESULT_OR_THROW_ON_ERROR(expression)                            \
        {                                                         \
            short callResult = expression;                        \
            if(callResult != MGM_OK)                              \
                throw CDMException(mgm_string_error(callResult)); \
            return callResult;                                    \
        };                                                        \

#define MGM_CHECK_POINT()                                 \
        {                                                 \
            std::cerr << __FILE__     << " @ "            \
                      << __FUNCTION__ << " @ "            \
                      << __LINE__     << " : "            \
                      << " CHECK POINT"                   \
                      << std::endl;                       \
        };                                                \

#define MGM_MESSAGE_POINT(msg)                            \
        {                                                 \
            std::cerr << __FILE__     << " @ "            \
                      << __FUNCTION__ << " @ "            \
                      << __LINE__     << " : "            \
                      << " " << msg << " "                \
                      << std::endl;                       \
        };                                                \

#endif // METGM_UTILS_H
