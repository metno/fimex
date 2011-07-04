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

#include "fimex/CDMException.h"

#ifndef METGM_UTILS_H
#define METGM_UTILS_H

#define MGM_THROW_ON_ERROR(expression)                            \
        {                                                         \
            short callResult = expression;                        \
            if(callResult != MGM_OK)                              \
                throw CDMException(mgm_string_error(callResult)); \
        };                                                        \

#endif
