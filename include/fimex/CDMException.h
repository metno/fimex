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

#ifndef CDMEXCEPTION_H_
#define CDMEXCEPTION_H_

#include <stdexcept>
#include <string>

namespace MetNoFimex
{

/**
 * @headerfile fimex/CDMException.h
 */
class CDMException : public std::runtime_error
{
public:
    CDMException() : std::runtime_error("CDMException") {}
    explicit CDMException(std::string msg) : std::runtime_error("CDMException: " + msg) {}
};

}

#endif /*CDMEXCEPTION_H_*/
