/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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
 */

#ifndef GRIBAPICDMWRITER_IMPL1_H_
#define GRIBAPICDMWRITER_IMPL1_H_

#include "GribApiCDMWriter_ImplAbstract.h"

namespace MetNoFimex {

/**
 * Implementation of a writer using GribApi for grib1
 */
class GribApiCDMWriter_Impl1 : public GribApiCDMWriter_ImplAbstract
{
public:
    GribApiCDMWriter_Impl1(CDMReader_p cdmReader, const std::string& outputFile, const XMLInput& config);
    ~GribApiCDMWriter_Impl1();

    void setParameter(const std::string& varName, double levelValue) override;
    void setProjection(const std::string& varName) override;
    void setLevel(const std::string& varName, double levelValue, size_t levelPos) override;
    DataPtr handleTypeScaleAndMissingData(const std::string& varName, double levelValue, DataPtr inData) override;
};

} // namespace MetNoFimex

#endif /* GRIBAPICDMWRITER_IMPL1_H_ */
