/*
 * (C) Copyright 2024-2026, met.no
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

#include "fimex/ProtobufCDM.h"

#include "fimex/CDMAttribute.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMVariable.h"
#include "fimex/Data.h"
#include "fimex/FileUtils.h"
#include "fimex/Logger.h"
#include "fimex/SharedArray.h"

#include <atomic>
#include <fstream>

#include "cdm.pb.h"

namespace MetNoFimex {

// FMX = fimex, IDX = index, 1 byte version, P = protobuf
const char FIMEX_PROTO_INDEX_MAGIC[8]{'F', 'M', 'X', 'I', 'D', 'X', 0, 'P'};

namespace ProtobufCDM {

namespace {

Logger_p logger = getLogger("fimex.ProtobufCDM");

MetNoFimex::CDMDataType dataTypeFromIndexCDM(const fimex_index::CDMDataType fdt)
{
    return MetNoFimex::CDMDataType(fdt);
}

} // namespace

void readCDM(MetNoFimex::CDM& cdm, const fimex_index::CDM& fcdm)
{
    for (const auto& fga : fcdm.global_attributes()) {
        CDMAttribute att;
        readAttribute(att, fga, fcdm);
        cdm.addAttribute(cdm.globalAttributeNS(), att);
    }

    for (const auto& fd : fcdm.dimensions()) {
        CDMDimension dim(fcdm.strings(fd.name()), fd.length());
        dim.setUnlimited(fd.is_unlimited());
        cdm.addDimension(dim);
    }

    for (const auto& fv : fcdm.variables()) {
        std::vector<std::string> shape;
        shape.reserve(fv.shape_size());
        for (const auto& fsi : fv.shape()) {
            shape.push_back(fcdm.strings(fsi));
        }

        CDMVariable var(fcdm.strings(fv.name()), dataTypeFromIndexCDM(fv.data().data_type()), shape);
        if (DataPtr data = readData(fv.data(), fcdm)) {
            var.setData(data);
        }
        cdm.addVariable(var);
#if 0
        std::cerr << "added cdm var '" << var.getName() << "' type=" << var.getDataType() << "\n   shape [";
        for (const auto& vs : var.getShape()) {
            std::cerr << " " << vs;
        }
        std::cerr << " ]" << std::endl;

        if (auto vd = var.getData()) {
            std::cerr << "   data size =" << vd->size() << std::endl;
            if (vd->size() < 100)
                std::cerr << "   data values = " << vd->asString("; ") << std::endl;
        }
#endif

        for (const auto& fga : fv.attributes()) {
            CDMAttribute att;
            readAttribute(att, fga, fcdm);
            cdm.addAttribute(var.getName(), att);
#if 0
            std::cerr << "   attribute '" << att.getName() << "'" << std::endl;
            if (auto ad = att.getData()) {
                std::cerr << "      data size =" << ad->size() << std::endl;
            }
#endif
        }
    }
}

void readAttribute(MetNoFimex::CDMAttribute& att, const fimex_index::CDMAttribute& fatt, const fimex_index::CDM& fcdm)
{
    DataPtr data = readData(fatt.data(), fcdm);
    att = MetNoFimex::CDMAttribute(fcdm.strings(fatt.name()), data);
}

namespace {
template <typename T>
DataPtr createDataFromBytes(CDMDataType dt, const std::string& values_as_bytes)
{
    if (values_as_bytes.empty())
        return {};
    auto values = reinterpret_cast<const T*>(&values_as_bytes[0]);
    return createData(dt, values, values + values_as_bytes.size() / sizeof(T));
}

template <typename T, typename F>
DataPtr createDataFromField(google::protobuf::RepeatedField<F> const& field)
{
    const size_t fsize = field.size();
    if (fsize == 0)
        return {};
    auto dvalues = make_shared_array<T>(fsize);
    std::copy(field.begin(), field.end(), &dvalues[0]);
    return createData(fsize, dvalues);
}
} // namespace

DataPtr readData(const fimex_index::CDMData& fdata, const fimex_index::CDM& fcdm)
{
    const auto dt = dataTypeFromIndexCDM(fdata.data_type());
    switch (dt) {
    case CDM_NAT:
        return {}; // ignore data
    case CDM_CHAR:
        return createDataFromBytes<char>(dt, fdata.values_u8());
    case CDM_SHORT:
        return createDataFromField<short>(fdata.values_u32());
    case CDM_INT:
        return createDataFromField<int>(fdata.values_u32());
    case CDM_FLOAT:
        return createDataFromField<float>(fdata.values_f32());
    case CDM_DOUBLE:
        return createDataFromField<double>(fdata.values_f64());
    case CDM_UCHAR:
        return createDataFromBytes<unsigned char>(dt, fdata.values_u8());
    case CDM_USHORT:
        return createDataFromField<unsigned short>(fdata.values_u32());
    case CDM_UINT:
        return createDataFromField<unsigned int>(fdata.values_u32());
    case CDM_INT64:
        return createDataFromField<long long>(fdata.values_u64());
    case CDM_UINT64:
        return createDataFromField<unsigned long long>(fdata.values_u64());
    case CDM_STRINGS: {
        if (fdata.strings_size() > 0) {
            auto strings = make_shared_array<std::string>(fdata.strings_size());
            for (size_t fsi = 0; fsi < fdata.strings_size(); ++fsi) {
                strings[fsi] = fcdm.strings(fsi);
            }
            return createData(fdata.strings_size(), strings);
        } else {
            return {};
        }
    }
    case CDM_STRING: {
        if (fdata.strings_size() > 0) {
            return createData(fcdm.strings(fdata.strings(0)));
        } else {
            return {}; // ignore data
            // return createData(dt, 0);
        }
    }
    } // end switch
    throw std::runtime_error("unknown datatype");
}

void writeCDM(fimex_index::CDM* fcdm, const MetNoFimex::CDM& cdm)
{
    StringListBuilder cdm_strings;
    for (const auto& ga : cdm.getAttributes(cdm.globalAttributeNS())) {
        writeAttribute(fcdm->add_global_attributes(), cdm_strings, ga);
    }

    for (const auto& d : cdm.getDimensions()) {
        auto fd = fcdm->add_dimensions();
        fd->set_name(cdm_strings.add(d.getName()));
        fd->set_length(d.getLength());
        fd->set_is_unlimited(d.isUnlimited());
    }

    for (const auto& v : cdm.getVariables()) {
        auto fv = fcdm->add_variables();
        fv->set_name(cdm_strings.add(v.getName()));
        for (const auto& va : cdm.getAttributes(v.getName())) {
            writeAttribute(fv->add_attributes(), cdm_strings, va);
        }
        writeData(fv->mutable_data(), v.getDataType(), cdm_strings, v.getData());
        for (const auto& vs : v.getShape()) {
            fv->add_shape(cdm_strings.add(vs));
        }
    }

    for (const auto& fs : cdm_strings.strings()) {
        fcdm->add_strings(fs);
    }
}

void writeAttribute(fimex_index::CDMAttribute* fatt, StringListBuilder& cdm_strings, const MetNoFimex::CDMAttribute& att)
{
    fatt->set_name(cdm_strings.add(att.getName()));
    writeData(fatt->mutable_data(), att.getDataType(), cdm_strings, att.getData());
}

namespace {
template <typename T>
std::string createBytesFromData(size_t size, shared_array<T> values)
{
    auto values_as_bytes = reinterpret_cast<const char*>(&values[0]);
    return std::string(values_as_bytes, values_as_bytes + size * sizeof(T));
}
template <typename T, typename F>
void copyDataToField(size_t dsize, shared_array<T> dvalues, google::protobuf::RepeatedField<F>* field)
{
    field->Reserve(dsize);
    std::copy(&dvalues[0], &dvalues[0] + dsize, google::protobuf::RepeatedFieldBackInserter(field));
}
} // namespace

void writeData(fimex_index::CDMData* fdata, const MetNoFimex::CDMDataType dt, StringListBuilder& cdm_strings, MetNoFimex::DataPtr data)
{
    fdata->set_data_type(fimex_index::CDMDataType(dt));
    if (!data)
        return;

    const auto dsize = data->size();
    switch (dt) {
    case CDM_NAT: {
        break; // do nothing
    }
    case CDM_CHAR: {
        fdata->set_values_u8(createBytesFromData(data->size(), data->asChar()));
        break;
    }
    case CDM_SHORT: {
        copyDataToField(dsize, data->asShort(), fdata->mutable_values_u32());
        break;
    }
    case CDM_INT: {
        copyDataToField(dsize, data->asInt(), fdata->mutable_values_u32());
        break;
    }
    case CDM_FLOAT: {
        copyDataToField(dsize, data->asFloat(), fdata->mutable_values_f32());
        break;
    }
    case CDM_DOUBLE: {
        copyDataToField(dsize, data->asDouble(), fdata->mutable_values_f64());
        break;
    }
    case CDM_UCHAR: {
        fdata->set_values_u8(createBytesFromData(data->size(), data->asUChar()));
        break;
    }
    case CDM_USHORT: {
        copyDataToField(dsize, data->asUShort(), fdata->mutable_values_u32());
        break;
    }
    case CDM_UINT: {
        copyDataToField(dsize, data->asUInt(), fdata->mutable_values_u32());
        break;
    }
    case CDM_INT64: {
        copyDataToField(dsize, data->asInt64(), fdata->mutable_values_u64());
        break;
    }
    case CDM_UINT64: {
        copyDataToField(dsize, data->asUInt64(), fdata->mutable_values_u64());
        break;
    }
    case MetNoFimex::CDM_STRINGS: {
        auto strings = data->asStrings();
        for (size_t i = 0; i < dsize; ++i) {
            fdata->add_strings(cdm_strings.add(strings[i]));
        }
        break;
    }
    case MetNoFimex::CDM_STRING: {
        auto string1 = data->asString();
        fdata->add_strings(cdm_strings.add(string1));
        break;
    }
    } // end switch
}

} // namespace ProtobufCDM
} // namespace MetNoFimex
