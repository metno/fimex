/*
 * Fimex, testGribReader.cc
 *
 * (C) Copyright 2009-2026, met.no
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
 *
 *  Created on: Oct 7, 2009
 *      Author: Heiko Klein
 */

#include "fimex/CDM.h"
#include "fimex/CDMDataType.h"
#include "fimex/CDMFileReaderFactory.h"
#include "fimex/Data.h"
#include "fimex/GridDefinition.h"
#include "fimex/MathUtils.h"
#include "fimex/Null_CDMWriter.h"
#include "fimex/SharedArray.h"
#include "fimex/SliceBuilder.h"
#include "fimex/XMLInputFile.h"

#include "fimex/ProtobufCDM.h"

#include "testinghelpers.h"

#include <memory>
#include <vector>

#include "cdm.pb.h"

using namespace std;
using namespace MetNoFimex;

TEST4FIMEX_TEST_CASE(test_read_write_data_char)
{
    fimex_index::CDMData pbf_data;
    const char text[] = "abcd";
    const size_t count = 4;
    auto w_array = make_shared_array<char>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = text[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_CHAR);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_CHAR);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u8().size(), count);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_CHAR);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asChar();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], text[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_uchar)
{
    fimex_index::CDMData pbf_data;
    const char text[] = "\xFA\xFB\xFC\xFD";
    const size_t count = 4;
    auto w_array = make_shared_array<unsigned char>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = text[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_UCHAR);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_UCHAR);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u8().size(), count);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_UCHAR);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asChar();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], text[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_short)
{
    fimex_index::CDMData pbf_data;
    const short values[] = {-12, 34, 56, 78};
    const size_t count = 4;
    auto w_array = make_shared_array<short>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_SHORT);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_SHORT);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u32_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u32().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_SHORT);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asShort();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_ushort)
{
    fimex_index::CDMData pbf_data;
    const unsigned short values[] = {0, 1234, 5678, 0xFFFF};
    const size_t count = 4;
    auto w_array = make_shared_array<unsigned short>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_USHORT);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), count);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_USHORT);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u32_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u32().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_USHORT);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asUShort();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_int)
{
    fimex_index::CDMData pbf_data;
    const int values[] = {-12, 34, 56, 78};
    const size_t count = 4;
    auto w_array = make_shared_array<int>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_INT);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_INT);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u32_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u32().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_INT);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asInt();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_uint)
{
    fimex_index::CDMData pbf_data;
    const unsigned int values[] = {0, 1234, 5678, 0xFFFF};
    const size_t count = 4;
    auto w_array = make_shared_array<unsigned int>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_UINT);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), count);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_UINT);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u32_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u32().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_UINT);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asUInt();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_int64)
{
    fimex_index::CDMData pbf_data;
    const long long values[] = {-12, 34, 56, 78};
    const size_t count = 4;
    auto w_array = make_shared_array<long long>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_INT64);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_INT64);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u64_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u64().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_INT64);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asInt64();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_uint64)
{
    fimex_index::CDMData pbf_data;
    const unsigned long long values[] = {0, 1234, 5678, 0xFFFFFFFFFFFFFFFF};
    const size_t count = 4;
    auto w_array = make_shared_array<unsigned long long>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_UINT64);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), count);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_UINT64);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_u64_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_u64().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_UINT64);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asUInt64();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_float)
{
    fimex_index::CDMData pbf_data;
    const float values[] = {-12.0, 34.56, 56.78, 78.90};
    const size_t count = 4;
    auto w_array = make_shared_array<float>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_FLOAT);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 4);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_FLOAT);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_f32_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_f32().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_FLOAT);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asFloat();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_double)
{
    fimex_index::CDMData pbf_data;
    const double values[] = {0, 1234.5678, 2345e30, 5678e45};
    const size_t count = 4;
    auto w_array = make_shared_array<double>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_DOUBLE);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), count);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_DOUBLE);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.values_f64_size(), count);
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(pbf_data.values_f64().Get(i), values[i]);

    const fimex_index::CDM pbf_cdm;
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_DOUBLE);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asDouble();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_strings)
{
    fimex_index::CDMData pbf_data;
    const std::string values[] = {"hei", "there"};
    const size_t count = 2;
    auto w_array = make_shared_array<std::string>(count);
    for (auto i = 0; i < count; ++i)
        w_array[i] = values[i];
    const auto w_data = createData(count, w_array);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_STRINGS);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 2);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_STRINGS);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.strings_size(), count);

    fimex_index::CDM pbf_cdm;
    for (auto s : cdm_strings.strings())
        pbf_cdm.add_strings(s);
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_STRINGS);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), count);
    const auto r_array = r_data->asStrings();
    for (auto i = 0; i < count; ++i)
        TEST4FIMEX_CHECK_EQ(r_array[i], values[i]);
}

TEST4FIMEX_TEST_CASE(test_read_write_data_string)
{
    fimex_index::CDMData pbf_data;
    const std::string value = "hei there";
    const auto w_data = createData(value);
    TEST4FIMEX_CHECK_EQ(w_data->getDataType(), CDMDataType::CDM_STRING);
    TEST4FIMEX_REQUIRE_EQ(w_data->size(), 9);

    StringListBuilder cdm_strings;
    ProtobufCDM::writeData(&pbf_data, w_data->getDataType(), cdm_strings, w_data);
    TEST4FIMEX_CHECK_EQ(pbf_data.data_type(), fimex_index::CDMDataType::CDM_STRING);
    TEST4FIMEX_REQUIRE_EQ(pbf_data.strings_size(), 1);

    fimex_index::CDM pbf_cdm;
    for (auto s : cdm_strings.strings())
        pbf_cdm.add_strings(s);
    const auto r_data = ProtobufCDM::readData(pbf_data, pbf_cdm);
    TEST4FIMEX_CHECK_EQ(r_data->getDataType(), CDMDataType::CDM_STRING);
    TEST4FIMEX_REQUIRE_EQ(r_data->size(), 9);
    const auto r_array = r_data->asString();
    TEST4FIMEX_CHECK_EQ(r_array, value);
}
