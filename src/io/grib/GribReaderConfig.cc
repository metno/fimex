/*
 * Fimex, GribReaderConfig.cc
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
 */

#include "GribReaderConfig.h"

#include "GribFileIndex.h"

#include "fimex/CDM_XMLConfigHelper.h"
#include "fimex/Logger.h"
#include "fimex/TokenizeDotted.h"
#include "fimex/XMLUtils.h"

namespace MetNoFimex {

namespace GribReader {

Logger_p logger = getLogger("fimex.GribReaderConfig");

std::string getConfigEarthFigure(XMLDoc_p doc)
{
    // get the overruled earthform
    XPathNodeSet nodes(doc, "/gr:cdmGribReaderConfig/gr:overrule/gr:earthFigure");
    std::string replaceEarthString;
    if (nodes.size() == 1) {
        replaceEarthString = getXmlProp(nodes[0], "proj4");
    }
    return replaceEarthString;
}

std::string getConfigExtraKeys(XMLDoc_p doc)
{
    std::set<std::string> extraKeys;
    // get the additinal keys from the xml-document
    for (auto node : XPathNodeSet(doc, "//gr:extraKey")) {
        extraKeys.insert(getXmlProp(node, "name"));
    }
    return join(extraKeys.begin(), extraKeys.end(), ",");
}

void set_optionals_long(ParamConfig& pc, const std::string& key, const std::string& optVal)
{
    if (!optVal.empty()) {
        const auto values = tokenizeDotted<long>(optVal, ",");
        pc.optionals_long[key] = std::set<long>(values.begin(), values.end());
    }
}

void set_optionals_long(ParamConfig& pc, xmlNodePtr node, const std::string& key)
{
    set_optionals_long(pc, key, getXmlProp(node, key));
}

bool has_optionals_long(const ParamConfig& pc, const std::string& key, long value)
{
    auto it = pc.optionals_long.find(key);
    if (it == pc.optionals_long.end())
        return true;
    const auto& ops = it->second;
    if (ops.empty())
        return true;
    return ops.count(value) > 0;
}

void set_optionals_string(ParamConfig& pc, xmlNodePtr node, const std::string& key)
{
    const auto optVal = getXmlProp(node, key);
    if (!optVal.empty()) {
        const auto values = split_any(optVal, ",");
        pc.optionals_string[key] = std::set<std::string>(values.begin(), values.end());
    }
}

bool has_optionals_string(const ParamConfig& pc, const std::string& key, const std::string& value)
{
    auto it = pc.optionals_string.find(key);
    if (it == pc.optionals_string.end())
        return true;
    const auto& ops = it->second;
    if (ops.empty())
        return true;
    return ops.count(value) > 0;
}

ParamConfig pc_grib_any(XMLDoc_p doc, VariableConfig_p vc, xmlNodePtr node_grib_any)
{
    ParamConfig pc;
    pc.variable = vc;
    set_optionals_long(pc, node_grib_any, "typeOfLevel");
    set_optionals_long(pc, node_grib_any, "levelNo");
    set_optionals_long(pc, node_grib_any, GK_timeRangeIndicator);
    set_optionals_long(pc, node_grib_any, GK_typeOfStatisticalProcessing);
    set_optionals_string(pc, node_grib_any, GK_stepType);

    for (const auto& extra : XPathNodeSet(doc, "gr:extraKey", node_grib_any)) {
        const auto name = getXmlProp(extra, "name");
        const auto value = getXmlProp(extra, "value");
        set_optionals_long(pc, name, value);
    }

    return pc;
}

std::ostream& operator<<(std::ostream& out, const ParamConfig& pc)
{
    out << "ParamConfig {";
    if (!pc.optionals_long.empty()) {
        out << "\n  long {";
        for (const auto& ol : pc.optionals_long) {
            out << "\n    " << ol.first << ":";
            for (const auto v : ol.second)
                out << ' ' << v;
        }
        out << "\n  }";
    }
    if (!pc.optionals_string.empty()) {
        out << "\n  string {";
        for (const auto& ol : pc.optionals_string) {
            out << "\n    " << ol.first << ":";
            for (const auto v : ol.second)
                out << ' ' << v;
        }
        out << "\n  }";
    }
    out << "\n}";
    return out;
}

XMLDoc_p initXMLConfig(const XMLInput& configXML)
{
    XMLDoc_p doc(configXML.getXMLDoc());
    doc->registerNamespace("gr", "http://www.met.no/schema/fimex/cdmGribReaderConfig");
    {
        // check config for root element
        XPathNodeSet nodes(doc, "/gr:cdmGribReaderConfig");
        if (nodes.size() != 1)
            throw CDMException("error with rootElement in cdmGribReaderConfig at: " + configXML.id());
    }
    return doc;
}

GribConfig_cp configFromXML(const XMLInput& configXML, const std::vector<std::pair<std::string, std::regex>>& members)
{
    auto config = std::make_shared<GribConfig>();
    config->ensembleMemberIds = members;
    config->configId = configXML.id();
    LOG4FIMEX(logger, Logger::DEBUG, "config id = '" << config->configId << "'");
    config->doc = initXMLConfig(configXML);

    config->replaceEarthString = getConfigEarthFigure(config->doc);
    if (!config->replaceEarthString.empty()) {
        LOG4FIMEX(logger, Logger::DEBUG, "overruling earth-parametes with " << config->replaceEarthString);
    }
    config->extraKeys = getConfigExtraKeys(config->doc);

    config->select_only_defined = false;
    {
        XPathNodeSet nodes(config->doc, "/gr:cdmGribReaderConfig/gr:processOptions/gr:option[@name='selectParameters']");
        if (!nodes.empty() > 0) {
            const auto select = getXmlProp(nodes[0], "value");
            if (select == "definedOnly") {
                config->select_only_defined = true;
            } else if (select != "all") {
                throw std::runtime_error("unknown select-parameter: '" + select + "'");
            }
            LOG4FIMEX(logger, Logger::DEBUG, "selecting parameters: " << select);
        }
    }

    static const std::string xpathString = "/gr:cdmGribReaderConfig/gr:variables/gr:parameter";
    for (auto node_par : XPathNodeSet(config->doc, xpathString)) {
        auto vc = std::make_shared<VariableConfig>();
        vc->constantTime = (getXmlProp(node_par, "constantTime") == "true");
        vc->varName = getXmlProp(node_par, "name");
        const std::string stype = getXmlProp(node_par, "type");
        if (!stype.empty()) {
            vc->type = string2datatype(stype);
        } else {
            vc->type = CDM_DOUBLE;
        }
        { // check vector
            static const std::string k_spatial_vector = "spatial_vector";
            for (xmlNodePtr varNodeChild = node_par->children; varNodeChild; varNodeChild = varNodeChild->next) {
                if ((varNodeChild->type == XML_ELEMENT_NODE) && k_spatial_vector == reinterpret_cast<const char*>(varNodeChild->name)) {
                    vc->vectorDirection = getXmlProp(varNodeChild, "direction");
                    vc->vectorCounterpart = getXmlProp(varNodeChild, "counterpart");
                }
            }
        }
        { // check precision
            static const std::string k_precision = "precision";
            for (xmlNodePtr varNodeChild = node_par->children; varNodeChild; varNodeChild = varNodeChild->next) {
                if ((varNodeChild->type == XML_ELEMENT_NODE) && k_precision == reinterpret_cast<const char*>(varNodeChild->name)) {
                    auto& p = vc->precision;

                    auto str = getXmlProp(varNodeChild, "scale_factor");
                    // TODO: fix scale_factor and add_offset attributes when already existing
                    if (!str.empty()) {
                        p.scale_factor = string2type<double>(str);
                    } else {
                        p.scale_factor = 1;
                    }

                    str = getXmlProp(varNodeChild, "add_offset");
                    if (!str.empty()) {
                        p.add_offset = string2type<double>(str);
                    } else {
                        p.add_offset = 0;
                    }

                    if (vc->type != CDM_FLOAT && vc->type != CDM_DOUBLE) {
                        if (p.scale_factor != 1.) {
                            vc->attributes.push_back(CDMAttribute("scale_factor", p.scale_factor));
                        }
                        if (p.add_offset != 0.) {
                            vc->attributes.push_back(CDMAttribute("add_offset", p.add_offset));
                        }
                    } else {
                        if (p.add_offset != 0.) {
                            LOG4FIMEX(logger, Logger::WARN, "grib cdm config add_offset is ignored for float and double variables");
                        }
                    }
                }
            }
        }

        // template replacements are not initialized here; values are changed in initUpdateTemplatedAttributeValues()
        fillAttributeListFromXMLNode(vc->attributes, node_par->children, {});
        config->variable_configs.push_back(vc);

        for (auto node_grib1 : XPathNodeSet(config->doc, "gr:grib1", node_par)) {
            const std::string idVal = getXmlProp(node_grib1, GK_indicatorOfParameter);
            if (idVal.empty())
                continue;

            ParamConfig pc = pc_grib_any(config->doc, vc, node_grib1);
            set_optionals_long(pc, node_grib1, GK_gribTablesVersionNo);
            set_optionals_long(pc, node_grib1, GK_identificationOfOriginatingGeneratingCentre);

            const long id = string2type<long>(idVal);
            config->nodeIdx1[id].push_back(pc);
#if 0
            LOG4FIMEX(logger, Logger::DEBUG, "id " << id << " grib1 pc " << pc);
#endif
        }

        // and the same for grib2
        for (auto node_grib2 : XPathNodeSet(config->doc, "gr:grib2", node_par)) {
            const std::string idVal = getXmlProp(node_grib2, "parameterNumber");
            if (idVal.empty())
                continue;

            ParamConfig pc = pc_grib_any(config->doc, vc, node_grib2);
            set_optionals_long(pc, node_grib2, GK_parameterCategory);
            set_optionals_long(pc, node_grib2, GK_discipline);

            long id = string2type<long>(idVal);
            config->nodeIdx2[id].push_back(pc);
#if 0
            LOG4FIMEX(logger, Logger::DEBUG, "id " << id << " grib2 pc " << pc);
#endif
        }
    }

    return config;
}

VariableConfig_cp GribConfig::findVariableConfig(const GribFileMessage& msg) const
{
    const auto& pars = msg.getParameterIds();

    const std::string& key_par1 = (msg.getEdition() == 1) ? GK_gribTablesVersionNo : GK_parameterCategory;
    const std::string& key_par2 = (msg.getEdition() == 1) ? GK_identificationOfOriginatingGeneratingCentre : GK_discipline;

#if 0
    LOG4FIMEX(logger, Logger::DEBUG,
              "searching GRIB message with "
                  << " edition=" << msg.getEdition() << " indOfPar/parNum=" << get<0>(pars) << " " << key_par1 << "=" << get<1>(pars) << " " << key_par2 << "="
                  << get<2>(pars) << " typeOfLevel=" << msg.getLevelType() << " levelNo=" << msg.getLevelNumber() << " " << GK_timeRangeIndicator << "="
                  << msg.getTimeRangeIndicator() << " " << GK_typeOfStatisticalProcessing << "=" << msg.getTypeOfStatisticalProcessing() << " " << GK_stepType
                  << "='" << msg.getStepType() << "'");
#endif

    const long param = std::get<0>(pars);
    const auto& nodeIdx = (msg.getEdition() == 1) ? nodeIdx1 : nodeIdx2;
    const auto it = nodeIdx.find(param);
    if (it == nodeIdx.end()) {
#if 0
        LOG4FIMEX(logger, Logger::DEBUG,
                  "<parameter> not found in config for message"
                  " from '"
                      << msg.getFileURL() << " at position '" << msg.getFilePosition() << "'");
#endif
        return nullptr;
    }

    GribReader::VariableConfig_cp matchingVC = nullptr;
    for (const auto& pc : it->second) {
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matching pc " << pc);
#endif // LOG_IFV
        if (!has_optionals_long(pc, key_par1, std::get<1>(pars)))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched par1");
#endif // LOG_IFV
        if (!has_optionals_long(pc, key_par2, std::get<2>(pars)))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched par2");
#endif // LOG_IFV

        if (!has_optionals_long(pc, "typeOfLevel", msg.getLevelType()))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched typeOfLevel");
#endif // LOG_IFV
        if (!has_optionals_long(pc, "levelNo", msg.getLevelNumber()))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched levelNo");
#endif // LOG_IFV
        if (!has_optionals_long(pc, GK_timeRangeIndicator, msg.getTimeRangeIndicator()))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched " << GK_timeRangeIndicator);
#endif // LOG_IFV
        if (!has_optionals_long(pc, GK_typeOfStatisticalProcessing, msg.getTypeOfStatisticalProcessing()))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched " << GK_typeOfStatisticalProcessing);
#endif // LOG_IFV
        if (!has_optionals_string(pc, GK_stepType, msg.getStepType()))
            continue;
#ifdef LOG_IFV
        LOG4FIMEX(logger, Logger::DEBUG, "matched " << GK_stepType);
#endif // LOG_IFV

        bool allOtherKeysMatch = true;
        for (const auto& other : msg.getOtherKeys()) {
#ifdef LOG_IFV
            LOG4FIMEX(logger, Logger::DEBUG, "check extra '" << other.first << "' => '" << other.second << "'");
#endif // LOG_IFV
            if (!has_optionals_long(pc, other.first, other.second)) {
                allOtherKeysMatch = false;
                break;
            }
        }
        if (!allOtherKeysMatch)
            continue;

        if (matchingVC) {
            LOG4FIMEX(logger, Logger::INFO,
                      "multiple <parameter>s found in config for message from '" << msg.getFileURL() << "' at position '" << msg.getFilePosition() << "'");
        } else {
#if 0
            LOG4FIMEX(logger, Logger::DEBUG,
                      "matching <parameter> in config for message from '"
                          << msg.getFileURL() << "' at position '" << msg.getFilePosition() << "'");
#endif
            matchingVC = pc.variable;
        }
    }

#if 0
    if (!matchingVC) {
        LOG4FIMEX(logger, Logger::DEBUG,
                  "no matching <parameter> found in config for message from '"
                      << msg.getFileURL() << "' at position '" << msg.getFilePosition() << "'");
    }
#endif
    return matchingVC;
}

std::string GribConfig::getVariableName(const GribFileMessage& gfm, VariableConfig_cp vc)
{
    if (vc) {
        return vc->varName;
    } else {
        // prepend names from grib-api with 'ga_'
        // since they might otherwise start numerical, which is against CF, and buggy in netcdf 3.6.3, 4.0.*
        return "ga_" + gfm.getShortName() + "_" + type2string(gfm.getLevelType());
    }
}

FimexTime GribConfig::getVariableValidTime(const GribFileMessage& gfm, VariableConfig_cp vc)
{
    if (vc && vc->constantTime)
        return FimexTime();
    return gfm.getValidTime();
}

} // namespace GribReader

} // namespace MetNoFimex
