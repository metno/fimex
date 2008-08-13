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

#include "fimex/GribApiCDMWriter_ImplAbstract.h"
#include "fimex/TimeUnit.h"

namespace MetNoFimex
{

/** helper classes Scale and UnScale to transform double vectors */
class Scale : public std::unary_function<std::string, bool>
{
	const double scale_factor;
	const double add_offset;
public:
	Scale(double scale_factor, double add_offset) : scale_factor(scale_factor), add_offset(add_offset) {}
	virtual ~Scale() {}
	virtual double operator() (double x) const {return x*scale_factor + add_offset;}
};

class UnScale : public Scale
{
public:
	UnScale(double scale_factor, double add_offset) : Scale(1/scale_factor, -1 * add_offset/scale_factor) {}
	virtual ~UnScale() {}
};

/** TimeUnit 2 FimexTime converter*/
class TimeUnit2FimexTime : public std::unary_function<double, FimexTime>
{
	const TimeUnit& tu;
public:
	TimeUnit2FimexTime(const TimeUnit& tu) : tu(tu) {}
	FimexTime operator() (double unitTime) {return tu.unitTime2fimexTime(unitTime);}
};

GribApiCDMWriter_ImplAbstract::GribApiCDMWriter_ImplAbstract(int gribVersion, const boost::shared_ptr<CDMReader>& cdmReader, const std::string& outputFile, const std::string& configFile)
: gribVersion(gribVersion), cdmReader(cdmReader), outputFile(outputFile), configFile(configFile), xmlConfig(new XMLDoc(configFile))
{
	std::string gribTemplate("GRIB" + type2string(gribVersion));
	gribHandle = boost::shared_ptr<grib_handle>(grib_handle_new_from_template(0, gribTemplate.c_str()), grib_handle_delete);
	if (gribHandle.get() == 0) throw CDMException("unable to open grib_handle_from_template for grib-template: " + gribTemplate);
}

GribApiCDMWriter_ImplAbstract::~GribApiCDMWriter_ImplAbstract()
{
}

void GribApiCDMWriter_ImplAbstract::setData(const boost::shared_ptr<Data>& data) {
	GRIB_CHECK(grib_set_double_array(gribHandle.get(), "values", data->asConstDouble().get(), data->size()), "setting values");
}

void GribApiCDMWriter_ImplAbstract::setTime(size_t timePos, const std::vector<FimexTime>& cdmVarTimes)
{
	const FimexTime& fiTime = cdmVarTimes[timePos];
	long date = fiTime.year * 10000 + fiTime.month * 100 + fiTime.mday;
	long time = fiTime.hour * 100 + fiTime.minute;
	GRIB_CHECK(grib_set_long(gribHandle.get(), "dataDate", date), "setting dataDate");
	GRIB_CHECK(grib_set_long(gribHandle.get(), "dataTime", time), "setting dataTime");
}

std::vector<double> GribApiCDMWriter_ImplAbstract::getLevels(const std::string& varName) throw(CDMException)
{
	Units units;
	std::vector<double> levelData;
	// TODO: proper definition of level (code table 3) (indicatorOfLevel)
	// recalculate level values to have units as defined in code table 3
	// recalculate units of level
	const CDM& cdm = cdmReader->getCDM();
	std::string verticalAxis = cdm.getVerticalAxis(varName);
	std::string verticalAxisXPath("/cdm_gribwriter_config/axes/vertical_axis");
	std::string unit;
	if (verticalAxis != ""){
		boost::shared_ptr<Data> myLevelData = cdmReader->getData(verticalAxis);
		const boost::shared_array<double> levelDataArray = myLevelData->asConstDouble();
		levelData= std::vector<double>(&levelDataArray[0], &levelDataArray[myLevelData->size()]);
		CDMAttribute attr;
		if (cdm.getAttribute(verticalAxis, "standard_name", attr)) {
			verticalAxisXPath += "[@standard_name=\""+ attr.getData()->asString() + "\"]";
		} else if (cdmReader->getCDM().getAttribute(verticalAxis, "units", attr)) {
			// units compatible to Pa or m
			std::string unit = attr.getData()->asString();
			if (units.areConvertible(unit, "m")) {
				verticalAxisXPath += "[@unitCompatibleTo=\"m\"]";
			} else if (units.areConvertible(unit, "Pa")) {
				verticalAxisXPath += "[@unitCompatibleTo=\"Pa\"]";
			} else {
				throw CDMException("units of vertical axis " + verticalAxis + " should be compatible with m or Pa but are: " + unit);
			}
		} else {
			throw CDMException("couldn't find standard_name or units for vertical Axis " + verticalAxis + ". Is this CF compatible?");
		}
	} else {
		// cdmGribWriterConfig should contain something like standard_name=""
		verticalAxisXPath += "[@standard_name=\"\"]";
		// TODO get default from config
		levelData.push_back(0);
	}
	// scale the original levels according to the cdm
	double scale_factor = 1.;
	double add_offset = 0.;
	CDMAttribute attr;
	if (cdm.getAttribute(verticalAxis, "scale_factor", attr)) {
		scale_factor = attr.getData()->asDouble()[0];
	}
	if (cdm.getAttribute(verticalAxis, "add_offset", attr)) {
		add_offset = attr.getData()->asDouble()[0];
	}
	std::transform(levelData.begin(), levelData.end(), levelData.begin(), Scale(scale_factor, add_offset));


	// scale the levels according to grib
	verticalAxisXPath += "/grib" + type2string(gribVersion);
	std::cerr << "looking at: " << verticalAxisXPath << std::endl;
	XPathObjPtr verticalXPObj = xmlConfig->getXPathObject(verticalAxisXPath);
	xmlNodeSetPtr nodes = verticalXPObj->nodesetval;
	int size = (nodes) ? nodes->nodeNr : 0;
	if (size == 1) {
		xmlNodePtr node = nodes->nodeTab[0];
		// scale the levels from cf-units to grib-untis
		std::string gribUnits = getXmlProp(node, "units");
		if (gribUnits != "") {
			if (cdm.getAttribute(verticalAxis, "units", attr)) {
				double slope;
				double offset;
				units.convert(attr.getData()->asString(), gribUnits, slope, offset);
				std::transform(levelData.begin(), levelData.end(), levelData.begin(), Scale(slope, offset));
			}
		}

		// unscale to be able to put the data into values suitable to grib
		std::string gribScaleFactorStr = getXmlProp(node, "scale_factor");
		std::string gribAddOffsetStr = getXmlProp(node, "add_offset");
		scale_factor = 1.;
		add_offset = 0.;
		if (gribScaleFactorStr != "") {
			scale_factor = string2type<double>(gribScaleFactorStr);
		}
		if (gribAddOffsetStr != "") {
			add_offset = string2type<double>(gribAddOffsetStr);
		}
		std::transform(levelData.begin(), levelData.end(), levelData.begin(), UnScale(scale_factor, add_offset));
	} else if (size > 1) {
		throw CDMException("several entries in grib-config at " + configFile + ": " + verticalAxisXPath);
	} else {
		std::cerr << "could not find vertical Axis " << verticalAxisXPath << " in " << configFile << ", skipping parameter " << varName << std::endl;
	}
	return levelData;
}

std::vector<FimexTime> GribApiCDMWriter_ImplAbstract::getTimes(const std::string& varName) throw(CDMException)
{
	const CDM& cdm = cdmReader->getCDM();
	const std::string& time = cdm.getTimeAxis(varName);
	std::vector<FimexTime> timeData;
	if (time != "") {
		const boost::shared_array<double> timeDataArray = cdmReader->getData(time)->asDouble();
		std::vector<double> timeDataVector(&timeDataArray[0], &timeDataArray[cdm.getDimension(time).getLength()]);
		TimeUnit tu(cdm.getAttribute(time, "units").getStringValue());
		std::transform(timeDataVector.begin(), timeDataVector.end(), std::back_inserter(timeData), TimeUnit2FimexTime(tu));
	} else {
		// TODO find a more useful default
		timeData.push_back(FimexTime());
	}
	return timeData;
}

}
