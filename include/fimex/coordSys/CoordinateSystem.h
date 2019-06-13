/*
 * Fimex, CoordinateSystem.h
 *
 * (C) Copyright 2009, met.no
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
 *  Created on: Mar 15, 2010
 *      Author: Heiko Klein
 */

#ifndef COORDINATESYSTEM_H_
#define COORDINATESYSTEM_H_

#include <functional>
#include <iosfwd>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "fimex/CDMReaderDecl.h"
#include "fimex/coordSys/CoordSysDecl.h"
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/deprecated.h"

namespace MetNoFimex {

//forward decl.
class CDM;
struct CoordSysImpl;

/**
 * @headerfile fimex/coordSys/CoordinateSystem.h
 */
/**
 * CoordinateSystems are usually created using the listCoordinateSystems(CDMReader_p) function, see example there.
 *
 * To investigate the coordinate systems of a file, use {\em fimex --printCS}.
 */
class CoordinateSystem
{
public:
    /**
     * CoordinateSystems are usually created within the listCoordinateSystems(CDMReader_p) funcion.
     */
    CoordinateSystem();
    explicit CoordinateSystem(const std::string& conventionName);
    virtual ~CoordinateSystem();

    /**
     * unique identifier for a coordinate system
     */
    virtual std::string id() const;

    /**
     * get an id for the horizontal part of the coordinate system, i.e.
     * geoXAxis and geoYAxis
     */
    virtual std::string horizontalId() const;

    /**
     * @return the name of convention used to build the CS
     */
    virtual std::string getConventionName() const;
    /**
     * set the convention name
     * @param conventionName
     */
    virtual void setConventionName(const std::string& conventionName);
    /**
     * All axes of this system are used by the variable varName
     * @param varName variable name
     */
    virtual bool isComplete(const std::string& varName) const;
    /**
     * set or unset if the coordinate system is complete for the variable
     */
    virtual void setComplete(const std::string& varName, bool set = true);
    /**
     * All dimensions of the variable are described by CoordinateSystem
     * @param varName variable name
     */
    virtual bool isCSFor(const std::string& varName) const;
    /**
     * Set or unset if all dimensions are described by the CoordinateSystem
     */
    virtual void setCSFor(const std::string& varName, bool set = true);

    /** Return true iff isComplete and isCSFor both return true. */
    virtual bool isCSAndCompleteFor(const std::string& varName) const;

    /**
     * Check if coordinate system has direct spatial axes, i.e. 1-dim x,y axes or 1-dim lon,lat axes
     */
    virtual bool isSimpleSpatialGridded() const;
    /**
     * Set or unset if this coordinate system has spatial axes, i.e. 1-dim x,y or lon/lat axes
     */
    virtual void setSimpleSpatialGridded(bool set = true);
    /**
     * Check if the coordinate-system has a projection (of GeoX, GeoY and optionally GeoZ)
     * This includes also coordinate-systems in latitude-longitude 'projection'.
     */
    virtual bool hasProjection() const;
    /**
     * Get the projection of the coordinate-system (projection of GeoX, GeoY and optionally GeoZ)
     * This includes also coordinate-systems in latitude-longitude 'projection'.
     * @return projection, or null ptr
     */
    virtual Projection_cp getProjection() const;
    /**
     * Set the projection of the coordinate-system (projection of GeoX, GeoY and optionally GeoZ)
     */
    virtual void setProjection(Projection_cp proj);
    /**
     * Check if the coordinate-system has a vertical transformation (of GeoZ)
     * This includes also coordinate-systems in pressure or height.
     */
    virtual bool hasVerticalTransformation() const;
    /**
     * Get the vertical transformation of the coordinate-system (GeoZ)
     * This includes also height or pressure 'transformations'
     * @return vtrans, or null ptr
     */
    virtual VerticalTransformation_cp getVerticalTransformation() const;
    /**
     * Set the vertical transformation of the coordinate-system (transformation of GeoZ)
     */
    virtual void setVerticalTransformation(VerticalTransformation_cp vtran);
    /**
     * Check if the CoordinateSystem contains exactly the axis type
     * @param type axis type to check against
     */
    virtual bool hasAxisType(CoordinateAxis::AxisType type) const;
    /**
     * find the first axis with exactly the types
     * @param type
     * @return an axis or null
     */
    virtual CoordinateAxis_cp findAxisOfType(CoordinateAxis::AxisType type) const;
    /**
     * find the first axis with one of the types
     * @param types list of types
     * @return an axis or null
     */
    virtual CoordinateAxis_cp findAxisOfType(const std::vector<CoordinateAxis::AxisType>& types) const;
    /**
     * Get the geographical x axis, that is one of
     * GeoX, Longitude (in that order if several exist)
     * @return an axis or null
     */
    virtual CoordinateAxis_cp getGeoXAxis() const;
    /**
     * Get the geographic y axis, that is one of
     * GeoY, Latitude (in that order if several exist)
     * @return an axis or null
     */
    virtual CoordinateAxis_cp getGeoYAxis() const;
    /**
     * get the geographical z-axis, that is one of
     * GeoZ, Height, Pressure (in that order if several exist)
     * @return an axis, or null
     */
    virtual CoordinateAxis_cp getGeoZAxis() const;
    /**
     * get the time-axis, or NULL/0
     */
    virtual CoordinateAxis_cp getTimeAxis() const;
    /**
     * get all axes, which are not duplicated (i.e. twice in auxiliary)
     */
    virtual CoordinateAxis_cp_v getAxes() const;
    /**
     * Set any axis.
     * @throw CDMException if an axis with the same axistype (except undefined) exists
     */
    virtual void setAxis(CoordinateAxis_cp axis);
    /**
     * Set an auxiliary axis. An auxiliary axis may be dropped silently if
     * another axis with the axistype exists.
     */
    virtual void setAuxiliaryAxis(CoordinateAxis_cp axis);
    /**
     * Get all dependency-variables for this Coordinate-System, i.e.
     * axes, grid-mapping-variable, formula-terms, ...
     */
    virtual std::set<std::string> getDependencyVariables() const;
    /**
     * add a variable to the dependencies
     * @param varName
     */
    virtual void addDependencyVariable(std::string varName);

private:
    CoordinateSystem(const CoordinateSystem&);            // not implemented
    CoordinateSystem& operator=(const CoordinateSystem&); // not implemented

private:
    std::unique_ptr<CoordSysImpl> pimpl_;
};

/**
 * output operator
 */
std::ostream& operator<<(std::ostream& out, const CoordinateSystem& p);

/**
 *  @example coordinateSystem.cpp
 * Example on using the CoordinateSystem in combination with a CDMReader.
 */
/**
 * fetch all coordinate system from a MetNoFimex::CDMReader
 * @param reader the data-source. Depending on the internal structure of the datasource,
 *        the source might be manipulated, i.e. the WRF-system is missing several variables.
 * @warning since this function might change the CDM of the reader, it is usually a good idea to
 *          run this function before copying the readers CDM, e.g.
 * @code
     CoordinateSystem_cp_v css = listCoordinateSystems(reader);
     CDM cdm = reader.getCDM();
   @endcode
 */
CoordinateSystem_cp_v listCoordinateSystems(CDMReader_p reader);

/**
 * set spatial-vector properties according to convention
 * won't overwrite any existing spatial vector properties
 * @param reader
 */
void enhanceVectorProperties(CDMReader_p reader);

/**
 * fetch all coordinate system from a MetNoFimex::CDM
 * @deprecated call listCoordinateSystems(CDMReader_p reader) instead
 */
MIFI_DEPRECATED(CoordinateSystem_cp_v listCoordinateSystems(CDM& cdm));

/**
 * fetch all coordinate system from a MetNoFimex::CDM
 * @deprecated call listCoordinateSystems(CDMReader_p reader) instead
 */
MIFI_DEPRECATED(CoordinateSystem_cp_v listCoordinateSystems(const CDM& cdm));

/**
 * find all horizontal coordinate systems, either with or without projections, list
 * the variables belonging to the coordinate systems and mention all incompatible variables
 * with a coordinate system
 *
 * @param withProjection select only horizontal coordinatesystems either with (true) or without (false) projection
 * @param reader the CDMReader to search coordinate systems in, comparable to listCoordinateSystems()
 * @param systems Output of coordinateSystems (horizontalId -> system)
 * @param variables Output with all variables having a horizontal coordinate system
 * @param incompatibleVariables Output with a all variables which have a partial horizontal coordinate system
 *        which might interfere when changing dimensions
 * @return number of horizontal coordinate systems found, i.e. systems.size()
 */
int findBestHorizontalCoordinateSystems(bool withProjection, CDMReader_p reader, std::map<std::string, CoordinateSystem_cp>& systems,
                                        std::map<std::string, std::string>& variables, std::vector<std::string>& incompatibleVariables);

/**
 * Functor to check if a coordinate system completely describes a variable, i.e. all axes match fully.
 */
struct CompleteCoordinateSystemForComparator : public std::unary_function<CoordinateSystem_cp, bool>
{
public:
    CompleteCoordinateSystemForComparator(const std::string& varName) : varName(varName) {}
    bool operator()(const CoordinateSystem_cp& cs) const { return cs->isCSAndCompleteFor(varName); }

private:
    const std::string& varName;
};

/**
 * find a complete coordinate system for a variable using CompleteCoordinateSystemForComparator
 * @return a shared_ptr to the coordinate system that has been found, or a null ptr if none was found
 */
CoordinateSystem_cp findCompleteCoordinateSystemFor(const CoordinateSystem_cp_v& coordSys, const std::string& varName);
}

#endif /* COORDINATESYSTEM_H_ */
