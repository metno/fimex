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
#include <vector>
#include <map>
#include <string>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include "fimex/coordSys/CoordinateAxis.h"
#include "fimex/coordSys/Projection.h"
#include "fimex/deprecated.h"


namespace MetNoFimex
{

//forward decl.
class CDM;
class CDMReader;
struct CoordSysImpl;

/**
 * @headerfile "fimex/coordSys/CoordinateSystem.h"
 */
/**
 * CoordinateSystems are usually created using the listCoordinateSystems(const CDM& cdm) function, see example there.
 *
 * To investigate the coordinate systems of a file, use {\em fimex --printCS}.
 */
class CoordinateSystem
{
public:
    /**
     * a garbage collected pointer to a constant coordinateAxis
     */
    typedef boost::shared_ptr<const CoordinateAxis> ConstAxisPtr;
    /**
     * a garbage collected pointer to a CoordinateAxis
     */
    typedef boost::shared_ptr<CoordinateAxis> AxisPtr;
    /**
     * a list to constant axis pointer
     */
    typedef std::vector<ConstAxisPtr> ConstAxisList;

    /**
     * CoordinateSystems are usually created within the listCoordinateSystems(const CDM& cdm) funcion.
     */
    CoordinateSystem();
    explicit CoordinateSystem(const std::string& conventionName);
    virtual ~CoordinateSystem() {}

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
    virtual boost::shared_ptr<const Projection> getProjection() const;
    /**
     * Set the projection of the coordinate-system (projection of GeoX, GeoY and optionally GeoZ)
     */
    virtual void setProjection(boost::shared_ptr<const Projection> proj);
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
    virtual ConstAxisPtr findAxisOfType(CoordinateAxis::AxisType type) const;
    /**
     * find the first axis with one of the types
     * @param types list of types
     * @return an axis or null
     */
    virtual ConstAxisPtr findAxisOfType(const std::vector<CoordinateAxis::AxisType>& types) const;
    /**
     * get the geographical x/lon-axis, that is one of
     * GeoX, Longitude (in that order if several exist)
     * @return an axis or null
     */
    virtual ConstAxisPtr getGeoXAxis() const;
    /**
     * Set/overwrite the geographic y axis, that is one of
     * GeoY, Latitude (in that order if several exist)
     * @return an axis or null
     */
    virtual ConstAxisPtr getGeoYAxis() const;
    /**
     * get the geographical z-axis, that is one of
     * GeoZ, Height, Pressure (in that order if several exist)
     * @return an axis, or null
     */
    virtual ConstAxisPtr getGeoZAxis() const;
    /**
     * get the time-axis, or NULL/0
     */
    virtual ConstAxisPtr getTimeAxis() const;
    /**
     * get all axes, which are not duplicated (i.e. twice in auxiliary)
     */
    virtual ConstAxisList getAxes() const;
    /**
     * Set any axis.
     * @throw CDMException if an axis with the same axistype (except undefined) exists
     */
    virtual void setAxis(ConstAxisPtr axis);
    /**
     * Set an auxiliary axis. An auxiliary axis may be dropped silently if
     * another axis with the axistype exists.
     */
    virtual void setAuxiliaryAxis(ConstAxisPtr axis);
private:
    boost::shared_ptr<CoordSysImpl> pimpl_;
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
 *          @verbatim
     vector<boost::shared_ptr<const CoordinateSystem> > css = listCoordinateSystems(reader);
     CDM cdm = reader.getCDM();
 *          @endverbatim
 */
std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(boost::shared_ptr<CDMReader> reader);

/**
 * fetch all coordinate system from a MetNoFimex::CDM
 * @deprecated call listCoordinateSystems(boost::shared_ptr<CDMReader> reader) instead
 */
DEPRECATED(std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(CDM& cdm));
/**
 * fetch all coordinate system from a MetNoFimex::CDM
 * @deprecated call listCoordinateSystems(boost::shared_ptr<CDMReader> reader) instead
 */
DEPRECATED(std::vector<boost::shared_ptr<const CoordinateSystem> > listCoordinateSystems(const CDM& cdm));

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
int findBestHorizontalCoordinateSystems(bool withProjection, boost::shared_ptr<CDMReader> reader, std::map<std::string, boost::shared_ptr<const CoordinateSystem> >& systems, std::map<std::string, std::string>& variables, std::vector<std::string>& incompatibleVariables);

/**
 * Functor to check if a coordinate system completely describes a variable, i.e. all axes match fully.
 */
struct CompleteCoordinateSystemForComparator : public std::unary_function<boost::shared_ptr<const CoordinateSystem>, bool>
{
public:
    CompleteCoordinateSystemForComparator(const std::string& varName) : varName(varName) {}
    virtual ~CompleteCoordinateSystemForComparator() {}
    bool operator()(const boost::shared_ptr<const CoordinateSystem>& cs) {return cs->isComplete(varName) && cs->isCSFor(varName);}
private:
    const std::string& varName;

};

}

#endif /* COORDINATESYSTEM_H_ */
