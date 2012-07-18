/*
 fimex

 Copyright (C) 2011 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: post@met.no

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA
 */

#ifndef GRIDINFORMATION_H_
#define GRIDINFORMATION_H_

#include <string>
#include <vector>
#include <libpq-fe.h>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include "fimex/DataDecl.h"

namespace MetNoFimex
{
class Projection;
class CDMVariable;
class CDM;

namespace wdb
{
class DataSanitizer;


/**
 * Information about a grid, and its projection.
 *
 * Some of the implementation of these objects are provided by subclasses.
 * Therefore, this class has no publicly available constructor. New objects
 * are acquired using one of the static get methods.
 */
class GridInformation : boost::noncopyable
{
public:

    typedef boost::shared_ptr<GridInformation> Ptr;

    /**
     * Get a GrindInformation object.
     */
    static Ptr get(PGresult * result, int row);

    /**
     * Get a grid information object. This method is intended for use by checks.
     */
    static Ptr get(const std::string & projDefinition, unsigned numberX, unsigned numberY);

    virtual ~GridInformation();

    /**
     * Access to grid's projection information
     */
    const boost::shared_ptr<Projection> & getProjection() const { return projection_; }

    /**
     * Get the number of points in horizontal (or longitude) direction.
     */
    unsigned numberX() const { return numberX_; };

    /**
     * Get the number of points in vertical (or latitude) direction.
     */
    unsigned numberY() const { return numberY_; };

    /**
     * Horizontal distance between points, in whatever measure the projection specifies
     */
    float incrementX() const { return incrementX_; }

    /**
     * Vertical distance between points, in whatever measure the projection specifies
     */
    float incrementY() const { return incrementY_; }

    /**
     * Location of lower-left point of grid in the projection
     */
    float startX() const { return startX_; }

    /**
     * Location of lower-left point of grid in the projection
     */
    float startY() const { return startY_; }

    std::string getProjectionName() const;

    /**
     * Add relevant dimensions and variables to the given CDM object
     *
     * @see MetNoFimex::CDM
     */
    virtual void addToCdm(CDM & cdm) const =0;

    /**
     * Get string to use in coordinates attribute for data, or an empty string
     * if this is not required.
     */
    virtual std::string getCoordinatesAttribute() const =0;

    /**
     * Get data for the given variable, if available. If the object cannot
     * give any such data, an empty pointer will be returned.
     */
    virtual DataPtr getField(const CDMVariable & variable) const;

    virtual bool canHandle(const std::string & name) const;

    /**
     * Fill the given vector with strings with names for x- and y-dimensions
     * for this.
     *
     * This is intended to be used when creating variables with a space dimension.
     *
     * @see MetNoFimex::CDMVariable
     */
    virtual void addSpatialDimensions(std::vector<std::string> & out) const =0;

    /**
     * Get a query string for requesting the all data contained in this object
     * from a wdb database.
     *
     * If you use libpq, the result of calling this query can be used as an
     * argument to the get function of this class.
     */
    static std::string query(const std::string & gridName, const DataSanitizer & sanitizer);


protected:
    GridInformation(PGresult * result, int row);
    GridInformation(const boost::shared_ptr<Projection> & projection, unsigned numberX, unsigned numberY);

    boost::shared_ptr<Projection> projection_;

private:
    unsigned numberX_;
    unsigned numberY_;
    float incrementX_;
    float incrementY_;
    float startX_;
    float startY_;

};

}

}

#endif /* GRIDINFORMATION_H_ */
