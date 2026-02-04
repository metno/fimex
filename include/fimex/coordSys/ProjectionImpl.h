/*
 * Fimex, ProjectionImpl.h
 *
 * (C) Copyright 2010-2026, met.no
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#ifndef PROJECTIONIMPL_H_
#define PROJECTIONIMPL_H_

#include "fimex/coordSys/Projection.h"
/**
 * @headerfile "fimex/coordSys/ProjectionImpl.h"
 */

namespace MetNoFimex {

/**
 * ProjectionImpl is a next to complete implementation of
 * Projection, storing all parameters as a vector<CDMAttribute>.
 * Implementations only need to implement a constructor calling
 * the ProjectionImpl("proj-name") and the method getProj4ProjectionPart()
 *
 * @note the implemented projection needs to get made visible in Projection::create() and  Projection::createByProj4
 *
 */
class ProjectionImpl : public Projection
{
public:
    ~ProjectionImpl();
    std::vector<CDMAttribute> getParameters() const override;
    void addParameter(const CDMAttribute& attribute) override;
    void addParameters(const std::vector<CDMAttribute>& attributes) override;
    void removeParameter(const std::string& paramName) override;
    /** get the projection name */
    const std::string& getName() const override;
    bool isDegree() const override;

    /**
     * get the proj4 string defined by the parameters. If a parameter
     * named 'proj4' exists, that one will be used and all other parameters will
     * be ignored.
     * @note Implementors should not overwrite this method, but the protected getProj4ProjectionPart()
     * method
     */
    std::string getProj4String() const override;

    /**
     * get the parts of the proj4 string defining the earth.
     */
    std::string getProj4EarthString() const override;

    /** get a string representation */
    std::string toString() const override;

protected:
    /** match the +proj= part of a proj4 string */
    static bool proj4ProjectionMatchesName(const std::string& proj4String, const std::string& name);

    /**
     * add the attributes describing the earth from a proj4-string to the outAttrs
     * @param proj4String string as used for proj4
     * @param attrList output list of CDMAttributes
     */
    static void proj4GetEarthAttributes(const std::string& proj4String, std::vector<CDMAttribute>& attrList);

    // set the name of the projection and assign the isDegree parameter
    explicit ProjectionImpl(std::string name, bool isDegree);

    /**
     * add the pure projection parameters for proj4 to the stream, i.e. no earth
     * definitions, and no +no_defs
     */
    virtual std::ostream& getProj4ProjectionPart(std::ostream&) const = 0;

    /**
     * Add the numeric value of a parameter named name as replaceName to oproj, e.g. name = false_easting, replaceName = +x_0, sets "+x_0=... "
     * Assume only one value at maximum
     * @param outStream
     * @param name the parameters name
     * @param replaceName the name to use in the stream to the parameter, defaults to original name ("")
     * @return true if parameter found and set
     */
    bool addParameterToStream(std::ostream& outStream, const std::string& name, const std::string& replaceName = std::string()) const;

    std::vector<CDMAttribute> params_;

private:
    const std::string name_;
    const bool isDegree_;
};

} // namespace MetNoFimex

#endif /* PROJECTIONIMPL_H_ */
