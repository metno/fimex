/*
 * Fimex, ProjectionImpl.h
 *
 * (C) Copyright 2010, met.no
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
 *  Created on: Apr 27, 2010
 *      Author: Heiko Klein
 */

#ifndef PROJECTIONIMPL_H_
#define PROJECTIONIMPL_H_

#include "fimex/coordSys/Projection.h"

namespace MetNoFimex
{

/**
 * ProjectionImpl is a next to complete implementation of
 * Projection, storing all parameters as a vector<CDMAttribute>.
 * Implementations only need to implement a constructor calling
 * the ProjectionImpl("proj-name") and the method getProj4ProjectionPart()
 *
 * @note the implemented projection needs to get made visible in Projection.cc create()
 *
 */
class ProjectionImpl: public MetNoFimex::Projection
{
public:
    virtual ~ProjectionImpl();
    virtual std::vector<CDMAttribute> getParameters() const;
    virtual void addParameter(CDMAttribute attribute);
    virtual void addParameters(std::vector<CDMAttribute> attributes);
    virtual void removeParameter(std::string paramName);
    /** get the projection name */
    virtual const std::string& getName() const;
    virtual const bool isDegree() const;

    /**
     * get the proj4 string defined by the parameters. If a parameter
     * named 'proj4' exists, that one will be used and all other parameters will
     * be ignored.
     * @note Implementors should not overwrite this method, but the protected getProj4ProjectionPart()
     * method
     */
    virtual std::string getProj4String() const;
    /** get a string representation */
    virtual std::string toString() const;
protected:
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
    virtual bool addParameterToStream(std::ostream& outStream, const std::string& name, std::string replaceName = "") const;
    std::vector<CDMAttribute> params_;
private:
    const std::string name_;
    const bool isDegree_;
};

}

#endif /* PROJECTIONIMPL_H_ */
