/*
 wdb

 Copyright (C) 2007-2019 met.no

 Contact information:
 Norwegian Meteorological Institute
 Box 43 Blindern
 0313 OSLO
 NORWAY
 E-mail: wdb@met.no

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
#ifndef FELTFIELD_H_
#define FELTFIELD_H_

#include "FeltConstants.h"
#include "FeltGridDefinition.h"

#include <boost/shared_array.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#include <array>
#include <string>
#include <vector>

namespace felt
{

typedef std::shared_ptr<FeltGridDefinition> FeltGridDefinitionPtr;

class FeltFile;

class FeltField
{
public:
    typedef std::array<word, 16> Header;

    FeltField(const FeltFile& ff, size_t index);
    FeltField(const FeltField&) = delete;
    FeltField& operator=(const FeltField&) = delete;

    ~FeltField();

    bool valid() const { return gridSize() != 0; }

    int producer() const { return header_[0]; }
    int gridArea() const { return header_[1]; }

    boost::posix_time::ptime referenceTime() const;
    boost::posix_time::ptime validTime() const;

    /**
     * Get the parameter value. This is the logical value, which means that if
     * the parameter is part of an ensemble run, it will not start with
     * 2000,3000,4000,5000.
     */
    int parameter() const;

    /**
     * read the time dataType, i.e. 1=analysis 2=interpolated/initialization 3=prognosis 4=parameter-field(no time)
     */
    int dataType() const { return header_[8]; }

    int verticalCoordinate() const { return header_[10]; }

    /**
     * Get primary level value
     */
    int level1() const;

    /**
     * Get the secondary level value, or 0 if that level field has internally
     * been used for something else. This will happen if the field is part of
     * an enseble run.
     */
    int level2() const;

    int gridType() const { return header_[14]; }

    /**
     * Read the grid from file.
     */
    void grid(std::vector<word>& out) const;
    size_t gridSize() const;
    int scaleFactor() const;
    int xNum() const;
    int yNum() const;
    /// this field is described in the felt documentation as "word 19 in data part"
    int miscField() const;

    bool isEpsRunParameter() const
    {
        int param = parameterUnmodified_();
        return 2000 <= param and param < 6000;
    }

    /**
     * Get data version if this is an eps parameter, otherwise 0
     */
    int dataVersion() const;

    /// throws
    FeltGridDefinitionPtr projectionInformation() const;

    std::string information() const;
    std::string gridInformation() const;

    /// access felt index header
    const Header& getHeader() const { return header_; }

private:

	int timeParameter() const { return header_[9]; }

	int parameterUnmodified_() const { return header_[11]; }

	/**
	 * Checks consistency between index and grid header
	 */
	bool isSane() const;

private:
	size_t startingGridBlock() const;

	const std::vector<word> & getGridHeader_() const;
	const std::vector<word> & getExtraGeometrySpecification_() const;

	mutable std::vector<word> gridHeader_;
	mutable std::vector<word> extraGridSpec_;
	Header header_;
	const FeltFile & feltFile_;
	size_t index_;
};

}

#endif /*FELTFIELD_H_*/
