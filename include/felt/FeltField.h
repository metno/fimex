/*
 wdb

 Copyright (C) 2007 met.no

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

#include "felt/feltConstants.h"
#include "felt/FeltGridDefinition.h"
#include <boost/shared_array.hpp>
#include <boost/array.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <vector>

namespace felt
{

typedef boost::shared_ptr<FeltGridDefinition> FeltGridDefinitionPtr;

class FeltFile;

class FeltField : boost::noncopyable
{
public:
	typedef boost::array<word, 16> Header;

	FeltField(const FeltFile & ff, size_t index);

	~FeltField();

	bool valid() const { return gridSize() != 0; }

	int producer() const { return header_[0]; }
	int gridArea() const { return header_[1]; }

	boost::posix_time::ptime referenceTime() const;
	boost::posix_time::ptime validTime() const;

	int parameter() const { return header_[11]; }

	int verticalCoordinate() const { return header_[10]; }
	int level1() const {return header_[12]; }
	int level2() const {return header_[13]; }
    int gridType() const { return header_[14]; }

	/**
	 * Read the grid from file.
	 */
	void grid(std::vector<word> & out) const;
	size_t gridSize() const;
	int scaleFactor() const;
	int xNum() const;
	int yNum() const;
	/// this field is described in the felt documentation as "word 19 in data part"
	int miscField() const;

	/**
	 * Get data version if this is an eps parameter, otherwise 0
	 */
	int dataVersion() const;

	/// throws
	FeltGridDefinitionPtr projectionInformation() const;

	std::string information() const;
	std::string gridInformation() const;

	/// access felt index header
	const Header& getHeader() const {return header_;}

private:

	int dataType() const { return header_[8]; }
	int timeParameter() const { return header_[9]; }


	/**
	 * Checks consistency between index and grid header
	 */
	bool isSane() const;

private:
	size_t startingGridBlock() const;

	const std::vector<word> & getGridHeader_() const;
	void getExtraGeometrySpecification_(std::vector<short int> & out) const;

	mutable std::vector<word> gridHeader_;
	Header header_;
	const FeltFile & feltFile_;
	size_t index_;
};

}

#endif /*FELTFIELD_H_*/
