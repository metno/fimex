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

#ifndef FELTGRIDDEFINITION_H_
#define FELTGRIDDEFINITION_H_

#include <string>
#include <vector>
#include <iosfwd>
#include <boost/array.hpp>

namespace felt
{

/**
 * retrieve the 6 gridparameters from the felt-data
 * @param gridType id of the grid,  (header[8] < 1000) ? header[8] : (int) header[8] / 1000
 * @param xNum number of points in x-direction, header[9]
 * @param yNum number of points in x-direction, header[10]
 * @param a used for different depending on gridType, header[14]
 * @param b used for different depending on gridType, header[15]
 * @param c used for different depending on gridType, header[16]
 * @param d used for different depending on gridType, header[17]
 * @param extraData data at the end of the data-region, used for high resolution information (header[8] < 1000) ? 0 : header[8] % 1000
 */
boost::array<float, 6> gridParameters(int gridType, int xNum, int yNum, int a, int b, int c, int d, const std::vector<short int> & extraData);

/**
 * convert the libmi-gridparameters to proj4 strings
 * @param gridType type defining the projection (1..6)
 * @param gridPars array containing libmi's six gridparameters
 * @return proj.4 string
 */
std::string gridParametersToProjDefinition(int gridType, const boost::array<float, 6>& gridPars);


class FeltGridDefinition
{
public:
    /**
     * Orientation describes the different ways that the values can
     * be ordered in the grid. There are four possible dimensions:
     * Left to Right or Right to Left
     * Lower to Upper or Upper to Lower
     * Horizontal scanning or Vertical scanning
     * Regular or Alternating (i.e., every second row changes direction)
     */
    enum Orientation {
        LeftUpperHorizontal = 0,  // 00000000
        LeftLowerHorizontal = 64  // 01000000
    };
	/**
	 * The parameters a, b, c, d are words 15 to 18 in the FELT header definition. These usually describe elements
	 * of the grid specification (variable meaning, depending on the grid specification used)
	 */
    FeltGridDefinition(int gridType, int xNum, int yNum, int a, int b, int c, int d, const std::vector<short int> & extraData);
    virtual ~FeltGridDefinition();
    virtual std::string projDefinition() const;
    virtual int getXNumber() const;
    virtual int getYNumber() const;
    virtual float getXIncrement() const;
    virtual float getYIncrement() const;
    virtual float startLongitude() const;
    virtual float startLatitude() const;
    virtual float startX() const;
    virtual float startY() const;
    virtual const boost::array<float, 6>& getGridParameters() const;
    Orientation getScanMode() const;


protected:
    std::string projDef_;
private:
    size_t xNum_;
    size_t yNum_;
    float startX_;
    float startY_;
    float incrementX_;
    float incrementY_;
    Orientation orientation_;
    boost::array<float, 6> gridPars_;

    Orientation getScanMode_();
    void polarStereographicProj_(int gridType, int a, int b, int c, int d, const std::vector<short int> & extraData);
    void geographicProj_(int gridType, int a, int b, int c, int d, const std::vector<short int> & extraData);
    void mercatorProj_(int gridType, int a, int b, int c, int d, const std::vector<short int> & extraData);
    void lambertConicProj_(int gridType, int a, int b, int c, int d, const std::vector<short int> & extraData);
    // those two below are deprecated
    void polarStereographicProj(int gridType, float poleX, float poleY, float gridD, float rot, const std::vector<short int> & extraData);
    void geographicProj(int gridType, float startLongitude, float startLatitude, float iInc, float jInc, const std::vector<short int> & extraData);

};

std::ostream & contentSummary(std::ostream & out, const FeltGridDefinition & grid);

}

#endif /*FELTGRIDDEFINITION_H_*/
