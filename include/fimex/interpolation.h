/*
 * Fimex
 *
 * (C) Copyright 2008-2026, met.no
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

#ifndef INTERPOLATION_H_
#define INTERPOLATION_H_

/**
 * @headerfile "fimex/interpolation.h"
 */

#include "fimex/mifi_constants.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Convert interpolation methods in string-format to mifi_interpol_method (see mifi_constants.h)
 * @param stringMethod one of nearestneighbor, bilinear,bicubic, forward_max, forward_min, forward_mean,
 *    forward_median, forward_sum, coord_nearestneighbor, coord_kdtree
 * @return mifi_interpol_method enum, or MIFI_INTERPOL_UNKNOWN on error
 */
extern int mifi_string_to_interpolation_method(const char* stringMethod);

/**
 * Get the nearest neighbor of a value. Values are rounded to array-position. Extrapolates to once the distance of the two leftmost values (or rightmost).
 *
 * @param infield 3d fortran array of size ix,iy,iz
 * @param outfield 1d array of size iz containing the values
 * @param x,y
 * @param ix,iy,iz
 */
extern int mifi_get_values_weak_extrapol_f(const float* infield, float* outfield, const double x, const double y, const int ix, const int iy, const int iz);

/**
 * Get the nearest neighbor of a value. Values are rounded to array-position. No extrapolation will occur.
 *
 * @param infield 3d fortran array of size ix,iy,iz
 * @param outfield 1d array of size iz containing the values
 * @param x,y
 * @param ix,iy,iz
 */
extern int mifi_get_values_no_extrapol_f(const float* infield, float* outfield, const double x, const double y, const int ix, const int iy, const int iz);

/**
 *  Bilinear interpolation requires a neighborhood extending one pixel to the right and below the central sample. If the fractional subsample position is given by (xfrac, yfrac), the resampled pixel value will be:
 *
 * @verbatim
     (1 - yfrac) * [(1 - xfrac)*s00 + xfrac*s01] +
     yfrac       * [(1 - xfrac)*s10 + xfrac*s11]
   @endverbatim
 *
 * This is documented by the following diagram:
 * @verbatim
                         s00    s01

                             .      < yfrac

                         s10    s11
                             ^
                            xfrac
   @endverbatim
 * @see http://java.sun.com/products/java-media/jai/forDevelopers/jai-apidocs/javax/media/jai/InterpolationBilinear.html
 * @warning if any of the 4 used values of infield is undefined or outside of infield, the return value will be undefined
 */
extern int mifi_get_values_bilinear_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

extern int mifi_get_values_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

/**
 * @brief not implemented yet
 *
 * The bicubic convolution algorithm assigns a value f(x,y) = X * M * F * Mt * Yt
 * with x, y between (0 <= x < 1), X = (1,x, x2, x3), Y = (1, y, y^2, y^3) and F a 4*4 matrix consisting
 * of the original values of f(-1,-1) to f(2,2).
 *
 * M is the convolution matrix with a = -0.5 as described by wikipedia (or Catmull-Rom for a = 1, not used here)
 *
 * Mt and Yt are the transposed matrices/vector.
 *
 * @see http://en.wikipedia.org/wiki/Bicubic_interpolation
 * @see http://java.sun.com/products/java-media/jai/forDevelopers/jai-apidocs/javax/media/jai/InterpolationBicubic.html
 */
extern int mifi_get_values_bicubic_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

/**
 * NearestNeighbor "interpolation"/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 * @return MIFI_OK return-value set for compatibility with mifi_get_values_log_f()
 */
extern int mifi_get_values_nearest_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);

/**
 * Linear interpolation/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 * with o(x) = in(a) + (x - a) * (in(b) - in(a)) / (b - a)
 * (that describes a linear function o(x) = m*x + c )
 *
 * This interpolation can be used for linear time-interpolation and uses full extrapolation.
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 * @return MIFI_OK return-value set for compatibility with mifi_get_values_log_f()
 */
extern int mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);
/**
 * Same as mifi_get_values_nearest_f() but with extrapolation limited to  a - (a-b), or b + (a-b).
 * @param infieldA
 * @param infieldB
 * @param outfield
 * @param n
 * @param a
 * @param b
 * @param x
 * @return
 */
extern int mifi_get_values_linear_weak_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);
/**
 * Same as mifi_get_values_nearest_f() but without any extrapolation
 * @param infieldA
 * @param infieldB
 * @param outfield
 * @param n
 * @param a
 * @param b
 * @param x
 * @return
 */
extern int mifi_get_values_linear_no_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);
/**
 * Same as mifi_get_values_nearest_f() but with constant extrapolation
 * @param infieldA
 * @param infieldB
 * @param outfield
 * @param n
 * @param a
 * @param b
 * @param x
 * @return
 */
extern int mifi_get_values_linear_const_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);
/**
 * This is the same as mifi_get_values_linear_f() for double input/output values.
 */
extern int mifi_get_values_linear_d(const double* infieldA, const double* infieldB, double* outfield, const size_t n, const double a, const double b, const double x);

/**
 * Logarithmic interpolation/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 * with o(x) = m*log(x) + c
 *
 * This interpolation can be used for i.e. log(p)-interpolation. It is tested against
 * results from ncl int2p and vintp2p_ecmwf log(p) interpolation.
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 * @return MIFI_OK on success, MIFI_ERROR if log of a, b or x undefined
 */
extern int mifi_get_values_log_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);
/**
 * Log-log interpolation/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 * that describes a function: o(x) = m*log(log(x)+c
 *
 * This interpolation can be used for i.e. log(log(p))-interpolation.
 * @warning It is tested against results from ncl vintp2p_ecmwf log(log(p)) interpolation, but results vary slightly (~1%) for
 * unknown reason.
 *
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 * @return MIFI_OK on success, MIFI_ERROR if log of a, b or x undefined
 */
extern int mifi_get_values_log_log_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);

extern int ascendingDoubleComparator(double a, double b);
extern int descendingDoubleComparator(double a, double b);
extern int bsearchDoubleIndex(const double key, const double* base, int num, int (*comparator)(double, double));

/**
 *  @brief find position in array of position in projection
 *
 * Points2position uses linear splines to find the array-position of points in the given axis.
 * Undefined (NaN/Inf) values will be translated to -999 to be far outside the position-range (0-(num-1)).
 *
 *
 *  @param points the values will get changed from points in axis coordinates to array coordinates
 *  @param n number of values in points
 *  @param axis coordinate axis
 *  @param num number of elements in coordinate axis
 *  @param axis_type type of axis, one of MIFI_LONGITUDE, MIFI_LATITUDE, MIFI_PROJ_AXIS
 */
extern int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type);

/**
 *
 * @brief Method to fill undefined values in a 2d field
 *
 * Solves Laplace's equation with Neumann boundary conditions
 * (dA/dn = 0) in rectangular coordinates by an iterative method to
 * fill-in reasonable values at gridpoints containing values with MIFI_UNDEFINED_F or NaNs
 *
 * Translated to C from Fortran code by H.Engedahl and A.Foss (1990-93).
 *
 * @param nx size of field in x-direction
 * @param ny size of field in x-direction
 * @param field the data-field to be filled (input/output)
 * @param relaxCrit relaxation criteria. Usually 4 orders of magnitude lower than data in field.
 * @param corrEff Coef. of overrelaxation, between +1.2 and +2.0
 * @param maxLoop Max. allowed no. of scans in relaxation procedure.
 * @param nChanged number of changed values (output)
 * @return error-code, usually MIFI_OK
 */
extern int mifi_fill2d_f(size_t nx, size_t ny, float* field, float relaxCrit, float corrEff, size_t maxLoop, size_t* nChanged);

/**
 * @brief Method to fill undefined values in a 2d field in stable time.
 *
 * This method will fill undefined values by interpolation of neighboring
 * defined values + the average. A value is assumed to be defined if it is defined in the input field,
 * or if it has been defined through the interpolation method (the defined fields will 'creep'
 * into the undefined area).
 *
 * The results are very similar to mifi_fill2d_f, but the time will vary only with
 * the size of the undefined area, not with the smoothness of the defined values.
 *
 * @param nx size of field in x-direction
 * @param ny size of field in x-direction
 * @param field the data-field to be filled (input/output)
 * @param repeat number of times values should be re-smoothed (depending on grid-size, 20-100 (linear with time used)).
 * @param setWeight default weight of original values (versus derived values with weight = 1). Must be >= 1, e.g. 2
 *        the higher the value, the smoother the approxamation from the undefined border to average.
 * @param nChanged number of changed values (output)
 * @return error-code, usually MIFI_OK
 */
extern int mifi_creepfill2d_f(size_t nx, size_t ny, float* field, unsigned short repeat, char setWeight, size_t* nChanged);

/**
 * @brief Method to fill undefined values in a 2d field in stable time.
 *
 * This method will fill undefined values by interpolation of neighboring
 * defined values + the defaultValue. A value is assumed to be defined if it is defined in the input field,
 * or if it has been defined through the interpolation method (the defined fields will 'creep'
 * into the undefined area).
 *
 * The results are very similar to mifi_fill2d_f, but the time will vary only with
 * the size of the undefined area, not with the smoothness of the defined values.
 *
 * @param nx size of field in x-direction
 * @param ny size of field in x-direction
 * @param field the data-field to be filled (input/output)
 * @param defaultVal the defaultValue to be used
 * @param repeat number of times values should be re-smoothed (depending on grid-size, 20-100 (linear with time used)).
 * @param setWeight default weight of original values (versus derived values with weight = 1). Must be >= 1, e.g. 2
 *        the higher the value, the smoother the approxamation from the undefined border to average.
 * @param nChanged number of changed values (output)
 * @return error-code, usually MIFI_OK
 */
extern int mifi_creepfillval2d_f(size_t nx, size_t ny, float* field, float defaultVal, unsigned short repeat, char setWeight, size_t* nChanged);


/**
 * Calculate the real distance in m between neigboring grid-cells (center to center). Distances
 * are calculated using the great-circle distance. The size of a gridcell is gridDistX*gridDistY.
 * @param nx points in x direction
 * @param ny points in y direction
 * @param lonVals longitude values in degree (j*ny + i)
 * @param latVals latitude values in degree (j*ny + i)
 * @param gridDistX output x-distance between neighbors in m (j*ny +i)
 * @param gridDistY output y-distance between neighbors in m (j*ny +i)
 * @return error-code or MIFI_OK
 */
int mifi_griddistance(size_t nx, size_t ny, const double* lonVals, const double* latVals, float* gridDistX, float* gridDistY);

/**
 * Compute vertical velocity from continuity equation, integrating over all model-levels. Derived from compw.f: J.E. Haugen, (C) 1995 DNMI
 *
 * @param nx size of grid in x direction
 * @param ny size of grid in y direction
 * @param nz size of levels
 * @param dx x-grid-distance in projection-plane (m) (use radian*R for spherical coordinates)
 * @param dy y-grid-distance in projection-plan (m) (use radian*R for spherical coordinates)
 * @param gridDistX distance in m on surface between two grid points in x-direction (nx*ny)
 * @param gridDistY distance in m on surface between two grid points in y-direction (nx*ny)
 * @param ap ap parameter of full hybrid-sigma coordinates in Pa
 * @param b b parameter of full hybrid-sigma coordinates (dimensionless)
 * @param zs surface-geopotential in m (nx*ny)
 * @param ps surface-pressure in Pa (nx*ny)
 * @param u x-velocity in m/s (nx*ny*nz)
 * @param v y-velocity in m/s (nx*ny*nz)
 * @param t abs. temperature in K (nx*ny*nz)
 * @param w output, vertical velocity in m/s, must be preallocated (nx*ny*nz)
 * @return MIFI_OK/MIFI_ERROR
 */
size_t mifi_compute_vertical_velocity(size_t nx, size_t ny, size_t nz, double dx, double dy, const float* gridDistX, const float* gridDistY, const double* ap, const double* b,
                          const float* zs, const float* ps, const float* u, const float* v, const float* t,
                          float* w);


/**
 * Convert bad-values to nan. The mifi_ functions don't handle bad values generally, but
 * forward this work to the floating-point IEEE NaN's. This function converts a general bad value
 * to a nan in a float array.
 *
 * @param posPtr start pointer of the float array
 * @param endPtr end-pointer of the float array (excluded from conversion)
 * @param badVal bad value to be converted to nan
 * @return 0 (in fimex < 0.61, this was possibly the number of conversions, but it was never strict)
 */
extern size_t mifi_bad2nanf(float* posPtr, float* endPtr, float badVal);
/**
 * Convert nan back to bad-values. See #mifi_bad2nanf
 *
 * @param posPtr start pointer of the float array
 * @param endPtr end-pointer of the float array (excluded from conversion)
 * @param badVal value NaNs will be  converted to
 * @return 0 (in fimex < 0.61, this was possibly the number of conversions, but it was never strict)
 */
extern size_t mifi_nanf2bad(float* posPtr, float* endPtr, float badVal);

extern size_t mifi_nand2bad(double* posPtr, double* endPtr, double badVal);

#ifdef __cplusplus
}
#endif

#endif /*INTERPOLATION_H_*/
