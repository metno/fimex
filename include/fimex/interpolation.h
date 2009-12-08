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

#ifndef INTERPOLATION_H_
#define INTERPOLATION_H_

#include <proj_api.h>
#include <math.h>
// M_PI no longer in c99
#define PI 3.1415926535897932384626433832795

#ifdef __cplusplus
extern "C" {
#endif


/**
 * @brief interpolation method
 *
 * flag for nearest neighbor interpolation
 */
#define MIFI_INTERPOL_NEAREST_NEIGHBOR 0
/**
 * @brief interpolation method
 *
 * flag for bilinear interpolation
 */
#define MIFI_INTERPOL_BILINEAR         1
/**
 * @brief interpolation method
 *
 * flag for bicubic interpolation
 */
#define MIFI_INTERPOL_BICUBIC          2
/**
 * @brief interpolation method
 *
 * Flag for nearest neighbor interpolation
 * using lon/lat coordinates rather than the input
 * projection. This is largely a brute force method
 * which may take long time.
 *
 * Vector projection is not implemented (not defined?)
 *
 * @warning this works only from CDMInterpolator
 */
#define MIFI_INTERPOL_COORD_NN         3
/**
 * @brief interpolation method
 *
 * Flag for nearest neighbor interpolation
 * using coordinates with KD-tree. This works
 * as nearest neighbor in the output-projection
 * and has therefore numerical problems in some points,
 * i.e. near southpole when using northpole-polarstereographic.
 *
 * It doesn't work with output projections in degree, i.e.
 * rotated latitude longitude.
 *
 * Vector projection is not implemented (not defined?)
 *
 * @warning this works only from CDMInterpolator
 *
 */
#define MIFI_INTERPOL_COORD_NN_KD      4

/**
 * @brief interpolation method
 *
 * forward interpolation, summing over all matching input-cells
 */
#define MIFI_INTERPOL_FORWARD_SUM      5

/**
  * @brief interpolation method
  *
  * forward interpolation, averaging (mean) over all matching defined input-cells
  */
#define MIFI_INTERPOL_FORWARD_MEAN      6

/**
  * @brief interpolation method
  *
  * forward interpolation, median over all matching defined input-cells
  */
#define MIFI_INTERPOL_FORWARD_MEDIAN      7


/**
  * @brief interpolation method
  *
  * forward interpolation, maximum over all matching defined input-cells
  */
#define MIFI_INTERPOL_FORWARD_MAX      8

/**
  * @brief interpolation method
  *
  * forward interpolation, minimum over all matching defined input-cells
  */
#define MIFI_INTERPOL_FORWARD_MIN      9

/**
 * @brief vector projection flag
 *
 * new size will be like old size
 */
#define MIFI_VECTOR_KEEP_SIZE 0
/**
 * @brief vector projection flag
 *
 * vector might change size with projection
 */
#define MIFI_VECTOR_RESIZE    1


/** @brief undefined value for floats */
#define MIFI_UNDEFINED_F (nanf(""))
/** @brief undefined value for doubles */
#define MIFI_UNDEFINED_D (nan(""))

/** @brief return code, error */
#define MIFI_ERROR -1
/** @brief return code, ok */
#define MIFI_OK 1

/** @brief projection axis in m-equivalent */
#define MIFI_PROJ_AXIS 0
/** @brief longitude projection axis in degrees */
#define MIFI_LONGITUDE 1
/** @brief latitude projection axis in degrees */
#define MIFI_LATITUDE 2

/** @brief debug flag */
#define MIFI_DEBUG 0

/**
 * Interpolation between two projections. Missing values are set to MIFI_UNDEFINED_F
 * which is implemented as C99 nanf. The coordinates of a cell give the midpoint of a cell,
 * i.e. cell (10,20) spans  ([9.5..10.5[,[19.5-20.5[)
 *
 * @param method one of MIFI_INTERPOL_NEAREST_NEIGHBOR MIFI_INTERPOL_BILINEAR MIFI_INTERPOL_BICUBIC
 * @param proj_input proj4-string of projection of infield
 * @param infield real rectangular array of dimension infield[iz,iy,ix]
 * @param in_x_axis field of size ix. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param in_y_axis field of size iy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param in_x_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param in_y_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param ix x-dimension of infield
 * @param iy y-dimension of infield
 * @param iz z-dimension of infield and outfield. The z-dim allows you to convert several fields at once without calculating the projection again and again.
 * @param proj_output proj4-string of projection of outfield
 * @param outfield real rectangular array of dimension outfield[iz,oy,ox]
 * @param out_x_axis field of size ox. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_y_axis field of size oy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_x_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param out_y_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 */
extern int mifi_interpolate_f(int method,
                        const char* proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis,
                        const int in_x_axis_type, const int in_y_axis_type, const int ix, const int iy, const int iz,
                        const char* proj_output, float* outfield, const double* out_x_axis, const double* out_y_axis,
                        const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy);

/**
 * @brief not implemented yet
 *
 * double version of mifi_interpolate_f
 * @see mifi_interpolate_f
 */
extern int mifi_interpolate_d(int method,
                        char* proj_input, double* infield, double* in_x_axis, double* in_y_axis,
                        int in_x_axis_type, int in_y_axis_type, int ix, int iy, int iz,
                        char* proj_output, double* outfield, double* out_x_axis, double* out_y_axis,
                        int out_x_axis_type, int out_y_axis_type, int ox, int oy);

/**
 * @brief interpolate the vector values
 *
 * When reprojecting a vector (i.e. wind (u, v)) from one projection to another,
 * not only the base-position of the vector will change, but also the
 * angle of the vector might change due to rotation and streching within the
 * projection. Thus, the values of (u,v) have to be changed accordingly to
 * projection.
 *
 * This function allows to only rotate the vector values (MIFI_VECTOR_KEEP_SIZE)
 * which is useful to keep the windspeed constant, even if the projected plane
 * has a different scale, or to completely reproject the vector (MIFI_VECTOR_RESIZE).
 *
 * This function is implemented by using a first order tailor expansion of the
 * projection: (u', v') = A (u,v) with A a matrix defined at each point (x,y) through
 * @code
 * proj(x,y)_x' = a11*x+a21*y
 * proj(x,y)_y' = a12*x+a22*y
 * @endcode
 * and the same formulars for (x+delta, y) and (x, y+delta) (with delta a small value against the x or y)
 *
 * @param method (one of MIFI_VECTOR_KEEP_SIZE, MIFI_VECTOR_RESIZE)
 * @param proj_input proj4-string of projection of infield
 * @param proj_output proj4-string of projection of outfield
 * @param u_out values of u, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param v_out values of v, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param out_x_axis field of size ox. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_y_axis field of size oy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_x_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param out_y_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param oz z-dimension of the outfield
 * @return MIFI_OK or error value
 */
extern int mifi_vector_reproject_values_f(int method,
						const char* proj_input,
						const char* proj_output,
						float* u_out, float* v_out,
						const double* out_x_axis, const double* out_y_axis,
						int out_x_axis_type, int out_y_axis_type,
						int ox, int oy, int oz);
/**
 * calculate the reprojected vectors with a known matrix for #mifi_vector_reproject_values_f
 * @param method (one of MIFI_VECTOR_KEEP_SIZE, MIFI_VECTOR_RESIZE)
 * @param matrix reprojection matrix of size (4,ox,oy)
 * @param u_out values of u, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param v_out values of v, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param oz z-dimension of the outfield
 * @return MIFI_OK or error value
 */
extern int mifi_vector_reproject_values_by_matrix_f(int method,
						const double* matrix,
						float* u_out, float* v_out,
						int ox, int oy, int oz);

/**
 * calculate the vector reprojection matrix used in #mifi_vector_reproject_values_f
 *
 * @param method (one of MIFI_VECTOR_KEEP_SIZE, MIFI_VECTOR_RESIZE)
 * @param proj_input proj4-string of projection of infield
 * @param proj_output proj4-string of projection of outfield
 * @param out_x_axis field of size ox. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_y_axis field of size oy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_x_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param out_y_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param matrix matrix of size (4*ox*oy)
 * @return MIFI_OK or error value
 */
extern int mifi_get_vector_reproject_matrix(const char* proj_input,
						const char* proj_output,
						const double* out_x_axis, const double* out_y_axis,
						int out_x_axis_type, int out_y_axis_type,
						int ox, int oy,
						double* matrix);


/**
 * Get the nearest neighbor of a value. Values are rounded to array-position.
 *
 * @param infield 3d fortran array of size ix,iy,iz
 * @param outfield 1d array of size iz containing the values
 */
extern int mifi_get_values_f(const float* infield, float* outfield, const double x, const double y, const int ix, const int iy, const int iz);

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
 * Linear interpolation/extrapolation of values in the arrays infieldA and infieldB at position a and b to a field at outfield at position x
 * with o(x) = in(a) + (x - a) * (in(b) - in(a)) / (b - a)
 *
 * This interpolation can be used for linear time-interpolation.
 *
 * @param infieldA array of size n with values of input at position a
 * @param infieldB array of size n with values of input at position b
 * @param outfield array of size n with values of input at position x, output
 * @param n size of arrays
 * @param a position of infieldA
 * @param b position of infieldB
 * @param x position of outfield
 */
extern void mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x);


/**
 *  @brief find position in array of position in projection
 *
 * points2position uses linear splines to find the array-position of points in the given axis
 *
 *  @param points the values will get changed from points in axis coordinates to array coordinates
 *  @param n number of values in points
 *  @param axis coordinate axis
 *  @param num number of elements in coordinate axis
 *  @param axis_type type of axis, one of MIFI_LONGITUDE, MIFI_LATITUDE, MIFI_PROJ_AXIS
 */
extern int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type);


/**
 * gives the position of an fortran like array of size ix, iy, iz
 *
 *  @return the position of x, y, z
 */
extern int mifi_3d_array_position(int x, int y, int z, int ix, int iy, int iz);


/**
 * @brief project values so that the projetion (x,y) => (x_proj), (y_proj) can be expressed as x_proj(x,y), y_proj(x,y)
 *
 * all values must be given or will be returned in radians when converted from/to latlon
 *
 * @param proj_input input projection proj string
 * @param proj_output output projection proj string
 * @param in_out_x_vals x-values, will be input and output
 * @param in_out_y_vals y-values, will be input and output
 * @param num size of arrays
 * @return error-code
 *
 */
int mifi_project_values(const char* proj_input, const char* proj_output, double* in_out_x_vals, double* in_out_y_vals, const int num);

/**
 * @brief project axes so that the projetion (x,y) => (x_proj), (y_proj) can be expressed as x_proj(x,y), y_proj(x,y)
 *
 * all axes must be given or will be returned in radians when converted from/to latlon
 *
 * @param proj_input input projection proj string
 * @param proj_output output projection proj string
 * @param in_x_axis x-axis in input-projection
 * @param in_y_axis y-axis in input-projection
 * @param ix size of x-axis
 * @param iy size of y-axis
 * @param out_xproj_axis output-values of x_proj(x,y), field needs to be allocated in at least ix*iy size
 * @param out_yproj_axis output-values of y_proj(x,y), field needs to be allocated in at least ix*iy size
 * @return error-code
 *
 */
extern int mifi_project_axes(const char* proj_input, const char* proj_output, const double* in_x_axis, const double* in_y_axis, const int ix, const int iy, double* out_xproj_axis, double* out_yproj_axis);

/**
 * Convert bad-values to nan. The mifi_ functions don't handle bad values generally, but
 * forward this work to the floating-point IEEE NaN's. This function converts a general bad value
 * to a nan in a float array.
 *
 * @param posPtr start pointer of the float array
 * @param endPtr end-pointer of the float array (excluded from conversion)
 * @param badVal bad value to be converted to nan
 * @return number of conversions
 */
extern size_t mifi_bad2nanf(float* posPtr, float* endPtr, float badVal);
/**
 * Convert nan back to bad-values. See #mifi_bad2nanf

 * @param posPtr start pointer of the float array
 * @param endPtr end-pointer of the float array (excluded from conversion)
 * @param badVal value NaNs will be  converted to
 * @return number of conversions
 */
extern size_t mifi_nanf2bad(float* posPtr, float* endPtr, float badVal);

#ifdef __cplusplus
}
#endif


#endif /*INTERPOLATION_H_*/
