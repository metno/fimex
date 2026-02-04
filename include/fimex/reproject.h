/*
 * Fimex
 *
 * (C) Copyright 2022-2026, met.no
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

#ifndef FIMEX_REPROJECT_H_
#define FIMEX_REPROJECT_H_

#include "fimex/SharedArray.h"

#include <string>

namespace MetNoFimex {
namespace reproject {

struct Matrix
{
    enum { stride = 3 };

    Matrix(int sizex, int sizey);

    const double* mtx() const { return matrix.get(); }
    double* mtx() { return matrix.get(); }

    int size_x;
    int size_y;
    MetNoFimex::shared_array<double> matrix;
};
typedef std::shared_ptr<const Matrix> Matrix_cp;

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
 * @throws CDMException in case of any error
 */
void reproject_f(int method, const std::string& proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis, const int in_x_axis_type,
                 const int in_y_axis_type, const int ix, const int iy, const int iz, const std::string& proj_output, float* outfield, const double* out_x_axis,
                 const double* out_y_axis, const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy);

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
 * and the same formulas for (x+delta, y) and (x, y+delta) (with delta a small value against the x or y)
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
 * @throws CDMException in case of any error
 */
void vector_reproject_values_f(const std::string& proj_input, const std::string& proj_output, float* u_out, float* v_out, const double* out_x_axis,
                               const double* out_y_axis, int out_x_axis_type, int out_y_axis_type, int ox, int oy, int oz);

/**
 * converts a point on earth to a projection plane
 * @param projStr projection definition for proj4
 * @param lon_x longitude in degree on input, x projection coordinate on output
 * @param lat latitude in degree on input, y projection coordinate on output
 * @throws CDMException if the projection is invalid or the point outside the area
 */
void reproject_point_from_lonlat(const std::string& proj_output, double* lon_x, double* lat_y);

/**
 * calculate the reprojected vectors with a known matrix for #mifi_vector_reproject_values_f
 * @param matrix reprojection matrix of size (3,ox,oy)
 * @param u_out values of u, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param v_out values of v, with position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param oz z-dimension of the outfield
 */
void vector_reproject_values_by_matrix_f(Matrix_cp matrix, float* u_out, float* v_out, int oz);

/**
 * Calculate the reprojected directions with a known matrix. Directions are the angle between the projections y-Axis (0degree) clockwise
 * to 360degree.
 * @param matrix reprojection matrix of size (3,ox,oy)
 * @param angle_out angles in degrees at position in the output-projection (i.e. by prevously applying mifi_interpolate_f). The values here will be changed!
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param oz z-dimension of the outfield
 */
void vector_reproject_direction_by_matrix_f(Matrix_cp matrix, float* angle_out, int oz);

/**
 * calculate the vector reprojection matrix used in #mifi_vector_reproject_values_f
 *
 * @param proj_input proj4-string of projection of infield
 * @param proj_output proj4-string of projection of outfield
 * @param out_x_axis field of size ox. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_y_axis field of size oy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_x_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param out_y_axis_type one of MIFI_LATITUDE, MIFI_LONGITUDE, MIFI_PROJ_AXIS
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 * @param matrix matrix of size (3*ox*oy)
 * @throws CDMException in case of any error
 */
Matrix_cp get_vector_reproject_matrix(const std::string& proj_input, const std::string& proj_output, const double* out_x_axis, const double* out_y_axis,
                                      int out_x_axis_type, int out_y_axis_type, int ox, int oy);

/**
 * calculate the vector reprojection matrix used in #mifi_vector_reproject_values_f without changing the axes, just the
 * direction of the vectors.
 *
 * @param proj_input proj4-string of projection of infield
 * @param proj_output proj4-string of projection of outfield
 * @param in_x_field field of size ox*oy with the values in the input-projection in_x_field[x+oy*y] = inXAxis[x]. The values must be in degree or m.
 * @param in_y_field field of size ox*oy with the values in the input-projection in_y_field[x+oy*y] = inYAxis[y]. The values must be in degree or m.
 * @param ox x-dimension of in/outfield
 * @param oy y-dimension of in/outfield
 * @param matrix matrix of size (3*ox*oy)
 * @throws CDMException in case of any error
 */
Matrix_cp get_vector_reproject_matrix_field(const std::string& proj_input, const std::string& proj_output, const double* in_x_field,
                                            const double* in_y_field, // both ox*oy
                                            int ox, int oy);

/**
 * calculate the vector reprojection matrix when projecting to a list of n-points
 *
 * @param proj_input proj4-string of projection of infield
 * @param proj_output proj4-string of projection of outfield
 * @param inputIsMetric 1 if projection, 0 if (rotated) latlon
 * @param out_x_points output-points values in the output-projection, size ox * oy. The values must be in degree or m.
 * @param out_y_points output-points values in the output-projection, size ox * oy. The values must be in degree or m.
 * @param ox number of x output-points
 * @param oy number of y output-points
 * @throws CDMException in case of any error
 */
Matrix_cp get_vector_reproject_matrix_points(const std::string& proj_input, const std::string& proj_output, int inputIsMetric, const double* out_x_points,
                                             const double* out_y_points, // both size on, must be in m or degree
                                             int ox, int oy);

/**
 * @brief project values so that the projetion (x,y) => (x_proj), (y_proj) can be expressed as x_proj(x,y), y_proj(x,y)
 *
 * all values must be given or will be returned in degrees for angular units
 *
 * @param proj_input input projection proj string
 * @param proj_output output projection proj string
 * @param in_out_x_vals x-values, will be input and output
 * @param in_out_y_vals y-values, will be input and output
 * @param num size of arrays
 * @throws CDMException in case of any error
 */
void reproject_values(const std::string& proj_input, const std::string& proj_output, double* in_out_x_vals, double* in_out_y_vals, const int num);

/**
 * @brief project axes so that the projetion (x,y) => (x_proj), (y_proj) can be expressed as x_proj(x,y), y_proj(x,y)
 *
 * all axes must be given or will be returned in degrees for angular units
 *
 * @param proj_input input projection proj string
 * @param proj_output output projection proj string
 * @param in_x_axis x-axis in input-projection
 * @param in_y_axis y-axis in input-projection
 * @param ix size of x-axis
 * @param iy size of y-axis
 * @param out_xproj_axis output-values of x_proj(x,y), field needs to be allocated in at least ix*iy size
 * @param out_yproj_axis output-values of y_proj(x,y), field needs to be allocated in at least ix*iy size
 * @throws CDMException in case of any error
 */
void reproject_axes(const std::string& proj_input, const std::string& proj_output, const double* in_x_axis, const double* in_y_axis, const int ix, const int iy,
                    double* out_xproj_axis, double* out_yproj_axis);

} // namespace reproject
} // namespace MetNoFimex

#endif /*INTERPOLATION_H_*/
