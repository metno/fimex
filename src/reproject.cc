/*
 * Fimex
 *
 * (C) Copyright 2022, met.no
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

#include "reproject.h"

#include "fimex/CDMException.h"
#include "fimex/CDMconstants.h"
#include "fimex/Logger.h"
#include "fimex/interpolation.h"

#include <algorithm>
#include <memory>
#include <sstream>

#include "fimex_config.h"
#ifdef HAVE_PROJ_H
#include "proj.h"
#else
#define ACCEPT_USE_OF_DEPRECATED_PROJ_API_H 1
#include "proj_api.h"
#if !defined(PJ_VERSION) || PJ_VERSION < 460 || (PJ_VERSION >= 500 && PJ_VERSION < 620)
#error "proj version known not to work with fimex"
#endif
#endif

namespace MetNoFimex {
namespace reproject {
namespace {

Logger_p logger = getLogger("fimex.reproject");

#ifdef HAVE_PROJ_H
#define DEG_TO_RAD (M_PI / 180)
#define RAD_TO_DEG (180 / M_PI)

PJ_CONTEXT* crs2crs_ctx()
{
    static PJ_CONTEXT* pj_ctx = 0;
    if (!pj_ctx) {
        pj_ctx = proj_context_create();
    }
    return pj_ctx;
}

void throw_proj_error(int pe)
{
    std::ostringstream out;
    out << "Proj error:" << pe << " " << proj_errno_string(pe);
    throw CDMException(out.str());
}

typedef std::shared_ptr<PJ> PJ_p;

PJ_p make_PJ(const std::string& src, const std::string& dst)
{
    PJ_p pj(proj_create_crs_to_crs(crs2crs_ctx(), src.c_str(), dst.c_str(), 0), proj_destroy);
    int pe = proj_context_errno(crs2crs_ctx());
    if (!pj || pe != 0)
        throw_proj_error(pe);
    return pj;
}

class crs2crs
{
public:
    crs2crs(const std::string& proj_input, const std::string& proj_output);

    bool src_is_latlon() const { return src_is_latlon_; }
    bool dst_is_latlon() const { return dst_is_latlon_; }

    void fwd(size_t count, double* x, double* y) { transform(PJ_FWD, count, x, y); }
    void inv(size_t count, double* x, double* y) { transform(PJ_INV, count, x, y); }

private:
    void transform(PJ_DIRECTION direction, size_t count, double* x, double* y);

private:
    PJ_p pj_;
    bool src_is_latlon_;
    bool dst_is_latlon_;
};

crs2crs::crs2crs(const std::string& proj_input, const std::string& proj_output)
    : pj_(make_PJ(proj_input, proj_output))
    , src_is_latlon_(proj_degree_input(pj_.get(), PJ_FWD))
    , dst_is_latlon_(proj_degree_output(pj_.get(), PJ_FWD))
{
}

void crs2crs::transform(PJ_DIRECTION direction, size_t count, double* x, double* y)
{
    const size_t stride = sizeof(double);
    const size_t completed = proj_trans_generic(pj_.get(), direction, x, stride, count, y, stride, count, 0, 0, 0, 0, 0, 0);
    if (completed != count)
        throw_proj_error(proj_errno(pj_.get()));
}

#else // !HAVE_PROJ_H

void throw_proj_error()
{
    if (pj_errno != 0) {
        std::ostringstream out;
        out << "Proj error:" << pj_errno << " " << pj_strerrno(pj_errno);
        throw MetNoFimex::CDMException(out.str());
    }
}

typedef void PJ;
typedef std::shared_ptr<PJ> projPJ_p;

projPJ_p make_PJ(const std::string& proj)
{
    projPJ_p pj = std::shared_ptr<PJ>(pj_init_plus(proj.c_str()), pj_free);
    if (!pj)
        throw_proj_error();
    return pj;
}

bool proj_is_angular(projPJ_p pj, const std::string& ps)
{
    if (pj_is_latlong(pj.get()))
        return true;
    if (ps.find("+proj=ob_tran") != std::string::npos)
        return true;
    return false;
}

void scale_x_y(size_t count, double* x, double* y, double factor)
{
    for (size_t i = 0; i < count; ++i, ++x, ++y) {
        *x *= factor;
        *y *= factor;
    }
}

class crs2crs
{
public:
    crs2crs(const std::string& proj_input, const std::string& proj_output);

    bool src_is_latlon() const { return src_is_latlon_; }
    bool dst_is_latlon() const { return dst_is_latlon_; }

    void fwd(size_t count, double* x, double* y);
    void inv(size_t count, double* x, double* y);

private:
    void transform(projPJ_p src, projPJ_p dst, size_t count, double* x, double* y);

private:
    projPJ_p src_;
    projPJ_p dst_;
    bool src_is_angular_;
    bool dst_is_angular_;
    bool src_is_latlon_;
    bool dst_is_latlon_;
};

crs2crs::crs2crs(const std::string& proj_input, const std::string& proj_output)
    : src_(make_PJ(proj_input))
    , dst_(make_PJ(proj_output))
    , src_is_angular_(proj_is_angular(src_, proj_input))
    , dst_is_angular_(proj_is_angular(dst_, proj_output))
    , src_is_latlon_(pj_is_latlong(src_.get()))
    , dst_is_latlon_(pj_is_latlong(dst_.get()))
{
}

void crs2crs::transform(projPJ_p src, projPJ_p dst, size_t count, double* x, double* y)
{
    std::unique_ptr<double[]> z(new double[count]); // z currently of no interest, no height attached to values
    std::fill(&z[0], &z[count], 0);
    if (pj_transform(src.get(), dst.get(), count, 0, x, y, z.get()) != 0)
        throw_proj_error();
}

void crs2crs::fwd(size_t count, double* x, double* y)
{
    if (src_is_angular_)
        scale_x_y(count, x, y, DEG_TO_RAD);
    transform(src_, dst_, count, x, y);
    if (dst_is_angular_)
        scale_x_y(count, x, y, RAD_TO_DEG);
}

void crs2crs::inv(size_t count, double* x, double* y)
{
    if (dst_is_angular_)
        scale_x_y(count, x, y, DEG_TO_RAD);
    transform(dst_, src_, count, x, y);
    if (src_is_angular_)
        scale_x_y(count, x, y, RAD_TO_DEG);
}

#endif

inline int mifi_3d_array_pos(int x, int y, int z, int ix, int iy, int iz)
{
    (void)iz; // suppress compiler warning
    return (z * iy + y) * ix + x;
}

int mifi_3d_array_position(int x, int y, int z, int ix, int iy, int iz)
{
    return mifi_3d_array_pos(x, y, z, ix, iy, iz);
}

/*
 * great-circle angle: http://en.wikipedia.org/wiki/Great-circle_distance
 * multiply by R to get distance
 * param phi1, phi2 input longitude in radian
 * param lambda1, lambda2 input latitudes in radian
 */
inline double mifi_great_circle_angle(double lat0, double lon0, double lat1, double lon1)
{
    return acos(sin(lat0) * sin(lat1) + cos(lat0) * cos(lat1) * cos(lon1 - lon0));
}

/*! calculate bearing in radian; all parameters in radian */
inline double mifi_bearing_rad(double lat0, double lon0, double lat1, double lon1)
{
    // use spherical distances
    // (lon1y, lat1y)
    //     ^
    //     |
    //     |
    // (lon0,lat0) ----> (lon1x,lat1x)
    double dlon = lon0 - lon1;
    double sin_dLon = sin(dlon);
    double cos_dLon = cos(dlon);
    double sin_lat0 = sin(lat0);
    double cos_lat0 = cos(lat0);
    double sin_lat1 = sin(lat1);
    double cos_lat1 = cos(lat1);
    double phi = atan2(sin_dLon * cos_lat1, cos_lat0 * sin_lat1 - sin_lat0 * cos_lat1 * cos_dLon);
    return phi;
}

/*! calculate bearing in radian; all parameters in degrees */
inline double mifi_bearing(double lat0, double lon0, double lat1, double lon1)
{
    return mifi_bearing_rad(lat0 * DEG_TO_RAD, lon0 * DEG_TO_RAD, lat1 * DEG_TO_RAD, lon1 * DEG_TO_RAD);
}

void interpolate_f_functional(int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz),
                              const std::string& proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis, const int in_x_axis_type,
                              const int in_y_axis_type, const int ix, const int iy, const int iz, const std::string& proj_output, float* outfield,
                              const double* out_x_axis, const double* out_y_axis, const int out_x_axis_type, const int out_y_axis_type, const int ox,
                              const int oy)
{
    /*
     * transforming from output to input, to receive later the correct input-values
     * for the output coordinates
     */
    std::unique_ptr<double[]> pointsX(new double[ox * oy]);
    std::unique_ptr<double[]> pointsY(new double[ox * oy]);
    reproject_axes(proj_output, proj_input, out_x_axis, out_y_axis, ox, oy, pointsX.get(), pointsY.get());

    mifi_points2position(pointsX.get(), ox * oy, in_x_axis, ix, in_x_axis_type);
    mifi_points2position(pointsY.get(), ox * oy, in_y_axis, iy, in_y_axis_type);

    // LOG4FIMEX(logger, Logger::DEBUG, "projection: (" << out_x_axis[0] << "," << out_y_axis[0] << ") <- (" << pointsX[0] << "," << pointsY[0]);

    float zValues[iz];
    for (int y = 0; y < oy; ++y) {
        for (int x = 0; x < ox; ++x) {
            if (func(infield, zValues, pointsX[y * ox + x], pointsY[y * ox + x], ix, iy, iz) != MIFI_ERROR) {
                for (int z = 0; z < iz; ++z) {
                    outfield[mifi_3d_array_pos(x, y, z, ox, oy, iz)] = zValues[z];
                }
            }
        }
    }
}

inline void store_rotation(double* mtx, size_t idx, double phi0)
{
    mtx[idx + 0] = cos(phi0);
    mtx[idx + 1] = sin(phi0);
    mtx[idx + 2] = RAD_TO_DEG * phi0;
}

/** useed by get_vector_reproject_matrix_*
 * @param in_x_field size ox*oy, units degrees or meters
 * @param in_y_field size ox*oy, units degrees or meters
 * @param out_x_field size ox*oy, units degrees or meters
 * @param out_y_field size ox*oy, units degrees or meters
 */
Matrix_cp get_vector_reproject_matrix_points_proj_delta(crs2crs& c2c, const double* in_x_field, const double* in_y_field, const double* out_x_field,
                                                        const double* out_y_field, double deltaX, double deltaY, int ox, int oy)
{
    // calculation of deltas: (x+d, y), (x, y+d) -> proj-values
    const size_t count = ox * oy;
    std::unique_ptr<double[]> out_x_delta_proj_axis(new double[count]);
    std::unique_ptr<double[]> out_y_delta_proj_axis(new double[count]);

    std::shared_ptr<Matrix> matrix = std::make_shared<Matrix>(ox, oy);
    double* mtx = matrix->mtx();
    if (!c2c.dst_is_latlon()) {
        // step along x axis; not used for lat-lon (lat-lon uses only true north rotation)
        for (int i = 0; i < count; ++i) {
            out_x_delta_proj_axis[i] = in_x_field[i] + deltaX;
            out_y_delta_proj_axis[i] = in_y_field[i];
        }
        c2c.fwd(count, out_x_delta_proj_axis.get(), out_y_delta_proj_axis.get());
        const double add_x = deltaX > 0 ? 0 : MIFI_PI;
        for (size_t i = 0, idx = 0; i < count; ++i, idx += Matrix::stride) {
            const double phi = atan2(out_y_delta_proj_axis[i] - out_y_field[i], out_x_delta_proj_axis[i] - out_x_field[i]) + add_x;
            mtx[idx + 0] = phi;
        }
    }

    // step along y axis
    for (int i = 0; i < count; ++i) {
        out_x_delta_proj_axis[i] = in_x_field[i];
        out_y_delta_proj_axis[i] = in_y_field[i] + deltaY;
    }
    c2c.fwd(count, out_x_delta_proj_axis.get(), out_y_delta_proj_axis.get());
    const double add_y = deltaY > 0 ? 0 : MIFI_PI;
    if (c2c.dst_is_latlon()) {
        // this does not handle rotated lat-lon - good!
        for (size_t i = 0, idx = 0; i < count; ++i, idx += Matrix::stride) {
            const double phi0 = mifi_bearing(out_y_field[i], out_x_field[i], out_y_delta_proj_axis[i], out_x_delta_proj_axis[i]) + add_y;
            store_rotation(mtx, idx, phi0);
        }
    } else {
        for (size_t i = 0, idx = 0; i < count; ++i, idx += Matrix::stride) {
            const double phi_x = mtx[idx + 0];
            const double phi_y = -atan2(out_x_delta_proj_axis[i] - out_x_field[i], out_y_delta_proj_axis[i] - out_y_field[i]) + add_y;
            // average, for non-conformal cases
            const double phi0 = (phi_y + phi_x) / 2;
            store_rotation(mtx, idx, phi0);
        }
    }
    return matrix;
}

/** common implementation of get_vector_reproject_matrix_*
 * @param in_x_field size ox*oy, units degrees or meters
 * @param in_y_field size ox*oy, units degrees or meters
 */
Matrix_cp get_vector_reproject_matrix_proj(crs2crs& c2c, const double* in_x_field, const double* in_y_field, const double* out_x_field,
                                           const double* out_y_field, int ox, int oy)
{
    // calculation of deltas: (x+d, y), (x, y+d) -> proj-values
    // delta usually .1% of distance between neighboring cells
    const double defaultDelta = 1e-3;
    double deltaXY;
    {
        if (ox > 1) {
            if (oy > 1) {
                deltaXY = defaultDelta * (in_x_field[(1) * ox + (1)] - in_x_field[0]);
                // and test another place in case of singularities
                size_t ox_2 = ox / 2;
                size_t oy_2 = oy / 2;
                double deltaX2 = defaultDelta * (in_x_field[(oy_2 + 1) * ox + (ox_2 + 1)] - in_x_field[(oy_2)*ox + ox_2]);
                deltaXY += deltaX2;
                deltaXY /= 2;
            } else {
                deltaXY = defaultDelta * (in_x_field[(0) * ox + (1)] - in_x_field[0]);
            }
        } else {
            if (oy > 1) {
                deltaXY = defaultDelta * (in_x_field[(1) * ox + (0)] - in_x_field[0]);
            } else {
                // no neighbors, e.g. rotation to single point
                deltaXY = (in_x_field[0] > 1) ? (in_x_field[0] * defaultDelta) : defaultDelta;
            }
        }
    }
    if (fabs(deltaXY) < 1e-9) {
        LOG4FIMEX(logger, Logger::WARN, "tiny deltaXY: " << deltaXY << " possible singularity in vector-reprojection. Using default: " << defaultDelta);
        deltaXY = defaultDelta;
    }
    return get_vector_reproject_matrix_points_proj_delta(c2c, in_x_field, in_y_field, out_x_field, out_y_field, deltaXY, deltaXY, ox, oy);
}

} // namespace

Matrix::Matrix(int sizex, int sizey)
    : size_x(sizex)
    , size_y(sizey)
    , matrix(new double[size_x * size_y * stride])
{
}

void reproject_f(const int method, const std::string& proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis,
                 const int in_x_axis_type, const int in_y_axis_type, const int ix, const int iy, const int iz, const std::string& proj_output, float* outfield,
                 const double* out_x_axis, const double* out_y_axis, const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy)
{
    int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

    switch (method) {
    case MIFI_INTERPOL_NEAREST_NEIGHBOR:
        func = mifi_get_values_f;
        break;
    case MIFI_INTERPOL_BILINEAR:
        func = mifi_get_values_bilinear_f;
        break;
    case MIFI_INTERPOL_BICUBIC:
        func = mifi_get_values_bicubic_f;
        break;
    default:
        throw CDMException("unknown interpolation method");
    }

    interpolate_f_functional(func, proj_input, infield, in_x_axis, in_y_axis, in_x_axis_type, in_y_axis_type, ix, iy, iz, proj_output, outfield, out_x_axis,
                             out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy);
}

Matrix_cp get_vector_reproject_matrix_points(const std::string& proj_input, const std::string& proj_output, int inputIsMetric, const double* out_x_points,
                                             const double* out_y_points, // both size ox*oy, must be in m or degrees
                                             int ox, int oy)
{
    crs2crs c2c(proj_input, proj_output);

    // calculate input-points corresponding to wanted points
    const size_t on = ox * oy;
    std::unique_ptr<double[]> in_x_points(new double[on]);
    std::unique_ptr<double[]> in_y_points(new double[on]);
    std::copy(&out_x_points[0], &out_x_points[on], &in_x_points[0]);
    std::copy(&out_y_points[0], &out_y_points[on], &in_y_points[0]);
    c2c.inv(on, in_x_points.get(), in_y_points.get());
    const double delta = (inputIsMetric) ? 100 : 0.00001; // use 100m or 0.00001 degree as delta
    return get_vector_reproject_matrix_points_proj_delta(c2c, in_x_points.get(), in_y_points.get(), out_x_points, out_y_points, delta, delta, ox, oy);
}

Matrix_cp get_vector_reproject_matrix_field(const std::string& proj_input, const std::string& proj_output, const double* in_x_field, const double* in_y_field,
                                            int ox, int oy)
{
    crs2crs c2c(proj_input, proj_output);

    std::unique_ptr<double[]> out_x_field(new double[ox * oy]);
    std::unique_ptr<double[]> out_y_field(new double[ox * oy]);
    std::copy(&in_x_field[0], &in_x_field[ox * oy], &out_x_field[0]);
    std::copy(&in_y_field[0], &in_y_field[ox * oy], &out_y_field[0]);

    c2c.fwd(ox * oy, out_x_field.get(), out_y_field.get());
    return get_vector_reproject_matrix_proj(c2c, in_x_field, in_y_field, out_x_field.get(), out_y_field.get(), ox, oy);
}

Matrix_cp get_vector_reproject_matrix(const std::string& proj_input, const std::string& proj_output, const double* out_x_axis, const double* out_y_axis,
                                      int out_x_axis_type, int out_y_axis_type, int ox, int oy)
{
    crs2crs c2c(proj_input, proj_output);

    std::unique_ptr<double[]> in_xproj_axis(new double[ox * oy]);
    std::unique_ptr<double[]> in_yproj_axis(new double[ox * oy]);
    std::unique_ptr<double[]> out_xproj_axis(new double[ox * oy]);
    std::unique_ptr<double[]> out_yproj_axis(new double[ox * oy]);

    for (int y = 0; y < oy; ++y) {
        for (int x = 0; x < ox; ++x) {
            in_xproj_axis[y * ox + x] = out_x_axis[x];
            in_yproj_axis[y * ox + x] = out_y_axis[y];
            out_xproj_axis[y * ox + x] = out_x_axis[x];
            out_yproj_axis[y * ox + x] = out_y_axis[y];
        }
    }

    // getting positions in the original projection
    c2c.inv(ox * oy, in_xproj_axis.get(), in_yproj_axis.get());
    // start real calc
    return get_vector_reproject_matrix_proj(c2c, in_xproj_axis.get(), in_yproj_axis.get(), out_xproj_axis.get(), out_yproj_axis.get(), ox, oy);
}

void vector_reproject_values_by_matrix_f(Matrix_cp matrix, float* u_out, float* v_out, int oz)
{
    const size_t layerSize = matrix->size_x * matrix->size_y;
    const double* mtx = matrix->mtx();
    for (int z = 0; z < oz; ++z) {
        float* uz = &u_out[z * layerSize]; // current z-layer of u
        float* vz = &v_out[z * layerSize]; // current z-layer of v

        // loop over one layer: calc uv' = A*uv at each pos
        for (size_t i = 0, idx = 0; i < layerSize; i++, idx += Matrix::stride) {
            const double c = mtx[idx + 0];
            const double s = mtx[idx + 1];
            const double u_new = uz[i] * c - vz[i] * s;
            const double v_new = uz[i] * s + vz[i] * c;
            // matrix is rotation matrix, no further normalization needed
            uz[i] = u_new;
            vz[i] = v_new;
        }
    }
}

void vector_reproject_direction_by_matrix_f(Matrix_cp matrix,
                                            float* angle_out, // angle in degree
                                            int oz)
{
    const size_t layerSize = matrix->size_x * matrix->size_y;
    const double* mtx = matrix->mtx();
    for (int z = 0; z < oz; ++z) {
        float* angZ = &angle_out[z * layerSize];
        for (size_t i = 0, idx = 0; i < layerSize; i++, idx += Matrix::stride) {
            const double rot_deg = mtx[idx + 2];
            // angle stored as true angle (in degree) in the matrix m[2]
            // fprintf(stderr, "asin: %f, acos: %f, phi: %f\n", RAD_TO_DEG* asin(m[1]), RAD_TO_DEG*acos(m[0]), rot_deg);
            double angle_new = angZ[i] - rot_deg;
            // normalize 0..360
            if (angle_new < 0)
                angle_new += 360;
            if (angle_new > 360)
                angle_new -= 360;
            angZ[i] = angle_new;
        }
    }
}

void vector_reproject_values_f(const std::string& proj_input, const std::string& proj_output, float* u_out, float* v_out, const double* out_x_axis,
                               const double* out_y_axis, int out_x_axis_type, int out_y_axis_type, int ox, int oy, int oz)
{
    // calculate the positions in the original proj.
    Matrix_cp matrix = get_vector_reproject_matrix(proj_input, proj_output, out_x_axis, out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy);
    vector_reproject_values_by_matrix_f(matrix, u_out, v_out, oz);
}

void reproject_point_from_lonlat(const std::string& proj_output, double* lon_x, double* lat_y)
{
#ifdef HAVE_PROJ_H
    PJ_p P = make_PJ("EPSG:4326", proj_output);

    PJ_COORD uv = proj_coord(*lat_y, *lon_x, 0, 0); // EPSG:4326 here needs lat first, lon second
    PJ_COORD xy = proj_trans(P.get(), PJ_FWD, uv);
    *lon_x = xy.enu.e;
    *lat_y = xy.enu.n;
#else  /* !HAVE_PROJ_H*/
    projPJ_p outputPJ = make_PJ(proj_output);
    if (!outputPJ)
        throw_proj_error();

    projUV uv;
    uv.u = *lon_x * DEG_TO_RAD;
    uv.v = *lat_y * DEG_TO_RAD;
    uv = pj_fwd(uv, outputPJ.get());

    if (uv.u == HUGE_VAL)
        throw MetNoFimex::CDMException("huge val");

    *lon_x = uv.u;
    *lat_y = uv.v;
#endif /* !HAVE_PROJ_H */
}

void reproject_values(const std::string& proj_input, const std::string& proj_output, double* in_out_x_vals, double* in_out_y_vals, const int num)
{
    crs2crs c2c(proj_input, proj_output);
    c2c.fwd(num, in_out_x_vals, in_out_y_vals);
}

void reproject_axes(const std::string& proj_input, const std::string& proj_output, const double* in_x_axis, const double* in_y_axis, const int ix, const int iy,
                    double* out_xproj_axis, double* out_yproj_axis)
{
    crs2crs c2c(proj_input, proj_output);

    for (int y = 0; y < iy; ++y) {
        for (int x = 0; x < ix; ++x) {
            out_xproj_axis[y * ix + x] = in_x_axis[x];
            out_yproj_axis[y * ix + x] = in_y_axis[y];
        }
    }

    c2c.fwd(ix * iy, out_xproj_axis, out_yproj_axis);
}

} // namespace reproject
} // namespace MetNoFimex
