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

#include "fimex/interpolation.h"
#include "proj_api.h"
#include <string.h>
#include <stdio.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#include "fimex/CDMconstants.h"

#ifdef __GNUC__
#  if  __GNUC_PREREQ(6,0)
// bug fixed
#  else
//   if  gcc_version <= 6.0, isnan/isinf not using __builtin in math.h (but in c++ <cmath>)
//   see https://sourceware.org/bugzilla/show_bug.cgi?id=15367
#    if __GNUC_PREREQ (4,4) && !defined __SUPPORT_SNAN__
#      define isnan(x) __builtin_isnan (x)
#    endif
#  endif
#endif


/**
 * compare two strings
 * @param str1
 * @param str2
 * @return 0 if str1 != str2, 1 otherwise
 */
static int mifi_string_equal(const char* str1, const char* str2)
{
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    if (len1 == len2) {
        return (strncmp(str1, str2, len1) == 0);
    }
    return 0;
}

int mifi_string_to_interpolation_method(const char* mString)
{
    int method = MIFI_INTERPOL_UNKNOWN;
    if (mifi_string_equal("bilinear", mString)) {
        method = MIFI_INTERPOL_BILINEAR;
    } else if (mifi_string_equal("nearestneighbor", mString)) {
        method = MIFI_INTERPOL_NEAREST_NEIGHBOR;
    } else if (mifi_string_equal("bicubic", mString)) {
        method = MIFI_INTERPOL_BICUBIC;
    } else if (mifi_string_equal("coord_nearestneighbor", mString)) {
        method = MIFI_INTERPOL_COORD_NN;
    } else if (mifi_string_equal("coord_kdtree", mString)) {
        method = MIFI_INTERPOL_COORD_NN_KD;
    } else if (mifi_string_equal("forward_sum", mString)) {
        method = MIFI_INTERPOL_FORWARD_SUM;
    } else if (mifi_string_equal("forward_mean", mString)) {
        method = MIFI_INTERPOL_FORWARD_MEAN;
    } else if (mifi_string_equal("forward_median", mString)) {
        method = MIFI_INTERPOL_FORWARD_MEDIAN;
    } else if (mifi_string_equal("forward_max", mString)) {
        method = MIFI_INTERPOL_FORWARD_MAX;
    } else if (mifi_string_equal("forward_min", mString)) {
        method = MIFI_INTERPOL_FORWARD_MIN;
    } else if (mifi_string_equal("forward_undef_sum", mString)) {
        method = MIFI_INTERPOL_FORWARD_UNDEF_SUM;
    } else if (mifi_string_equal("forward_undef_mean", mString)) {
        method = MIFI_INTERPOL_FORWARD_UNDEF_MEAN;
    } else if (mifi_string_equal("forward_undef_median", mString)) {
        method = MIFI_INTERPOL_FORWARD_UNDEF_MEDIAN;
    } else if (mifi_string_equal("forward_undef_max", mString)) {
        method = MIFI_INTERPOL_FORWARD_UNDEF_MAX;
    } else if (mifi_string_equal("forward_undef_min", mString)) {
        method = MIFI_INTERPOL_FORWARD_MIN;
    }
    return method;
}


static inline int ascendingDoubleComparator(const void * a, const void * b)
{
    double x = *(double*)a;
    double y = *(double*)b;
    if (x == y) {}
    if ( *(double*)a >  *(double*)b ) return 1;
    else if ( *(double*)a == *(double*)b ) return 0;
    else return -1;
}

static inline int descendingDoubleComparator(const void * a, const void * b)
{
    return -1 * ascendingDoubleComparator(a,b);
}

/*
 * this works similar to bsearch in stdlib.h, except that it returns the index
 * of the found element rather than the element
 * In addition, it returns -1 + (-1 * (smallest element > key)) if key cannot be found
 */
static int bsearchDoubleIndex(const double key, const double* base, int num, int ( * comparator ) ( const void *, const void * ))
{
// Initialize first and last variables.
    int first = 0;
    int last = num - 1;

    int pos = 0;
    int comp = 0;
      while(first <= last) {
        pos = (first + last)/2;
           comp = comparator(&key, &base[pos]);
        if(comp > 0) {
            first = pos + 1;
        } else if (comp < 0) {
            last = pos - 1;
        } else {
              first = last + 1; // found, break loop
        }
      }
      if (comp == 0) return pos;
      else if (comp > 0) return (-1 + (-1 * (pos+1)));
      else return (-1 + (-1 * pos));
}

int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type)
{
    int circularLongitude = 0;
    int (*comparator)(const void * a, const void * b);
    if (axis[0] < axis[num-1]) comparator = ascendingDoubleComparator;
    else comparator = descendingDoubleComparator;

    if (axis_type == MIFI_LONGITUDE) {
        // decide if longitude axis is -180 to 180
        if (axis[0] < 0 || axis[num-1] < 0) {
            // change points > 180
            for (int i = 0; i < n; i++) {
                if (points[i] > MIFI_PI) points[i] -= 2*MIFI_PI;
            }
        } else {
            // change negative points (0-360)
            for (int i = 0; i < n; i++) {
                if (points[i] < 0) points[i] += 2*MIFI_PI;
            }
        }
        double nextOnAxis = axis[num-1] + (axis[1]-axis[0])*1.01; // slightly enlarged gitter-distance to avoid numerics
        if (axis[0] < axis[num-1]) { // ascending
            nextOnAxis -= 2*MIFI_PI;
            if (nextOnAxis >= axis[0]) {
                circularLongitude = 1;
            }
        } else { // descending
            nextOnAxis += 2*MIFI_PI;
            if (nextOnAxis <= axis[0]) {
                circularLongitude = 1;
            }
        }
    }

    for (int i = 0; i < n; i++) {
        if (!isfinite(points[i])) {
            points[i] = -999.; // outside range
            continue;
        }
        int pos = bsearchDoubleIndex(points[i], axis, num, comparator);
        if (pos >= 0) {
            points[i] = (double) pos;
        } else {
            // linear fit between [pos-1, pos}
            int nPos = -1 * (pos + 1);
            if (nPos == num) {
                nPos--; // extrapolate to the right
            } else if (nPos == 0) {
                nPos++; // extrapolate to the left
            }
            // linear spline interpolation
            double slope = axis[nPos] - axis[nPos-1];
            double offset = axis[nPos] - (slope*nPos);
            double arrayPos = (points[i] - offset) / slope;
            if (circularLongitude && arrayPos <= -0.5) {
                arrayPos += num;
            }
            if (circularLongitude && arrayPos > (num-0.5)) {
                arrayPos -= num;
            }
            if (MIFI_DEBUG > 0) {
                if (arrayPos <= -0.5 || arrayPos >= (num-0.5)) {
                    fprintf(stderr, "outside range: %f => %f (isCircular=%d)\n", points[i]*RAD_TO_DEG, arrayPos, circularLongitude);
                }
            }
            points[i] = arrayPos;
        }
    }
    return MIFI_OK;
}
/*
 * copy or convert array from degree to rad if required, otherwise just copy
 */
static void convertAxis(const double* orgAxis, const int num, const int type, double* outAxis)
{
    switch (type) {
        case MIFI_LONGITUDE:
        case MIFI_LATITUDE: for (int i = 0; i < num; i++) *outAxis++ = DEG_TO_RAD * *orgAxis++; break;
        case MIFI_PROJ_AXIS:
        default: memcpy(outAxis, orgAxis, num * sizeof(double)); break;
    }
}

static int mifi_interpolate_f_functional(int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz),
                        const char* proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis,
                        const int in_x_axis_type, const int in_y_axis_type, const int ix, const int iy, const int iz,
                        const char* proj_output, float* outfield, const double* out_x_axis, const double* out_y_axis,
                        const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy)
{
    double inXAxis[ix];
    double outXAxis[ox];
    double inYAxis[iy];
    double outYAxis[oy];
    convertAxis(in_x_axis, ix, in_x_axis_type, inXAxis);
    convertAxis(in_y_axis, iy, in_y_axis_type, inYAxis);
    convertAxis(out_x_axis, ox, out_x_axis_type, outXAxis);
    convertAxis(out_y_axis, oy, out_y_axis_type, outYAxis);

    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "in axis conversion: x %f -> %f; y %f -> %f\n", in_x_axis[0], inXAxis[0], in_y_axis[0], inYAxis[0]);
        fprintf(stderr, "out axis conversion: x %f -> %f; y %f -> %f\n", out_x_axis[0], outXAxis[0], out_y_axis[0], outYAxis[0]);
    }

    /*
     * transforming from output to input, to receive later the correct input-values
     * for the output coordinates
     */
    double pointsX[ox*oy];
    double pointsY[ox*oy];
    mifi_project_axes(proj_output, proj_input, outXAxis, outYAxis, ox, oy, pointsX, pointsY);

    mifi_points2position(pointsX, ox*oy, inXAxis, ix, in_x_axis_type);
    mifi_points2position(pointsY, ox*oy, inYAxis, iy, in_y_axis_type);

    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "projection: (%f, %f) <- (%f, %f)\n", out_x_axis[0], out_y_axis[0], pointsX[0], pointsY[0]);
    }

    float zValues[iz];
    for (int y = 0; y < oy; ++y) {
        for (int x = 0; x < ox; ++x) {
            if (func(infield, zValues, pointsX[y*ox+x], pointsY[y*ox+x], ix, iy, iz) != MIFI_ERROR) {
                for (int z = 0; z < iz; ++z) {
                    outfield[mifi_3d_array_position(x, y, z, ox, oy, iz)] = zValues[z];
                }
            }
        }
    }


    return MIFI_OK;
}

int mifi_interpolate_f(const int method,
                       const char* proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis,
                       const int in_x_axis_type, const int in_y_axis_type, const int ix, const int iy, const int iz,
                       const char* proj_output, float* outfield, const double* out_x_axis, const double* out_y_axis,
                       const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy)
{
    int (*func)(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

    switch (method) {
        case MIFI_INTERPOL_NEAREST_NEIGHBOR: func = mifi_get_values_f; break;
        case MIFI_INTERPOL_BILINEAR:         func = mifi_get_values_bilinear_f; break;
        case MIFI_INTERPOL_BICUBIC:          func = mifi_get_values_bicubic_f; break;
        default:                    return MIFI_ERROR; /* error */
    }

    return mifi_interpolate_f_functional(func, proj_input, infield, in_x_axis, in_y_axis, in_x_axis_type, in_y_axis_type, ix, iy, iz, proj_output, outfield, out_x_axis, out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy);
}

/*
 * great-circle angle: http://en.wikipedia.org/wiki/Great-circle_distance
 * multiply by R to get distance
 * param phi1, phi2 input longitude in radian
 * param lambda1, lambda2 input latitudes in radian
 */
static inline double mifi_great_circle_angle(double lat0, double lon0, double lat1, double lon1)
{
    return acos(sin(lat0)*sin(lat1) + cos(lat0)*cos(lat1)*cos(lon1-lon0));
}


static inline double mifi_bearing(double lat0, double lon0, double lat1, double lon1)
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
    double phi = atan2(sin_dLon*cos_lat1, cos_lat0*sin_lat1 - sin_lat0*cos_lat1*cos_dLon);
    return phi;

}
static int mifi_get_vector_reproject_matrix_points_proj_delta(projPJ inputPJ, projPJ outputPJ,
        const double* in_x_field, const double* in_y_field, // both on
        const double* out_x_field, const double* out_y_field, // both on
        double* pointsZ, // on, used within proj, but values not used
        double deltaX, double deltaY,
        int on, // number of points in output
        double* matrix) // 4*on
{
    // calculation of deltas: (x+d, y), (x, y+d) -> proj-values
    double* out_x_delta_proj_axis = (double*) malloc(on*sizeof(double));
    double* out_y_delta_proj_axis = (double*) malloc(on*sizeof(double));

    {
        // conversion along x axis
        if (out_x_delta_proj_axis == NULL || out_y_delta_proj_axis == NULL
                || pointsZ == NULL ) {
            fprintf(stderr, "error allocating memory of double(%d)", on);
            exit(1);
        }

        for (int i = 0; i < on; ++i) {
            out_x_delta_proj_axis[i] = in_x_field[i] + deltaX;
            out_y_delta_proj_axis[i] = in_y_field[i];
            pointsZ[i] = 0;
        }
        if (pj_transform(inputPJ, outputPJ, on, 0, out_x_delta_proj_axis, out_y_delta_proj_axis, pointsZ) != 0) {
            fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
            free(out_y_delta_proj_axis);
            free(out_x_delta_proj_axis);
            return MIFI_ERROR;
        }

        // metric calculations
        double sign = deltaX > 0 ? 1. : -1.;
        for (int i = 0; i < on; ++i) {
            double phi = 0;
            if (pj_is_latlong(outputPJ)) {
                phi = mifi_bearing(out_y_field[i], out_x_field[i], out_y_delta_proj_axis[i], out_x_delta_proj_axis[i]); //atan2(sin(lon_pnt0-dLonNorth)*cos(dLatNorth), cos(lat_pnt0)*sin(dLatNorth)-sin(lat_pnt0)*cos(dLatNorth)*cos(lon_pnt0-dLonNorth));
                // bearing is towards north, so we have to rotate by 90 degrees since we map x/east-axis
                // phi += MIFI_PI*.5;
                // only look at true north rotation for lat-lon
            } else {
                phi = atan2((out_y_delta_proj_axis[i] - out_y_field[i]),
                            (out_x_delta_proj_axis[i] - out_x_field[i]));
                if (sign < 0) {
                    phi += MIFI_PI;
                }
            }
            //fprintf(stderr, "atan2-y/x: %f\n", phi/MIFI_PI*180);
            matrix[0 + 4 * i] = phi; // temporary storage
            //matrix[1 + 4 * i] = sin(phi);
        }
    }
    {
        // conversion along y axis
        if (out_x_delta_proj_axis == NULL || out_y_delta_proj_axis == NULL
                || pointsZ == NULL ) {
            fprintf(stderr, "error allocating memory of double(%d)", on);
            exit(1);
        }

        for (int i = 0; i < on; ++i) {
            out_x_delta_proj_axis[i] = in_x_field[i];
            out_y_delta_proj_axis[i] = in_y_field[i] + deltaY;
            pointsZ[i] = 0;
        }
        if (pj_transform(inputPJ, outputPJ, on, 0, out_x_delta_proj_axis, out_y_delta_proj_axis, pointsZ) != 0) {
            fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
            free(out_y_delta_proj_axis);
            free(out_x_delta_proj_axis);
            return MIFI_ERROR;
        }
        if (pj_is_latlong(outputPJ)) {
            if (MIFI_DEBUG) fprintf(stderr,"output-projection is latlon\n");
        }
        double sign = deltaY > 0 ? 1. : -1.;
        for (int i = 0; i < on; ++i) {
            double phi0;
            if (pj_is_latlong(outputPJ)) { // this does not handle rotated lat-lon - good!
                phi0 = mifi_bearing(out_y_field[i], out_x_field[i], out_y_delta_proj_axis[i], out_x_delta_proj_axis[i]); //atan2(sin(lon_pnt0-dLonNorth)*cos(dLatNorth), cos(lat_pnt0)*sin(dLatNorth)-sin(lat_pnt0)*cos(dLatNorth)*cos(lon_pnt0-dLonNorth));
                if (sign < 0) {
                    phi0 += MIFI_PI;
                }
            } else {
                double phix = -1 * atan2((out_x_delta_proj_axis[i] - out_x_field[i]),
                                         (out_y_delta_proj_axis[i] - out_y_field[i]));
                if (sign < 0) {
                    phix += MIFI_PI;
                }
                //fprintf(stderr, "%f-%f / %f-%f", out_x_delta_proj_axis[i], out_x_field[i], out_y_delta_proj_axis[i], out_y_field[i]);
                //fprintf(stderr, "atan2-x/y: %f %f %f %f\n", phi/MIFI_PI*180, acos(matrix[0+4*i])/MIFI_PI*180, deltaX, deltaY);

                double phiy = matrix[0+4*i];
                //average, for non-conformal cases
                phi0 = .5*(phix+phiy);
            }
            //fprintf(stderr,"%f: %f, %f, %f, %f\n", RAD_TO_DEG*phi0, RAD_TO_DEG*out_y_field[i], RAD_TO_DEG*out_x_field[i], RAD_TO_DEG*out_y_delta_proj_axis[i], RAD_TO_DEG*out_x_delta_proj_axis[i]);
            double c = cos(phi0);
            double s = sin(phi0);
            matrix[0 + 4 * i] = c;
            matrix[1 + 4 * i] = s;
            matrix[2 + 4 * i] = -1 * s;
            matrix[3 + 4 * i] =  phi0;
        }
    }
    free(out_y_delta_proj_axis);
    free(out_x_delta_proj_axis);
    return MIFI_OK;
}

// common implementation of mifi_get_vector_reproject_matrix_*
static int mifi_get_vector_reproject_matrix_proj(projPJ inputPJ, projPJ outputPJ,
                        const double* in_x_field, const double* in_y_field, // both ox*oy
                        const double* out_x_field, const double* out_y_field, // both ox*oy
                        double* pointsZ, // ox*oy, used within proj, but values not used
                        int ox, int oy,
                        double* matrix) // 4*ox*oy
{
    // calculation of deltas: (x+d, y), (x, y+d) -> proj-values
#if 0
    double* out_x_delta_proj_axis = (double*) malloc(ox*oy*sizeof(double));
    double* out_y_delta_proj_axis = (double*) malloc(ox*oy*sizeof(double));

    if (out_x_delta_proj_axis == NULL || out_y_delta_proj_axis == NULL || pointsZ == NULL) {
        fprintf(stderr, "error allocating memory of double(%d*%d)", ox, oy);
        exit(1);
    }
#endif
    // delta usually .1% of distance between neighboring cells
    const double defaultDelta = 1e-3;
    double deltaX;
    {
        // conversion along x axis
        if (ox > 1) {
            if (oy > 1) {
                deltaX = defaultDelta * (in_x_field[(1)*ox +(1)] - in_x_field[0]);
                // and test another place in case of singularities
                size_t ox_2 = ox/2;
                size_t oy_2 = oy/2;
                double deltaX2 = defaultDelta * (in_x_field[(oy_2+1)*ox +(ox_2+1)] - in_x_field[(oy_2)*ox + ox_2]);
                deltaX += deltaX2;
                deltaX /= 2;
            } else {
                deltaX = defaultDelta * (in_x_field[(0)*ox +(1)] - in_x_field[0]);
            }
        } else {
            if (oy > 1) {
                deltaX = defaultDelta * (in_x_field[(1)*ox +(0)] - in_x_field[0]);
            } else {
                // no neighbors, e.g. rotation to single point
                deltaX = (in_x_field[0] > 1) ? (in_x_field[0]*defaultDelta) : defaultDelta;
            }
        }
    }
    double deltaY;
    {
        // conversion along y axis
        // deltaY usually .1% of distance between neighboring cells
        if (ox > 1) {
            if (oy > 1) {
                deltaY = defaultDelta * (in_x_field[(1)*ox +(1)] - in_x_field[0]);
                // and test another place in case of singularities
                size_t ox_2 = ox/2;
                size_t oy_2 = oy/2;
                double deltaY2 = defaultDelta * (in_x_field[(oy_2+1)*ox +(ox_2+1)] - in_x_field[(oy_2)*ox + ox_2]);
                deltaY += deltaY2;
                deltaY /= 2;
            } else {
                deltaY = defaultDelta * (in_x_field[(0)*ox +(1)] - in_x_field[0]);
            }
        } else {
            if (oy > 1) {
                deltaY = defaultDelta * (in_x_field[(1)*ox +(0)] - in_x_field[0]);
            } else {
                // no neighbors, e.g. rotation to single point
                deltaY = (in_x_field[0] > 1) ? (in_x_field[0]*defaultDelta) : defaultDelta;
            }
        }
    }
    if (fabs(deltaX) < 1e-9 || fabs(deltaY) < 1e-9 ) {
        fprintf(stderr, "WARNING, tiny deltaX/Y: %f %f possible singularity in vector-reprojection. Using default: %f\n", deltaX, deltaY, defaultDelta);
        deltaX = defaultDelta;
        deltaY = defaultDelta;
    }
    return mifi_get_vector_reproject_matrix_points_proj_delta(inputPJ, outputPJ,
            in_x_field, in_y_field, // both on
            out_x_field, out_y_field, // both on
            pointsZ, // on, used within proj, but values not used
            deltaX, deltaY,
            ox*oy, // number of points in output
            matrix); // 4*on
}
#if 0
    {
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
                out_x_delta_proj_axis[y*ox +x] = in_x_field[y*ox +x] + deltaX;
                out_y_delta_proj_axis[y*ox +x] = in_y_field[y*ox +x];
                pointsZ[y*ox +x] = 0;
            }
        }
        if (pj_transform(inputPJ, outputPJ, ox*oy, 0, out_x_delta_proj_axis,
                out_y_delta_proj_axis, pointsZ) != 0) {
            fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
            free(out_y_delta_proj_axis);
            free(out_x_delta_proj_axis);
            return MIFI_ERROR;
        }
//        fprintf(stderr, "delta_x: %f, in_x_field0: %f, out_x_field0: %f, out_x_delt_proj_axis0: %f\n", deltaX, in_x_field[0], out_x_field[0], out_x_delta_proj_axis[0] );

        // direction of rotation
        double sign = deltaX > 0 ? 1. : -1.;
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
                double phi = atan2((out_y_delta_proj_axis[y*ox+x] - out_y_field[y*ox+x]),
                      (out_x_delta_proj_axis[y*ox+x] - out_x_field[y*ox+x]));
                //fprintf(stderr, "atan2-x: %f\n", phi/MIFI_PI*180);
                matrix[mifi_3d_array_position(0,x,y,4,ox,oy)] = sign*cos(phi);
                matrix[mifi_3d_array_position(1,x,y,4,ox,oy)] = sign*sin(phi);
            }
        }
    }

    {
        // conversion along y axis
        // delta usually .1% of distance between neighboring cells
        double delta;
        if (ox > 1) {
            if (oy > 1) {
                delta = xDelta * (in_x_field[(1)*ox +(1)] - in_x_field[0]);
            } else {
                delta = xDelta * (in_x_field[(0)*ox +(1)] - in_x_field[0]);
            }
        } else {
            if (oy > 1) {
                delta = xDelta * (in_x_field[(1)*ox +(0)] - in_x_field[0]);
            } else {
                // no neighbors, e.g. rotation to single point
                delta = (in_x_field[0] > 1) ? (in_x_field[0]*xDelta) : xDelta;
            }
        }
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
                out_x_delta_proj_axis[y*ox +x] = in_x_field[y*ox +x];
                out_y_delta_proj_axis[y*ox +x] = in_y_field[y*ox +x] + delta;
                pointsZ[y*ox +x] = 0;
            }
        }
        if (pj_transform(inputPJ, outputPJ, ox*oy, 0, out_x_delta_proj_axis,
                out_y_delta_proj_axis, pointsZ) != 0) {
            fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
            free(out_y_delta_proj_axis);
            free(out_x_delta_proj_axis);
            return MIFI_ERROR;
        }

        // direction of rotation
        double sign = delta > 0 ? 1. : -1.;
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
                // phi might be slightly different than phi for x, this will
                // be corrected in post-processing
                double phi = atan2((out_x_delta_proj_axis[y*ox+x] - out_x_field[y*ox+x]),
                      (out_y_delta_proj_axis[y*ox+x] - out_y_field[y*ox+x]));
                //fprintf(stderr, "atan2-y: %f\n", at/MIFI_PI*180);
                matrix[mifi_3d_array_position(2,x,y,4,ox,oy)] = sign* sin(phi);
                matrix[mifi_3d_array_position(3,x,y,4,ox,oy)] = sign* cos(phi);
//                fprintf(stderr, "Proj matrix: %d %d: %e %e %e %e\n", x, y, matrix[mifi_3d_array_position(0,x,y,4,ox,oy)], matrix[mifi_3d_array_position(1,x,y,4,ox,oy)], matrix[mifi_3d_array_position(2,x,y,4,ox,oy)], matrix[mifi_3d_array_position(3,x,y,4,ox,oy)]);
            }
        }
    }
    free(out_y_delta_proj_axis);
    free(out_x_delta_proj_axis);
    return MIFI_OK;
}
#endif

int mifi_get_vector_reproject_matrix_points(const char* proj_input,
        const char* proj_output,
        int inputIsMetric,
        const double* out_x_points, const double* out_y_points, // both size on, must be in m or rad
        int on,
        double* matrix
        )
{
    // init projections
    projPJ inputPJ;
    projPJ outputPJ;
    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "input proj: %s\n", proj_input);
        fprintf(stderr, "output proj: %s\n", proj_output);
    }

    if (!(inputPJ = pj_init_plus(proj_input))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        return MIFI_ERROR;
    }
    if (!(outputPJ = pj_init_plus(proj_output))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        return MIFI_ERROR;
    }

    // calculate input-points corresponding to wanted points
    double* in_x_points = (double*) malloc(on*sizeof(double));
    double* in_y_points = (double*) malloc(on*sizeof(double));
    double* pointsZ     = (double*) calloc(sizeof(double),on); // z currently of no interest, no height attached to values
    if (in_x_points == NULL || in_y_points == NULL || pointsZ == NULL) {
        fprintf(stderr, "error allocating memory of 3*double(%d)", on);
        exit(1);
    }
    memcpy(in_x_points, out_x_points, on*sizeof(double));
    memcpy(in_y_points, out_y_points, on*sizeof(double));
    int retVal;
    if (pj_transform(outputPJ, inputPJ, on, 0, in_x_points, in_y_points, pointsZ) != 0) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        retVal = MIFI_ERROR;
    } else {

        double delta = (inputIsMetric) ? 100 : 0.00001; // use 100m or 0.00001 degree as delta
        retVal = mifi_get_vector_reproject_matrix_points_proj_delta(inputPJ, outputPJ,
                in_x_points, in_y_points, // both on
                out_x_points, out_y_points, // both on
                pointsZ, // on, used within proj, but values not used
                delta, delta,
                on, // number of points in output
                matrix); // 4*on
    }


    free(in_x_points);
    free(in_y_points);
    free(pointsZ);

    return retVal;
}

int mifi_get_vector_reproject_matrix_field(const char* proj_input,
                        const char* proj_output,
                        const double* in_x_field, const double* in_y_field, // both ox*oy
                        int ox, int oy,
                        double* matrix) // 4*ox*oy
{
    // init projections
    projPJ inputPJ;
    projPJ outputPJ;
    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "input proj: %s\n", proj_input);
        fprintf(stderr, "output proj: %s\n", proj_output);
    }

    if (!(inputPJ = pj_init_plus(proj_input))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        return MIFI_ERROR;
    }
    if (!(outputPJ = pj_init_plus(proj_output))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        return MIFI_ERROR;
    }
    double* out_x_field = (double*) malloc(ox*oy*sizeof(double));
    double* out_y_field = (double*) malloc(ox*oy*sizeof(double));
    double* pointsZ     = (double*) calloc(sizeof(double),ox*oy); // z currently of no interest, no height attached to values
    if (out_x_field == NULL || out_y_field == NULL || pointsZ == NULL) {
        fprintf(stderr, "error allocating memory of double(%d*%d)", ox, oy);
        exit(1);
    }
    memcpy(out_x_field, in_x_field, ox*oy*sizeof(double));
    memcpy(out_y_field, in_y_field, ox*oy*sizeof(double));
    int retVal = MIFI_ERROR;
    if (pj_transform(inputPJ, outputPJ, ox*oy, 0, out_x_field, out_y_field, pointsZ) != 0) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        retVal = MIFI_ERROR;
    } else {
        // start real calc
//        for (int i = 0; i < ox*oy; i++) {
//           fprintf(stderr, "in: %f,%f out: %f,%f %d %d\n", in_x_field[i], in_y_field[i], out_x_field[i], out_y_field[i], ox, oy);
//        }
        retVal = mifi_get_vector_reproject_matrix_proj(inputPJ, outputPJ, in_x_field, in_y_field, out_x_field, out_y_field, pointsZ, ox, oy, matrix);
//        fprintf(stderr, "matrix: %e %e %e %e\n", matrix[0], matrix[1], matrix[2], matrix[3]);
    }
    pj_free(inputPJ);
    pj_free(outputPJ);
    free(out_x_field);
    free(out_y_field);
    free(pointsZ);
    return retVal;
}

int mifi_get_vector_reproject_matrix(const char* proj_input,
                        const char* proj_output,
                        const double* out_x_axis, const double* out_y_axis,
                        int out_x_axis_type, int out_y_axis_type,
                        int ox, int oy,
                        double* matrix) // 4*ox*oy
{
    // init projections
    projPJ inputPJ;
    projPJ outputPJ;
    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "input proj: %s\n", proj_input);
        fprintf(stderr, "output proj: %s\n", proj_output);
    }

    if (!(inputPJ = pj_init_plus(proj_input))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        return MIFI_ERROR;
    }
    if (!(outputPJ = pj_init_plus(proj_output))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        return MIFI_ERROR;
    }

    // convert longitude/latitude to rad
    double outXAxis[ox];
    double outYAxis[oy];
    convertAxis(out_x_axis, ox, out_x_axis_type, outXAxis);
    convertAxis(out_y_axis, oy, out_y_axis_type, outYAxis);


    double* in_xproj_axis = (double*) malloc(ox*oy*sizeof(double));
    double* in_yproj_axis = (double*) malloc(ox*oy*sizeof(double));
    double* out_xproj_axis = (double*) malloc(ox*oy*sizeof(double));
    double* out_yproj_axis = (double*) malloc(ox*oy*sizeof(double));
    double* pointsZ        = (double*) calloc(sizeof(double),ox*oy); // z currently of no interest, no height attached to values
    if (in_xproj_axis == NULL || in_yproj_axis == NULL ||
        out_xproj_axis == NULL || out_yproj_axis == NULL ||
        pointsZ == NULL) {
        fprintf(stderr, "error allocating memory of double(%d*%d)", ox, oy);
        exit(1);
    }
    for (int y = 0; y < oy; ++y) {
        for (int x = 0; x < ox; ++x) {
            in_xproj_axis[y*ox +x] = outXAxis[x];
            in_yproj_axis[y*ox +x] = outYAxis[y];
            out_xproj_axis[y*ox +x] = outXAxis[x];
            out_yproj_axis[y*ox +x] = outYAxis[y];
        }
    }

    // getting positions in the original projection
    int retVal = MIFI_ERROR;
    if (pj_transform(outputPJ, inputPJ, ox*oy, 0, in_xproj_axis, in_yproj_axis, pointsZ) != 0) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        retVal = MIFI_ERROR;
    } else {
        // start real calc
        retVal = mifi_get_vector_reproject_matrix_proj(inputPJ, outputPJ, in_xproj_axis, in_yproj_axis, out_xproj_axis, out_yproj_axis, pointsZ, ox, oy, matrix);
    }
    pj_free(inputPJ);
    pj_free(outputPJ);
    free(in_yproj_axis);
    free(in_xproj_axis);
    free(out_yproj_axis);
    free(out_xproj_axis);
    free(pointsZ);
    return retVal;
}

int mifi_vector_reproject_values_by_matrix_f(int method,
                        const double* matrix,
                        float* u_out, float* v_out,
                        int ox, int oy, int oz)
{
    size_t layerSize = ox*oy;
    for (size_t z = 0; z < oz; ++z) {
        const double *matrixPos = matrix; // reset matrix for each z
        float *uz = &u_out[z*layerSize]; // current z-layer of u
        float *vz = &v_out[z*layerSize]; // current z-layer of v

        // loop over one layer: calc uv' = A*uv at each pos
        for (size_t i = 0; i < layerSize; i++) {
            const double* m = &matrixPos[4*i];
            double u_new = uz[i] * m[0] - vz[i] * m[1];
            double v_new = uz[i] * m[1] + vz[i] * m[0];
            // matrix is rotation matrix, no further normalization needed
            uz[i] = u_new;
            vz[i] = v_new;
        }
    }
    return MIFI_OK;
}

int mifi_vector_reproject_direction_by_matrix_f(int method,
                        const double* matrix,
                        float* angle_out, // angle in degree
                        int ox, int oy, int oz)
{
    size_t layerSize = ox*oy;
    for (size_t z = 0; z < oz; ++z) {
        const double *matrixPos = matrix;
        float* angZ = &angle_out[z*layerSize];
        for (size_t i = 0; i < layerSize; i++) {
            const double* m = &matrixPos[4*i];
            // angle stored as true angle (in rad) in the matrix m[3]
            // fprintf(stderr, "asin: %f, acos: %f, phi: %f\n", RAD_TO_DEG* asin(m[1]), RAD_TO_DEG*acos(m[0]), RAD_TO_DEG*m[3]);
            double angle_new = angZ[i] - RAD_TO_DEG * m[3];
            // normalize 0..360
            if (angle_new < 0) angle_new += 360;
            if (angle_new > 360) angle_new -= 360;
            angZ[i] = angle_new;
        }
    }
    return MIFI_OK;
}

int mifi_vector_reproject_values_f(int method,
                        const char* proj_input,
                        const char* proj_output,
                        float* u_out, float* v_out,
                        const double* out_x_axis, const double* out_y_axis,
                        int out_x_axis_type, int out_y_axis_type,
                        int ox, int oy, int oz)
{
    double* matrix = malloc(ox*oy*4*sizeof(double));
    if (matrix == NULL) {
        fprintf(stderr, "error allocating memory of double(4*%d*%d)", ox, oy);
        exit(1);
    }
    // calculate the positions in the original proj.
    int errcode = mifi_get_vector_reproject_matrix(proj_input, proj_output, out_x_axis, out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy, matrix);
    if (errcode != MIFI_OK) {
        free(matrix);
        return errcode;
    }
    errcode = mifi_vector_reproject_values_by_matrix_f(method, matrix, u_out, v_out, ox, oy, oz);
    free(matrix);
    return errcode;
}


int mifi_get_values_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz)
{
    int rx = lround(x);
    int ry = lround(y);
    // fprintf(stderr, "values: %f, %f (%d ,%d)\n", x, y, rx, ry);
    if (((rx >= 0) && (rx < ix)) &&
        ((ry >= 0) && (ry < iy))) { // pos in range
        for (int z = 0; z < iz; ++z) {
            outvalues[z] = infield[mifi_3d_array_position(rx,ry,z,ix,iy,iz)];
        }
    } else {
        // fprintf(stderr, "outside values: %f, %f (0,%d : 0,%d)\n", x, y, ix, iy);
        for (int z = 0; z < iz; ++z) {
            outvalues[z] = MIFI_UNDEFINED_F;
        }
    }
    return MIFI_OK;
}

int mifi_get_values_bilinear_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz)
{
    int x0 = (int) floor(x);
    int x1 = x0 + 1;
    float xfrac = x - x0;
    int y0 = (int) floor(y);
    int y1 = y0 + 1;
    float yfrac = y - y0;
    if ((0 <= x0) && (x1 < ix)) {
        if ((0 <= y0) && (y1 < iy)) {
            // pos in range
            size_t pos = mifi_3d_array_position(x0, y0, 0, ix, iy, iz);
            for (int z = 0; z < iz; ++z) {
                float s00 = infield[pos];
                float s01 = infield[pos+1];
                float s10 = infield[pos+ix];
                float s11 = infield[pos+ix+1];
                // Missing values: NANs will be propagated by IEEE
                outvalues[z] = (1.f - yfrac) * ((1.f - xfrac)*s00 + xfrac*s01) +
                                yfrac      * ((1.f - xfrac)*s10 + xfrac*s11);
                pos += iy*ix;
            }
        } else {
            y0 = lround(y);
            if ((0 <= y0) && (y0 < iy)) {
                // linear interpolation in x, nearest-neighbor in y
                size_t pos = mifi_3d_array_position(x0, y0, 0, ix, iy, iz);
                for (int z = 0; z < iz; ++z) {
                    float s00 = infield[pos];
                    float s01 = infield[pos+1];
                    outvalues[z] = (1.f - xfrac)*s00 + xfrac*s01;
                    pos += iy*ix;
                }
            } else {
                // outside usefull y
                for (int z = 0; z < iz; ++z) {
                    outvalues[z] = MIFI_UNDEFINED_F;
                }
            }
        }
    } else {
        x0 = lround(x);
        if ((0 <= x0) && (x0 < ix)) {
            // nearest neighbor in x
            if ((0 <= y0) && (y1 < iy)) {
                // linear in y
                size_t pos = mifi_3d_array_position(x0, y0, 0, ix, iy, iz);
                for (int z = 0; z < iz; ++z) {
                    float s00 = infield[pos];
                    float s10 = infield[pos+ix];
                    outvalues[z] = (1 - yfrac)*s00 + (yfrac*s10);
                    pos += iy*ix;
                }
            } else {
                y0 = lround(y);
                if ((0 <= y0) && (y0 <= iy)) {
                    // nearest neighbor in y
                    size_t pos = mifi_3d_array_position(x0, y0, 0, ix, iy, iz);
                    for (int z = 0; z < iz; ++z) {
                        outvalues[z] = infield[pos];
                        pos += iy*ix;
                    }
                } else {
                    for (int z = 0; z < iz; ++z) {
                        outvalues[z] = MIFI_UNDEFINED_F;
                    }
                }
            }
        } else {
            for (int z = 0; z < iz; ++z) {
                outvalues[z] = MIFI_UNDEFINED_F;
            }
        }
    }

    return MIFI_OK;
}

int mifi_get_values_bicubic_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz)
{
    // convolution matrix for a = -0.5
    double M[4][4] = {{ 0, 2, 0, 0},
                      {-1, 0, 1, 0},
                      { 2,-5, 4,-1},
                      {-1, 3,-3, 1}};
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            M[i][j] *= .5;

    int x0 = floor(x);
    double xfrac = x - x0;
    int y0 = floor(y);
    double yfrac = y - y0;

    if (((1 <= x0) && ((x0+2) < ix)) &&
        ((1 <= y0) && ((y0+2) < iy))) {
        double X[4];
        double XM[4]; /* X*M */
        double Y[4];
        double MY[4]; /* M*Y */
        X[0] = 1;
        X[1] = xfrac;
        X[2] = xfrac * xfrac;
        X[3] = X[2] * xfrac;
        for (int i = 0; i < 4; i++) {
            XM[i] = 0;
            for (int j = 0; j < 4; j++) {
                XM[i] += X[j] * M[j][i];
            }
        }
        Y[0] = 1;
        Y[1] = yfrac;
        Y[2] = yfrac * yfrac;
        Y[3] = Y[2] * yfrac;
        for (int i = 0; i < 4; i++) {
            MY[i] = 0;
            for (int j = 0; j < 4; j++) {
                MY[i] += Y[j] * M[j][i];
            }
        }

        for (int z = 0; z < iz; ++z) {
            double F[4][4];
            double XMF[4];
            outvalues[z] = 0;
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                    F[i][j] = infield[mifi_3d_array_position(x0+i-1, y0+j-1, z, ix, iy, iz)];
                }
            }

            for (int i = 0; i < 4; i++) {
                XMF[i] = 0;
                for (int j = 0; j < 4; j++) {
                    XMF[i] += XM[j] * F[j][i];
                }
            }
            for (int i = 0; i < 4; i++) {
                outvalues[z] += XMF[i] * MY[i];
            }
        }
    } else { // border cases
        for (int z = 0; z < iz; ++z) {
            outvalues[z] = MIFI_UNDEFINED_F;
        }
    }
    return MIFI_OK;
}

int mifi_get_values_nearest_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    memcpy(outfield, infieldA, n * sizeof(float));
    return MIFI_OK;
}

//o(x) = in(a) + (x - a) * (in(b) - in(a)) / (b - a)
//b = o(a)
static void mifi_get_values_linear_f_simple_(const float* infieldA, const float* infieldB, float* outfield, const size_t n, float f)
{
    int i = 0;
    while (n > i++) {
        float iA = *infieldA++;
        float iB = *infieldB++;
        float* o = outfield++; // position!
        *o = iA + f * (iB - iA);
    }
    return;
}
int mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    const float f = (a == b) ? 0 :  ((x - a) / (b - a));
    if (f == 0) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldA, n * sizeof(float));
    } else if (f == 1) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldB, n * sizeof(float));
    } else {
        mifi_get_values_linear_f_simple_(infieldA, infieldB, outfield, n, f);
    }
    return MIFI_OK;
}

int mifi_get_values_linear_d(const double* infieldA, const double* infieldB, double* outfield, const size_t n, const double a, const double b, const double x)
{
    const double f = (a == b) ? 0 :  ((x - a) / (b - a));
    if (f == 0) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldA, n * sizeof(double));
    } else if (f == 1) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldB, n * sizeof(double));
    } else {
        int i = 0;
        while (n > i++) {
            double iA = *infieldA++;
            double iB = *infieldB++;
            double* o = outfield++; // position!
            *o = iA + f * (iB - iA);
        }
    }
    return MIFI_OK;
}

static int mifi_get_values_linear_conf_extrapol_f(float leftLimit, float rightLimit, const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    const float f = (a == b) ? 0 :  ((x - a) / (b - a));
    if (f == 0) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldA, n * sizeof(float));
    } else if (f == 1) {
        // avoid numerical side-effects, like 0*nan = nan
        memcpy(outfield, infieldB, n * sizeof(float));
    } else if ((f >= leftLimit) && (f <= rightLimit)) {
        mifi_get_values_linear_f_simple_(infieldA, infieldB, outfield, n, f);
    } else {
        int i = 0;
        while (n > i++) {
            float* o = outfield++; // position!
            *o = MIFI_UNDEFINED_F;
        }
    }
    return MIFI_OK;
}

int mifi_get_values_linear_weak_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    return mifi_get_values_linear_conf_extrapol_f(-1., 2., infieldA, infieldB, outfield, n, a, b, x);
}
int mifi_get_values_linear_no_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    return mifi_get_values_linear_conf_extrapol_f(0., 2., infieldA, infieldB, outfield, n, a, b, x);
}

int mifi_get_values_linear_const_extrapol_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    const float f = (a == b) ? 0 :  ((x - a) / (b - a));
    if (f >= 1) {
        memcpy(outfield, infieldB, n * sizeof(float));
    } else if (f <= 0) {
        memcpy(outfield, infieldA, n * sizeof(float));
    } else {
        mifi_get_values_linear_f_simple_(infieldA, infieldB, outfield, n, f);
    }
    return MIFI_OK;
}

// o(x) = m*log(x) + c
// exp(o(x)) = exp(m*log(x) + c) = exp(m*log(x)) * exp(c)
// exp(o(xO)) = x^m * exp(c)
int mifi_get_values_log_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    // see mifi_get_values_log_f + log of infields and outfield
    if (a <= 0 || b <= 0 || x <= 0) {
        return MIFI_ERROR;
    }
    double log_a = log(a);
    double log_b = log(b);
    double log_x = log(x);
    mifi_get_values_linear_f(infieldA, infieldB, outfield, n, log_a, log_b, log_x);
    return MIFI_OK;
}

int mifi_get_values_log_log_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
    if (a <= 0 || b <= 0 || x <= 0) {
        return MIFI_ERROR;
    }
    // add M_E to make sure that the log remains positive
    double log_a = log(a + M_E);
    double log_b = log(b + M_E);
    double log_x = log(x + M_E);
    mifi_get_values_log_f(infieldA, infieldB, outfield, n, log_a, log_b, log_x);
    return MIFI_OK;

}

int mifi_project_values(const char* proj_input, const char* proj_output, double* in_out_x_vals, double* in_out_y_vals, const int num)
{
    // init projections
    projPJ inputPJ;
    projPJ outputPJ;
    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "input proj: %s\n", proj_input);
        fprintf(stderr, "output proj: %s\n", proj_output);
    }

    if (!(inputPJ = pj_init_plus(proj_input))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        return MIFI_ERROR;
    }
    if (!(outputPJ = pj_init_plus(proj_output))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        return MIFI_ERROR;
    }
    // z currently of no interest, no height attached to values
    double* pointsZ= (double*) calloc(num, sizeof(double));
    if (pointsZ == NULL) {
        fprintf(stderr, "memory allocation error");
        pj_free(inputPJ);
        pj_free(outputPJ);
        return MIFI_ERROR;
    }
    if (pj_transform(inputPJ, outputPJ, num, 0, in_out_x_vals, in_out_y_vals, pointsZ) != 0) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        pj_free(outputPJ);
        free(pointsZ);
        return MIFI_ERROR;
    }
    pj_free(inputPJ);
    pj_free(outputPJ);
    free(pointsZ);
    return MIFI_OK;

}

int mifi_project_axes(const char* proj_input, const char* proj_output, const double* in_x_axis, const double* in_y_axis, const int ix, const int iy, double* out_xproj_axis, double* out_yproj_axis) {
    // init projections
    projPJ inputPJ;
    projPJ outputPJ;
    if (MIFI_DEBUG > 0) {
        fprintf(stderr, "input proj: %s\n", proj_input);
        fprintf(stderr, "output proj: %s\n", proj_output);
    }

    if (!(inputPJ = pj_init_plus(proj_input))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        return MIFI_ERROR;
    }
    if (!(outputPJ = pj_init_plus(proj_output))) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        pj_free(inputPJ);
        return MIFI_ERROR;
    }
    // z currently of no interest, no height attached to values
    double* pointsZ= (double*) calloc(ix*iy, sizeof(double));
    if (pointsZ == NULL) {
        fprintf(stderr, "memory allocation error");
        pj_free(inputPJ);
        pj_free(outputPJ);
        return MIFI_ERROR;
    }
    for (int y = 0; y < iy; ++y) {
        for (int x = 0; x < ix; ++x) {
            out_xproj_axis[y*ix +x] = in_x_axis[x];
            out_yproj_axis[y*ix +x] = in_y_axis[y];
        }
    }

    // transforming
    if (pj_transform(inputPJ, outputPJ, ix*iy, 0, out_xproj_axis, out_yproj_axis, pointsZ) != 0) {
        fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
        free(pointsZ);
        pj_free(inputPJ);
        pj_free(outputPJ);
        return MIFI_ERROR;
    }
    free(pointsZ);
    pj_free(inputPJ);
    pj_free(outputPJ);
    return MIFI_OK;
}

int mifi_fill2d_f(size_t nx, size_t ny, float* field, float relaxCrit, float corrEff, size_t maxLoop, size_t* nChanged) {
    size_t totalSize = nx*ny;
    if (totalSize == 0) return MIFI_OK;

    double sum = 0;
    *nChanged = 0;

    float* fieldPos = field;
    int i = 0;
    // calculate sum and number of valid values
    while (i < totalSize) {
        if (isnan(*fieldPos)) {
            (*nChanged)++;
        } else {
            sum += *fieldPos;
        }
        fieldPos++;
        i++;
    }
    size_t nUnchanged = totalSize - *nChanged;
    if (nUnchanged == 0 || *nChanged == 0) {
        return MIFI_OK; // nothing to do
    }

    // working field
    float* wField = malloc(totalSize*sizeof(float));
    if (wField == NULL) {
        fprintf(stderr, "error allocating memory of float(%zd*%zd)", nx, ny);
        exit(1);
    }

    // The value of average ( i.e. the MEAN value of the array field ) is filled in
    // the array field at all points with an undefined value.
    // field(i,j) = average may be regarded as the "first guess" in the iterative
    // method.
    double average = sum / nUnchanged;
    //fprintf(stderr, "sum: %f, average: %f, unchanged: %d\n", sum, average, nUnchanged);
    // calculate stddev
    double stddev = 0;
    fieldPos = field;
    float *wFieldPos = wField;
    i = 0;
    while (i < totalSize) {
        if (isnan(*fieldPos)) {
            *wFieldPos = 1.;
            *fieldPos = average;
        } else {
            stddev += fabs(*fieldPos - average);
            *wFieldPos = 0.;
        }
        wFieldPos++;
        fieldPos++;
        i++;
    }
    stddev /= nUnchanged;

    double crit = relaxCrit * stddev;

    //fprintf(stderr, "crit %f, stddev %f", crit, stddev);

    // starting the iterative method, border-values are left at average, nx,ny >=1
    size_t nxm1 = (size_t)(nx - 1);
    size_t nym1 = (size_t)(ny - 1);

    // initialize a variational field from the border
    for (size_t y = 1; y < nym1; y++) {
        for (size_t x = 1; x < nxm1; x++) {
            wField[y*nx +x] *= corrEff;
        }
    }

    // error field
    float* eField = malloc(totalSize*sizeof(float));
    if (eField == NULL) {
        fprintf(stderr, "error allocating memory of float(%zd*%zd)", nx, ny);
        exit(1);
    }
    // start the iteration loop
    for (size_t n = 0; n < maxLoop; n++) {
        // field-positions, start of inner loop, forwarded one row
        float *f = &field[nx];
        float *e = &eField[nx];
        float *w = &wField[nx];
        for (size_t y = 1; y < nym1; y++) {
            for (size_t x = 1; x < nxm1; x++) {
                f++; e++; w++;
                *e = (*(f+1) + *(f-1) + *(f+nx) + *(f-nx))*0.25 - *f;
                (*f) += *e * *w;
            }
            f+=2; e+=2; w+=2; // skip first and last element in row
        }

        // Test convergence now and then (slow test loop)
        if ((n < (maxLoop-5)) &&
            (n%10 == 0)) {
            float crtest = crit*corrEff;
            size_t nbad = 0;
            float *e = &eField[nx];
            float *w = &wField[nx];
            for (size_t y = 1; y < nym1; y++) {
                if (nbad != 0) break;
                for (size_t x = 1; x < nxm1; x++) {
                    e++; w++;
                    if (fabs(*e * *w) > crtest) {
                        nbad = 1;
                    }
                }
                e+=2; w+=2;  // skip first and last element in row
            }
            if (nbad == 0) {
                free(eField);
                free(wField);
                return MIFI_OK; // convergence
            }
        }

        // some work on the borders
        for (size_t y = 1; y < nym1; y++) {
            field[y*nx+0] += (field[y*nx+1] - field[y*nx+0]) * wField[y*nx+0];
            field[y*nx+(nx-1)] += (field[y*nx+(nx-2)] - field[y*nx+(nx-1)]) * wField[y*nx+(nx-1)];
        }
        for (size_t x = 0; x < nx; x++) {
            field[0*nx +x] += (field[1*nx+x] - field[0*nx+x]) * wField[0*nx+x];
            field[nym1*nx+x] += (field[(nym1-1)*nx+x] - field[nym1*nx+x]) * wField[nym1*nx+x];
        }
    }

    free(eField);
    free(wField);
    return MIFI_OK;
}

static int mifi_creepfillval2dImpl_f(size_t nx, size_t ny, float* field, float defaultVal, unsigned short repeat, char setWeight, size_t nChanged) {
    size_t totalSize = nx*ny;
    if (totalSize == 0) return MIFI_OK;

    size_t nUnchanged = totalSize - nChanged;
    //fprintf(stderr, "defaultVal: %f, unchanged: %d\n", defaultVal, nUnchanged);
    if (nUnchanged == 0 || nChanged == 0) {
        return MIFI_OK; // nothing to do
    }

    // working field, 5: valid value, 0: invalid value, 1 number of valid neighbours
    char* wField = (char*) malloc(totalSize*sizeof(char));
    if (wField == NULL) {
        fprintf(stderr, "error allocating memory of char(%zd*%zd)", nx, ny);
        exit(1);
    }
    unsigned short* rField = (unsigned short*) malloc(totalSize*sizeof(unsigned short));
    if (rField == NULL) {
        fprintf(stderr, "error allocating memory of short(%zd*%zd)", nx, ny);
        exit(1);
    }

    // The defaultValue (or mean) is filled in
    // the array field at all points with an undefined value.
    // field(i,j) = defaultVal may be regarded as the "first guess" in the iterative
    // method.
    float* fieldPos = field;
    char *wFieldPos = wField;
    unsigned short *rFieldPos = rField;
    int i = 0;
    while (i < totalSize) {
        if (isnan(*fieldPos)) {
            *wFieldPos = 0;
            *rFieldPos = 0;
            *fieldPos = defaultVal;
        } else { // defined
            *wFieldPos = setWeight;
            *rFieldPos = repeat;
        }
        wFieldPos++;
        rFieldPos++;
        fieldPos++;
        i++;
    }

    // starting the iterative method, border-values are left at defaultVal, nx,ny >=1
    size_t nxm1 = (size_t)(nx - 1);
    size_t nym1 = (size_t)(ny - 1);

    // and the loop, with a maximum of nUnchanged rounds
    int l = 0;
    size_t changedInLoop = 1;
    while ((changedInLoop > 0) && (l < nUnchanged)) {
        //fprintf(stderr, "loop %d, change %d\n", l, changedInLoop);
        changedInLoop = 0; // stopps when a loop didn't manage to seriously change more values
        l++;

        // field-positions, start of inner loop, forwarded one row
        float *f = &field[nx];
        unsigned short *r = &rField[nx];
        char *w = &wField[nx];
        // loop over inner array
        for (size_t y = 1; y < nym1; y++) {
            for (size_t x = 1; x < nxm1; x++) {
                f++; r++; w++; // propagate positions
                if (*r < repeat) {
                    // undefined value or changed enough
                    size_t wFieldSum = *(w+1)+ *(w-1) + *(w+nx) + *(w-nx);
                    if (wFieldSum != 0) {
                        // some neighbours defined

                        // weight defaultVal of neigbouring cells, with double weight on original values
                        // + 1 defaultVal "center"
                        (*f) += *(w+1) * *(f+1) + *(w-1) * *(f-1) + *(w+nx) * *(f+nx) + *(w-nx) * *(f-nx);
                        (*f) /= (1+wFieldSum);
                        // field has been changed
                        (*w) = 1; // this is a implicit defined field
                        (*r)++; // it has been changed
                        changedInLoop++;
                    }
                }
            }
            f+=2; r+=2; w+=2; // skip last and first element in row
        }
    }
    // simple calculations at the borders
    for (size_t l = 0; l < repeat; l++) {
        for (size_t y = 1; y < nym1; y++) {
            if (rField[y*nx+0] < repeat) { // unset
                field[y*nx+0] += field[y*nx+1]*wField[y*nx+1];
                field[y*nx+0] /= (1+wField[y*nx+1]);
                wField[y*nx+0] = 1;
            }
            if (rField[y*nx+(nx-1)] < repeat) { // unset
                field[y*nx+(nx-1)] += field[y*nx+(nx-2)]*wField[y*nx+(nx-2)];
                field[y*nx+(nx-1)] /= (1+wField[y*nx+(nx-2)]);
                wField[y*nx+(nx-1)] = 1;
            }
        }
        for (size_t x = 0; x < nx; x++) {
            if (rField[0*nx +x] < repeat) { // unset
                field[0*nx +x] += field[1*nx+x]*wField[1*nx+x];
                field[0*nx +x] /= (1+wField[1*nx+x]);
                wField[0*nx +x] = 1;
            }
            if (rField[nym1*nx+x] < repeat) { // unset
                field[nym1*nx+x] += field[(nym1-1)*nx+x]*wField[(nym1-1)*nx+x];
                field[nym1*nx+x] /= (1+wField[(nym1-1)*nx+x]);
                wField[nym1*nx+x] = 1;
            }
        }
    }
    free(rField);
    free(wField);
    return MIFI_OK;
}

int mifi_creepfill2d_f(size_t nx, size_t ny, float* field, unsigned short repeat, char setWeight, size_t* nChanged) {
    size_t totalSize = nx*ny;
    if (totalSize == 0) return MIFI_OK;

    double sum = 0;
    *nChanged = 0;

    float* fieldPos = field;
    int i = 0;
    // calculate sum and number of valid values
    while (i < totalSize) {
        if (isnan(*fieldPos)) {
            (*nChanged)++;
        } else {
            sum += *fieldPos;
        }
        fieldPos++;
        i++;
    }
    size_t nUnchanged = totalSize - *nChanged;
    if (nUnchanged == 0) return MIFI_OK;
    float average = sum/nUnchanged;

    return mifi_creepfillval2dImpl_f(nx, ny, field, average , repeat, setWeight, *nChanged);
}

int mifi_creepfillval2d_f(size_t nx, size_t ny, float* field, float defaultVal, unsigned short repeat, char setWeight, size_t* nChanged) {
    size_t totalSize = nx*ny;
    if (totalSize == 0) return MIFI_OK;

    *nChanged = 0;
    float* fieldPos = field;
    int i = 0;
    // calculate sum and number of valid values
    while (i < totalSize) {
        if (isnan(*fieldPos++)) {
            (*nChanged)++;
        }
        i++;
    }
    //fprintf(stderr, "defaultVal: %f, changed: %d\n", defaultVal, *nChanged);
    return mifi_creepfillval2dImpl_f(nx, ny, field, defaultVal, repeat, setWeight, *nChanged);
}

int mifi_griddistance(size_t nx, size_t ny, const double* lonVals, const double* latVals, float* gridDistX, float* gridDistY)
{
    size_t fieldSize = nx*ny;
    if (fieldSize == 1) {
        gridDistX[0] = 0;
        gridDistY[0] = 0;
        return MIFI_ERROR; // no grid
    } else if (nx == 1 || ny == 1) {
        // 1-dimensional 'grid'
        for (size_t p = 0; p < (fieldSize - 1); p++) {
            gridDistX[p] = MIFI_EARTH_RADIUS_M
                    * mifi_great_circle_angle(DEG_TO_RAD * latVals[p],
                            DEG_TO_RAD * lonVals[p],
                            DEG_TO_RAD * latVals[p+1],
                            DEG_TO_RAD * lonVals[p+1]);
            gridDistY[p] = gridDistX[p];
        }
        gridDistX[(fieldSize - 1)] = gridDistX[(fieldSize - 2)];
        gridDistY[(fieldSize - 1)] = gridDistY[(fieldSize - 2)];
    } else {
        for (size_t j = 0; j < (ny - 1); j++) {
            for (size_t i = 0; i < (nx - 1); i++) {
                size_t p = i + nx * j;
                size_t right = p + 1;
                size_t down = p + nx;
                double lat0 = latVals[p];
                double lat1 = latVals[right];
                double latY = latVals[down];
                // avoid singularities at pole

                gridDistX[p] = MIFI_EARTH_RADIUS_M
                        * mifi_great_circle_angle(DEG_TO_RAD * lat0,
                                DEG_TO_RAD * lonVals[p],
                                DEG_TO_RAD * lat1,
                                DEG_TO_RAD * lonVals[right]);
                gridDistY[p] = MIFI_EARTH_RADIUS_M
                        * mifi_great_circle_angle(DEG_TO_RAD * lat0,
                                DEG_TO_RAD * lonVals[p],
                                DEG_TO_RAD * latY,
                                DEG_TO_RAD * lonVals[down]);
            }
        }
        // last column
        for (size_t j = 0; j < ny; j++) {
            size_t p = j*nx + (nx-1);
            gridDistX[p] = gridDistX[p-1];
            gridDistY[p] = gridDistY[p-1];
        }
        // last row
        for (size_t i = 0; i < nx; i++) {
            size_t p = (ny-1)*nx + i;
            gridDistX[p] = gridDistX[p-ny];
            gridDistY[p] = gridDistY[p-ny];
        }
    }
    return MIFI_OK;
}

size_t mifi_compute_vertical_velocity(size_t nx, size_t ny, size_t nz, double dx, double dy, const float* gridDistX, const float* gridDistY, const double* ap, const double* b,
                          const float* zs, const float* ps, const float* u, const float* v, const float* t, float* w)
{
    const double R = MIFI_GAS_CONSTANT / MIFI_MOLAR_MASS_DRY_AIR;  // specific gas constant dry air
    const double g = MIFI_EARTH_GRAVITY; // gravity

    double* mapRatioX = (double*) malloc(nx*ny*sizeof(double));
    double* mapRatioY = (double*) malloc(nx*ny*sizeof(double));
    double* rhx       = (double*) malloc(nx*ny*sizeof(double));
    double* rhy       = (double*) malloc(nx*ny*sizeof(double));
    double* rhxy      = (double*) malloc(nx*ny*sizeof(double));
    double* sum       = (double*) malloc(nx*ny*sizeof(double));
    double* uu        = (double*) malloc(nx*ny*sizeof(double));
    double* vv        = (double*) malloc(nx*ny*sizeof(double));
    double* dp        = (double*) malloc(nx*ny*nz*sizeof(double));
    double* dlnp      = (double*) malloc(nx*ny*nz*sizeof(double));
    double* alfa      = (double*) malloc(nx*ny*nz*sizeof(double));
    double* z         = (double*) malloc(nx*ny*nz*sizeof(double));

    if (mapRatioX == NULL || mapRatioY == NULL || rhx == NULL || rhy == NULL ||
            rhxy == NULL || sum == NULL || uu == NULL || vv == NULL || dp == NULL
            || dlnp == NULL || alfa == NULL || z == NULL) {
        fprintf(stderr, "memory allocation error in mifi_compute_vertical_verlocity\n");
        free(mapRatioX);
        free(mapRatioY);
        free(rhx);
        free(rhy);
        free(rhxy);
        free(sum);
        free(uu);
        free(vv);
        free(dp);
        free(dlnp);
        free(alfa);
        free(z);
        return MIFI_ERROR;
    }

    double rdx_2 = 1/(2*dx);
    double rdy_2 = 1/(2*dy);

    if (MIFI_DEBUG)
        fprintf(stderr, "compute map-factors\n");
    for (size_t j = 0; j < ny; j++) {
        for (size_t i = 0; i < nx; i++) {
            size_t ij = i+nx*j;
            mapRatioX[ij] = gridDistX[ij] / dx; // this is hx in original code
            mapRatioY[ij] = gridDistY[ij] / dy;
            rhx[ij] = 1 / mapRatioX[ij];
            rhy[ij] = 1 / mapRatioY[ij];
            rhxy[ij] = rhx[ij] * rhy[ij];
        }
    }

    if (MIFI_DEBUG)
        fprintf(stderr, "compute half model levels");
    double ah[nz+1], bh[nz+1];
    ah[0]  = 0.0;
    bh[0]  = 0.0;
    ah[nz] = 0.0;
    bh[nz] = 1.0;


    for (size_t k = nz-1; k > 0; --k) {
          ah[k] = 2.0*ap[k]-ah[k+1];
          bh[k] = 2.0*b[k] -bh[k+1];
    }


    if (MIFI_DEBUG)
        fprintf(stderr, "compute pressure variables needed only once\n");
    double ln2= log(2.);
    double da=ah[1]-ah[0];
    double db=bh[1]-bh[0];

    for (size_t j = 0; j < ny; ++j) {
        for (size_t i = 0; i < nx; ++i) {
            dp[j*nx + i]   = da+db*ps[i+j*nx];
            dlnp[j*nx + i] = 0.;
            alfa[j*nx + i] = ln2;
        }
    }
    for (size_t k = 1; k < nz; k++) {
        da = ah[k+1]- ah[k];
        db = bh[k+1]- bh[k];
        if (MIFI_DEBUG)
            fprintf(stderr, "k = %lu, ah = %f, bh= %f, da = %f, db = %f\n", k, ah[k], bh[k], da, db);
        for (size_t j = 0; j < ny; ++j) {
            for (size_t i = 0; i < nx; ++i) {
                double pm = ah[k]   + bh[k]*ps[i+j*nx];
                double pp = ah[k+1] + bh[k+1]*ps[i+j*nx];
                dp[i+nx*(j+ny*k)]   = da + db*ps[i+j*nx];
                dlnp[i+nx*(j+ny*k)] = log(pp/pm);
                alfa[i+nx*(j+ny*k)] = 1.-pm*dlnp[i+nx*(j+ny*k)]/dp[i+nx*(j+ny*k)];
            }
        }
    }
    if (MIFI_DEBUG)
        fprintf(stderr, "k = %lu, ah = %f, bh= %f\n", nz, ah[nz], bh[nz]);

    if (MIFI_DEBUG)
        fprintf(stderr, "vertical integration of hydrostatic equation\n");
    for (size_t j = 0; j < ny; j++) {
        for (size_t i = 0; i < nx; i++) {
            sum[j*nx+i] = zs[j*nx+i]*g;
        }
    }
    for (size_t k = nz-1; k <= 0; k--) {
        for (size_t j = 0; j < ny; j++) {
            for (size_t i = 0; i < nx; i++) {
                size_t ij = i+nx*j;
                size_t ijk = i+nx*(j+k*ny);
                double rt = R*t[ijk];
                z[ijk] = sum[ij] + rt*alfa[ijk];
                sum[ij] += rt*dlnp[ijk];
            }
        }
    }

    if (MIFI_DEBUG)
        fprintf(stderr, "vertical integral of divergence, compute w\n");
    for (size_t j = 0; j < ny; j++) {
        for (size_t i = 0; i < nx; i++) {
             sum[i+nx*j]=0.;
        }
    }
    for (size_t k = 1; k < nz; k++) {
        for (size_t j = 0; j < ny; j++) {
            for (size_t i = 0; i < nx; i++) {
                size_t ij = i+nx*j;
                size_t ijk = i+nx*(j+ny*k);
                uu[ij] = mapRatioY[ij] * u[ijk]*dp[ijk];
                vv[ij] = mapRatioX[ij] * v[ijk]*dp[ijk];
            }
        }
        for (size_t j = 1; j < ny-1; j++) {
            for (size_t i = 1; i < nx-1; i++) {
                size_t ij = i+nx*j;
                size_t ijk = i+nx*(j+ny*k);
                double div = rhxy[ij]*(  rdx_2 * (uu[ij+1]  - uu[ij-1])
                                       + rdy_2 * (vv[ij+nx] - vv[ij-nx]));
                double w1 = R*t[ijk]
                             * (dlnp[ijk]*sum[ij] + alfa[ijk]*div)
                             / dp[ijk];
                double w2 =   rhx[ij] * rdx_2 * (z[ijk+i]  - z[ijk-1])
                            + rhy[ij] * rdy_2 * (z[ijk+nx] - z[ijk-nx]);
                w[ijk] = (w1+w2) / g;
                sum[ij] = sum[ij] + div;
            }
        }
        for (size_t i = 1; i < nx-1; i++) {
            w[i+nx*(0+ny*k)] = w[i+nx*(1+ny*k)];
            w[i+nx*(ny-1+ny*k)] = w[i+nx*(ny-2 + ny*k)];
        }
        for (size_t j = 0; j < ny; j++) {
            w[0+nx*(j+ny*k)] = w[1+nx*(j+ny*k)];
            w[nx-1+nx*(j+ny*k)] = w[nx-2+nx*(j+ny*k)];
        }
    }
    free(mapRatioX);
    free(mapRatioY);
    free(rhx);
    free(rhy);
    free(rhxy);
    free(sum);
    free(uu);
    free(vv);
    free(dp);
    free(dlnp);
    free(alfa);
    free(z);
    return MIFI_OK;
}

size_t mifi_bad2nanf(float* posPtr, float* endPtr, float badVal) {
    if (!isnan(badVal)) {
        while (posPtr != endPtr) {
            *posPtr = (*posPtr == badVal) ? MIFI_UNDEFINED_F : *posPtr;
            posPtr++;
        }
    }
    return 0;
}

size_t mifi_nanf2bad(float* posPtr, float* endPtr, float badVal) {
    if (!isnan(badVal)) {
        while (posPtr != endPtr) {
            *posPtr = (isnan(*posPtr)) ? badVal : *posPtr;
            posPtr++;
        }
    }
    return 0;
}

int mifi_isnanf(float val)
{
    return isnan(val);
}

int mifi_isnand(double val)
{
    return isnan(val);
}
