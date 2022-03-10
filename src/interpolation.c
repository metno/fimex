/*
 * Fimex
 *
 * (C) Copyright 2008-2022, met.no
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "fimex/CDMconstants.h"

#if defined(__GNUC__) && !defined(__INTEL_COMPILER) && !defined(__clang__)
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

#ifndef M_E
static const double M_E = 2.7182818284590452354; /* e */
#endif

static const double HALF_CIRCLE_ANGLE = 180;

#define DEG_TO_RAD (M_PI / 180)
#define RAD_TO_DEG (180 / M_PI)
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

int ascendingDoubleComparator(double a, double b)
{
    if (a == b)
        return 0;
    else if (a > b)
        return 1;
    else
        return -1;
}

int descendingDoubleComparator(double a, double b)
{
    return -1 * ascendingDoubleComparator(a,b);
}

/*
 * this works similar to bsearch in stdlib.h, except that it returns the index
 * of the found element rather than the element
 * In addition, it returns -1 + (-1 * (smallest element > key)) if key cannot be found
 */
int bsearchDoubleIndex(const double key, const double* base, int num, int (*comparator)(double, double))
{
    int first = 0;
    int last = num - 1;
    int pos = 0;
    int comp = 0;
    while (first <= last) {
        pos = (first + last)/2;
        comp = comparator(key, base[pos]);
        if (comp == 0) {
            break;
        } else if (comp > 0) {
            first = pos + 1;
        } else {
            last = pos - 1;
        }
    }
    if (comp == 0)
        return pos;
    else if (comp > 0)
        return (-1 + (-1 * (pos + 1)));
    else
        return (-1 + (-1 * pos));
}

int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type)
{
    int circularLongitude = 0;
    int (*comparator)(double a, double b);
    if (axis[0] < axis[num - 1])
        comparator = ascendingDoubleComparator;
    else
        comparator = descendingDoubleComparator;

    if (axis_type == MIFI_LONGITUDE) {
        // decide if longitude axis is -180 to 180
        if (axis[0] < 0 || axis[num-1] < 0) {
            // change points > 180
            for (int i = 0; i < n; i++) {
                if (points[i] > HALF_CIRCLE_ANGLE)
                    points[i] -= 2 * HALF_CIRCLE_ANGLE;
            }
        } else {
            // change negative points (0-360)
            for (int i = 0; i < n; i++) {
                if (points[i] < 0)
                    points[i] += 2 * HALF_CIRCLE_ANGLE;
            }
        }
        double nextOnAxis = axis[num-1] + (axis[1]-axis[0])*1.01; // slightly enlarged gitter-distance to avoid numerics
        if (axis[0] < axis[num-1]) { // ascending
            nextOnAxis -= 2 * HALF_CIRCLE_ANGLE;
            if (nextOnAxis >= axis[0]) {
                circularLongitude = 1;
            }
        } else { // descending
            nextOnAxis += 2 * HALF_CIRCLE_ANGLE;
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

static inline int mifi_3d_array_pos(int x, int y, int z, int ix, int iy, int iz)
{
    (void)iz; // suppress compiler warning
    return (z*iy + y)*ix + x;
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
static inline double mifi_great_circle_angle(double lat0, double lon0, double lat1, double lon1)
{
    return acos(sin(lat0)*sin(lat1) + cos(lat0)*cos(lat1)*cos(lon1-lon0));
}

int mifi_get_values_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz)
{
    int rx = lround(x);
    int ry = lround(y);
    if (rx >= 0 && rx < ix && ry >= 0 && ry < iy) { // pos in range
        for (int z = 0; z < iz; ++z) {
            outvalues[z] = infield[mifi_3d_array_pos(rx,ry,z,ix,iy,iz)];
        }
    } else {
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
            size_t pos = mifi_3d_array_pos(x0, y0, 0, ix, iy, iz);
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
                size_t pos = mifi_3d_array_pos(x0, y0, 0, ix, iy, iz);
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
                size_t pos = mifi_3d_array_pos(x0, y0, 0, ix, iy, iz);
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
                    size_t pos = mifi_3d_array_pos(x0, y0, 0, ix, iy, iz);
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
                    F[i][j] = infield[mifi_3d_array_pos(x0+i-1, y0+j-1, z, ix, iy, iz)];
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
    (void)infieldB;
    (void)a;
    (void)b;
    (void)x;

    memcpy(outfield, infieldA, n * sizeof(float));
    return MIFI_OK;
}

//o(x) = in(a) + (x - a) * (in(b) - in(a)) / (b - a)
//b = o(a)
static void mifi_get_values_linear_f_simple_(const float* infieldA, const float* infieldB, float* outfield, const size_t n, float f)
{
    size_t i = 0;
    while (n > i++) {
        float iA = *infieldA++;
        float iB = *infieldB++;
        float* o = outfield++; // position!
        *o = iA + f * (iB - iA);
    }
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
        size_t i = 0;
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
        size_t i = 0;
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
    return mifi_get_values_linear_conf_extrapol_f(0., 1., infieldA, infieldB, outfield, n, a, b, x);
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

int mifi_fill2d_f(size_t nx, size_t ny, float* field, float relaxCrit, float corrEff, size_t maxLoop, size_t* nChanged) {
    size_t totalSize = nx*ny;
    if (totalSize == 0) return MIFI_OK;

    double sum = 0;
    *nChanged = 0;

    float* fieldPos = field;
    // calculate sum and number of valid values
    for (size_t i = 0; i < totalSize; ++i, ++fieldPos) {
        if (isnan(*fieldPos)) {
            (*nChanged)++;
        } else {
            sum += *fieldPos;
        }
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
    for (size_t i = 0; i < totalSize; ++i) {
        if (isnan(*fieldPos)) {
            *wFieldPos = 1.;
            *fieldPos = average;
        } else {
            stddev += fabs(*fieldPos - average);
            *wFieldPos = 0.;
        }
        wFieldPos++;
        fieldPos++;
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
    for (size_t i = 0; i < totalSize; ++i) {
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
    }

    // starting the iterative method, border-values are left at defaultVal, nx,ny >=1
    size_t nxm1 = (size_t)(nx - 1);
    size_t nym1 = (size_t)(ny - 1);

    // and the loop, with a maximum of nUnchanged rounds
    size_t l = 0;
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
    // calculate sum and number of valid values
    for (size_t i = 0; i < totalSize; ++i, ++fieldPos) {
        if (isnan(*fieldPos)) {
            (*nChanged)++;
        } else {
            sum += *fieldPos;
        }
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
    // calculate sum and number of valid values
    for (size_t i = 0; i < totalSize; ++i) {
        if (isnan(*fieldPos++)) {
            (*nChanged)++;
        }
    }
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
        // last row, overwriting corner
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
            size_t ij = i+nx*j;
            dp[ij]   = da+db*ps[ij];
            dlnp[ij] = 0.;
            alfa[ij] = ln2;
        }
    }
    for (size_t k = 1; k < nz; k++) {
        if (MIFI_DEBUG)
            fprintf(stderr, "k = %lu, ah = %f, bh= %f\n", k, ah[k], bh[k]);
        for (size_t j = 0; j < ny; ++j) {
            for (size_t i = 0; i < nx; ++i) {
                size_t ij = i+j*nx, ijk = ij + nx*ny*k;
                double pm = ah[k]   + bh[k]  *ps[ij];
                double pp = ah[k+1] + bh[k+1]*ps[ij];
                dp[ijk]   = pp - pm;
                dlnp[ijk] = log(pp/pm);
                alfa[ijk] = 1.-pm*dlnp[ijk]/dp[ijk];
            }
        }
    }
    if (MIFI_DEBUG)
        fprintf(stderr, "k = %lu, ah = %f, bh= %f\n", nz, ah[nz], bh[nz]);

    if (MIFI_DEBUG)
        fprintf(stderr, "vertical integration of hydrostatic equation\n");
    for (size_t j = 0; j < ny; j++) {
        for (size_t i = 0; i < nx; i++) {
            size_t ij = i+nx*j;
            sum[ij] = zs[ij]*g;
        }
    }
    for (int k = nz-1; k >= 0; k--) {
        for (size_t j = 0; j < ny; j++) {
            for (size_t i = 0; i < nx; i++) {
                size_t ij = i+nx*j, ijk = ij + nx*ny*k;
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
            size_t ij = i+nx*j;
            sum[ij] = 0;
            w[ij] = 0;
        }
    }
    for (size_t k = 1; k < nz; k++) {
        for (size_t j = 0; j < ny; j++) {
            for (size_t i = 0; i < nx; i++) {
                size_t ij = i+nx*j, ijk = ij + nx*ny*k;
                uu[ij] = mapRatioY[ij] * u[ijk]*dp[ijk];
                vv[ij] = mapRatioX[ij] * v[ijk]*dp[ijk];
            }
        }
        for (size_t j = 1; j < ny-1; j++) {
            for (size_t i = 1; i < nx-1; i++) {
                size_t ij = i+nx*j, ijk = ij + nx*ny*k;
                double div = rhxy[ij]*(  rdx_2 * (uu[ij+1]  - uu[ij-1])
                                       + rdy_2 * (vv[ij+nx] - vv[ij-nx]));
                double w1 = R*t[ijk]
                             * (dlnp[ijk]*sum[ij] + alfa[ijk]*div)
                             / dp[ijk];
                double w2 =   rhx[ij] * rdx_2 * (z[ijk+1]  - z[ijk-1])
                            + rhy[ij] * rdy_2 * (z[ijk+nx] - z[ijk-nx]);
                w[ijk] = (w1+w2) / g;
                sum[ij] = sum[ij] + div;
            }
        }
        for (size_t i = 1; i < nx-1; i++) {
            size_t i0k = i+nx*(0+ny*k), i1k = i0k + nx;
            size_t im1k = i+nx*(ny-1+ny*k), im2k = im1k - nx;
            w[i0k] = w[i1k];
            w[im1k] = w[im2k];
        }
        for (size_t j = 0; j < ny; j++) {
            size_t j0k = nx*(j+ny*k), j1k = j0k + 1;
            size_t jm1k = nx-1+nx*(j+ny*k), jm2k = jm1k - 1;
            w[j0k] = w[j1k];
            w[jm1k] = w[jm2k];
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

size_t mifi_nand2bad(double* posPtr, double* endPtr, double badVal) {
    if (!isnan(badVal)) {
        while (posPtr != endPtr) {
            *posPtr = (isnan(*posPtr)) ? badVal : *posPtr;
            posPtr++;
        }
    }
    return 0;
}
