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

static int ascendingDoubleComparator(const void * a, const void * b)
{
	double x = *(double*)a;
	double y = *(double*)b;
	if (x == y) {}
	if ( *(double*)a >  *(double*)b ) return 1;
    else if ( *(double*)a == *(double*)b ) return 0;
    else return -1;
}

static int descendingDoubleComparator(const void * a, const void * b)
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

int mifi_3d_array_position(int x, int y, int z, int ix, int iy, int iz) {
	return (z*iy + y)*ix + x;
}


int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type)
{
	int (*comparator)(const void * a, const void * b);
	if (axis[0] < axis[num-1]) comparator = ascendingDoubleComparator;
	else comparator = descendingDoubleComparator;

	if (axis_type == MIFI_LONGITUDE) {
		// decide if longitude axis is -180 to 180
		if (axis[0] < 0 || axis[num-1] < 0) {
			// change points > 180
			for (int i = 0; i < n; i++) {
				if (points[i] > PI) points[i] -= 2*PI;
			}
		} else {
			// change negative points
			for (int i = 0; i < n; i++) {
				if (points[i] < 0) points[i] += 2*PI;
			}
		}
	}

	for (int i = 0; i < n; i++) {
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
		default: memcpy(outAxis, orgAxis, num * sizeof(double));
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


	double* in_xproj_axis = malloc(ox*oy*sizeof(double));
	double* in_yproj_axis = malloc(ox*oy*sizeof(double));
	if (in_xproj_axis == NULL || in_yproj_axis == NULL) {
		fprintf(stderr, "error allocating memory of double(%d*%d)", ox, oy);
		exit(1);
	}
	double pointsZ[ox*oy]; // z currently of no interest, no height attached to values
    for (int y = 0; y < oy; ++y) {
        for (int x = 0; x < ox; ++x) {
			in_xproj_axis[y*ox +x] = outXAxis[x];
			in_yproj_axis[y*ox +x] = outYAxis[y];
			pointsZ[y*ox +x] = 0;
		}
	}

	// getting positions in the original projection
	if (pj_transform(outputPJ, inputPJ, ox*oy, 0, in_xproj_axis, in_yproj_axis, pointsZ) != 0) {
		fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
		pj_free(inputPJ);
		pj_free(outputPJ);
		free(in_yproj_axis);
		free(in_xproj_axis);
		return MIFI_ERROR;
	}

	// calculation of deltas: (x+d, y), (x, y+d) -> proj-values
	double* out_x_delta_proj_axis = malloc(ox*oy*sizeof(double));
	double* out_y_delta_proj_axis = malloc(ox*oy*sizeof(double));
	double* delta = malloc(ox*oy*sizeof(double)); // dynamic delta
	if (out_x_delta_proj_axis == NULL || out_y_delta_proj_axis == NULL || delta == NULL) {
		fprintf(stderr, "error allocating memory of double(%d*%d)", ox, oy);
		exit(1);
	}
	{// conversion along x axis
		delta[0] = 1e-3; // will be overwritten if x > 1
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
				// find a delta <<< than the diagonal to the next cell
				if (x < (ox-1) && y < (oy-1)) {
					delta[y*ox+x] = in_xproj_axis[(y+1)*ox +(x+1)] - in_xproj_axis[y*ox +x] * 1e-3;
				} else if (x < (ox-1)) {
					delta[y*ox+x] = in_xproj_axis[y*ox +(x+1)] - in_xproj_axis[y*ox +x] * 1e-3;
				} else if (y < (oy-1)){
					delta[y*ox+x] = in_xproj_axis[(y+1)*ox +x] - in_xproj_axis[y*ox +x] * 1e-3;
				} else {
					delta[y*ox+x] = delta[y*ox+x-1];
				}
				out_x_delta_proj_axis[y*ox +x] = in_xproj_axis[y*ox +x] + delta[y*ox+x];
				out_y_delta_proj_axis[y*ox +x] = in_yproj_axis[y*ox +x];
				pointsZ[y*ox +x] = 0;
			}
		}
		if (pj_transform(inputPJ, outputPJ, ox*oy, 0, out_x_delta_proj_axis,
				out_y_delta_proj_axis, pointsZ) != 0) {
			fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
			pj_free(inputPJ);
			pj_free(outputPJ);
			free(in_yproj_axis);
			free(in_xproj_axis);
			free(delta);
			free(out_y_delta_proj_axis);
			free(out_x_delta_proj_axis);
			return MIFI_ERROR;
		}

        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
				double deltaInv = 1/delta[y*ox+x];
				matrix[mifi_3d_array_position(0,x,y,4,ox,oy)] = (out_x_delta_proj_axis[y*ox+x]
						- outXAxis[x]) * deltaInv;
				matrix[mifi_3d_array_position(1,x,y,4,ox,oy)] = (out_y_delta_proj_axis[y*ox+x]
						- outYAxis[y]) * deltaInv;
			}
		}
	}

	{	// conversion along y axis
		delta[0] = 1e-3; // will be overwritten if x > 1
        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
				// find a delta <<< than the diagonal to the next cell
				if (x < (ox-1) && y < (oy-1)) {
					delta[y*ox+x] = in_yproj_axis[(y+1)*ox +(x+1)] - in_yproj_axis[y*ox +x] * 1e-3;
				} else if (x < (ox-1)) {
					delta[y*ox+x] = in_yproj_axis[y*ox +(x+1)] - in_yproj_axis[y*ox +x] * 1e-3;
				} else if (y < (oy-1)){
					delta[y*ox+x] = in_yproj_axis[(y+1)*ox +x] - in_yproj_axis[y*ox +x] * 1e-3;
				} else {
					delta[y*ox+x] = delta[y*ox+x-1];
				}
				out_x_delta_proj_axis[y*ox +x] = in_xproj_axis[y*ox +x];
				out_y_delta_proj_axis[y*ox +x] = in_yproj_axis[y*ox +x] + delta[y*ox+x];
				pointsZ[y*ox +x] = 0;
			}
		}
		if (pj_transform(inputPJ, outputPJ, ox*oy, 0, out_x_delta_proj_axis,
				out_y_delta_proj_axis, pointsZ) != 0) {
			fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
			pj_free(inputPJ);
			pj_free(outputPJ);
			free(in_yproj_axis);
			free(in_xproj_axis);
			free(delta);
			free(out_y_delta_proj_axis);
			free(out_x_delta_proj_axis);
			return MIFI_ERROR;
		}

        for (int y = 0; y < oy; ++y) {
            for (int x = 0; x < ox; ++x) {
				double deltaInv = 1/delta[y*ox+x];
				matrix[mifi_3d_array_position(2,x,y,4,ox,oy)] = (out_x_delta_proj_axis[y*ox+x]
						- outXAxis[x]) * deltaInv;
				matrix[mifi_3d_array_position(3,x,y,4,ox,oy)] = (out_y_delta_proj_axis[y*ox+x]
						- outYAxis[y]) * deltaInv;
				//fprintf(stderr, "Proj matrix: %d %d: %f %f %f %f\n", x, y, matrix[mifi_3d_array_position(x,y,0,ox,oy,4)], matrix[mifi_3d_array_position(x,y,1,ox,oy,4)], matrix[mifi_3d_array_position(x,y,2,ox,oy,4)], matrix[mifi_3d_array_position(x,y,3,ox,oy,4)]);
			}
		}
	}
	pj_free(inputPJ);
	pj_free(outputPJ);
	free(in_yproj_axis);
	free(in_xproj_axis);
	free(delta);
	free(out_y_delta_proj_axis);
	free(out_x_delta_proj_axis);
	return MIFI_OK;
}

int mifi_vector_reproject_values_by_matrix_f(int method,
						const double* matrix,
						float* u_out, float* v_out,
						int ox, int oy, int oz)
{
	/*forks off the threads*/
	int layerSize = ox*oy;
	{
		for (int z = 0; z < oz; ++z) {
			const double *matrixPos = matrix; // reset matrix for each z
			float *uCur = &u_out[z*layerSize]; // current layer of u (cannot use global because of omp)
			float *vCur = &v_out[z*layerSize]; // current layer of v (cannot use global because of omp)
			double m0, m1, m2, m3, u_old, v_old, u_new, v_new;
			// loop over one layer: calc uv' = A*uv at each pos
			for (int i = 0; i < layerSize; i++) {
				m0 = *matrixPos++;
				m1 = *matrixPos++;
				m2 = *matrixPos++;
				m3 = *matrixPos++;
				u_old = *uCur;
				v_old = *vCur;
				u_new = u_old * m0 + v_old * m1;
				v_new = u_old * m2 + v_old * m3;
				if (method == MIFI_VECTOR_KEEP_SIZE) {
					double norm = sqrt( (u_old*u_old + v_old*v_old) /
										(u_new*u_new + v_new*v_new) );
					u_new *= norm;
					v_new *= norm;
				}
				*uCur++ = u_new;
				*vCur++ = v_new;
			}
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
	int rx = (int) round(x);
	int ry = (int) round(y);
	if (((rx >= 0) && (rx < ix)) &&
		((ry >= 0) && (ry < iy))) { // pos in range
		for (int z = 0; z < iz; ++z) {
			outvalues[z] = infield[mifi_3d_array_position(rx,ry,z,ix,iy,iz)];
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
	int x0 = floor(x);
	int x1 = ceil(x);
	double xfrac = x - x0;
	int y0 = floor(y);
	int y1 = ceil(y);
	double yfrac = y - y0;
	if (((0 <= x0) && (x0 < ix)) &&
		((0 <= y0) && (y0 < iy)) &&
		((0 <= x1) && (x1 < ix)) &&
		((0 <= y1) && (y1 < iy))) { // pos in range

		for (int z = 0; z < iz; ++z) {
			float s00 = infield[mifi_3d_array_position(x0, y0, z, ix, iy, iz)];
			float s01 = infield[mifi_3d_array_position(x1, y0, z, ix, iy, iz)];
			float s10 = infield[mifi_3d_array_position(x0, y1, z, ix, iy, iz)];
			float s11 = infield[mifi_3d_array_position(x1, y1, z, ix, iy, iz)];
			// Missing values: NANs will be propagated by IEEE
			outvalues[z] = (1 - yfrac) * ((1 - xfrac)*s00 + xfrac*s01) +
							yfrac      * ((1 - xfrac)*s10 + xfrac*s11);
		}
	} else {
		for (int z = 0; z < iz; ++z) {
			outvalues[z] = MIFI_UNDEFINED_F;
		}
	}

	return MIFI_OK;
}

int mifi_get_values_bicubic_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz)
{
	// TODO: better tests
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

void mifi_get_values_linear_f(const float* infieldA, const float* infieldB, float* outfield, const size_t n, const double a, const double b, const double x)
{
	const double f = (a == b) ? 0 :  ((x - a) / (b - a));
	int i = 0;
	while (n > i++) {
		float iA = *infieldA++;
		float iB = *infieldB++;
		float* o = outfield++;
		*o = iA + f * (iB - iA);
	}
	return;
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
        fprintf(stderr, "error allocating memory of float(%d*%d)", nx, ny);
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
        fprintf(stderr, "error allocating memory of float(%d*%d)", nx, ny);
        exit(1);
    }
    // start the iteration loop
    for (size_t n = 0; n < maxLoop; n++) {
        for (size_t y = 1; y < nym1; y++) {
            for (size_t x = 1; x < nxm1; x++) {
                eField[y*nx+x] = (field[y*nx+(x+1)] + field[y*nx+(x-1)] + field[(y+1)*nx+x] + field[(y-1)*nx+x])*0.25 - field[y*nx+x];
                field[y*nx+x] += eField[y*nx+x] * wField[y*nx+x];
            }
        }

        // Test convergence now and then (slow test loop)
        if ((n < (maxLoop-5)) &&
            (n%10 == 0)) {
            float crtest = crit*corrEff;
            size_t nbad = 0;
            for (size_t y = 1; y < nym1; y++) {
                if (nbad != 0) break;
                for (size_t x = 1; x < nxm1; x++) {
                    if (fabs(eField[y*nx+x])*wField[y*nx+x] > crtest) {
                        nbad = 1;
                    }
                }
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

size_t mifi_bad2nanf(float* posPtr, float* endPtr, float badVal) {
	size_t retVal = 0;
	if (!isnan(badVal)) {
		while (posPtr != endPtr) {
			if (*posPtr == badVal) {
				*posPtr = MIFI_UNDEFINED_F;
				retVal++;
			}
			posPtr++;
		}
	}
	return retVal;
}

size_t mifi_nanf2bad(float* posPtr, float* endPtr, float badVal) {
	size_t retVal = 0;
	if (!isnan(badVal)) {
		while (posPtr != endPtr) {
			if (isnan(*posPtr)) {
				*posPtr = badVal;
				retVal++;
			}
			posPtr++;
		}
	}
	return retVal;
}

int mifi_isnanf(float val)
{
    return isnan(val);
}

int mifi_isnand(double val)
{
    return isnan(val);
}
