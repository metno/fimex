#include "interpolation.h"
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

int mifi_3d_array_position(int x, int y, int z, int ix, int iy, int iz)
{
	return (z*iy + y)*ix + x;
}


int mifi_points2position(double* points, const int n, const double* axis, const int num, const int axis_type) 
{
	int (*comparator)(const void * a, const void * b);
	if (axis[0] < axis[num-1]) comparator = ascendingDoubleComparator;
	else comparator = descendingDoubleComparator;
	
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
	for (int x = 0; x < ox; ++x) {
		for (int y = 0; y < oy; ++y) {
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
		case MIFI_NEAREST_NEIGHBOR: func = mifi_get_values_f; break;
		case MIFI_BILINEAR:         func = mifi_get_values_bilinear_f; break;
		case MIFI_BICUBIC:          ;//not implemented: func = mifi_get_values_bicubic_f; break;
		default:                    return MIFI_ERROR; /* error */
	}
	
	return mifi_interpolate_f_functional(func, proj_input, infield, in_x_axis, in_y_axis, in_x_axis_type, in_y_axis_type, ix, iy, iz, proj_output, outfield, out_x_axis, out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy);
}

int mifi_get_delta_matrix_f(const char* proj_input, 
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
	
	
	double in_xproj_axis[ox*oy];
	double in_yproj_axis[ox*oy];
	double pointsZ[ox*oy]; // z currently of no interest, no height attached to values
	for (int x = 0; x < ox; ++x) {
		for (int y = 0; y < oy; ++y) {
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
		return MIFI_ERROR;
	}
	
	// calculation of deltas: (x+d, y), (x, y+d) -> proj-values
	{// conversion along x axis
		double out_x_delta_proj_axis[ox*oy];
		double out_y_delta_proj_axis[ox*oy];
		double delta[ox*oy]; // dynamic delta
		delta[0] = 1e-3; // will be overwritten if x > 1
		for (int x = 0; x < ox; ++x) {
			for (int y = 0; y < oy; ++y) {
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
			return MIFI_ERROR;
		}

		for (int x = 0; x < ox; ++x) {
			for (int y = 0; y < oy; ++y) {
				double deltaInv = 1/delta[y*ox+x];
				matrix[mifi_3d_array_position(x,y,0,ox,oy,4)] = (out_x_delta_proj_axis[y*ox+x]
						- outXAxis[x]) * deltaInv;
				matrix[mifi_3d_array_position(x,y,1,ox,oy,4)] = (out_y_delta_proj_axis[y*ox+x]
						- outYAxis[y]) * deltaInv;
			}
		}
	}
	
	{	// conversion along y axis
		double out_x_delta_proj_axis[ox*oy];
		double out_y_delta_proj_axis[ox*oy];
		double delta[ox*oy]; // dynamic delta
		delta[0] = 1e-3; // will be overwritten if x > 1
		for (int x = 0; x < ox; ++x) {
			for (int y = 0; y < oy; ++y) {
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
			return MIFI_ERROR;
		}

		for (int x = 0; x < ox; ++x) {
			for (int y = 0; y < oy; ++y) {
				double deltaInv = 1/delta[y*ox+x];
				matrix[mifi_3d_array_position(x,y,2,ox,oy,4)] = (out_x_delta_proj_axis[y*ox+x]
						- outXAxis[x]) * deltaInv;
				matrix[mifi_3d_array_position(x,y,3,ox,oy,4)] = (out_y_delta_proj_axis[y*ox+x]
						- outYAxis[y]) * deltaInv;
				//fprintf(stderr, "Proj matrix: %d %d: %f %f %f %f\n", x, y, matrix[mifi_3d_array_position(x,y,0,ox,oy,4)], matrix[mifi_3d_array_position(x,y,1,ox,oy,4)], matrix[mifi_3d_array_position(x,y,2,ox,oy,4)], matrix[mifi_3d_array_position(x,y,3,ox,oy,4)]);
			}
		}
	}
	pj_free(inputPJ);
	pj_free(outputPJ);
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
	double matrix[ox*oy*4];
	// calculate the positions in the original proj.
	int errcode = mifi_get_delta_matrix_f(proj_input, proj_output, out_x_axis, out_y_axis, out_x_axis_type, out_y_axis_type, ox, oy, matrix);
	if (errcode != MIFI_OK) return errcode;
	for (int x = 0; x < ox; ++x) {
		for (int y = 0; y < oy; ++y) {
			for (int z = 0; z < oz; ++z) {
				int pos = mifi_3d_array_position(x,y,z,ox,oy,oz);
				double u_old = u_out[pos];
				double v_old = v_out[pos];
				u_out[pos] = u_old * matrix[mifi_3d_array_position(x,y,0,ox,oy,4)] +
							 v_old * matrix[mifi_3d_array_position(x,y,1,ox,oy,4)];
				v_out[pos] = u_old * matrix[mifi_3d_array_position(x,y,2,ox,oy,4)] +
				 			 v_old * matrix[mifi_3d_array_position(x,y,3,ox,oy,4)];
				if (method == MIFI_VECTOR_KEEP_SIZE) {
					double norm = sqrt(u_old*u_old + v_old*v_old) /
						   		  sqrt(u_out[pos]*u_out[pos] + v_out[pos]*v_out[pos]);
					u_out[pos] *= norm;
					v_out[pos] *= norm;
				}
			}
		}
	}
	return MIFI_OK;
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
	// TODO implement and reset error value to 0
	return 1;
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
	double pointsZ[ix*iy]; // z currently of no interest, no height attached to values
	for (int x = 0; x < ix; ++x) {
		for (int y = 0; y < iy; ++y) {
			out_xproj_axis[y*ix +x] = in_x_axis[x];
			out_yproj_axis[y*ix +x] = in_y_axis[y];
			pointsZ[y*ix +x] = 0;
		}
	}

	// transforming
	if (pj_transform(inputPJ, outputPJ, ix*iy, 0, out_xproj_axis, out_yproj_axis, pointsZ) != 0) {
		fprintf(stderr, "Proj error:%d %s", pj_errno, pj_strerrno(pj_errno));
		pj_free(inputPJ);
		pj_free(outputPJ);
		return MIFI_ERROR;
	}
	pj_free(inputPJ);
	pj_free(outputPJ);
	return MIFI_OK;
}
