#ifndef INTERPOLATION_H_
#define INTERPOLATION_H_

#include <proj_api.h>
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MIUP_NEAREST_NEIGHBOR 0
#define MIUP_BILINEAR         1
#define MIUP_BICUBIC          2

#define MIUP_UNDEFINED_F (nanf(""))
#define MIUP_UNDEFINED_D (nand(""))

#define MIUP_ERROR -1
#define MIUP_OK 1

#define MIUP_PROJ_AXIS 0
#define MIUP_LONGITUDE 1
#define MIUP_LATITUDE 2

#define MIUP_DEBUG 1

/**
 * Interpolation between two projections. Missing values are set to MIUP_UNDEFINED_F
 * which is implemented as C99 nanf. The coordinates of a cell give the midpoint of a cell,
 * i.e. cell (10,20) spans  ([9.5..10.5[,[19.5-20.5[)
 * 
 * @param method one of MIUP_NEAREST_NEIGHBOR MIUP_BILINEAR MIUP_BICUBIC
 * @param proj_input proj4-string of projection of infield
 * @param infield real rectangular array of dimension infield[iz,iy,ix]  
 * @param in_x_axis field of size ix. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param in_y_axis field of size iy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param in_x_axis_type one of MIUP_LATITUDE, MIUP_LONGITUDE, MIUP_PROJ_AXIS
 * @param in_y_axis_type one of MIUP_LATITUDE, MIUP_LONGITUDE, MIUP_PROJ_AXIS
 * @param ix x-dimension of infield
 * @param iy y-dimension of infield
 * @param iz z-dimension of infield and outfield. The z-dim allows you to convert several fields at once without calculating the projection again and again.
 * @param proj_output proj4-string of projection of outfield
 * @param outfield real rectangular array of dimension outfield[iz,oy,ox]
 * @param out_x_axis field of size ox. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_y_axis field of size oy. Axis needs to be strong monotonous and if longitude/latitude in degree
 * @param out_x_axis_type one of MIUP_LATITUDE, MIUP_LONGITUDE, MIUP_PROJ_AXIS
 * @param out_y_axis_type one of MIUP_LATITUDE, MIUP_LONGITUDE, MIUP_PROJ_AXIS
 * @param ox x-dimension of outfield
 * @param oy y-dimension of outfield
 */
extern int miup_interpolate_f(int method,
                        const char* proj_input, const float* infield, const double* in_x_axis, const double* in_y_axis, 
                        const int in_x_axis_type, const int in_y_axis_type, const int ix, const int iy, const int iz,
                        const char* proj_output, float* outfield, const double* out_x_axis, const double* out_y_axis, 
                        const int out_x_axis_type, const int out_y_axis_type, const int ox, const int oy);

/**
 * @brief not implemented yet
 * 
 * double version of miup_interpolate_f
 * @see miup_interpolate_f
 */
extern int miup_interpolate_d(int method,
                        char* proj_input, double* infield, double* in_x_axis, double* in_y_axis, 
                        int in_x_axis_type, int in_y_axis_type, int ix, int iy, int iz,
                        char* proj_output, double* outfield, double* out_x_axis, double* out_y_axis, 
                        int out_x_axis_type, int out_y_axis_type, int ox, int oy);
                        
/** 
 * @param infield 3d fortran array of size ix,iy,iz
 * @param outfield 1d array of size iz containing the values
 */
extern int miup_get_values_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);

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
extern int miup_get_values_bilinear_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);


/**
 * @brief not implemented yet
 * 
 * @see http://java.sun.com/products/java-media/jai/forDevelopers/jai-apidocs/javax/media/jai/InterpolationBicubic.html
 */
extern int miup_get_values_bicubic_f(const float* infield, float* outvalues, const double x, const double y, const int ix, const int iy, const int iz);                        
                        
              
/**
 *  @brief find position in array of position in projection
 * 
 * points2position uses linear splines to find the array-position of points in the given axis 
 * 
 *  @param points the values will get changed from points in axis coordinates to array coordinates
 *  @param n number of values in points
 *  @param axis coordinate axis
 *  @param num number of elements in coordinate axis
 *  @param axis_type type of axis, one of MIUP_LONGITUDE, MIUP_LATITUDE, MIUP_PROJ_AXIS
 */
extern int miup_points2position(double* points, const int n, const double* axis, const int num, const int axis_type); 
       
       
/**
 * gives the position of an fortran like array of size ix, iy, iz
 * 
 *  @return the position of x, y, z
 */
extern inline int miup_3d_array_position(int x, int y, int z, int ix, int iy, int iz)
{
	return (z*iy + y)*ix + x;
}


                                     
#ifdef __cplusplus
}
#endif


#endif /*INTERPOLATION_H_*/
