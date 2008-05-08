#ifndef DOCUMENTATION_H_
#define DOCUMENTATION_H_

/*! @mainpage Fimex User Documentation
 * 
 * @section setup Setup Files
 * @subsection fimex_config Fimex Configuration
 * @subsection felt_config Configuration files for felt reader
 * 
 * The xml configuration files are defined by the /felt2nc_variables.dtd/
 * definition. Since part of this configuration are quite stable, e.g.
 * the axes (time, level, lat, lon, x, y), other parts change, e.g.
 * the variables to translate change very often. It is therefore useful
 * to split the variables from the rest of the configuration via /xinclude/
 * 
 * Before running fimex with a new felt configuration, make sure the file
 * is valid, e.g. with
 * @code
 * xmllint --valid --noout felt2nc_config.xml
 * @endcode
 * 
 * Unfortuneatly, xinclude and validation don't play well together, since
 * usual validation happens before the inclusion of external parts. xmllint
 * uses special options to fix those problem:
 * @code
 * xmllint --xinclude --postvalid --noout felt2nc_config.xml
 * @endcode
 *  
 * 
 * 
 */

#endif /*DOCUMENTATION_H_*/
