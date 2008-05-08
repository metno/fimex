#ifndef CDMINTERPOLATOR_H_
#define CDMINTERPOLATOR_H_

#include "CDMReader.h"
#include "CachedInterpolation.h"

namespace MetNoFimex
{

class CDMInterpolator : public MetNoFimex::CDMReader
{
private: 
	boost::shared_ptr<CDMReader> dataReader;
	std::vector<std::string> projectionVariables;
	CachedInterpolation cachedInterpolation;
	std::string latitudeName;
	std::string longitudeName;
	
public:
	CDMInterpolator(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMInterpolator();
	virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);
	/**
	 * @ brief change the (main) projection of the dataReaders cdm to this new projection
	 * 
	 * @param method Interpolation method 
	 */
	virtual void changeProjection(int method, const std::string& proj_input, const std::vector<double>& out_x_axis, const std::vector<double>& out_y_axis, const std::string& out_x_axis_unit, const std::string& out_y_axis_unit) throw(CDMException);
	/**
	 * set the name for the automatically generated latitude coordinate axis. This must be set before changeProjection is called.
	 * @param latName name for latitude
	 */
	virtual void setLatitudeName(const std::string& latName) {this->latitudeName = latName;}
	/**
	 * @return the name used for latitude in the automatic coordinate generation
	 */
	virtual const std::string& getLatitudeName() const {return latitudeName;}
	/**
	 * set the name for the automatically generated longitude coordinate axis. This must be set before changeProjection is called.
	 * @param latName name for longitude
	 */
	virtual void setLongitudeName(const std::string& lonName) {this->longitudeName = lonName;}
	/**
	 * @return the name used for longitude in the automatic coordinate generation
	 */
	virtual const std::string& getLongitudeName() const {return longitudeName;}
};

}

#endif /*CDMINTERPOLATOR_H_*/
