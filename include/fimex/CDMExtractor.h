#ifndef CDMEXTRACTOR_H_
#define CDMEXTRACTOR_H_

#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>

#include "CDMReader.h"

namespace MetNoFimex
{

class CDMExtractor : public MetNoFimex::CDMReader
{
private:
	boost::shared_ptr<CDMReader> dataReader;
	typedef std::map<std::string, boost::array<size_t, 2> > DimChangeMap; 
	DimChangeMap dimChanges;
	/**
	 * all extractors need to have another Reader with input-data
	 */
	CDMExtractor();

public:
	CDMExtractor(boost::shared_ptr<CDMReader> dataReader);
	virtual ~CDMExtractor();
	
	virtual const boost::shared_ptr<Data> getDataSlice(const std::string& varName, size_t unLimDimPos = 0) throw(CDMException);

	/**
	 * @brief Remove a variable from the CDM
	 * 
	 * @param name of the variable
	 * @throw CDMException if variable doesn't exist
	 */
	virtual void removeVariable(std::string variable) throw(CDMException);
	/**
	 * @brief Reduce a dimension of the file
	 * 
	 * @param name  dimension to change
	 * @param start start-position corresponding to the original dimension
	 * @param size  size of the new dimension
	 * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
	 */
	virtual void reduceDimension(std::string dimName, size_t start, size_t length) throw(CDMException);
	/**
	 * @brief Reduce a dimension of the file
	 * 
	 * @param name  dimension to change
	 * @param start start-position corresponding to the original dimension, defaults to 0
	 * @param end end-position of dimension, 0 means full size, negative values start from end
	 * @throw CDMException if dimension doesn't exist or start+size outside range of the original dimension
	 */
	virtual void reduceDimensionStartEnd(std::string dimName, size_t start = 0, long end = 0) throw(CDMException);
	/**
	 * @brief change the datatype of the variable
	 * 
	 * a change of the variable will also change the datatype of the _FillValue attribute
	 * 
	 * @param variable name of the variable
	 * @param datatype new datatype
	 * @throw CDMException if variable doesn't exist or conversion to datatype is not supported
	 */
	virtual void changeDataType(std::string variable, CDMDataType datatype) throw(CDMException);
	
};

}

#endif /*CDMEXTRACTOR_H_*/
