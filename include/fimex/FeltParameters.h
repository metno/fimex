#ifndef FELTPARAMETERS_H_
#define FELTPARAMETERS_H_

#include <map>
#include <string>
#include <boost/array.hpp>


namespace MetNoFelt {

class FeltParameters
{
public:
	FeltParameters();
	FeltParameters(std::string filename);
	virtual ~FeltParameters();
	const boost::array<short, 16>& getParameters(const std::string&);
	const std::string& getParameterName(const boost::array<short, 16>&);
	// local static objects
	const static std::string& DEFAULT_CONFIG() {
		const static std::string DEFAULT_CONFIG("/metno/local/diana/etc/diana.setup-COMMON");
		return DEFAULT_CONFIG; 
	}
private:
	void init(std::string filename=DEFAULT_CONFIG());
	boost::array<short, 16> diana2feltparameters(const std::string&);
	std::map<std::string, boost::array<short,16> > parameterMap;

};



const int ANY_VALUE();
const std::string& UNDEFINED();
const boost::array<short, 16>& ANY_ARRAY();

} // end namespace MetNoFelt

#endif /*FELTPARAMETERS_H_*/
