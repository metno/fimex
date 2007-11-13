#ifndef FELT_FILE_ERROR_H_
#define FELT_FILE_ERROR_H_

#include <exception>
#include <string>

namespace MetNoFelt {
	
using namespace std;

class Felt_File_Error : public std::exception
{
private:
	std::string message;
public:
	explicit Felt_File_Error(const std::string& message);
	virtual ~Felt_File_Error() throw();
	virtual const char* what() const throw();
};

} // end namespace MetNoFelt

#endif /*FELT_FILE_ERROR_H_*/
