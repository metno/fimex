#ifndef FELT_FILE_ERROR_H_
#define FELT_FILE_ERROR_H_

#include <string>

namespace MetNoFelt {

class Felt_File_Error
{
private:
	std::string message;
public:
	Felt_File_Error(std::string message);
	virtual ~Felt_File_Error();
	const std::string& toString();
};

} // end namespace MetNoFelt

#endif /*FELT_FILE_ERROR_H_*/
