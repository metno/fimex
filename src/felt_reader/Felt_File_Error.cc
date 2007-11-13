#include "felt_reader/Felt_File_Error.h"

namespace MetNoFelt {

Felt_File_Error::Felt_File_Error(const std::string& message)
: message(message)
{
}

Felt_File_Error::~Felt_File_Error() throw()
{
}

const char* Felt_File_Error::what() const throw() {
	return message.c_str();
}

}
