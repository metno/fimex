#include "felt_reader/Felt_File_Error.h"

namespace MetNoFelt {

Felt_File_Error::Felt_File_Error(std::string message)
: message(message)
{
}

Felt_File_Error::~Felt_File_Error()
{
}

const std::string& Felt_File_Error::toString() {
	return message;
}

}
