#include "felt_reader/Felt_File_Error.h"

Felt_File_Error::Felt_File_Error(string message)
: message(message)
{
}

Felt_File_Error::~Felt_File_Error()
{
}

const string& Felt_File_Error::toString() {
	return message;
}
