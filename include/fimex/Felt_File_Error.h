#ifndef FELT_FILE_ERROR_H_
#define FELT_FILE_ERROR_H_

#include <string>

using namespace std;

class Felt_File_Error
{
private:
	string message;
public:
	Felt_File_Error(string message);
	virtual ~Felt_File_Error();
	const string& toString();
};

#endif /*FELT_FILE_ERROR_H_*/
