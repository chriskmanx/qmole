////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Macros to help us debugging the program
////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdarg.h>

#ifdef _WIN32
 #define vsnprintf _vsnprintf 
 #include <windows.h>
#endif

void DbgOutput(const char *szText);

void Trace(const char *fmt, ...)
{
	char buffer[1024] = "";

	// create string using format and list of parameters
	va_list args;
	va_start(args, fmt);
	vsnprintf(buffer, sizeof(buffer)-1, fmt, args);
	va_end(args);

	DbgOutput(buffer);
}

void Assert(int exp, const char *fmt, ...)
{
	if(!exp)
	{
		char buffer[1024] = "";

		// create string using format and list of parameters
		va_list args;
		va_start(args, fmt);
		vsnprintf(buffer, sizeof(buffer)-1, fmt, args);
		va_end(args);

		DbgOutput(buffer);
    }
}

void DbgOutput(const char *szText)
{
#ifdef _WIN32
	OutputDebugString(szText);
#else
	printf("%s", szText);
#endif
}
