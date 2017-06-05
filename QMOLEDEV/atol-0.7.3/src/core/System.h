////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: System utility class.
////////////////////////////////////////////////////////////////////////////

#ifndef SYSTEM_H__
#define SYSTEM_H__

#if _MSC_VER > 1000
 #pragma warning (disable: 4786)
#endif

#include <string>
#include <vector>
#include "String.h"
#include "types.h"

// Enumeration description/filtering flags.
enum
{
	ENUM_LST_FILES  = 0x01,
	ENUM_LST_DIRS   = 0x02,
	ENUM_LST_ALL    = ENUM_LST_FILES | ENUM_LST_DIRS,
	ENUM_RECURSIVE  = 0x04, // visit directories recursively
};

// Enumeration result codes.
enum EnumResult
{
	ENUM_ERR_OK   =  0,
	ENUM_ERR_DIR  = -1, // failed to open dir
	ENUM_ERR_FILE = -2, // failed to stat file
};

// Enumeration callback method definition.
// path: Path. EnumDirectory returns paths in native filesystem encoding
// data: Callback data.
// Returns true to keep enumerating, false to stop enumerating.
typedef bool (* FN_ENUMDIR)(const std::string& path, void* data);

// System utility class.
class System
{
public:
	static std::string GetHomeDir();

	static std::string GetAppPath();

	// Converts a string which is in the encoding used by GLib for filenames
	// into a UTF-8 string. Exception is done for Windows, where it converts
	// a string from the encoding used in current locale into a UTF-8 string.
	// Conversion errors are ignored.
	// fileName: File name in the filename encoding.
	static std::string FileNameToUTF8(const std::string& fileName);

	// Converts a string from UTF8 to the encoding used by GLib for filenames.
	// Exception is done for Windows, where it converts a string from UTF-8 to
	// the encoding used in current locale.
	// Convertion errors are ignored.
	// fileName: File name in UTF-8.
	static std::string FileNameFromUTF8(const std::string& fileName);

	// Executes program.
	// path: Path to program in UTF-8.
	// args: Program arguments.
	// dir: Path to working directory in UTF-8.
	// bWaitForTermination: If true, function waits for program to terminate before returning	
	// Returns true on success, false on failure.
	static bool Execute(
		const std::string& path, const std::string& args, const std::string& dir, bool bWaitForTermination = false);

	// Opens file with associated application.
	// If file is executable it is run.
	// path: Path to file in UTF-8.
	// bWaitForTermination: If true, function waits for program to terminate before returning	
	// Returns true on success, false on failure.
	static bool Open(const std::string& path, bool bWaitForTermination = false);

	// Removes file or empty directory.
	// path: Path to file in UTF-8.
	// Returns true on success, false on failure.
	static bool Remove(const std::string& path);

	// Moves file to trash.
	// path: Path to file in UTF-8.
	// Returns true on success, false on failure.
	static bool MoveToTrash(const std::string& path);

	// Returns the number of bytes available to current user on
	// a file system where file specified by path resides or
	// INT64_MAX if it can't be determined.
	// path: Path to file in UTF-8.
	static INT64 GetAvailBytesOnFS(const std::string& path, INT64 &nTotal);

	// Returns UTF-8 path to the target of a symbolic link.
	// linkPath: Path to symbolic link in UTF-8.
	static std::string GetLinkTarget(const std::string& linkPath);

	// TODO: find out if it is really needed
	//void OpenCommandPrompt(const String &strDir, void *nData);

	// For each file in given directory call user's callback method.
	// path: Path to directory in native encoding
	// callback: Callback method.
	// callbackData: Data passed to callback.
	// flags: Enumeration flags.
	// Returns result code.
	static EnumResult EnumDirectory(
		const std::string& nativePath,
		FN_ENUMDIR callback,
		void* callbackData,
		int flags = ENUM_LST_FILES);

	// Runs a command as a background process.
	// command: Command in UTF-8.
	// output: Pointer to string that will receive command output or 0 to discard output.
	// Returns true on success, false on failure.
	// TODO: unite with Execute?
	static bool RunCommand(const std::string& command, std::string* output = 0); 

	static void GetPartitionList(std::vector<std::string> &lstPartitions);

	static bool IsSamePartition(const char *szPath1, const char *szPath2);

	static bool Rename(const char *szOld, const char *szNew);

	static bool MkDir(const char *szDir);

	static bool IsReadOnly(const char *szFile);

	static bool EnsureDirExists(String &strDir);

private:
	// This class is not intended to be instantiated.
	System();
};

#endif // SYSTEM_H__
