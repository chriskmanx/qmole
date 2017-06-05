

The ZipArchive library				

Copyright (c) 2000 - 2006 Tadeusz Dracz

Version: 2.4.11 				
Date: 22 Jul 2006

This library adds zip compression and decompression functionality to your program, allowing you to create and modify ZIP files in the compatible way with WinZip, PKZIP and other popular archivers.
Its easy and practical interface makes the library suitable for the beginners as well as for the advanced users. 

See what's new in this version.
To be notified about the future library updates, sign up for the \ref pageSubsc.

Platforms supported: 
- Windows 9x\Me\NT\2000\XP (MFC and STL) Microsoft Visual Studio (C++ 6.0, 2003 and 2005), Borland C++ - Linux (STL) , Mac OS X, MinGW

Author: Tadeusz Dracz		
E-Mail: tdracz@artpol-software.com 
Web Site: http://www.artpol-software.com 

This library uses the zlib library by Jean-loup Gailly and Mark Adler to perform inflate and deflate operations.

Features Summary:
- work in a compatible way with PKZIP and WinZip
- create, modify, extract and test zip archives
- create and extract multi-disk archives (on non-removable disks as well)
- add file to the archive from another archive without decompressing the file (copy compressed data) (see CZipArchive::GetFromArchive)
- highly optimized deleting multiple files from the archive
- optimized replacing and renaming files in the archive
- compression from and decompression to memory, create the whole archive in memory, extract the archive from memory 
- password encryption and decryption supported
- possibility to create or extract self-extracting archives
- smart compression, if enabled, prevents the file in the archive to be larger after compression (see CZipArchive::Smartness)
- safe compression with CZipArchive::Flush function
- using callback objects for notifications
	- to provide easy disk change in a multi-disk archives
	- for the progress control when adding, extracting, testing or deleting files or saving archive's central directory
- extracting and deleting using wildcard pattern matching (see CZipArchive::FindMatches)
- UNC and Windows Unicode paths recognized (still, due to the zip format specification, the filnames in the archive are stored using OEM-defined character set)
- wide characters supported
- support for the Java TM Archive (jar) File Format (see CZipArchive::SetIgnoreCRC)
- can be used as a static library or DLL (necessary VC++ projects included)
- possibility to integrate help system with MSDN 
- easy interface
- easy transfer to other system platforms
- speedy
- well documented
- full source code provided
- Visual Studio Projects: version 2003 (available with the library), versions 6.0 and 2005 (available at download page .)
- sample applications provided (for the STL version located in stl/zippie,
the MFC version (multithreaded) is available separately - please visit
 the download page )

If you find a bug, please mail me. Many thanks to the people that already tracked them down and submitted.

The Introduction

All you need to know about the licensing: \ref pageLic .

It's a good start to read these pages first (prior to reading the raw documentation):
- \ref pageSyst
- \ref pageGen
- \ref pageFaq.

\ref pageHist not only shows how the development of the library went so far, but also you may find here an 
interesting library feature without digging through the documentation.

If you wish to be notified about the future library updates, sign up for the ZipArchive Library Newsletter.

\page pageSyst Compilation & Integration
 
	- \ref secCompil
		- \ref winMFC
		- \ref winSTL
		- \ref LnxSTL	
	- \ref sectVisual
		- \ref subsM1
		- \ref subsM2
		- \ref subsDLL
	- \ref sectBorl
		- \ref subExample
	- \ref sectLinux
		- \ref subsLnxNot
		- \ref subsLnxCom
	- \ref sectNotes
		- \ref stlNotes
		- \ref subDLLnotes
		- \ref MFCsample

	Compiling for different implementations and platforms

	
	The files required for all the library versions are located in the program 
	root directory. You also need to copy additional files to the library 
	root directory from the two more subfolders. Depending on the configuration 
	these files are located in:

	
	Windows MFC
		\Windows and \mfc 
		You can just execute _copy from Win-MFC.bat batch file.

	Windows STL
		\Windows and \stl 
		You can just execute _copy from Win-STL.bat batch file.

	Linux (STL version)
		\Linux and \stl 
		You can just execute _copy_from_Linux.sh script file
		(don't forget to set executable rights before e.g. with the command:
		 chmod +x _copy_from_Linux.sh ).

	\note If you are compiling under FreeBSD, Mac, you need to comment include of sys/vfs.h in ZipPlatform.cpp and uncomment includes of sys/param.h and sys/mount.h.
	
	\note On some systems (m68k and the SPARC) when you want to compile the library for dynamic linking you need to modify makefile files (for the zlib and ZipArchive) so that CFLAGS have -fPIC option set.
		
	Visual C++ : integrating with the project
	
	To add ZipArchive library functionality into your project you need to link 
	the library to the project. You can do this at least in two ways 
	(in both cases you need to include ZipArchive.h header in your sources).

	Method 1

	Add ZipArchive.lib with the proper path e.g. ..\ZipArchive\debug\ZipArchive.lib to
 VS 6.0
	 Project Settings->Link->Input->Object/library modules 
 VS 2003 and VS 2005
	 Project Properties->Linker->Input->Additional Dependencies
	
	and add ZipArchive library directory to the preprocessor searches 
 VS 6.0
	 Project Settings -> C++ -> Preprocessor -> Additional include directories 
 VS 2003 and VS 2005
	 Project Properties -> C/C++ -> General -> Additional include directories 

	Method 2 (simpler)
 VS 6.0
	Insert Zip project into workspace and set project dependencies: your project dependent on ZipArchive project ( Project -> Dependencies ).
	On the dialog that will appear you select your project name from the combo box and check the box next to ZipArchive project name.
 VS 2003 and VS 2005
	Set your project to reference the ZipArchive project (on the Solutions Explorer).
		
	When you use this method, you link configurations in your project with
	configurations in the ZipArchive project that have the same name in both projects. So if you need to use
	for example "Debug" configuration from ZipArchive project, you need to create one with the same name
	in your application project and make sure that your project uses MFC library and run-time library in same way.
 VS 6.0
	 Project->Settings->General->Microsoft Fundation Classes and Project->Settings-> C/C++ ->Code Generation->Use run-time library 
 VS 2003 and VS 2005
	 Project Properties->General->Use of MFC and Project Properties-> C/C++ ->Code Generation->Runtime library 
	

	DLL version

	When you're using the DLL version of the ZipArchive library, you need to define in your program ZIP_HAS_DLL e.g. in:
 VS 6.0
	 Project Settings -> C++ -> Preprocessor -> Preprocessor definitions 
 VS 2003 and VS 2005
	 Project Properties -> C/C++ -> Preprocessor -> Preprocessor definitions 
 Only for VS 6.0
	Apart from integrating the ZipArchive library with your program (use one of the methods above), you also need to take into account zlib.lib file (use zlib/zlib.dsw to create it and add to preprocessor includes) or zlib/zlib.dsw project (insert into workspace and set ZipArchive project dependent on it).
	
	Files ZipArchive.dll and zlib.dll (the last one only when you were compiling using Visual Studio 6.0 projects) must be available for the program when running (e.g. in the program's directory).

	You can read about linking problems in the \ref pageFaq.

	Borland C++ compatibility
	Project files for Borland C++ 5.0 are available.
	They were created using Visual C++ Project Conversion Utility (VCTOBPR.EXE).
	You can start this tool with the command Tools->Visual C++ Project Conversion Utility .
	
	\note Be sure to create Release subfolder before compiling one of these projects, otherwise you'll get a write error.

	In case the projects provided don't work for you, you can create your own. You need to copy 
	to the root directory appropriate files for MFC or STL versions. 
	You may use the Borland project conversion utility.

	There are also makefiles available which should work with other versions of Borland.
	
	Compiling the sample application
	There is a Borland C++ project available for the sample application ZipArc. 
	To compile it you need compiled MFC version of ZipArchive library.

	\note Be sure to create Release subfolder first, otherwise you'll get a write error.
	
	Add the library (ZipArchive.lib ) to the project (Project->Add To Project) and compile.

	If you wish to convert Visual C++ project using Visual C++ Project Conversion Utility then after 
	converting to properly compile the application you need to remove odbccp32.lib from the project file
	and comment everything out in ZipArc_TLB.cpp or remove this file from the project files. 

	Linux platform

	Notes

 Usage 

	When using the library under Linux you should be aware of a few things:
	- after you get the system attributes with CZipFileHeader::GetSystemAttr() function, 
	you need to shift them right by 16 ( e.g. uAttr = header.GetSystemAttr() >> 16 ) - 
	the reason for that is the way the attributes are stored in the archive created under (or for) Linux.
	- due to lack of implementation of ZipPlatform::IsDriveRemovable(), which has proven to be a kind of difficult to do,
	the device containing the archive is always assumed to be removable; the only effect of this
	is that you need to set iVolumeSize to a value different from 0 when opening with the function
	CZipArchive::Open() the archive created	in tdSpan mode.

 Compiling 

	- the library was tested under g++ version 3.2.2	

	Compiling the library and liniking with the application
	- First you need to copy the appropriate files (see above )
	- If you haven't got the zlib library already installed on your system, you can install
	it using Makefile in the zlib subdirectory (type make and then make install )
	- only static version (you can download a full version of the zlib library from the ZipArchive library site).
	If you don't want to install the zlib library, you need to include it in the ZipArchive library
	( edit the Makefile in the main directory and change comments, so that
	OBJSZLIB is not an empty value but points to zlib library object files)
	- Compile the library by typing make. The resulting file is a static library libziparch.a
	- You can copy the library and the headers (if you have an appropriate rights) to /usr/lib and
	/usr/include/ziparchive (you can change them in Makefile) with the command make install
	- Now you can link the library to your application 
	e.g. with the command (if the library is in the same directory)
	 g++ $I. -o app app.cpp -lstdc++ -lz libziparch.a 
	or if you have the library installed in the /usr subdirectories:
	 g++ $I/usr/include/ziparchive -o app app.cpp -lstdc++ -lz -lziparch
	If you haven't got the zlib library installed, you need to omit the switch -lz in the above commands.
	- There is a test application named zippie.cpp (in stl/zippie which you can compile typing make zippie
	(providing that you have installed the ZipArchive library). If you haven't got the zlib library installed,
	you need to switch the comments (comment one line and uncomment another) in the Makefile in the section "zippie:".
	- If you wish to uninstall the library type make uninstall

	Notes

	STL version
	- [Windows only] If your locale is different from English and you wish to use non-English 
	characters in zip files, you need to set your locale with function
	std::locale::global(std::locale("")) to set it to be the same as your 
	system locale or e.g. std::locale::global(std::locale("French"))
	to set it to the specified value (do not use _T() macro here when using 
	Unicode); setlocale() function is not sufficient in this case.
	- Remember to restore the global locale to the previous value (returned by std::locale::global) after processing (it may affect other parts of your application).
	- There is a sample application that compiles under Windows (MSVC) and Linux (see below 
	to find out how to compile it under Linux ). This sample application demonstrates most of the 
	ZipArchive library features and is located in stl/zippie.

	Compiling as DLL
	[Windows only]
	- The project that compiles the DLL version of the ZipArchive library needs to have defined ZIP_HAS_DLL, ZIP_BUILD_DLL and for Visual Studio 6.0 also ZLIB_DLL .
	- The project that uses the ZipArchive library as the DLL version need to have defined ZIP_HAS_DLL

	MFC sample application (ZipArc)
	MFC sample application using ZipArchive library is available separately. Main features:
		- MDI application
		- multithreaded - you can work with many zip files at one time
		- shell integration (remembers the last application used to open zip files and can restore it correctly)
		- drag & drop support
		- detailed error reports
		- you can open and modify SFX archives
		- it demonstrates the use of the following functions (most of them are placed in ZipArcDoc.cpp) :
		CZipArchive::AddNewFile,
		CZipArchive::Close,
		CZipArchive::CloseFile,
		CZipArchive::CloseNewFile,
		CZipArchive::DeleteFiles,
		CZipArchive::EnableFindFast,
		CZipArchive::ExtractFile,
		CZipArchive::FindFile,
		CZipArchive::FindMatches,
		CZipArchive::Flush,
		CZipArchive::GetArchivePath, 
		CZipArchive::GetCentralDirInfo,
		CZipArchive::GetCentralDirSize,
		CZipArchive::GetCurrentDisk,
		CZipArchive::GetFileInfo,
		CZipArchive::GetFindFastIndex,
		CZipArchive::GetGlobalComment,
		CZipArchive::GetCount,
		CZipArchive::GetPassword,
		CZipArchive::GetSpanMode,
		CZipArchive::IsClosed,
		CZipArchive::IsReadOnly,
		CZipArchive::Open,
		CZipArchive::RenameFile,
		CZipArchive::PredictExtractedFileName,
		CZipArchive::SetAdvanced,
		CZipArchive::SetCallback,
		CZipArchive::SetFileComment,
		CZipArchive::SetGlobalComment,
		CZipArchive::SetIgnoreCRC,
		CZipArchive::SetPassword,
		CZipArchive::SetRootPath,
		CZipArchive::SetSpanCallback,
		CZipArchive::SetTempPath,
		CZipArchive::TestFile,
		CZipArchive::WillBeDuplicated, 
		CZipFileHeader::IsEncrypted,
		CZipFileHeader::IsDirectory,
		CZipFileHeader::GetTime,
		CZipFileHeader::GetSystemCompatibility,
		CZipFileHeader::GetSystemAttr,
		CZipFileHeader::GetSize,
		CZipFileHeader::GetFileName,
		CZipFileHeader::GetEffComprSize,
		CZipFileHeader::GetCompressionRatio,
		CZipFileHeader::GetComment,
		CZipFileHeader::CompressionEfficient,

\page pageGen General Information

\ref sectCompress

\ref sectSpan

\ref sectPass 

\ref sectSE 

\ref sectExc 

\ref sectMemory

\ref sectCallb

\ref sectHelp

 

Compression and decompression

There are some functions defined for fast operations on archive: CZipArchive::AddNewFile,
 CZipArchive::ExtractFile, CZipArchive::DeleteFile, CZipArchive::TestFile. 
 You only need to call functions CZipArchive::Open - before and CZipArchive::Close - after using them. Calling CZipArchive::Close function after you've done modifying the archive is necessary 
for the archive to be intact.

Multi-disk archives

This library can create multi-disk archives in two ways (modes):
- PKSpan mode. Disk spanning on removable media:
	- the archive can only be created on a removable device, 
	- the size of the volume is auto-detected
	- the label is written to the disk
	- you need to define a callback object for changing disks and set it with CZipArchive::SetSpanCallback function.
- TDSpan mode. Disk spanning on non-removable media:
	- the archive can be created on non-removable device as well
	- you need to define a single volume size
	- there is no need to set callback object in this mode.

These two disk spanning modes create volumes with compatible internal structure. It means that you can easily convert the volumes created in one mode to the other one by renaming the files (in TDSpan mode each volume's extension (apart from the last one) consists of a 'z' letter and a number). To convert the archive from TD to PKZIP compatible archive, copy each file to the removable media, giving them the extension ".zip". You should also label each disk with the appropriate label starting from "pkback# 001"
(note the space between '#' and '0').

There is a limited functions set available while working with multi-disk archives. Only adding is allowed when creating the archive and only extracting and testing after opening an existing one. Deleting files from these archives is not allowed at all.

Class CZipArchive uses write buffer to make write operations as fast as possible. You can change its size with CZipArchive::SetAdvanced function (first argument). While creating a multi-disk archive, set the size of the buffer to the maximum size of the volume for the best performance.

Popular archivers such as PKZIP and WinZip should be able to open the archive created in both modes.

Password encryption and decryption

This library supports creating and extracting the password protected archives. There are several issues you should be aware of when using this feature. To set the password for the file to be added or extracted call the function CZipArchive::SetPassword with the password as the argument. To clear the password call this function without arguments or with an empty string argument. The function has no effect on a closed archive and on the currently opened file (whether new or existing) inside archive. During opening the archive the password is cleared. You can set different passwords for different files inside the same archive, but remember to set it BEFORE opening the file. You cannot use ASCII characters with codes above 127 in the password, if you do so, the function CZipArchive::SetPassword returns false and the password is cleared.
You can find out what files are password encrypted by calling CZipArchive::GetFileInfo, which fills the structure CZipFileHeader with data, and then call the method ZipFileHeader::IsEncrypted. If it returns true, the file needs a password to extract.
The successful extraction of the encrypted file doesn't always mean that the password is correct. You also need to check the return value of CZipArchive::CloseFile. You could also check the size of the extracted file since it can be smaller than it should be in case of the bad password.

Self extract support

The library is capable of extracting and modifying self-extracting archives. You can create your own SFX archive as well. This is the simplest code responsible for the self-extracting:

\code
//Windows code

int APIENTRY WinMain(HINSTANCE hInstance,
HINSTANCE hPrevInstance,
LPSTR lpCmdLine,
int nCmdShow)
{
	CZipArchive zip;

	// get the path of the executable
	TCHAR szBuff[_MAX_PATH];
	if (!::GetModuleFileName(hInstance, szBuff, _MAX_PATH))
		return -1;

	CZipString szDest;
	// ...
	// add some code here to get the destination directory from the user 
	// for example:
	// CBrowseForFolder bf;
	// bf.strTitle = _T("Select directory to extract files");
	// if (!bf.GetFolder(szDest))
	// return -1;
	//
	// class CBrowseForFolder is included in the sample application project
	// remember about including the header!
	zip.Open(szBuff, CZipArchive::zipOpenReadOnly); 
	// zipOpenReadOnly mode is necessary for self extract archives
	for (WORD i = 0; i < zip.GetCount(); i++)
		zip.ExtractFile(i, szDest);

	zip.Close();
	return 0;
	// this code will not work for the encrypted archives since it is needed
	// to get the password from the user ( a small addition to the 
	// existing code I suppose )
}

\endcode

After compiling it and appending a zip archive to it (e.g. with the DOS command: copy /b SelfExtract.exe + ZipFile.zip FinalFile.exe ) you have a self extracting archive.

Exceptions

The ZipArchive library mostly uses exceptions to notify about the error occured. The library throws CZipException to notify about errors specific to the internal zip file processing. In the MFC version CZipException class is derived from CException whereas in the STL version it is derived from std::exception.

MFC version
The library throws the following exceptions inherited from CException: CMemoryException*, CFileExeption* and CZipException* (be sure to delete the object when you done with it). Handling them may be done in the following way:

\code

try
{
	// ...
	// some operations on the ZipArchive library
}
catch (CException* e)
{
	if (e->IsKindOf( RUNTIME_CLASS( CZipException )))
	{
		CZipException* p = (CZipException*) e;
		//... and so on 
	}
	else if (e->IsKindOf( RUNTIME_CLASS( CFileException )))
	{
		CFileException* p = (CFileException*) e;
		//... and so on 
	} 
	else
	{
		// the only possibility is a memory exception I suppose
		//... and so on
	}
	e->Delete();
}

\endcode

STL version
The library throws exceptions inherited from std::exception. In this case you should catch std::exception object (not a pointer to it).

Creating and extracting archives from/in memory

With the function CZipArchive::Open(CZipMemFile&, int) you can create the archive in memory and then write to disk, e.g.:

\code

CZipArchive zip;
CZipMemFile mf;
// create archive in memory
zip.Open(mf, CZipArchive::zipCreate);
// ...
// add some files to archive here e.g. by calling CZipArchive::AddNewFile
// ...
zip.Close();
// write the archive to disk
CZipFile f;
if (f.Open("c:\temp.zip", CZipFile::modeWrite|CZipFile::modeCreate, false)
{
	int iLen = mf.GetLength();
	BYTE* b = mf.Detach();
	f.Write(b, iLen);
	f.Close();
	// we must free the detached memory
	free(b);
}
\endcode

You can as well read the archive from disk and then extract files, e.g.:

\code
CZipFile f;
if (f.Open("c:\temp.zip", CZipFile::modeRead, false)
{
	int iLen = f.GetLength();	
	BYTE* b = (BYTE*)malloc((UINT)iLen);
	f.Read(b, iLen);
	f.Close();
	CZipMemFile mf;
	mf.Attach(b, iLen);
	CZipArchive zip;
	zip.Open(mf);
	// ...
	// extract files here from the archive e.g. by calling CZipArchive::ExtractFile
	// ...
	zip.Close();
}
\endcode

With functions CZipArchive::AddNewFile(CZipMemFile&, LPCTSTR, int, int, unsigned long) and 
CZipArchive::ExtractFile(WORD, CZipMemFile&, DWORD) you can add files to archive from memory 
and extract them to a memory file. Now a bit larger example:

\code
// create the archive in memory with two files inside
CZipMemFile mf;
CZipArchive zip;
zip.Open(mf, CZipArchive::zipCreate);
zip.AddNewFile("c:\testfile1.txt");
zip.AddNewFile("c:\testfile2.txt");
zip.Close();
//create the archive on disk and add a previously zipped file from memory
zip.Open("c:\test.zip", CZipArchive::zipCreate);
zip.AddNewFile(mf, "File1.zip");
zip.Close();
// we have now zip-in-zip file on the disk, 
// let's extract the embedded zip file back to memory
zip.Open("c:\test.zip");
// reset the contents of the CZipMemFile object
mf.SetLength(0);
zip.ExtractFile(0, mf);
zip.Close();
// write the file from memory to disk
CZipFile f;
if (f.Open("c:\File1.zip", CZipFile::modeWrite|CZipFile::modeCreate, false))
{
	int iLen = mf.GetLength();
	BYTE* b = mf.Detach();
	f.Write(b, iLen);
	f.Close();
	// we must free the detached memory
	free(b);
}
\endcode

One important thing: when you operate on the archive in memory, you must ensure
that CZipMemory object will not be destroyed before calling CZipArchive::Close.
In some cases you'll need to construct the object using the new operator, e.g.:
\code
// ...
CZipMemFile* pmf = new CZipMemFile;
zip.Open(*pmf, CZipArchive::zipCreate);
// ...
zip.Close();
delete pmf;
\endcode

Action progress notifications (callbacks)
 
The library has the possibility to notify about the progress of the various actions (see CZipArchive::CallbackType).
To use this feature you need to define a new class derived from CZipActionCallback or from CZipSpanCallback and override
CZipCallback::Callback function. Then you need to declare an object of your class and pass its address to 
function CZipArchive::SetCallback or CZipArchive::SetSpanCallback. Make sure that the object exists while the library 
performs the action the callback object was assigned to; or tell the library not to use the callback (use the same functions).

Integrating with MSDN (Visual Studio 6.0 only)

If you wish to integrate the ZipArchive help system with the MSDN library you need to:
- download from 

the Artpol Software site

ZipArchive HTML Help documentation or ZipArchive HTML documentation if you don't have it.
- in the latter case you need to compile the html files to the HTML Help format
with HTML Web Workshop by Microsoft (at the moment of writing available 

here)

and using provided index.hhp file (it is at the same location as ZipArchive html help files)
- now you should have index.chm and index.chi files, rename them if you want to and put them
to the directory of your choice
- you need to download a free MSDN Integration Utility by Kirk Stowell; you can download it from 

the Code Project site
or from the Artpol Software site 

(Download->ZipArchive)
- use the MSDN Integration Utility for the files you have prepared
- now pressing the F1 key on the ZipArchive library method or class in the Visual Studio brings up the MSDN help;
you have also a searchable ZipArchive collection inside MSDN
\note After integrating the ZipArchive help system with the MSDN library, you need
to be patient when you use the Index for the first time, because it'll be rebuilt then which can
be a lengthy process.

 