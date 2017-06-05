////////////////////////////////////////////////////////////////////////////////
// This source file is part of the ZipArchive library source distribution and
// is Copyrighted 2000 - 2006 by Tadeusz Dracz (http://www.artpol-software.com/)
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// For the licensing details see the file License.txt
////////////////////////////////////////////////////////////////////////////////


/**
* \file ZipCompatibility.h
* ZipCompatibility namespace declaration.
*
*/

#if !defined(AFX_ZIPCOMPATIBILITY_H__8E8B9904_84C7_4B22_B364_A10ED0E7DAD6__INCLUDED_)
#define AFX_ZIPCOMPATIBILITY_H__8E8B9904_84C7_4B22_B364_A10ED0E7DAD6__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CZipAutoBuffer;
class CZipFileHeader;

/**
	Functions that provide the proper conversion of attributes 
	and filename strings between different system platforms and buffer operations that take 
	into account machine's byte-order.
*/
namespace ZipCompatibility  
{
	/**
		The codes of the compatibility of the file attribute information.
		\see CZipArchive::GetSystemCompatibility
		\see CZipFileHeader::GetSystemCompatibility
		\see ZipPlatform::GetSystemID
	*/
	enum ZipPlatforms
	{		   
			   zcDosFat,		///< MS-DOS and OS/2 (FAT / VFAT / FAT32 file systems)
               zcAmiga,			///< Amiga 
               zcVaxVms,		///< VAX/VMS
               zcUnix,			///< Unix / Linux
               zcVmCms,			///< VM/CMS
               zcAtari,			///< Atari ST
               zcOs2Hpfs,		///<  OS/2 H.P.F.S.
               zcMacintosh,		///< Macintosh 
               zcZsystem,		///< Z-System
               zcCpm,			///< CP/M 
               zcNtfs			///< Windows NTFS
	};

/**
	Check whether the system with the given code is supported by the ZipArchive library.
	\param	iCode
		\link #ZipPlatforms the system code \endlink
	\return	\c true if supported
*/
	bool IsPlatformSupported(int iCode);


/**
	Convert the system attributes between different system platforms.
	It calls one of the converting functions.
	\param	uAttr
		attributes to convert
	\param	iFromSystem
		system code to convert from 
	\param	iToSystem
		system code to convert to
	\return	the converted attributes
	\note Throws exceptions.
	\see ZipPlatforms
*/
	DWORD ConvertToSystem(DWORD uAttr, int iFromSystem, int iToSystem);

/**
	Convert the filename of the file inside archive.
	This conversion may not change the size of the filename, otherwise an
	error may occur in CZipFileHeader::ReadLocal while comparing the filename sizes.
	\param header 
		the file header to have the filename converted
	\param bFromZip		
		if \c true convert the path from the from it is stored in the archive
		to the current system compatible form; otherwise vice-versa.
	\param bOemConversion
		if \c true, the OEM conversion on the filename is performed (valid only under Windows platform)
	\see CZipCentralDir::ConvertFileName
*/
	void FileNameUpdate(CZipFileHeader& header, bool bFromZip, bool bOemConversion);

/**
	Change the slash to backslash or vice-versa in \e buffer.
	\param	buffer
	\param	bReplaceSlash
		if \c true, change slash to backslash
*/
	void SlashBackslashChg(CZipAutoBuffer& buffer, bool bReplaceSlash);

	/**
		Determine whether the current machine had a big- or little-endian architecture.
	*/
	bool IsBigEndian();

	/**
		Read \e iCount bytes from \e pSource into \e pDestination.
		\param pDestination
			big-endian order
		\param pSource
			little-endian order
		\param iCount 
			bytes to read
	*/
	void ReadBytesBigEndian(void* pDestination, const char* pSource, int iCount);

	/**
		Read \e iCount bytes from \e pSource into \e pDestination.
		\param pDestination
			little-endian order
		\param pSource
			little-endian order
		\param iCount 
			bytes to read
	*/
	void ReadBytesLittleEndian(void* pDestination, const char* pSource, int iCount);

	/**
		Write \e iCount bytes from \e pSource into \e pDestination.
		\param pDestination
			little-endian order
		\param pSource
			big-endian order
		\param iCount 
			bytes to write
	*/
	void WriteBytesBigEndian(char* pDestination, const void* pSource, int iCount);

	/**
		Write \e iCount bytes from \e pSource into \e pDestination.
		\param pDestination
			little-endian order
		\param pSource
			little-endian order
		\param iCount 
			bytes to write
	*/
	void WriteBytesLittleEndian(char* pDestination, const void* pSource, int iCount);

	/**
		Compare \e iCount bytes.
		\param pBuffer
			little-endian order
		\param pBytes
			big-endian order
		\param iCount 
			bytes to compare
	*/
	bool CompareBytesBigEndian(const char* pBuffer, const void* pBytes, int iCount);

	/**
		Compare \e iCount bytes.
		\param pBuffer
			little-endian order
		\param pBytes
			little-endian order
		\param iCount 
			bytes to compare
	*/
	bool CompareBytesLittleEndian(const char* pBuffer, const void* pBytes, int iCount);

};

#endif // !defined(AFX_ZIPCOMPATIBILITY_H__8E8B9904_84C7_4B22_B364_A10ED0E7DAD6__INCLUDED_)
