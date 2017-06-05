////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to read/write .gz archive files (uses zlib library to do the job)
////////////////////////////////////////////////////////////////////////////

/* zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.4, March 11th, 2002

  Copyright (C) 1995-2002 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly jloup@gzip.org
  Mark Adler madler@alumni.caltech.edu

*/
/*! CGZip, a C++ wrapper for gzip

This documents present CGZip, C++ class wrapper for the gzip methods, which are included in the zlib libary.
The intention of this class is to have a <b>simple</b> class for zipping-unzipping buffers.

  The main features of the class are:
	- Compress, decompress string to file,
	- Compress, decompress memory buffer to file,
	- Non-MFC,
	- Hides zlib so you don't have to distribute the zlib headers,
	- UNICODE compliant.

\author Jonathan de Halleux, dehalleux@pelikhan.com, 2002
\version 1.1
*/

#ifndef GZIPFILE_H_
#define GZIPFILE_H_

#include <string>

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CGZip
{
public:
	//! void pointer to void
	typedef void* voidp;

	//! \brief Archive mode
	enum EArchiveMode {
		ArchiveModeClosed=0,	//! Archive Mode Closed
		ArchiveModeWrite = 1,	//! Writing to file
		ArchiveModeRead = 2		//! Reading from file
	};

	//! \brief Compression mode
	enum ECompressionMode {
		CompressionModeNoCompression= 0,	//! no compression
		CompressionModeFast = 1,			//! Fast compression
		CompressionModeBest= 9,				//! Best compression
		CompressionModeDefault = -1			//! Default compression
	};

	/*! \brief Strategy

	The strategy parameter is used to tune the compression algorithm. Use the
	value #StrategyDefault for normal data, #StrategyFiltered for data produced by a
   filter (or predictor), or #StrategyHuffmanOnly to force Huffman encoding only (no
   string match).

	 Filtered data consists mostly of small values with a
	somewhat random distribution. In this case, the compression algorithm is
   tuned to compress them better. The effect of #StrategyFiltered is to force more
   Huffman coding and less string matching; it is somewhat intermediate
   between #StrategyDefault and #StrategyHuffmanOnly.

	 The strategy parameter only affects
   the compression ratio but not the correctness of the compressed output even
   if it is not set appropriately.
	*/
	enum EStrategy {
		StrategyDefault=0,		//! Default
		StrategyFiltered=1,		//! filtered data
		StrategyHuffmanOnly=2	//! Huffman codes only
	};

	//! Flush modes
	enum EFlushMode {
		FlushModeNoFlush=   0,
		FlushModeSync =		2,
		FlushModeFull=		3,
		FlushModeFinish =	4
	};

	CGZip();
	virtual ~CGZip();

	//! returns zlib version
	static const char *GetVersion()									{	return m_stsVersion;};

	/*! \brief Sets compression level
	  \param eCompression compression level
	  \sa ECompressionMode
	*/
	void SetCompression(ECompressionMode eCompression)			{	m_eCompressionMode = eCompression; UpdateParams();};

	/*! \brief returns compression
	\sa SetCompression
	*/
	ECompressionMode GetCompression() const						{	return m_eCompressionMode;};

	/*! \brief Sets the strategy
	\sa EStrategy
	*/
	void SetStrategy(EStrategy eStrategy)						{	m_eStrategy = eStrategy; UpdateParams();};

	/*! \brief Returns the compression strategy 
	*/
	EStrategy GetStrategy() const								{	return m_eStrategy;};

	/*! \brief Open a file
	\param szFileName name of the file to open
	\param eArchiveMode archive mode, see #EOpenMode
	\return true if file opened succesfully, false otherwize
	#Open can be used to read a file which is not in gzip format; in this
	#ReadBuffer and #ReadBufferSize will directly read from the file without decompression.
	*/
	bool Open(const char *szFileName, EArchiveMode eArchiveMode);

	/*! \brief Closes the file
	\return true if succesful, false otherwize
	If writing, flushes the pending writing operator and closes the file. 
	*/
	bool Close();

	/*! brief Flushes all pending output into the compressed file.
	\param eFlush See EFlushMode
	\return true if succesful, false otherwize
     #Flush should be called only when strictly necessary because it can
	degrade compression.
	*/
	bool Flush(EFlushMode eFlush);

	//! returns true if file is currently opened
	bool IsOpen() const					{	return m_gzf != 0;};
	//! returns true if end of file has been reached
	bool IsEOF() const;
	
	//! returns true if zip is in write mode
	bool IsWriting() const				{	return m_eArchiveMode == ArchiveModeWrite;};
	//! returns true if zip is in read mode
	bool IsReading() const				{	return m_eArchiveMode == ArchiveModeRead;};

	/*!  \brief Writes buffer to zip file
	\param pBuffer memory buffer
	\param nBytes size in bytes of pBuffer
	\return true if writing succesful
	*/
	bool WriteBuffer( void*  pBuffer, int nBytes);

	/*! \brief Writes const string to zip file
	\param str the string to zip
	\return true if writing succesful
	*/
	bool WriteString(const char *str);

	/*! \brief Reads buffer
	\param pBuffer pointer to memory buffer of size nBytes
	\param nBytes size of the memory buffer
	\return number of bytes filled in memory buffer
	*/
	int ReadBufferSize( void*  pBuffer, int nBytes);


	void Rewind();

	long GetUncompressSize();
	long GetCompressSize();
	
	bool GetFileDate(time_t &timeDest);

	FILE *GetFileStream();

	std::string GetFileName();

	std::string GetArchivePath(){ return m_strArchivePath; };

protected:
	std::string	m_strArchivePath;

	//! Update compression and strategy parameters (only in write mode)
	void UpdateParams();

	//! zip file handle
	void*  m_gzf;
	//! Archive opening mode
	EArchiveMode m_eArchiveMode;
	//! compression level
	ECompressionMode m_eCompressionMode;
	//! strategy
	EStrategy m_eStrategy;
	//! ZLib version string
	static const char *m_stsVersion;
};

#endif // GZIPFILE_H_


