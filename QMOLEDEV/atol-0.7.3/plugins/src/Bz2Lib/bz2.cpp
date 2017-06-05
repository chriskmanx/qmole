////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file packing/unpacking using BZip algorithm (.bz2 archives)
////////////////////////////////////////////////////////////////////////////

#include "bz2.h"

CBZ2::CBZ2()
{
	memset(&m_Stream,0, sizeof(m_Stream));
	m_pWriteBuffer=m_szWriteBuffer;
	m_nWriteBufferSize=sizeof(m_szWriteBuffer);

	m_pArchive	= NULL;
	m_pFile		= NULL;
	m_pfnProgress = NULL;
}

bool CBZ2::Open(const char *szFile, bool bRead)
{
	if(m_pArchive)
		Close();
	
	m_pArchive = fopen(szFile, (bRead)?"rb":"wb");
	return (NULL != m_pArchive);
}

void CBZ2::Close()
{
	if(m_pArchive)
		fclose(m_pArchive);
	m_pArchive = NULL;
}

unsigned int CBZ2::Compress(int nBlockSize,int nWorkFactor)
{
	BZ2_bzCompressInit(&m_Stream, nBlockSize, 0, nWorkFactor);

	m_Stream.next_out=m_pWriteBuffer;
	m_Stream.avail_out=m_nWriteBufferSize;
	while (true)
	{
		if (m_Stream.avail_in==0)
		{
			if ((m_Stream.avail_in=OnCompressRead(m_Stream.next_in))==0)
			{	// No more data left to read
				int nError;
				while (true)
				{
					nError=BZ2_bzCompress(&m_Stream,BZ_FINISH);
					switch (nError)
					{
					case BZ_FINISH_OK:
					case BZ_STREAM_END:
						OnCompressWrite(m_pWriteBuffer,m_nWriteBufferSize-m_Stream.avail_out);
						break;
					default:
						BZ2_bzCompressEnd(&m_Stream);
						return 0;
					}
					if (nError==BZ_STREAM_END){
						BZ2_bzCompressEnd(&m_Stream);
						return m_Stream.total_out_lo32;
					}
					m_Stream.next_out=m_pWriteBuffer;
					m_Stream.avail_out=m_nWriteBufferSize;
				}
			}
		}
		if (BZ2_bzCompress(&m_Stream,BZ_RUN)!=BZ_RUN_OK){
			BZ2_bzCompressEnd(&m_Stream);
			return 0;
		}
		if (m_Stream.avail_out==0)
		{	// Flush data
			OnCompressWrite(m_pWriteBuffer,m_nWriteBufferSize);
			m_Stream.next_out=m_pWriteBuffer;
			m_Stream.avail_out=m_nWriteBufferSize;
		}
	}

	BZ2_bzCompressEnd(&m_Stream);
	return 0;	// Won't be executed
}

unsigned int CBZ2::Decompress(int nSmall)
{
	BZ2_bzDecompressInit(&m_Stream, 0, nSmall);

	int nTotal = 0;

	m_Stream.next_out=m_pWriteBuffer;
	m_Stream.avail_out=m_nWriteBufferSize;
	while (true)
	{
		if (m_Stream.avail_in==0)
		{
			if ((m_Stream.avail_in=OnDecompressRead(m_Stream.next_in))==0)
			{	// No more data left to read
				int nError;
				while (true)
				{
					nError=BZ2_bzDecompress(&m_Stream);
					switch (nError)
					{
					case BZ_OK:
					case BZ_STREAM_END:
						OnDecompressWrite(m_pWriteBuffer,m_nWriteBufferSize-m_Stream.avail_out);
						nTotal += m_nWriteBufferSize-m_Stream.avail_out;
						break;
					default:
						BZ2_bzDecompressEnd(&m_Stream);
						return 0;
					}
					if (nError==BZ_STREAM_END){
						BZ2_bzDecompressEnd(&m_Stream);
						return nTotal;
					}
					m_Stream.next_out=m_pWriteBuffer;
					m_Stream.avail_out=m_nWriteBufferSize;
				}
			}
		}

		m_Stream.total_out_lo32 = 0;
		m_Stream.total_out_hi32 = 0;

		switch (BZ2_bzDecompress(&m_Stream))
		{
		case BZ_OK:
			OnDecompressWrite(m_pWriteBuffer,m_Stream.total_out_lo32);
			nTotal += m_Stream.total_out_lo32;
			m_Stream.next_out=m_pWriteBuffer;
			m_Stream.avail_out=m_nWriteBufferSize;
			break;
		case BZ_STREAM_END:
			OnDecompressWrite(m_pWriteBuffer,m_nWriteBufferSize-m_Stream.avail_out);
			nTotal += m_nWriteBufferSize-m_Stream.avail_out;
			BZ2_bzDecompressEnd(&m_Stream);
			return nTotal;
			break;
		default:
			BZ2_bzDecompressEnd(&m_Stream);
			return 0;
		}
	}

	BZ2_bzDecompressEnd(&m_Stream);
	return 0;
}

unsigned int CBZ2::OnCompressRead(char* &pBuffer)
{
	pBuffer = m_szReadBuffer;
	return fread(m_szReadBuffer, 1, sizeof(m_szReadBuffer), m_pFile);
}

void CBZ2::OnCompressWrite(const char* pBuffer,unsigned int nLength)
{
	//write to archive file
	fwrite(pBuffer, 1, nLength, m_pArchive);
}

unsigned int CBZ2::OnDecompressRead(char* &pBuffer)
{
	pBuffer = m_szReadBuffer;
	return fread(m_szReadBuffer, 1, sizeof(m_szReadBuffer), m_pArchive);
}

void CBZ2::OnDecompressWrite(const char* pBuffer, unsigned int nLength)
{
	//write to output file
	if(m_pFile){
		fwrite(pBuffer, 1, nLength, m_pFile);

		if(m_pfnProgress){
			int nRes = m_pfnProgress(m_strEntry.c_str(), ftell(m_pFile), m_dwUserData);
			if(0 == nRes)	//abort request
			{
				BZ2_bzDecompressEnd(&m_Stream);

				//close and remove partial file
				fclose(m_pFile);
				m_pFile = NULL;
				remove(m_strFile.c_str());
			}
		}
	}
}

bool CBZ2::CompressFile(const char *szInFile, const char *szArchive)
{
	if(Open(szArchive, false))
	{
		//open input file
		m_strFile = szInFile;
		m_pFile = fopen(szInFile, "rb");
		if(NULL != m_pFile)
		{
			Decompress();
			
			Close();
			fclose(m_pFile);
			m_pFile = NULL;
			return true;
		}

		Close();
	}

	return false;
}

bool CBZ2::DecompressFile(const char *szArchive, const char *szOutFile)
{
	if(Open(szArchive, true))
	{
		//open output file
		m_strFile = szOutFile;
		m_pFile = fopen(szOutFile, "wb");
		if(NULL != m_pFile)
		{
			Decompress();

			Close();
			fclose(m_pFile);
			m_pFile = NULL;
			return true;
		}

		Close();
	}

	return false;
}

int CBZ2::GetUnpackSize(const char *szArchive)
{
	if(Open(szArchive, true))
	{
		int nSize = Decompress();

		Close();

		return nSize;
	}

	return -1;
}


