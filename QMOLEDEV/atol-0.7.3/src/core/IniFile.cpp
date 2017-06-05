////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File:  INI file implementation
////////////////////////////////////////////////////////////////////////////

#include "_crypt/SHA1.h"	// must be the top one, because its defines can cause linker error
#include "IniFile.h"
#include "PathName.h"
#include "File64Enc.h"
#include "String.h"
#include "System.h"
#include "debug.h"
#include <algorithm>    //find() function
#include <string>

IniFile::IniFile()
{
    m_bDirty = false;
	m_bEncrypted = false;
}

IniFile::~IniFile()
{
}

bool IniFile::Load(const char *szFile)
{
    m_bDirty = false;   //init flag
    m_contents.clear(); //clear storage
    SetPath(szFile);

    std::string line;

	File64 *pFile = NULL;
	if(m_bEncrypted)
	{
		pFile = new File64Enc;

		((File64Enc *)pFile)->SetPassword(m_strPass.c_str());
		if(!pFile->Open(szFile, F64_READ|F64_SHARE_READ|F64_OPEN_EXISTING)){
			delete pFile;
			return false;
		}

		//read header with SHA1 password hash
		// 1. format signature text "ENC_INI" (7 bytes)
		char szFormatData[8];
		szFormatData[7] = '\0';
		pFile->File64::Read(szFormatData, 7);
		if(0 != strcmp(szFormatData, "ENC_INI")){
			delete pFile;
			return false;	//invalid format code
		}

		// 2. write format version string (1 byte)
		pFile->File64::Read(szFormatData, 1);
		szFormatData[1] = '\0';
		if(0 != strcmp(szFormatData, "1")){
			delete pFile;
			return false;	//invalid format version
		}

		// 3. SHA-1 password hash value (20 bytes)
		char szHashRead[20] = "";
		CSHA1 hash;
		hash.Update((UINT_8 *)m_strPass.c_str(), m_strPass.size());
		hash.Final();
		pFile->File64::Read(szHashRead, 20);
		szFormatData[1] = '\0';
		if(0 != memcmp(szHashRead, hash.m_digest, 20)){
			delete pFile;
			return false;	//invalid password
		}
	}
	else
	{
		pFile = new File64;
		if(!pFile->Open(szFile, F64_READ|F64_SHARE_READ|F64_OPEN_EXISTING)){
			delete pFile;
			return false;
		}
	}

    //read from file line by line
    bool bDone = false;
    char szBuffer[4000];

	int nBytes = 0;

    while(!bDone)
    {
		if(nBytes <= 0)
		{
			szBuffer[0] = '\0';
			nBytes = pFile->Read(szBuffer, sizeof(szBuffer)-1); //TOFIX only one line!
			if(nBytes == 0){
				delete pFile;
				return true;  //done
			}
			szBuffer[nBytes] = '\0';
		}
        line = szBuffer;

		//cut line at proper position
		std::string::size_type nPos = line.find('\n');
		if(nPos != std::string::npos){
			line = line.substr(0, nPos);
			memmove(szBuffer, szBuffer+nPos+1, nBytes-nPos-1);
		}
		nBytes -= line.size();
		nBytes -= 1; //account for new line

		//strip '\r'
		nPos = line.find('\r');
		if(nPos != std::string::npos)
			line = line.substr(0, nPos);

		TRACE("Ini line = %s\n", line.c_str());

        //is that a section in the buffer
        if('[' == line[0])
        {
            int nPos1 = line.find_first_of(']');
            if(-1 != nPos1)
            {
                IniSection section;
                section.m_name = line.substr(1, nPos1-1).c_str();

                //add new section to the end of storage
                m_contents.push_back(section);
            }
        }
        else
        {
            //else is the line "key=value"
            int nPos1 = line.find_first_of('=');
            if(nPos1 > 0)    //there must be space for key name
            {
                //add new key to the last section in the storage
                IniKey key;
                key.m_name  = line.substr(0, nPos1).c_str();
                key.m_value = line.substr(nPos1+1, line.size()-nPos1-1).c_str();

                int size = m_contents.size();
                if(size > 0)
                    m_contents[size-1].m_Keys.push_back(key);
            }
        }

        //check for exit
		bDone = false;	//TOFIX
    }

	delete pFile;
    return true;
}

bool IniFile::Save()
{
    m_bDirty = false;   //reset flag

	//ensure directory exists
    String strDir = PathName::GetParentDirPath(m_strPath.c_str());
    PathName::EnsureNotTerminated(strDir);
	System::EnsureDirExists(strDir);    //TOFIX check success

    //open the INI file for writing
	File64 *pFile = NULL;
	if(m_bEncrypted)
	{
		pFile = new File64Enc;

		((File64Enc *)pFile)->SetPassword(m_strPass.c_str());
		if(!pFile->Open(m_strPath.c_str(), F64_WRITE|F64_SHARE_READ|F64_OPEN_NEW)){
			delete pFile;
			return false;
		}

		//write header with SHA1 password hash
		// 1. format signature text "ENC_INI" (7 bytes)
		pFile->File64::Write("ENC_INI", 7);
		// 2. write format version string (1 byte)
		pFile->File64::Write("1", 1);
		// 3. SHA-1 password hash value (20 bytes)
		CSHA1 hash;
		hash.Update((UINT_8 *)m_strPass.c_str(), m_strPass.size());
		hash.Final();
		pFile->File64::Write((char *)hash.m_digest, 20);
	}
	else
	{
		pFile = new File64;
		if(!pFile->Open(m_strPath.c_str(), F64_WRITE|F64_SHARE_READ|F64_OPEN_NEW)){
			delete pFile;
			return false;
		}
	}

    char szBuffer[4000];

    int size = m_contents.size();
    for(int i=0; i<size; i++)
    {
        //write line with section name
        sprintf(szBuffer, "[%s]\n", m_contents[i].m_name.c_str());
		pFile->Write(szBuffer, strlen(szBuffer));

        int count = m_contents[i].m_Keys.size();
        for(int j=0; j<count; j++)
        {
            //write "key = value"
            sprintf(szBuffer, "%s=%s\n",
                m_contents[i].m_Keys[j].m_name.c_str(),
                m_contents[i].m_Keys[j].m_value.c_str());
			
			pFile->Write(szBuffer, strlen(szBuffer));
        }
        
		pFile->Write("\n", 1);
    }

	if(m_bEncrypted)
		((File64Enc *)pFile)->WriteFinal();

	delete pFile;
    return true;
}

bool IniFile::ClearAll()
{
    m_bDirty = true;
    m_contents.clear();
    return true;
}

bool IniFile::SectionExists(const char *szSection)
{
    IniSectionIterator It;
    return FindSection(It, szSection);
}

bool IniFile::KeyExists(const char *szSection, const char *szKey)
{
    IniKeyIterator It;
    return FindKey(It, szSection, szKey);
}

bool IniFile::GetValue(const char *szSection, const char *szKey, std::string &value, const char *szDefault)
{
    IniKeyIterator It;
    if(FindKey(It, szSection, szKey)){
        value = It->m_value;
        return true;
    }

    value = szDefault;
    return false;
}

bool IniFile::GetValue(const char *szSection, const char *szKey, int  &nValue, int nDefault)
{
    IniKeyIterator It;
    if(FindKey(It, szSection, szKey)){
        nValue = atoi(It->m_value.c_str());
        return true;
    }

    nValue = nDefault;
    return false;
}

bool IniFile::GetValue(const char *szSection, const char *szKey, unsigned short &nValue, int nDefault)
{
    IniKeyIterator It;
    if(FindKey(It, szSection, szKey)){
        nValue = atoi(It->m_value.c_str());
        return true;
    }

    nValue = nDefault;
    return false;
}

bool IniFile::GetValue(const char *szSection, const char *szKey, bool &bValue, int nDefault)
{
    IniKeyIterator It;
    if(FindKey(It, szSection, szKey)){
        bValue = atoi(It->m_value.c_str()) > 0;
        return true;
    }

    bValue = nDefault > 0;
    return false;
}

bool IniFile::SetValue(const char *szSection, const char *szKey, const int  &nValue)
{
    String strVal;
    strVal.Printf("%d", nValue);

    return SetValue(szSection, szKey, strVal.c_str());
}

bool IniFile::SetValue(const char *szSection, const char *szKey, const char *szVal)
{
    m_bDirty = true;    //content changes

    IniKeyIterator It;
    if(FindKey(It, szSection, szKey))
    {
        It->m_value = szVal;    //update existing key
        return true;
    }

    if(AddSection(szSection))   //ensure section exists
    {
        IniSectionIterator SecIt;
        if(FindSection(SecIt, szSection))
        {
            IniKey key;
            key.m_name  = szKey;
            key.m_value = szVal;
            
            SecIt->m_Keys.push_back(key);
            return true;
        }
    }

    return false;
}

bool IniFile::AddSection(const char *szSection)
{
    m_bDirty = true;    //content changes

    IniSectionIterator It;

    if(!FindSection(It, szSection))
    {
        IniSection section;
        section.m_name = szSection;

        m_contents.push_back(section);
        return true;
    }

    return true;    //section already exists
}

bool IniFile::RemoveSection(const char *szSection)
{
    IniSectionIterator It;

    if(FindSection(It, szSection))
        m_contents.erase(It);

    return true;
}

bool IniFile::FindSection(IniSectionIterator &It, const char *szSection)
{
    IniSection section;
    section.m_name = szSection;

    It = std::find(m_contents.begin(), m_contents.end(), section);

    return (It != m_contents.end());
}

bool IniFile::FindKey(IniKeyIterator &It, const char *szSection, const char *szKey)
{
    IniSectionIterator SecIt;

    if(FindSection(SecIt, szSection)) //section exists
    {
        IniKey key;
        key.m_name = szKey;

        It = std::find(SecIt->m_Keys.begin(), SecIt->m_Keys.end(), key);

        return (It != SecIt->m_Keys.end());
    }

    return false;
}

