////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: IniFile class handles usage of initialisation files
//       (series of key=value entries inside one or several sections)
////////////////////////////////////////////////////////////////////////////

#ifndef INIFILE_H_
#define INIFILE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
  #pragma warning(disable:4786)
#endif

#include <vector>
#include <string>

//TOFIX? use map<> to speedup searches

class IniFile
{
    //Ini file keys
    class IniKey
    {
    public:
        IniKey(){};
        IniKey(const IniKey &that){ operator =(that); }
        void operator = (const IniKey &that)
        {
            if(this != &that){   // don't copy yourself
                m_name          = that.m_name;
                m_value         = that.m_value;
            }
        }

        //needed for searching key list
        bool operator == (const IniKey &that) { return m_name == that.m_name; };

        std::string m_name;    //std::string
        std::string m_value;
    };

    //ini file section
    class IniSection
    {
    public:
        IniSection(){};
        IniSection(const IniSection &that){ operator =(that); }
        void operator = (const IniSection &that)
        {
            if(this != &that){   // don't copy yourself
                m_name          = that.m_name;
                m_Keys          = that.m_Keys;
            }
        }

        //needed for searching section list
        bool operator == (const IniSection &that) { return m_name == that.m_name; };

        std::string m_name;
        std::vector<IniKey>  m_Keys;
    };

    typedef std::vector<IniKey>::iterator        IniKeyIterator;
    typedef std::vector<IniSection>::iterator    IniSectionIterator;

public:
    IniFile();
    virtual ~IniFile();

    bool Load(const char *szFile);
    bool Save();

    inline bool    GetDirty() const { return m_bDirty;  }
    inline std::string  GetPath()  const { return m_strPath; }
    void SetPath(const char *szFile) { m_strPath = szFile; }

    //
    bool ClearAll();

    //read values in different formats
    bool GetValue(const char *szSection, const char *szKey, std::string &value, const char *szDefault);
    bool GetValue(const char *szSection, const char *szKey, int  &nValue, int nDefault = 0);
	bool GetValue(const char *szSection, const char *szKey, unsigned short &nValue, int nDefault = 0);
    bool GetValue(const char *szSection, const char *szKey, bool &bValue, int nDefault = 0);

    //TOFIX support comment?
    bool SetValue(const char *szSection, const char *szKey, const char *szVal);
    bool SetValue(const char *szSection, const char *szKey, const int  &nValue);
    //bool SetValue(const char *szSection, const char *szKey, const bool &bValue);

    bool KeyExists(const char *szSection, const char *szKey);

    bool SectionExists(const char *szSection);
    bool AddSection(const char *szSection);
    bool RemoveSection(const char *szSection);
    bool RemoveKey(const char *szSection, const char *szKey);
    //TOFIX support section/key enumeration -> GetXXXCount, GetXXXName(index)

	void SetEncrypted(const char *szPass){ m_bEncrypted = true; m_strPass = szPass; };

protected:
    bool FindKey(IniKeyIterator &It, const char *szSection, const char *szKey);
    bool FindSection(IniSectionIterator &It, const char *szSection);

	bool m_bEncrypted;
	std::string m_strPass;

    std::vector<IniSection> m_contents;
    std::string   m_strPath;
    bool        m_bDirty;   //is content changed without saving
};

#endif // INIFILE_H_



