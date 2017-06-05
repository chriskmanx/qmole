////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class for handling internationalisation issues (message catalogs)
////////////////////////////////////////////////////////////////////////////

#ifndef GUILANGUAGE_H__
#define GUILANGUAGE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
 #pragma warning (disable: 4786)
#endif
#ifdef _WIN32
 #ifndef ENABLE_NLS
  #pragma message ("Warning: you must globally define ENABLE_NLS for translation to work!")
 #endif
#endif
#include <vector>
#include <string>
#include <locale.h>

class GuiLanguage  
{
public:
	GuiLanguage(const char *szPackage, const char *szLocaleDir);
	virtual ~GuiLanguage();

	//sets locale
	void Initialize(const char *szIsoCode);

	std::string GetCurLanguage(){ return m_strCurLang; }
	int  GetCatalogCount(){ return m_lstCatalogs.size(); };
	std::string GetCatalogAt(int nIdx){ return m_lstCatalogs[nIdx]; };
	
	int	 GetLocaleIdx(const char *szLocale);
	bool IsLocaleAvailable(const char *szLocale);

	int  GetLocaleTableIdx(const char *locale);

	static const char *GetLangName(const char *locale);
	static const char *GetLocaleCode(const char *szLocale);
	static const char *QueryLocale(int category = LC_ALL);

protected:
	void ListAvailableCatalogs();

protected:
	std::string m_strPackage;		//usually program name
	std::string m_strLocaleDir;		//directory where to keep localisations (message catalogs)
	std::string m_strCurLang;		//current language ISO code ("en","hr",...)

	std::vector<std::string> m_lstCatalogs;	//list of available locales
};

#endif // GUILANGUAGE_H__
