////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class for handling internationalisation issues (message catalogs)
////////////////////////////////////////////////////////////////////////////

#include "GuiLanguage.h"
#include <algorithm>
#include <gtk/gtk.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#endif
#include "System.h"

#ifdef _WIN32
 #include <io.h> //access
 #include <windows.h> 
 #define access _access
#else
 #include<unistd.h>
#endif 

extern GuiLanguage g_lang;

bool OnDirEnum(const std::string &dir, void *data);
#define ARRAY_COUNT(x) (sizeof(x)/sizeof(x[0]))

#ifndef _WIN32
	//define dummy versions of Windows language IDs
	#define LANG_AFRIKAANS	0
	#define LANG_ALBANIAN	0
	#define LANG_AMHARIC    0
 	#define LANG_ARABIC     0
	#define LANG_ARMENIAN	0
	#define LANG_BASQUE     0	
	#define LANG_BELARUSIAN	0
	#define LANG_BENGALI	0
	#define LANG_BULGARIAN	0
	#define LANG_CATALAN	0
	#define LANG_CROATIAN	0
	#define LANG_CZECH      0
	#define LANG_DANISH     0
	#define LANG_DUTCH      0
	#define LANG_ENGLISH	0
	#define LANG_ESTONIAN	0
	#define LANG_ESPERANTO	0
	#define LANG_FARSI	    0
	#define LANG_FRENCH		0
	#define LANG_FINNISH	0
	#define LANG_GEORGIAN	0
	#define LANG_GERMAN		0
	#define LANG_GREEK		0
	#define LANG_HEBREW		0
	#define LANG_HINDI		0
	#define LANG_HUNGARIAN	0
	#define LANG_ICELANDIC	0
	#define LANG_INDONESIAN	0
	#define LANG_ITALIAN	0
	#define LANG_JAPANESE	0
	#define LANG_KOREAN		0
	#define LANG_LATVIAN	0
	#define LANG_LITHUANIAN	0
	#define LANG_MACEDONIAN	0
	#define LANG_MONGOLIAN  0
	#define LANG_MALAY		0
	#define LANG_NORWEGIAN	0
	#define LANG_POLISH		0
	#define	LANG_PORTUGUESE	0
	#define LANG_ROMANIAN	0
	#define LANG_RUSSIAN	0
	#define LANG_SERBIAN	0
	#define LANG_SLOVAK		0
	#define LANG_SLOVENIAN	0
	#define LANG_SPANISH	0
	#define LANG_SWEDISH	0
	#define LANG_TAMIL		0
	#define LANG_THAI		0
	#define LANG_TURKISH	0
	#define LANG_UKRAINIAN	0
	#define LANG_URDU		0
	#define LANG_VIETNAMESE	0
	#define LANG_CHINESE	0

	//define dummy versions of Windows sublanguage IDs
	#define SUBLANG_DEFAULT				0
	#define SUBLANG_BENGALI_INDIA       0
	#define SUBLANG_BENGALI_BANGLADESH  0
	#define SUBLANG_CHINESE_TRADITIONAL 0
	#define SUBLANG_CHINESE_SIMPLIFIED  0
	#define SUBLANG_DUTCH				0
	#define SUBLANG_DUTCH_BELGIAN		0
	#define SUBLANG_ENGLISH_UK			0
	#define SUBLANG_ENGLISH_US			0
	#define SUBLANG_FRENCH				0
	#define SUBLANG_FRENCH_BELGIAN		0
	#define SUBLANG_FRENCH_CANADIAN		0
	#define SUBLANG_GERMAN				0
	#define SUBLANG_GERMAN_AUSTRIAN		0
	#define SUBLANG_GERMAN_SWISS		0
	#define SUBLANG_LITHUANIAN			0
	#define SUBLANG_KOREAN				0
	#define SUBLANG_MALAY_MALAYSIA		0
	#define SUBLANG_MALAY_BRUNEI_DARUSSALAM	0
	#define SUBLANG_NORWEGIAN_BOKMAL	0
	#define SUBLANG_NORWEGIAN_NYNORSK	0
	#define SUBLANG_PORTUGUESE			0
	#define SUBLANG_PORTUGUESE_BRAZILIAN	0
	#define SUBLANG_SPANISH				0
	#define SUBLANG_SWEDISH				0
#else
	//some codes are not defined in Visual C++ 6.0
	#ifndef LANG_AMHARIC
		#define LANG_AMHARIC 0x5e
	#endif
	#ifndef LANG_ESPERANTO
		#define LANG_ESPERANTO 0x8f
	#endif
	#ifndef LANG_MONGOLIAN
		#define LANG_MONGOLIAN 0x50
	#endif
	#ifndef SUBLANG_BENGALI_INDIA
		#define SUBLANG_BENGALI_INDIA 0x00
	#endif
	#ifndef SUBLANG_BENGALI_BANGLADESH
		#define SUBLANG_BENGALI_BANGLADESH 0x01
	#endif
#endif


typedef struct {
    const char *iso, *locale, *lang;
	int nLangID_W32;
	int nSubLangID_W32;
} LanguageStruct;

//TOFIX add more languages, add native language name column?
static LanguageStruct isoLanguages[] = 
{
    {"af",	"af_AF",	"Afrikaans",	LANG_AFRIKAANS, SUBLANG_DEFAULT},
    {"sq",	"sq_AL",	"Albanian",		LANG_ALBANIAN,  SUBLANG_DEFAULT},
	{"am",  "am_ET",    "Amharic",      LANG_AMHARIC,   SUBLANG_DEFAULT},
    {"ar",	"ar",		"Arabic",	 	LANG_ARABIC,    SUBLANG_DEFAULT}, 
    {"hy",	"hy",		"Armenian",	 	LANG_ARMENIAN,  SUBLANG_DEFAULT}, 
	{"eu",	"eu",		"Basque",		LANG_BASQUE,	SUBLANG_DEFAULT}, 
    {"be",	"be_BE",	"Belarusian",	LANG_BELARUSIAN,SUBLANG_DEFAULT}, 
	{"bn",	"bn",		"Bengali",		LANG_BENGALI,   SUBLANG_DEFAULT}, 
	{"bn_IN",	"bn_IN",	"Bengali (India)",	LANG_BENGALI,   SUBLANG_BENGALI_INDIA}, 
	{"bn_BD",	"bn_BD",	"Bengali (Bangladesh)",	LANG_BENGALI,   SUBLANG_BENGALI_BANGLADESH}, 
    {"bg",	"bg_BG",	"Bulgarian",	LANG_BULGARIAN,	SUBLANG_DEFAULT},
    {"ca",	"ca_ES",	"Catalan",		LANG_CATALAN,	SUBLANG_DEFAULT},
    {"zh_CN",	"zh_CN",	"Chinese (simplified)",		LANG_CHINESE,	SUBLANG_CHINESE_SIMPLIFIED},
	{"zh_TW",	"zh_TW",	"Chinese (traditional)",	LANG_CHINESE,	SUBLANG_CHINESE_TRADITIONAL},
    {"hr",	"hr_HR",	"Croatian",		LANG_CROATIAN,	SUBLANG_DEFAULT},
    {"cs",	"cs_CZ",	"Czech",		LANG_CZECH,		SUBLANG_DEFAULT},
    {"da",	"da_DK",	"Danish",		LANG_DANISH,	SUBLANG_DEFAULT},
    {"nl",	"nl_NL",	"Dutch",		LANG_DUTCH,		SUBLANG_DUTCH},
    {"nl_BE","nl_BE",	"Dutch (Belgian)",	LANG_DUTCH,		SUBLANG_DUTCH_BELGIAN},
    {"en",	"en_GB",	"English",		LANG_ENGLISH,	SUBLANG_ENGLISH_UK},
    {"en_US","en_US",	"English (American)",LANG_ENGLISH,	SUBLANG_ENGLISH_US},
    {"et",	"et_EE",	"Estonian",		LANG_ESTONIAN,	SUBLANG_DEFAULT},
	{"eo",	"eo",		"Esperanto",	LANG_ESPERANTO,	SUBLANG_DEFAULT},
    {"fa",	"fa_IR",	"Farsi",		LANG_FARSI,		SUBLANG_DEFAULT},
    {"fi",	"fi_FI",	"Finnish",		LANG_FINNISH ,	SUBLANG_DEFAULT},
    {"fr",	"fr_FR",	"French",		LANG_FRENCH,	SUBLANG_FRENCH},
    {"fr_BE","fr_BE",	"French (Belgian)", LANG_FRENCH,	SUBLANG_FRENCH_BELGIAN},
    {"fr_CA","fr_CA",	"French (Canadian)",LANG_FRENCH,	SUBLANG_FRENCH_CANADIAN},
    {"ka",	"ka",		"Georgian",		LANG_GEORGIAN,	SUBLANG_DEFAULT}, 
    {"de",	"de_DE",	"German",		LANG_GERMAN,	SUBLANG_GERMAN},
    {"de_AT","de_AT",	"German (Austrian)",LANG_GERMAN, SUBLANG_GERMAN_AUSTRIAN},
    {"de_CH","de_CH",	"German (Swiss)",	LANG_GERMAN, SUBLANG_GERMAN_SWISS},
    {"el",	"el_GR",	"Greek",		LANG_GREEK,		SUBLANG_DEFAULT}, 
    {"he",	"he_IL",	"Hebrew",		LANG_HEBREW,	SUBLANG_DEFAULT}, 
    {"hi",	"hi_IN",	"Hindi",		LANG_HINDI,		SUBLANG_DEFAULT}, 
    {"hu",	"hu_HU",	"Hungarian",	LANG_HUNGARIAN, SUBLANG_DEFAULT}, 
    {"is",	"is_IS",	"Icelandic",	LANG_ICELANDIC, SUBLANG_DEFAULT}, 
    {"id",	"id_ID",	"Indonesian",	LANG_INDONESIAN,SUBLANG_DEFAULT}, 
    {"it",	"it_IT",	"Italian",		LANG_ITALIAN,	SUBLANG_DEFAULT},
    {"ja",	"ja_JP",	"Japanese",		LANG_JAPANESE,	SUBLANG_DEFAULT}, 
    {"ko",	"ko_KR",	"Korean",		LANG_KOREAN,	SUBLANG_KOREAN}, 
    {"lv",	"lv_LV",	"Latvian",		LANG_LATVIAN,	SUBLANG_DEFAULT}, 
    {"lt",	"lt_LT",	"Lithuanian",	LANG_LITHUANIAN,SUBLANG_LITHUANIAN}, 
    {"mk",	"mk_MK",	"Macedonian (FYROM)",LANG_MACEDONIAN,SUBLANG_DEFAULT}, 
	{"mn",	"mn",   	"Mongolian",LANG_MONGOLIAN,SUBLANG_DEFAULT}, 
    {"ms",	"ms_MY",	"Malay (Malaysia)",		LANG_MALAY,		SUBLANG_MALAY_MALAYSIA}, 
    {"ms_BN","ms_BN",	"Malay (Brunei)",LANG_MALAY,		SUBLANG_MALAY_BRUNEI_DARUSSALAM}, 
    {"nb",	"nb_NO",	"Norwegian Bokmal",	LANG_NORWEGIAN, SUBLANG_NORWEGIAN_BOKMAL},
    {"nn",	"nn_NO",	"Norwegian Nynorsk",LANG_NORWEGIAN, SUBLANG_NORWEGIAN_NYNORSK},
    {"no",	"no_NO",	"Norwegian",	LANG_NORWEGIAN, SUBLANG_DEFAULT},
    {"pl",	"pl_PL",	"Polish",		LANG_POLISH,	SUBLANG_DEFAULT},
    {"pt",	"pt_PT",	"Portuguese",	LANG_PORTUGUESE,SUBLANG_PORTUGUESE}, 
    {"pt_BR", "pt_BR",	"Portuguese (Brazilian)",	LANG_PORTUGUESE,SUBLANG_PORTUGUESE_BRAZILIAN}, 
    {"ro",	"ro_RO",	"Romanian",		LANG_ROMANIAN,	SUBLANG_DEFAULT}, 
    {"ru",	"ru_RU",	"Russian",		LANG_RUSSIAN,	SUBLANG_DEFAULT}, 
    {"sr",	"sr_YU",	"Serbian",		LANG_SERBIAN,	SUBLANG_DEFAULT}, 
    {"sk",	"sk_SK",	"Slovak",		LANG_SLOVAK,	SUBLANG_DEFAULT}, 
    {"sl",	"sl_SI",	"Slovenian",	LANG_SLOVENIAN, SUBLANG_DEFAULT}, 
    {"es",	"es_ES",	"Spanish",		LANG_SPANISH,	SUBLANG_SPANISH}, 
    {"sv",	"sv_SE",	"Swedish",		LANG_SWEDISH,	SUBLANG_SWEDISH}, 
    {"th",	"th_TH",	"Thai",			LANG_THAI,		SUBLANG_DEFAULT}, 
    {"bo",	"bo",		"Tibetan",		0,	0}, 
    {"ta",	"ta_IN",	"Tamil",		LANG_TAMIL,		SUBLANG_DEFAULT}, 
    {"tr",	"tr_TR",	"Turkish",		LANG_TURKISH,	SUBLANG_DEFAULT}, 
    {"uk",	"uk_UA",	"Ukrainian",	LANG_UKRAINIAN,	SUBLANG_DEFAULT}, 
    {"ur",	"ur",		"Urdu",			LANG_URDU,		SUBLANG_DEFAULT}, 
    {"vi",	"vi_VN",	"Vietnamese",	LANG_VIETNAMESE,SUBLANG_DEFAULT}, 
};

// comparator class to sort locale list by language name (ascending)
class CmpLang
{
public:
	CmpLang(){};
	~CmpLang(){};

	bool operator ()(const std::string &a, const std::string &b){
		#ifdef _WIN32
			return (g_lang.GetLangName(a.c_str()) > g_lang.GetLangName(b.c_str()));
		#else
			return (g_lang.GetLangName(a.c_str()) < g_lang.GetLangName(b.c_str()));
		#endif
	}
};

GuiLanguage::GuiLanguage(const char *szPackage, const char *szLocaleDir)
{
	m_strPackage	= szPackage;
	m_strLocaleDir	= szLocaleDir;
}

GuiLanguage::~GuiLanguage()
{
}

//maps from locale code to the language name
const char *GuiLanguage::GetLangName(const char *locale)
{
	//match either short or long version of the locale
	for (unsigned int i=0; i<ARRAY_COUNT(isoLanguages); i++)
		if (0 == strcmp (isoLanguages[i].iso, locale) || 
		    0 == strcmp (isoLanguages[i].locale, locale))
			return isoLanguages[i].lang;

	return "";    // not found
}

const char *GuiLanguage::GetLocaleCode(const char *iso)
{
	//match either short or long version of the locale
	for (unsigned int i=0; i<ARRAY_COUNT(isoLanguages); i++)
		if (0 == strcmp (isoLanguages[i].iso, iso))
			return isoLanguages[i].locale;

	return "";    // not found
}

int GuiLanguage::GetLocaleTableIdx(const char *locale)
{
	//match either short or long version of the locale
	for (unsigned int i=0; i<ARRAY_COUNT(isoLanguages); i++)
		if (0 == strcmp (isoLanguages[i].iso, locale) || 
		    0 == strcmp (isoLanguages[i].locale, locale))
			return i;

	return -1;    // not found
}

void GuiLanguage::Initialize(const char *szIsoCode)
{
#ifdef ENABLE_NLS
	ListAvailableCatalogs();

	//ensure language selected
	if(NULL == szIsoCode || strlen(szIsoCode)==0) 
		szIsoCode = "en";

	// convert language ISO code to locale code
	const char *szLocaleCode = GetLocaleCode(szIsoCode);
	if(0 != strlen(szLocaleCode))
	{
		//set language localisation for this program
		putenv( g_strconcat("LANG=", szLocaleCode, NULL) );
		setlocale(LC_ALL, "");

	#ifdef _WIN32
		//FIX for Windows code
		int nIdx = GetLocaleTableIdx(szIsoCode);
		if(nIdx >= 0)
			SetThreadLocale(MAKELCID(MAKELANGID(isoLanguages[nIdx].nLangID_W32, isoLanguages[nIdx].nSubLangID_W32), SORT_DEFAULT));
	#endif
	}

	// init NLS system settings
	bindtextdomain (m_strPackage.c_str(), m_strLocaleDir.c_str());
	bind_textdomain_codeset (m_strPackage.c_str(), "UTF-8");
	textdomain (m_strPackage.c_str());

	//store locale code for later lookup
	m_strCurLang = szIsoCode;	
#endif
}

void GuiLanguage::ListAvailableCatalogs()
{
	//clear catalog list
	m_lstCatalogs.erase(m_lstCatalogs.begin(), m_lstCatalogs.end());	//empty the list

	//list all subdirectories in the locale directory (subdirectory name matches the locale name)
	std::vector<std::string> lstDirs;
	System::EnumDirectory(m_strLocaleDir, OnDirEnum, &lstDirs, ENUM_LST_DIRS);
	
	//store each locale containing catalog for current package
	for( unsigned int i=0; i<lstDirs.size(); i++)
	{
		//check if message catalog exists for given locale subdirectory
		std::string strCatalog;
		#ifdef _WIN32
			strCatalog  = m_strLocaleDir;
			strCatalog += lstDirs[i];
			strCatalog += "\\LC_MESSAGES\\";
			strCatalog += m_strPackage;
			strCatalog += ".mo";
		#else
			char szBuffer[1024];
			sprintf(szBuffer, "%s/%s/LC_MESSAGES/%s.mo",m_strLocaleDir.c_str(), lstDirs[i].c_str(), m_strPackage.c_str());
			strCatalog  = szBuffer;
		#endif
				
		if(0 == access(strCatalog.c_str(), 00))
			m_lstCatalogs.push_back(lstDirs[i]);	//add locale into the list of available catalogs

		//sort list by language names
		std::sort(m_lstCatalogs.begin(), m_lstCatalogs.end(), CmpLang());
	}
}

bool GuiLanguage::IsLocaleAvailable(const char *szLocale)
{
	return (-1 != GetLocaleIdx(szLocale));
}

int GuiLanguage::GetLocaleIdx(const char *szLocale)
{
	for(unsigned int i=0; i<m_lstCatalogs.size(); i++)
	{
		const char *szCatalogEntry = m_lstCatalogs[i].c_str();
		if( 0 == strcmp(szCatalogEntry, szLocale) )
			return i;
	}

	return -1; //not found
}

bool OnDirEnum(const std::string &dirPath, void *data)
{
	std::vector<std::string> *lstDirs = (std::vector<std::string> *)data;	
	if(data)
	{
		//calculate base name and add it into the list
		#ifdef _WIN32
			//calculate base name
			std::string name(dirPath);
			int nPos = dirPath.find_last_of('\\');
			if(nPos > 0)
				name = dirPath.substr(nPos+1, 1000);
			nPos = dirPath.find_last_of('/');	// Linux delimiter support
			if(nPos > 0)
				name = dirPath.substr(nPos+1, 1000);
		#else
			// basename may modify buffer, so copy directory path to temporary buffer
			const char* const dirPathStr = dirPath.c_str();
			std::vector<char> buffer(dirPathStr, dirPathStr + dirPath.size() + 1);
			std::string name(basename((char *)&buffer[0]));
		#endif	
				
		lstDirs->push_back(name);
	}
	
	return true;	//keep enumerating
}

const char *GuiLanguage::QueryLocale(int category)
{
	//return current locale settings
	return setlocale(category, NULL);
}

