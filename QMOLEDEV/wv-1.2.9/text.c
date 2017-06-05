/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "wv.h"
#include <glib.h>

int (*wvConvertUnicodeToEntity) (U16 char16) = NULL;

/* enough word docs use the cp1252 encoding and enough iconv
 * implementations don't include it that it's worth special-
 * casing */
static const U16 cp1252_to_ucs2_table[] = {
  0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007
, 0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F
, 0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017
, 0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F
, 0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027
, 0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F
, 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037
, 0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F
, 0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047
, 0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F
, 0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057
, 0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F
, 0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067
, 0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F
, 0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077
, 0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F
, 0x20AC, 0x0000, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021
, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x0000, 0x017D, 0x0000
, 0x0000, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014
, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x0000, 0x017E, 0x0178
, 0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7
, 0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF
, 0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7
, 0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF
, 0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7
, 0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF
, 0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7
, 0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF
, 0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7
, 0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF
, 0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7
, 0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF
};

U16
wvnLocaleToLIDConverter (U8 nLocale)
{
    switch (nLocale)
      {
#if 0
	/* case 0: */		/* ANSI_CHARSET */
	/* case 1: */		/* DEFAULT_CHARSET */
	/* case 2: */		/* SYMBOL_CHARSET */
#endif
	  case 77:			/* MAC_CHARSET */
	  return (0xFFF);	/* This number is a hack */
	  case 128:			/* SHIFTJIS_CHARSET */
	  return (0x411);	/* Japanese */
	  case 129:			/* HANGEUL_CHARSET */
	  return (0x412);	/* Korean */
	  case 130:			/* JOHAB_CHARSET */
	  return (0x812);	/* Korean (Johab) */
	  case 134:			/* GB2312_CHARSET - Chinese Simplified */
	  return (0x804);	/* China PRC - And others!! */
	  case 136:			/* CHINESEBIG5_CHARSET - Chinese Traditional */
	  return (0x404);	/* Taiwan - And others!! */
	  case 161:			/* GREEK_CHARSET */
	  return (0x408);	/* Greek */
	  case 162:			/* TURKISH_CHARSET */
	  return (0x41f);	/* Turkish */
	  case 163:			/* VIETNAMESE_CHARSET */
	  return (0x42a);	/* Vietnamese */
	  case 177:			/* HEBREW_CHARSET */
	  return (0x40d);	/* Hebrew */
	  case 178:			/* ARABIC_CHARSET */
	  return (0x01);	/* Arabic */
	  case 186:			/* BALTIC_CHARSET */
	  return (0x425);	/* Estonian - And others!! */
	  case 204:			/* RUSSIAN_CHARSET */
	  return (0x419);	/* Russian - And others!! */
	  case 222:			/* THAI_CHARSET */
	  return (0x41e);	/* Thai */
	  case 238:			/* EASTEUROPE_CHARSET */
	  return (0x405);	/* Czech - And many others!! */

#if 0
	  /* case 255: */		/* OEM_CHARSET */
#endif

      default:
	  return (0x0);
      }
    return (0x0);
}

int
wvOutputTextChar (U16 eachchar, U8 chartype, wvParseStruct * ps, CHP * achp)
{
    U16 lid = 0;

    wvVersion v = wvQuerySupported (&ps->fib, NULL);

    /* testing adding a language */

    /* For version <= WORD7, The charset used could
     * depend on the font's charset.
     */
    if ((v <= WORD7) && (!ps->fib.fFarEast))
      {
	  FFN currentfont;
	
	  if (ps->fonts.ffn == NULL)
          {
	      lid = 0;
	  }
	  else
          {
	  	currentfont = ps->fonts.ffn[achp->ftc];
	  	/* Return 0 if no match */
	  	lid = wvnLocaleToLIDConverter (currentfont.chs);
	  }
      }

    if ((v > WORD6) && !lid)
	lid = achp->lidDefault;

    /* No lidDefault for ver < WORD6 */
    if (lid == 0x400 || lid == 0)
	lid = ps->fib.lid;

    /* end testing adding a language */

    if (achp->fSpec)
      {
	  /*
	     if the character is still one of the special ones then call this other 
	     handler 
	     instead
	   */
	  if (ps->scharhandler)
	      return ((*(ps->scharhandler)) (ps, eachchar, achp));
      }
    else
      {
	  /* Most Chars go through this baby */
	  if (ps->charhandler)
	    {
		if (!((v == WORD7 || v == WORD6) && ps->fib.fFarEast))
		    if (v <= WORD7)
		      {
			  /* versions <= 7 do not use unicode. versions >= 8 always do */
			  /* versions 7 and 6 use unicode iff the far-east flag is set */
			  chartype = 1;
		      }

		return ((*(ps->charhandler)) (ps, eachchar, chartype, lid));
	    }
      }
    wvError (("No CharHandler registered, programmer error\n"));
    return (0);
}

void
wvOutputHtmlChar (U16 eachchar, U8 chartype, char *outputtype, U16 lid)
{
    if (chartype)
	eachchar = wvHandleCodePage (eachchar, lid);
    wvOutputFromUnicode (eachchar, outputtype);
}

#define CPNAME_OR_FALLBACK(name,fallbackname)  \
{      \
       static char* cpname = NULL;                             \
       if (!cpname)    \
               {       \
                       GIConv cd = g_iconv_open(name,name);     \
                       if (cd==(GIConv)-1)    \
                               {       \
                                       cpname = fallbackname;  \
                               }       \
                       else    \
                               {       \
                                       cpname = name;  \
                                       g_iconv_close(cd);        \
                               }       \
               };      \
       return cpname;  \
}

typedef struct {
	const char * language_tag ;
	U16 lid ;
} wvLanguageId ;

static const wvLanguageId mLanguageIds[] =
	{
		{ "-none-", 0x0000 }, /* none (language neutral) */
		{ "-none-", 0x0400 }, /* none */
		{ "af-ZA", 0x0436 }, /* Afrikaans */
		{ "am", 0x045e }, /* Amharic */
		{ "sq-AL", 0x041c }, /* Albanian */
		{ "ar-SA", 0x0401 }, /* Arabic (Saudi) */
		{ "ar-IQ", 0x0801 }, /* Arabic (Iraq) */
		{ "ar-EG", 0x0c01 }, /* Arabic (Egypt) */		
		{ "ar-LY", 0x1001 }, /* Arabic (Libya) */
		{ "ar-DZ", 0x1401 }, /* Arabic (Algeria) */
		{ "ar-MA", 0x1801 }, /* Arabic (Morocco) */
		{ "ar-TN", 0x1c01 }, /* Arabic (Tunisia) */
		{ "ar-OM", 0x2001 }, /* Arabic (Oman) */
		{ "ar-YE", 0x2401 }, /* Arabic (Yemen) */		
		{ "ar-SY", 0x2801 }, /* Arabic (Syria) */
		{ "ar-JO", 0x2c01 }, /* Arabic (Jordan) */
		{ "ar-LB", 0x3001 }, /* Arabic (Lebanon) */
		{ "ar-KW", 0x3401 }, /* Arabic (Kuwait) */
		{ "ar-AE", 0x3801 }, /* Arabic (United Arab Emirates) */
		{ "ar-BH", 0x3c01 }, /* Arabic (Bahrain) */		
		{ "ar-QA", 0x4001 }, /* Arabic (Qatar) */
		{ "as", 0x044d }, /* Assamese */
		{ "az", 0x042c }, /* Azerbaijani */
		{ "hy-AM", 0x042b }, /* Armenian */
		{ "az", 0x044c }, /* Azeri (Latin) az- */
		{ "az", 0x082c }, /* Azeri (Cyrillic) az- */
		{ "eu-ES", 0x042d }, /* Basque */
		{ "be-BY", 0x0423 }, /* Belarussian */		
		{ "bn", 0x0445 }, /* Bengali bn- */
		{ "bg-BG", 0x0402 }, /* Bulgarian */
		{ "ca-ES", 0x0403 }, /* Catalan */
		{ "zh-TW", 0x0404 }, /* Chinese (Taiwan) */
		{ "zh-CN", 0x0804 }, /* Chinese (PRC) */
		{ "zh-HK", 0x0c04 }, /* Chinese (Hong Kong) */		
		{ "zh-SG", 0x1004 }, /* Chinese (Singapore) */
		{ "ch-MO", 0x1404 }, /* Chinese (Macau SAR) */
		{ "hr-HR", 0x041a }, /* Croatian */
		{ "cs-CZ", 0x0405 }, /* Czech */
		{ "da-DK", 0x0406 }, /* Danish */
		{ "div", 0x465 }, /* Divehi div-*/
		{ "nl-NL", 0x0413 }, /* Dutch (Netherlands) */		
		{ "nl-BE", 0x0813 }, /* Dutch (Belgium) */
		{ "en-US", 0x0409 }, /* English (USA) */
		{ "en-GB", 0x0809 }, /* English (UK) */
		{ "en-AU", 0x0c09 }, /* English (Australia) */
		{ "en-CA", 0x1009 }, /* English (Canada) */
		{ "en-NZ", 0x1409 }, /* English (New Zealand) */		
		{ "en-IE", 0x1809 }, /* English (Ireland) */
		{ "en-ZA", 0x1c09 }, /* English (South Africa) */
		{ "en-JM", 0x2009 }, /* English (Jamaica) */
		{ "en", 0x2409 }, /* English (Caribbean) */
		{ "en-BZ", 0x2809 }, /* English (Belize) */
		{ "en-TT", 0x2c09 }, /* English (Trinidad) */		
		{ "en-ZW", 0x3009 }, /* English (Zimbabwe) */
		{ "en-PH", 0x3409 }, /* English (Phillipines) */
		{ "et-EE", 0x0425 }, /* Estonian */
		{ "fo", 0x0438 }, /* Faeroese fo- */
		{ "fa-IR", 0x0429 }, /* Farsi */
		{ "fi-FI", 0x040b }, /* Finnish */		
		{ "fr-FR", 0x040c }, /* French (France) */
		{ "fr-BE", 0x080c }, /* French (Belgium) */
		{ "fr-CA", 0x0c0c }, /* French (Canada) */
		{ "fr-CH", 0x100c }, /* French (Switzerland) */
		{ "fr-LU", 0x140c }, /* French (Luxembourg) */
		{ "fr-MC", 0x180c }, /* French (Monaco) */		
		{ "gl", 0x0456 }, /* Galician gl- */
		{ "ga-IE", 0x083c }, /* Irish Gaelic */
		{ "gd-GB", 0x100c }, /* Scottish Gaelic */
		{ "ka-GE", 0x0437 }, /* Georgian */
		{ "de-DE", 0x0407 }, /* German (Germany) */
		{ "de-CH", 0x0807 }, /* German (Switzerland) */
		{ "de-AT", 0x0c07 }, /* German (Austria) */
		{ "de-LU", 0x1007 }, /* German (Luxembourg) */
		{ "de-LI", 0x1407 }, /* German (Liechtenstein) */		
		{ "el-GR", 0x0408 }, /* Greek */
		{ "gu", 0x0447 }, /* Gujarati gu- */
		{ "ha", 0x0468 }, /* Hausa */
		{ "he-IL", 0x040d }, /* Hebrew */
		{ "hi-IN", 0x0439 }, /* Hindi */
		{ "hu-HU", 0x040e }, /* Hungarian */
		{ "is-IS", 0x040f }, /* Icelandic */		
		{ "id-ID", 0x0421 }, /* Indonesian */
		{ "iu", 0x045d }, /* Inkutitut */
		{ "it-IT", 0x0410 }, /* Italian (Italy) */
		{ "it-CH", 0x0810 }, /* Italian (Switzerland) */
		{ "ja-JP", 0x0411}, /* Japanese */
		{ "kn", 0x044b }, /* Kannada kn- */
		{ "ks", 0x0860 }, /* Kashmiri (India) ks- */		
		{ "kk", 0x043f }, /* Kazakh kk- */
		{ "kok", 0x0457 }, /* Konkani kok- */
		{ "ko-KR", 0x0412 }, /* Korean */
		{ "ko", 0x0812 }, /* Korean (Johab) ko- */
		{ "kir", 0x0440 }, /* Kyrgyz */
		{ "la", 0x0476 }, /* Latin */
		{ "lo", 0x0454 }, /* Laothian */
		{ "lv-LV", 0x0426 }, /* Latvian */
		{ "lt-LT", 0x0427 }, /* Lithuanian */		
		{ "lt-LT", 0x0827 }, /* Lithuanian (Classic) */
		{ "mk", 0x042f }, /* FYRO Macedonian */
		{ "my-MY", 0x043e }, /* Malaysian */
		{ "my-BN", 0x083e }, /* Malay Brunei Darussalam */
		{ "ml", 0x044c }, /* Malayalam ml- */
		{ "mr", 0x044e }, /* Marathi mr- */
		{ "mt", 0x043a }, /* Maltese */
		{ "mo", 0x0450 }, /* Mongolian */
		{ "ne-NP", 0x0461 }, /* Napali (Nepal) */
		{ "ne-IN", 0x0861 }, /* Nepali (India) */
		{ "nb-NO", 0x0414 }, /* Norwegian (Bokmai) */
		{ "nn-NO", 0x0814 }, /* Norwegian (Nynorsk) */
		{ "or", 0x0448 }, /* Oriya or- */
		{ "om", 0x0472 }, /* Oromo (Afan, Galla) */
		{ "pl-PL", 0x0415 }, /* Polish */		
		{ "pt-BR", 0x0416 }, /* Portuguese (Brazil) */
		{ "pt-PT", 0x0816 }, /* Portuguese (Portugal) */
		{ "pa", 0x0446 }, /* Punjabi pa- */
		{ "ps", 0x0463 }, /* Pashto (Pushto) */
		{ "rm", 0x0417 }, /* Rhaeto-Romanic rm- */
		{ "ro-RO", 0x0418 }, /* Romanian */
		{ "ro-MD", 0x0818 }, /* Romanian (Moldova) */		
		{ "ru-RU", 0x0419 }, /* Russian */
		{ "ru-MD", 0x0819 }, /* Russian (Moldova) */
		{ "se", 0x043b }, /* Sami (Lappish) se- */
		{ "sa", 0x044f }, /* Sanskrit sa- */
		{ "sr", 0x0c1a }, /* Serbian (Cyrillic) sr- */
		{ "sr", 0x081a }, /* Serbian (Latin) sr- */		
		{ "sd", 0x0459 }, /* Sindhi sd- */
		{ "sk-SK", 0x041b }, /* Slovak */
		{ "sl-SI", 0x0424 }, /* Slovenian */
		{ "wen", 0x042e }, /* Sorbian wen- */
		{ "so", 0x0477 }, /* Somali */
		{ "es-ES", 0x040a }, /* Spanish (Spain, Traditional) */
		{ "es-MX", 0x080a }, /* Spanish (Mexico) */		
		{ "es-ES", 0x0c0a }, /* Spanish (Modern) */
		{ "es-GT", 0x100a }, /* Spanish (Guatemala) */
		{ "es-CR", 0x140a }, /* Spanish (Costa Rica) */
		{ "es-PA", 0x180a }, /* Spanish (Panama) */
		{ "es-DO", 0x1c0a }, /* Spanish (Dominican Republic) */
		{ "es-VE", 0x200a }, /* Spanish (Venezuela) */		
		{ "es-CO", 0x240a }, /* Spanish (Colombia) */
		{ "es-PE", 0x280a }, /* Spanish (Peru) */
		{ "es-AR", 0x2c0a }, /* Spanish (Argentina) */
		{ "es-EC", 0x300a }, /* Spanish (Ecuador) */
		{ "es-CL", 0x340a }, /* Spanish (Chile) */
		{ "es-UY", 0x380a }, /* Spanish (Uruguay) */		
		{ "es-PY", 0x3c0a }, /* Spanish (Paraguay) */
		{ "es-BO", 0x400a }, /* Spanish (Bolivia) */
		{ "es-SV", 0x440a }, /* Spanish (El Salvador) */
		{ "es-HN", 0x480a }, /* Spanish (Honduras) */
		{ "es-NI", 0x4c0a }, /* Spanish (Nicaragua) */
		{ "es-PR", 0x500a }, /* Spanish (Puerto Rico) */		
		{ "sx", 0x0430 }, /* Sutu */
		{ "sw", 0x0441 }, /* Swahili (Kiswahili/Kenya) */
		{ "sv-SE", 0x041d }, /* Swedish */
		{ "sv-FI", 0x081d }, /* Swedish (Finland) */
		{ "ta", 0x0449 }, /* Tamil ta- */
		{ "tt", 0x0444 }, /* Tatar (Tatarstan) tt- */		
		{ "te", 0x044a }, /* Telugu te- */
		{ "th-TH", 0x041e }, /* Thai */
		{ "ts", 0x0431 }, /* Tsonga ts- */
		{ "tn", 0x0432 }, /* Tswana tn- */
		{ "tr-TR", 0x041f }, /* Turkish */
		{ "tl", 0x0464 }, /* Tagalog */
		{ "tg", 0x0428 }, /* Tajik */
		{ "bo", 0x0451 }, /* Tibetan */
		{ "ti", 0x0473 }, /* Tigrinya */
		{ "uk-UA", 0x0422 }, /* Ukrainian */		
		{ "ur-PK", 0x0420 }, /* Urdu (Pakistan) */
		{ "ur-IN", 0x0820 }, /* Urdu (India) */
		{ "uz", 0x0443 }, /* Uzbek (Latin) uz- */
		{ "uz", 0x0843 }, /* Uzbek (Cyrillic) uz- */
		{ "ven", 0x0433 }, /* Venda ven- */
		{ "vi-VN", 0x042a }, /* Vietnamese */
		{ "cy-GB", 0x0452 }, /* Welsh */
		{ "xh", 0x0434 }, /* Xhosa xh */
		{ "yi", 0x043d }, /* Yiddish yi- */
		{ "yo", 0x046a }, /* Yoruba */
		{ "zu", 0x0435 }, /* Zulu zu- */
		{ "en-US", 0x0800 } /* Default */
	};

#define NrMappings (sizeof(mLanguageIds)/sizeof(mLanguageIds[0]))

U16 wvLangToLIDConverter ( const char * lang )
{
	unsigned int i = 0 ;

	if (!lang)
		return 0x0400;   /* return -none- */
	
	for ( i = 0 ; i < NrMappings ; i++ )
		if (!strcmp (lang, mLanguageIds[i].language_tag))
			return mLanguageIds[i].lid ;
	
	return 0x0400 ;   /* return -none- */
}

const char *
wvLIDToLangConverter (U16 lid)
{
	unsigned int i = 0 ;
	
	if ( lid == 0 ) /* language netural */
		return "-none-" ;
	
	for ( i = 0 ; i < NrMappings ; i++ )
		if ( mLanguageIds[i].lid == lid )
			return mLanguageIds[i].language_tag ;

	return "-none-"; /* default */
}

static int
wvIsCP1252 (U16 lid)
{
  switch (lid & 0xff)
    {
      case 0x03:		/*Catalan */
      case 0x06:		/*Danish */
      case 0x07:		/*German */
      case 0x09:		/*English */
      case 0x0a:		/*Spanish */
      case 0x0b:		/*Finnish */
      case 0x0c:		/*French */
      case 0x0f:		/*Icelandic */
      case 0x10:		/*Italian */
      case 0x13:		/*Dutch */
      case 0x14:		/*Norwegian */
      case 0x16:		/*Portuguese */
      case 0x17:		/*Rhaeto-Romanic */
      case 0x1d:		/*Swedish */
      case 0x21:		/*Bahasa Indonesian */
      case 0x2d:		/*Basque */
      case 0x36:		/*Afrikaans */
      case 0x38:		/*Faeroese */
      case 0x3E:		/*Malaysian / Malay */
      case 0x41:		/*Swahili */
	return 1;

      case 0x1a:		/*Serbian, Croatian, (Bosnian?) */
	switch (lid)
	  {
	  case 0x041a:		/*Croatian */
	  case 0x081a:		/*Serbian (Latin) */
	    return 1;
	  }

    default:
      return 0;
    }

  return 0;
}

const char *
wvLIDToCodePageConverter (U16 lid)
{
    if (lid == 0x0FFF)	/*Macintosh Hack */
	  return ("MACINTOSH");
    else if (wvIsCP1252 (lid))
      return ("CP1252");

    switch (lid & 0xff)
	  {
      case 0x01:		/*Arabic */
	  return ("CP1256");
      case 0x02:		/*Bulgarian */
	  return ("CP1251");
      case 0x04:		/*Chinese */
      switch (lid)
		{
#if 0
      	case 0x1404:		/*Chinese (Macau SAR) */
#endif
      	case 0x0c04:		/*Chinese (Hong Kong SAR, PRC) */
	  	CPNAME_OR_FALLBACK ("CP950", "BIG5-HKSCS");
      	case 0x0804:		/*Chinese (PRC) */
	  	CPNAME_OR_FALLBACK ("CP936", "GBK");
#if 0
      	case 0x1004:		/*Chinese (Singapore) */
#endif
      	case 0x0404:		/*Chinese (Taiwan) */
	  	CPNAME_OR_FALLBACK ("CP950", "BIG5");
		}
      case 0x05:		/*Czech */
	  return ("CP1250");
      case 0x08:		/*Greek */
	  return ("CP1253");
      case 0x0d:		/*Hebrew */
	  return ("CP1255");
      case 0x0e:		/*Hungarian */
	  return ("CP1250");
      case 0x11:		/*Japanese */
	  return ("CP932");
      case 0x12:		/*Korean */
      switch (lid)
		{
      	case 0x0812:		/*Korean (Johab) */
	  	return ("CP1361");
      	case 0x0412:		/*Korean */
	  	return ("CP949");
		}
      case 0x15:		/*Polish */
	  return ("CP1250");
      case 0x18:		/*Romanian */
	  return ("CP1250");
      case 0x19:		/*Russian */
	  return ("CP1251");
      case 0x1a:		/*Serbian, Croatian, (Bosnian?) */
      switch (lid)
		{
      	case 0x0c1a:		/*Serbian (Cyrillic) */
	  	return ("CP1251");
		}
      case 0x1b:		/*Slovak */
	  return ("CP1250");
      case 0x1c:		/*Albanian */
	  return ("CP1251");
      case 0x1e:		/*Thai */
	  return ("CP874");
      case 0x1f:		/*Turkish */
	  return ("CP1254");
      case 0x20:		/*Urdu. This is Unicode only. */
	  return ("CP0");
      case 0x22:		/*Ukrainian */
	  return ("CP1251");
      case 0x23:		/*Byelorussian / Belarusian */
	  return ("CP1251");
      case 0x24:		/*Slovenian */
	  return ("CP1250");
      case 0x25:		/*Estonian */
	  return ("CP1257");
      case 0x26:		/*Latvian */
	  return ("CP1257");
      case 0x27:		/*Lithuanian */
	  return ("CP1257");
      case 0x29:		/*Farsi / Persian. This is Unicode only. */
	  return ("CP0");
      case 0x2a:		/*Vietnamese */
	  return ("CP1258");
      case 0x2b:		/*Windows 2000: Armenian. This is Unicode only. */
	  return ("CP0");
      case 0x2c:		/*Azeri */
      switch (lid)
		{
      	case 0x082c:		/*Azeri (Cyrillic) */
	  	return ("CP1251");
#if 0
      	case 0x042c:		/*Azeri (Latin) */
#endif
		}
      case 0x2f:		/*Macedonian */
	  return ("CP1251");
#if 0
      case 0x30:		/*Sutu */
#endif
      case 0x37:		/*Windows 2000: Georgian. This is Unicode only. */
	  return ("CP0");
      case 0x39:		/*Windows 2000: Hindi. This is Unicode only. */
	  return ("CP0");
#if 0
      case 0x3f:		/*Kazakh */
#endif
      case 0x43:		/*Uzbek */
      switch (lid)
		{
      	case 0x0843:		/*Uzbek (Cyrillic) */
	  	return ("CP1251");
#if 0
      	case 0x0443:		/*Uzbek (Latin) */
#endif
		}
#if 0
      case 0x44:		/*Tatar */
#endif
      case 0x45:		/*Windows 2000: Bengali. This is Unicode only. */
      case 0x46:		/*Windows 2000: Punjabi. This is Unicode only. */
      case 0x47:		/*Windows 2000: Gujarati. This is Unicode only. */
      case 0x48:		/*Windows 2000: Oriya. This is Unicode only. */
      case 0x49:		/*Windows 2000: Tamil. This is Unicode only. */
      case 0x4a:		/*Windows 2000: Telugu. This is Unicode only. */
      case 0x4b:		/*Windows 2000: Kannada. This is Unicode only. */
      case 0x4c:		/*Windows 2000: Malayalam. This is Unicode only. */
      case 0x4d:		/*Windows 2000: Assamese. This is Unicode only. */
      case 0x4e:		/*Windows 2000: Marathi. This is Unicode only. */
      case 0x4f:		/*Windows 2000: Sanskrit. This is Unicode only. */
	  return ("CP0");
      case 0x55:		/*Myanmar / Burmese. This is Unicode only. */
	  return ("CP0");
      case 0x57:		/*Windows 2000: Konkani. This is Unicode only. */
	  return ("CP0");
#if 0
      case 0x58:		/*Manipuri */
      case 0x59:		/*Sindhi */
      case 0x60:		/*Kashmiri (India) */
#endif
      case 0x61:		/*Windows 2000: Nepali (India). This is Unicode only. */
	  return ("CP0");
      };

	/* TODO output a warning since this is a guess */
    return ("CP1252");
}

static U32
swap_iconv (U16 lid)
{
    GIConv handle;
    char f_code[33];		/* From CCSID                           */
    char t_code[33];		/* To CCSID                             */
    const char *codepage;
    size_t ibuflen, obuflen;

    U8 buffer[2];
    U8 buffer2[2];

    gchar *ibuf, *obuf;

    /* do a bit of caching */
    static U16 lastlid = -1;
    static U32 ret = -1;

    /* shortcut */
    if (ret != -1 && lastlid == lid)
	return ret;

    ibuf = buffer;
    obuf = buffer2;

    lastlid = lid;
    codepage = wvLIDToCodePageConverter (lid);

    memset (f_code, '\0', 33);
    memset (t_code, '\0', 33);

    strcpy (f_code, codepage);
    strcpy (t_code, "UCS-2");

    handle = g_iconv_open (t_code, f_code);
    if (handle == (GIConv)-1)
            return 0;

    buffer[0] = 0x20 & 0xff;
    buffer[1] = 0;

    ibuflen = obuflen = 2;

    g_iconv (handle, &ibuf, &ibuflen, &obuf, &obuflen);

    g_iconv_close (handle);

    ret = *(U16 *) buffer2 != 0x20;
    return ret;
}

U16
wvHandleCodePage (U16 eachchar, U16 lid)
{
    char f_code[33];		/* From CCSID                           */
    char t_code[33];		/* To CCSID                             */
    const char *codepage;
    GIConv g_iconv_handle;	/* Conversion Descriptor returned       */
    /* from g_iconv_open() function           */
    size_t ibuflen;		/* Length of input buffer               */
    size_t obuflen;		/* Length of output buffer              */

    gchar *ibuf;
    gchar *obuf;			/* Buffer for converted characters      */
    U8 *p;
    U8 buffer[2];
    U8 buffer2[2];

    U16 rtn;

    if (wvIsCP1252 (lid) && eachchar <= 0xFF)
      {
	return cp1252_to_ucs2_table[eachchar];
      }

    if (eachchar > 0xff)
      {
	  buffer[0] = (char) (eachchar >> 8);
	  buffer[1] = (char) eachchar & 0xff;
      }
    else
      {
	  buffer[0] = eachchar & 0xff;
	  buffer[1] = 0;
      }

    ibuf = buffer;
    obuf = buffer2;

    codepage = wvLIDToCodePageConverter (lid);

    /* All reserved positions of from code (last 12 characters) and to code   */
    /* (last 19 characters) must be set to hexadecimal zeros.                 */

    memset (f_code, '\0', 33);
    memset (t_code, '\0', 33);

    strcpy (f_code, codepage);
    strcpy (t_code, "UCS-2");

    g_iconv_handle = g_iconv_open (t_code, f_code);
    if (g_iconv_handle == (GIConv) - 1)
      {
	  wvError (
		   ("g_iconv_open fail: %d, cannot convert %s to unicode\n",
		    errno, codepage));
	  return ('?');
      }

    ibuflen = obuflen = 2;
    p = obuf;

    g_iconv (g_iconv_handle, &ibuf, &ibuflen, &obuf, &obuflen);

    /* We might have double byte char here. */

    if (swap_iconv (lid))
      {
	  rtn = (U16) buffer2[0] << 8;
	  rtn |= (U16) buffer2[1];
      }
    else
      {
	  rtn = *(U16 *) buffer2;
      }

    g_iconv_close (g_iconv_handle);

    return (rtn);
}

void
wvOutputFromUnicode (U16 eachchar, char *outputtype)
{
    static char cached_outputtype[33];	/* Last outputtype                  */
    static GIConv g_iconv_handle = (GIConv)-1;	/* Cached iconv descriptor          */
    static int need_swapping;
    gchar *ibuf, *obuf;
    size_t ibuflen, obuflen, len, count, i;
    U8 buffer[2], buffer2[5];

    if ((wvConvertUnicodeToEntity != NULL)
	&& wvConvertUnicodeToEntity (eachchar))
	return;

    if ((g_iconv_handle == (GIConv)-1) || strcmp (cached_outputtype, outputtype) != 0)
      {
	  if ((g_iconv_handle != (GIConv)-1))
	      g_iconv_close (g_iconv_handle);

	  g_iconv_handle = g_iconv_open (outputtype, "UCS-2");
	  if (g_iconv_handle == (GIConv) - 1)
	    {
		wvError (
			 ("g_iconv_open fail: %d, cannot convert %s to %s\n",
			  errno, "UCS-2", outputtype));
		printf ("?");
		return;
	    }

	  /* safe to cache the output type here */
	  strcpy (cached_outputtype, outputtype);

	  /* Determining if unicode biteorder is swapped (glibc < 2.2) */
	  need_swapping = 1;

	  buffer[0] = 0x20;
	  buffer[1] = 0;
	  ibuf = buffer;
	  obuf = buffer2;
	  ibuflen = 2;
	  obuflen = 5;

	  count = g_iconv (g_iconv_handle, &ibuf, &ibuflen, &obuf, &obuflen);
	  if (count >= 0)
	      need_swapping = buffer2[0] != 0x20;
      }

    if (need_swapping)
      {
	  buffer[0] = (eachchar >> 8) & 0x00ff;
	  buffer[1] = eachchar & 0x00ff;
      }
    else
      {
	  buffer[0] = eachchar & 0x00ff;
	  buffer[1] = (eachchar >> 8) & 0x00ff;
      }

    ibuf = buffer;
    obuf = buffer2;

    ibuflen = 2;
    len = obuflen = 5;

    count = g_iconv (g_iconv_handle, &ibuf, &ibuflen, &obuf, &obuflen);
    if (count == (size_t) - 1)
      {
	  wvError (("iconv failed errno: %d, char: 0x%X, %s -> %s\n",
		    errno, eachchar, "UCS-2", outputtype));

	  /* I'm torn here - do i just announce the failure, continue, or copy over to the other buffer? */

	  /* errno is usually 84 (illegal byte sequence)
	     should i reverse the bytes and try again? */
	  printf ("%c", ibuf[1]);
      }
    else
      {
	  len = len - obuflen;

	  for (i = 0; i < len; i++)
	      printf ("%c", buffer2[i]);
      }
}

int
wvHandleElement (wvParseStruct * ps, wvTag tag, void *props, int dirty)
{
    if (ps->elehandler)
	return ((*(ps->elehandler)) (ps, tag, props, dirty));
    wvError (("No element handler registered!!\n"));
    return (0);
}

int
wvHandleDocument (wvParseStruct * ps, wvTag tag)
{
    if (ps->dochandler)
	return ((*(ps->dochandler)) (ps, tag));
    wvError (("No dochandler!!\n"));
    return (0);
}

void
wvSetCharHandler (wvParseStruct * ps,
		  int (*proc) (wvParseStruct *, U16, U8, U16))
{
    ps->charhandler = proc;
}

void
wvSetSpecialCharHandler (wvParseStruct * ps,
			 int (*proc) (wvParseStruct *, U16, CHP *))
{
    ps->scharhandler = proc;
}

void
wvSetElementHandler (wvParseStruct * ps, 
		     int (*proc) (wvParseStruct *, wvTag, void *, int))
{
    ps->elehandler = proc;
}

void
wvSetDocumentHandler (wvParseStruct * ps,
		      int (*proc) (wvParseStruct *, wvTag))
{
    ps->dochandler = proc;
}
    
int
wvConvertUnicodeToLaTeX (U16 char16)
{
    /* 
       german and scandinavian characters, MV 1.7.2000 
       See man iso_8859_1

       This requires the inputencoding latin1 package,
       see latin1.def. Chars in range 160...255 are just
       put through as these are legal iso-8859-1 symbols.
       (see above)

       Best way to do it until LaTeX is Unicode enabled 
       (Omega project).
       -- MV 4.7.2000

       We use a separate if-statement here ... the 'case range'
       construct is gcc specific :-(  -- MV 13/07/2000
     */

    if ((char16 >= 0xa0) && (char16 <= 0xff))
      {
	  switch (char16)
	    {
	    case 0xa0:
		printf ("\\ ");	/* hard space */
		return (1);

		/* Fix up these as math characters: */
	    case 0xb1:
		printf ("$\\pm$");
		return (1);
	    case 0xb2:
		printf ("$\\mathtwosuperior$");
		return (1);
	    case 0xb3:
		printf ("$\\maththreesuperior$");
		return (1);
	    case 0xb5:
		printf ("$\\mu$");
		return (1);
	    case 0xb9:
		printf ("$\\mathonesuperior$");
		return (1);
	    case 0xd7:
		printf ("$\\times$");
		return (1);
	    }
	  printf ("%c", char16);
	  return (1);
      }
    switch (char16)
      {
      case 37:
	  printf ("\\%%");
	  return (1);
      case 10:
      case 11:
	  printf ("\\\\\n");
	  return (1);
      case 31:			/* non-required hyphen */
	  printf ("\\-");
	  return (1);
      case 30:			/* non-breaking hyphen */
	  printf ("-");
	  return (1);

	  /* case 45: minus/hyphen, pass through */

      case 12:
	  printf("\\newpage\n");
	  return (1);
      case 13:
      case 14:
      case 7:
	  return (1);
      case 9:
	  printf ("\\hfill{}");	/* tab -- horrible cludge */
	  return (1);
      case 0xf020:
	  printf (" ");		/* Mac specialty ? MV 10.10.2000 */
	  return (1);
      case 0xf02c:
	  printf (",");		/* Mac */
	  return (1);
      case 0xf028:
	  printf ("(");		/* Mac */
	  return (1);

      case 34:
	  printf ("\"");
	  return (1);
      case 35:
	  printf ("\\#");	/* MV 14.8.2000 */
	  return (1);
      case 36:
	  printf ("\\$");	/* MV 14.8.2000 */
	  return (1);
      case 38:
	  printf ("\\&");	/* MV 1.7.2000 */
	  return (1);
      case 92:
	  printf ("$\\backslash$");	/* MV 23.9.2000 */
	  return (1);
      case 94:
	  printf ("\\^");	/* MV 13.9.2000 */
	  return (1);
      case 95:
	  printf ("\\_");	/* MV 13.9.2000 */
	  return (1);
      case 60:
	  printf ("<");
	  return (1);
      case 0xf03e:		/* Mac */
      case 62:
	  printf (">");
	  return (1);

      case 0xF8E7:
	  /* without this, things should work in theory, but not for me */
	  printf ("_");
	  return (1);

	  /* Added some new Unicode characters. It's probably difficult
	     to write these characters in AbiWord, though ... :(
	     -- 2000-08-11 huftis@bigfoot.com */

      case 0x0100:
	  printf ("\\=A");	/* A with macron */
	  return (1);
      case 0x0101:
	  printf ("\\=a");	/* a with macron */
	  return (1);
      case 0x0102:
	  printf ("\\u{A}");	/* A with breve */
	  return (1);
      case 0x0103:
	  printf ("\\u{a}");	/* a with breve */
	  return (1);
      case 0x0104:
	  printf ("\\k{A}");	/* A with ogonek */
	  return (1);
      case 0x0105:
	  printf ("\\k{a}");	/* a with ogonek */
	  return (1);
      case 0x0106:
	  printf ("\\'C");	/* C with acute */
	  return (1);
      case 0x0107:
	  printf ("\\'c");	/* c with acute */
	  return (1);
      case 0x0108:
	  printf ("\\^C");	/* C with circumflex */
	  return (1);
      case 0x0109:
	  printf ("\\^c");	/* c with circumflex */
	  return (1);
      case 0x010A:
	  printf ("\\.C");	/* C with dot above */
	  return (1);
      case 0x010B:
	  printf ("\\.c");	/* c with dot above */
	  return (1);
      case 0x010C:
	  printf ("\\v{C}");	/* C with caron */
	  return (1);
      case 0x010D:
	  printf ("\\v{c}");	/* c with caron */
	  return (1);
      case 0x010E:
	  printf ("\\v{D}");	/* D with caron */
	  return (1);
      case 0x010F:
	  printf ("\\v{d}");	/* d with caron */
	  return (1);
      case 0x0110:
	  printf ("\\DJ{}");	/* D with stroke */
	  return (1);
      case 0x0111:
	  printf ("\\dj{}");	/* d with stroke */
	  return (1);
      case 0x0112:
	  printf ("\\=E");	/* E with macron */
	  return (1);
      case 0x0113:
	  printf ("\\=e");	/* e with macron */
	  return (1);
      case 0x0114:
	  printf ("\\u{E}");	/* E with breve */
	  return (1);
      case 0x0115:
	  printf ("\\u{e}");	/* e with breve */
	  return (1);
      case 0x0116:
	  printf ("\\.E");	/* E with dot above */
	  return (1);
      case 0x0117:
	  printf ("\\.e");	/* e with dot above */
	  return (1);
      case 0x0118:
	  printf ("\\k{E}");	/* E with ogonek */
	  return (1);
      case 0x0119:
	  printf ("\\k{e}");	/* e with ogonek */
	  return (1);
      case 0x011A:
	  printf ("\\v{E}");	/* E with caron */
	  return (1);
      case 0x011B:
	  printf ("\\v{e}");	/* e with caron */
	  return (1);
      case 0x011C:
	  printf ("\\^G");	/* G with circumflex */
	  return (1);
      case 0x011D:
	  printf ("\\^g");	/* g with circumflex */
	  return (1);
      case 0x011E:
	  printf ("\\u{G}");	/* G with breve */
	  return (1);
      case 0x011F:
	  printf ("\\u{g}");	/* g with breve */
	  return (1);
      case 0x0120:
	  printf ("\\.G");	/* G with dot above */
	  return (1);
      case 0x0121:
	  printf ("\\u{g}");	/* g with dot above */
	  return (1);
      case 0x0122:
	  printf ("^H");	/* H with circumflex */
	  return (1);
      case 0x0123:
	  printf ("^h");	/* h with circumflex */
	  return (1);

      case 0x0128:
	  printf ("\\~I");	/* I with tilde */
	  return (1);
      case 0x0129:
	  printf ("\\~{\\i}");	/* i with tilde (dotless) */
	  return (1);
      case 0x012A:
	  printf ("\\=I");	/* I with macron */
	  return (1);
      case 0x012B:
	  printf ("\\={\\i}");	/* i with macron (dotless) */
	  return (1);
      case 0x012C:
	  printf ("\\u{I}");	/* I with breve */
	  return (1);
      case 0x012D:
	  printf ("\\u{\\i}");	/* i with breve */
	  return (1);

      case 0x0130:
	  printf ("\\.I");	/* I with dot above */
	  return (1);
      case 0x0131:
	  printf ("\\i{}");	/* dotless i */
	  return (1);
      case 0x0132:
	  printf ("IJ");	/* IJ ligature */
	  return (1);
      case 0x0133:
	  printf ("ij");	/* ij ligature  */
	  return (1);
      case 0x0134:
	  printf ("\\^J");	/* J with circumflex (dotless) */
	  return (1);
      case 0x0135:
	  printf ("\\^{\\j}");	/* j with circumflex (dotless) */
	  return (1);
      case 0x0136:
	  printf ("\\c{K}");	/* K with cedilla */
	  return (1);
      case 0x0137:
	  printf ("\\c{k}");	/* k with cedilla */
	  return (1);

      case 0x0138:
	  printf ("k");		/* NOTE: Not the correct character (kra), but similar */
	  return (1);

      case 0x0139:
	  printf ("\\'L");	/* L with acute */
	  return (1);
      case 0x013A:
	  printf ("\\'l");	/* l with acute  */
	  return (1);
      case 0x013B:
	  printf ("\\c{L}");	/* L with cedilla */
	  return (1);
      case 0x013C:
	  printf ("\\c{l}");	/* l with cedilla */
	  return (1);
      case 0x013D:
	  printf ("\\v{L}");	/* L with caron */
	  return (1);
      case 0x013E:
	  printf ("\\v{l}");	/* l with caron */
	  return (1);

      case 0x0141:
	  printf ("\\L{}");	/* L with stroke */
	  return (1);
      case 0x0142:
	  printf ("\\l{}");	/* l with stroke  */
	  return (1);
      case 0x0143:
	  printf ("\\'N");	/* N with acute */
	  return (1);
      case 0x0144:
	  printf ("\\'n");	/* n with acute */
	  return (1);
      case 0x0145:
	  printf ("\\c{N}");	/* N with cedilla */
	  return (1);
      case 0x0146:
	  printf ("\\c{n}");	/* n with cedilla */
	  return (1);
      case 0x0147:
	  printf ("\\v{N}");	/* N with caron */
	  return (1);
      case 0x0148:
	  printf ("\\v{n}");	/* n with caron */
	  return (1);
      case 0x0149:
	  printf ("'n");	/* n preceed with apostroph  */
	  return (1);
      case 0x014A:
	  printf ("\\NG{}");	/* ENG character */
	  return (1);
      case 0x014B:
	  printf ("\\ng{}");	/* eng character */
	  return (1);
      case 0x014C:
	  printf ("\\=O");	/* O with macron */
	  return (1);
      case 0x014D:
	  printf ("\\=o");	/* o with macron */
	  return (1);
      case 0x014E:
	  printf ("\\u{O}");	/* O with breve */
	  return (1);
      case 0x014F:
	  printf ("\\u{o}");	/* o with breve */
	  return (1);
      case 0x0150:
	  printf ("\\H{O}");	/* O with double acute */
	  return (1);
      case 0x0151:
	  printf ("\\H{o}");	/* o with double acute */
	  return (1);
      case 0x0152:
	  printf ("\\OE{}");	/* OE ligature */
	  return (1);
      case 0x0153:
	  printf ("\\oe{}");	/* oe ligature */
	  return (1);
      case 0x0154:
	  printf ("\\'R");	/* R with acute */
	  return (1);
      case 0x0155:
	  printf ("\\'r");	/* r with acute */
	  return (1);
      case 0x0156:
	  printf ("\\c{R}");	/* R with cedilla */
	  return (1);
      case 0x0157:
	  printf ("\\c{r}");	/* r with cedilla */
	  return (1);
      case 0x0158:
	  printf ("\\v{R}");	/* R with caron */
	  return (1);
      case 0x0159:
	  printf ("\\v{r}");	/* r with caron */
	  return (1);
      case 0x015A:
	  printf ("\\'S");	/* S with acute */
	  return (1);
      case 0x015B:
	  printf ("\\'s");	/* s with acute */
	  return (1);
      case 0x015C:
	  printf ("\\^S");	/* S with circumflex */
	  return (1);
      case 0x015D:
	  printf ("\\^s");	/* c with circumflex */
	  return (1);
      case 0x015E:
	  printf ("\\c{S}");	/* S with cedilla */
	  return (1);
      case 0x015F:
	  printf ("\\c{s}");	/* s with cedilla */
	  return (1);
      case 0x0160:
	  printf ("\\v{S}");	/* S with caron */
	  return (1);
      case 0x0161:
	  printf ("\\v{s}");	/* s with caron */
	  return (1);
      case 0x0162:
	  printf ("\\c{T}");	/* T with cedilla */
	  return (1);
      case 0x0163:
	  printf ("\\c{t}");	/* t with cedilla */
	  return (1);
      case 0x0164:
	  printf ("\\v{T}");	/* T with caron */
	  return (1);
      case 0x0165:
	  printf ("\\v{t}");	/* t with caron */
	  return (1);

      case 0x0168:
	  printf ("\\~U");	/* U with tilde */
	  return (1);
      case 0x0169:
	  printf ("\\~u");	/* u with tilde */
	  return (1);
      case 0x016A:
	  printf ("\\=U");	/* U with macron */
	  return (1);

	  /* Greek (thanks Petr Vanicek!): */
      case 0x0391:
	  printf ("$A$");
	  return (1);
      case 0x0392:
	  printf ("$B$");
	  return (1);
      case 0x0393:
	  printf ("$\\Gamma$");
	  return (1);
      case 0xf044:		/* Mac ? */
      case 0x2206:		/* Mac */
      case 0x0394:
	  printf ("$\\Delta$");
	  return (1);
      case 0x0395:
	  printf ("$E$");
	  return (1);
      case 0x0396:
	  printf ("$Z$");
	  return (1);
      case 0x0397:
	  printf ("$H$");
	  return (1);
      case 0x0398:
	  printf ("$\\Theta$");
	  return (1);
      case 0x0399:
	  printf ("$I$");
	  return (1);
      case 0x039a:
	  printf ("$K$");
	  return (1);
      case 0x039b:
	  printf ("$\\Lambda$");
	  return (1);
      case 0xf04d:		/* Mac? */
      case 0x039c:
	  printf ("$M$");
	  return (1);
      case 0x039d:
	  printf ("$N$");
	  return (1);
      case 0x039e:
	  printf ("$\\Xi$");
	  return (1);
      case 0x039f:
	  printf ("$O$");	/* Omicron */
	  return (1);
      case 0x03a0:
	  printf ("$\\Pi$");
	  return (1);
      case 0x03a1:
	  printf ("$R$");
	  return (1);

      case 0x03a3:
	  printf ("$\\Sigma$");
	  return (1);
      case 0x03a4:
	  printf ("$T$");
	  return (1);
      case 0x03a5:
	  printf ("$Y$");
	  return (1);
      case 0x03a6:
	  printf ("$\\Phi$");
	  return (1);
      case 0x03a7:
	  printf ("$X$");	/* Chi */
	  return (1);
      case 0x03a8:
	  printf ("$\\Psi$");
	  return (1);
      case 0x2126:		/* Mac */
      case 0x03a9:
	  printf ("$\\Omega$");
	  return (1);

	  /* ...and lower case: */

      case 0x03b1:
	  printf ("$\\alpha$");
	  return (1);
      case 0x03b2:
	  printf ("$\\beta$");
	  return (1);
      case 0xf067:		/* Mac */
      case 0x03b3:
	  printf ("$\\gamma$");
	  return (1);
      case 0xf064:		/* Mac */
      case 0x03b4:
	  printf ("$\\delta$");
	  return (1);
      case 0x03b5:
	  printf ("$\\epsilon$");
	  return (1);
      case 0xf04e:		/* Mac? variant? */
      case 0xf07a:		/* Mac? */
      case 0x03b6:
	  printf ("$\\zeta$");
	  return (1);
      case 0x03b7:
	  printf ("$\\eta$");
	  return (1);
      case 0x03b8:
	  printf ("$\\theta$");
	  return (1);
      case 0x03b9:
	  printf ("$\\iota$");
	  return (1);
      case 0x03ba:
	  printf ("$\\kappa$");
	  return (1);
      case 0xf06c:		/* Mac? */
      case 0x03bb:
	  printf ("$\\lambda$");
	  return (1);
      case 0x03bc:
	  printf ("$\\mu$");
	  return (1);
      case 0x03bd:
	  printf ("$\\nu$");
	  return (1);
      case 0x03be:
	  printf ("$\\xi$");
	  return (1);
      case 0x03bf:
	  printf ("$o$");	/* omicron */
	  return (1);
      case 0x03c0:
	  printf ("$\\pi$");
	  return (1);
      case 0xf072:		/* Mac */
	  printf ("$\\varrho$");
	  return (1);
      case 0x03c1:
	  printf ("$\\rho$");
	  return (1);
      case 0xf073:		/* Mac */
      case 0x03c3:
	  printf ("$\\sigma$");
	  return (1);
      case 0x03c4:
	  printf ("$\\tau$");
	  return (1);
      case 0x03c5:
	  printf ("$\\upsilon$");
	  return (1);
      case 0x03c6:
	  printf ("$\\phi$");
	  return (1);
      case 0x03c7:
	  printf ("$\\chi$");
	  return (1);
      case 0x03c8:
	  printf ("$\\psi$");
	  return (1);
      case 0x03c9:
	  printf ("$\\omega$");
	  return (1);
      case 0xf06a:		/* Mac? */
      case 0x03d5:
	  printf ("$\\varphi$");	/* ? */
	  return (1);

	  /* More math, typical inline: */
      case 0x2111:
	  printf ("$\\Im$");
	  return (1);
      case 0x2118:
	  printf ("$\\wp$");	/* Weierstrass p */
	  return (1);
      case 0x211c:
	  printf ("$\\Re$");
	  return (1);
      case 0x2135:
	  printf ("$\\aleph$");
	  return (1);

      case 0x2190:
	  printf ("$\\leftarrow$");
	  return (1);
      case 0x2191:
	  printf ("$\\uparrow$");
	  return (1);
      case 0xf0ae:		/* Mac */
      case 0x2192:
	  printf ("$\\rightarrow$");
	  return (1);
      case 0x2193:
	  printf ("$\\downarrow$");
	  return (1);
      case 0x21d0:
	  printf ("$\\Leftarrow$");
	  return (1);
      case 0x21d1:
	  printf ("$\\Uparrow$");
	  return (1);
      case 0x21d2:
	  printf ("$\\Rightarrow$");
	  return (1);
      case 0x21d3:
	  printf ("$\\Downarrow$");
	  return (1);
      case 0x21d4:
	  printf ("$\\Leftrightarrow$");
	  return (1);

      case 0x2200:
	  printf ("$\\forall$");
	  return (1);
      case 0xf0b6:		/* Mac */
      case 0x2202:
	  printf ("$\\partial$");
	  return (1);
      case 0x2203:
	  printf ("$\\exists$");
	  return (1);
      case 0x2205:
	  printf ("$\\emptyset$");
	  return (1);
      case 0x2207:
	  printf ("$\\nabla$");
	  return (1);
      case 0x2208:
	  printf ("$\\in$");	/* element of */
	  return (1);
      case 0x2209:
	  printf ("$\\notin$");	/* not an element of */
	  return (1);
      case 0x220b:
	  printf ("$\\ni$");	/* contains as member */
	  return (1);
      case 0x221a:
	  printf ("$\\surd$");	/* sq root */
	  return (1);
      case 0x2212:
	  printf ("$-$");	/* minus */
	  return (1);
      case 0x221d:
	  printf ("$\\propto$");
	  return (1);
      case 0x221e:
	  printf ("$\\infty$");
	  return (1);
      case 0x2220:
	  printf ("$\\angle$");
	  return (1);
      case 0x2227:
	  printf ("$\\land$");	/* logical and */
	  return (1);
      case 0x2228:
	  printf ("$\\lor$");	/* logical or */
	  return (1);
      case 0x2229:
	  printf ("$\\cap$");	/* intersection */
	  return (1);
      case 0x222a:
	  printf ("$\\cup$");	/* union */
	  return (1);
      case 0x223c:
	  printf ("$\\sim$");	/* similar to  */
	  return (1);
      case 0x2248:
	  printf ("$\\approx$");
	  return (1);
      case 0x2261:
	  printf ("$\\equiv$");
	  return (1);
      case 0x2260:
	  printf ("$\\neq$");
	  return (1);
      case 0x2264:
	  printf ("$\\leq$");
	  return (1);
      case 0xf0b3:		/* Mac? */
      case 0x2265:
	  printf ("$\\geq$");
	  return (1);
      case 0x2282:
	  printf ("$\\subset$");
	  return (1);
      case 0x2283:
	  printf ("$\\supset$");
	  return (1);
      case 0x2284:
	  printf ("$\\notsubset$");
	  return (1);
      case 0x2286:
	  printf ("$\\subseteq$");
	  return (1);
      case 0x2287:
	  printf ("$\\supseteq$");
	  return (1);
      case 0x2295:
	  printf ("$\\oplus$");	/* circled plus */
	  return (1);
      case 0x2297:
	  printf ("$\\otimes$");
	  return (1);
      case 0x22a5:
	  printf ("$\\perp$");	/* perpendicular */
	  return (1);




      case 0x2660:
	  printf ("$\\spadesuit$");
	  return (1);
      case 0x2663:
	  printf ("$\\clubsuit$");
	  return (1);
      case 0x2665:
	  printf ("$\\heartsuit$");
	  return (1);
      case 0x2666:
	  printf ("$\\diamondsuit$");
	  return (1);


      case 0x01C7:
	  printf ("LJ");	/* the LJ letter */
	  return (1);
      case 0x01C8:
	  printf ("Lj");	/* the Lj letter */
	  return (1);
      case 0x01C9:
	  printf ("lj");	/* the lj letter */
	  return (1);
      case 0x01CA:
	  printf ("NJ");	/* the NJ letter */
	  return (1);
      case 0x01CB:
	  printf ("Nj");	/* the Nj letter */
	  return (1);
      case 0x01CC:
	  printf ("nj");	/* the nj letter */
	  return (1);
      case 0x01CD:
	  printf ("\\v{A}");	/* A with caron */
	  return (1);
      case 0x01CE:
	  printf ("\\v{a}");	/* a with caron */
	  return (1);
      case 0x01CF:
	  printf ("\\v{I}");	/* I with caron */
	  return (1);
      case 0x01D0:
	  printf ("\\v{\\i}");	/* i with caron (dotless) */
	  return (1);
      case 0x01D1:
	  printf ("\\v{O}");	/* O with caron */
	  return (1);
      case 0x01D2:
	  printf ("\\v{o}");	/* o with caron */
	  return (1);
      case 0x01D3:
	  printf ("\\v{U}");	/* U with caron */
	  return (1);
      case 0x01D4:
	  printf ("\\v{u}");	/* u with caron */
	  return (1);

      case 0x01E6:
	  printf ("\\v{G}");	/* G with caron */
	  return (1);
      case 0x01E7:
	  printf ("\\v{g}");	/* g with caron */
	  return (1);
      case 0x01E8:
	  printf ("\\v{K}");	/* K with caron */
	  return (1);
      case 0x01E9:
	  printf ("\\v{k}");	/* k with caron */
	  return (1);


      case 0x01F0:
	  printf ("\\v{\\j}");	/* j with caron (dotless) */
	  return (1);
      case 0x01F1:
	  printf ("DZ");	/* the DZ letter */
	  return (1);
      case 0x01F2:
	  printf ("Dz");	/* the Dz letter */
	  return (1);
      case 0x01F3:
	  printf ("dz");	/* the dz letter */
	  return (1);
      case 0x01F4:
	  printf ("\\'G");	/* G with acute */
	  return (1);
      case 0x01F5:
	  printf ("\\'g");	/* g with acute */
	  return (1);

      case 0x01FA:
	  printf ("\\'{\\AA}");	/* Å with acute */
	  return (1);
      case 0x01FB:
	  printf ("\\'{\\aa}");	/* å with acute */
	  return (1);
      case 0x01FC:
	  printf ("\\'{\\AE}");	/* Æ with acute */
	  return (1);
      case 0x01FD:
	  printf ("\\'{\\ae}");	/* æ with acute */
	  return (1);
      case 0x01FE:
	  printf ("\\'{\\O}");	/* Ø with acute */
	  return (1);
      case 0x01FF:
	  printf ("\\'{\\o}");	/* ø with acute */
	  return (1);

      case 0x2010:
	  printf ("-");		/* hyphen */
	  return (1);
      case 0x2011:
	  printf ("-");		/* non-breaking hyphen (is there a way to get this in LaTeX?) */
	  return (1);
      case 0x2012:
	  printf ("--");	/* figure dash (similar to en-dash) */
	  return (1);
      case 0x2013:
	  /* 
	     soft-hyphen? Or en-dash? I find that making 
	     this a soft-hyphen works very well, but makes
	     the occasional "hard" word-connection hyphen 
	     (like the "-" in roller-coaster) disappear.
	     (Are these actually en-dashes? Dunno.)
	     How does MS Word distinguish between the 0x2013's
	     that signify soft hyphens and those that signify
	     word-connection hyphens? wvware should be able
	     to as well. -- MV 8.7.2000

	     U+2013 is the en-dash character and not a soft
	     hyphen. Soft hyphen is U+00AD. Changing to
	     "--". -- 2000-08-11 huftis@bigfoot.com
	   */
	  printf ("--");
	  return (1);

      case 0x016B:
	  printf ("\\=u");	/* u with macron */
	  return (1);
      case 0x016C:
	  printf ("\\u{U}");	/* U with breve */
	  return (1);
      case 0x016D:
	  printf ("\\u{u}");	/* u with breve */
	  return (1);
      case 0x016E:
	  printf ("\\r{U}");	/* U with ring above */
	  return (1);
      case 0x016F:
	  printf ("\\r{u}");	/* u with ring above */
	  return (1);
      case 0x0170:
	  printf ("\\H{U}");	/* U with double acute */
	  return (1);
      case 0x0171:
	  printf ("\\H{u}");	/* u with double acute */
	  return (1);

      case 0x0174:
	  printf ("\\^W");	/* W with circumflex */
	  return (1);
      case 0x0175:
	  printf ("\\^w");	/* w with circumflex */
	  return (1);
      case 0x0176:
	  printf ("\\^Y");	/* Y with circumflex */
	  return (1);
      case 0x0177:
	  printf ("\\^y");	/* y with circumflex */
	  return (1);
      case 0x0178:
	  printf ("\\\"Y");	/* Y with diaeresis */
	  return (1);
      case 0x0179:
	  printf ("\\'Z");	/* Z with acute */
	  return (1);
      case 0x017A:
	  printf ("\\'z");	/* z with acute */
	  return (1);
      case 0x017B:
	  printf ("\\.Z");	/* Z with dot above */
	  return (1);
      case 0x017C:
	  printf ("\\.z");	/* z with dot above */
	  return (1);
      case 0x017D:
	  printf ("\\v{Z}");	/* Z with caron */
	  return (1);
      case 0x017E:
	  printf ("\\v{z}");	/* z with caron */
	  return (1);

	  /* Windows specials (MV 4.7.2000). More could be added. 
	     See http://www.hut.fi/u/jkorpela/www/windows-chars.html
	   */

      case 0x2014:
	  printf ("---");	/* em-dash */
	  return (1);
      case 0x2018:
	  printf ("`");		/* left single quote, Win */
	  return (1);
      case 0x2019:
	  printf ("'");		/* Right single quote, Win */
	  return (1);
      case 0x201A:
	  printf ("\\quotesinglbase{}");	/* single low 99 quotation mark */
	  return (1);
      case 0x201C:
	  printf ("``");	/* inverted double quotation mark */
	  return (1);
      case 0x201D:
	  printf ("''");	/* double q.m. */
	  return (1);
      case 0x201E:
	  printf ("\\quotedblbase{}");	/* double low 99 quotation mark */
	  return (1);
      case 0x2020:
	  printf ("\\dag{}");	/* dagger */
	  return (1);
      case 0x2021:
	  printf ("\\ddag{}");	/* double dagger */
	  return (1);
      case 0x25cf:		/* FilledCircle */
      case 0x2022:
	  printf ("$\\bullet$");	/* bullet */
	  return (1);
      case 0x2023:
	  printf ("$\\bullet$");	/* NOTE: Not a real triangular bullet */
	  return (1);

      case 0x2024:
	  printf (".");		/* One dot leader (for use in TOCs) */
	  return (1);
      case 0x2025:
	  printf ("..");	/* Two dot leader (for use in TOCs) */
	  return (1);
      case 0x2026:
	  printf ("\\ldots");	/* ellipsis */
	  return (1);

      case 0x2039:
	  printf ("\\guilsinglleft{}");	/* single left angle quotation mark */
	  return (1);
      case 0x203A:
	  printf ("\\guilsinglright{}");	/* single right angle quotation mark */
	  return (1);

      case 0x203C:
	  printf ("!!");	/* double exclamation mark */
	  return (1);

      case 0x2215:
	  printf ("$/$");	/* Division slash */
	  return (1);

      case 0x2030:
	  printf ("o/oo");
	  return (1);

      case 0x20ac:
	  printf ("\\euro");
	  /* No known implementation ;-)

	     Shouldn't we use the package 'eurofont'?
	     -- 2000-08-15 huftis@bigfoot.com 
	   */
	  return (1);

      case 0x2160:
	  printf ("I");		/* Roman numeral I */
	  return (1);
      case 0x2161:
	  printf ("II");	/* Roman numeral II */
	  return (1);
      case 0x2162:
	  printf ("III");	/* Roman numeral III */
	  return (1);
      case 0x2163:
	  printf ("IV");	/* Roman numeral IV */
	  return (1);
      case 0x2164:
	  printf ("V");		/* Roman numeral V */
	  return (1);
      case 0x2165:
	  printf ("VI");	/* Roman numeral VI */
	  return (1);
      case 0x2166:
	  printf ("VII");	/* Roman numeral VII */
	  return (1);
      case 0x2167:
	  printf ("VIII");	/* Roman numeral VIII */
	  return (1);
      case 0x2168:
	  printf ("IX");	/* Roman numeral IX */
	  return (1);
      case 0x2169:
	  printf ("X");		/* Roman numeral X */
	  return (1);
      case 0x216A:
	  printf ("XI");	/* Roman numeral XI */
	  return (1);
      case 0x216B:
	  printf ("XII");	/* Roman numeral XII */
	  return (1);
      case 0x216C:
	  printf ("L");		/* Roman numeral L */
	  return (1);
      case 0x216D:
	  printf ("C");		/* Roman numeral C */
	  return (1);
      case 0x216E:
	  printf ("D");		/* Roman numeral D */
	  return (1);
      case 0x216F:
	  printf ("M");		/* Roman numeral M */
	  return (1);
      case 0x2170:
	  printf ("i");		/* Roman numeral i */
	  return (1);
      case 0x2171:
	  printf ("ii");	/* Roman numeral ii */
	  return (1);
      case 0x2172:
	  printf ("iii");	/* Roman numeral iii */
	  return (1);
      case 0x2173:
	  printf ("iv");	/* Roman numeral iv */
	  return (1);
      case 0x2174:
	  printf ("v");		/* Roman numeral v */
	  return (1);
      case 0x2175:
	  printf ("vi");	/* Roman numeral vi */
	  return (1);
      case 0x2176:
	  printf ("vii");	/* Roman numeral vii */
	  return (1);
      case 0x2177:
	  printf ("viii");	/* Roman numeral viii */
	  return (1);
      case 0x2178:
	  printf ("ix");	/* Roman numeral ix */
	  return (1);
      case 0x2179:
	  printf ("x");		/* Roman numeral x */
	  return (1);
      case 0x217A:
	  printf ("xi");	/* Roman numeral xi */
	  return (1);
      case 0x217B:
	  printf ("xiii");	/* Roman numeral xii */
	  return (1);
      case 0x217C:
	  printf ("l");		/* Roman numeral l */
	  return (1);
      case 0x217D:
	  printf ("c");		/* Roman numeral c */
	  return (1);
      case 0x217E:
	  printf ("d");		/* Roman numeral d */
	  return (1);
      case 0x217F:
	  printf ("m");		/* Roman numeral m */
	  return (1);

      }
    /* Debugging aid: */
    if (char16 >= 0x80)
      {
	printf ("[%x]", char16);
	return (1);
      }
    return (0);
}

int
wvConvertUnicodeToHtml (U16 char16)
{
    switch (char16)
      {
      case 11:
	  printf ("<br>");
	  return (1);
      case 31:        /* non-required hyphen */   
	  printf("&shy;"); /*vladimir@lukianov.name HTML 4.01 spec*/
	  return (1);
      case 30:
      case 45:
      case 0x2013:
	  printf ("-");		/* en-dash */
	  return (1);
      case 12:
      case 13:
      case 14:
      case 7:
	  return (1);
      case 34:
	  printf ("&quot;");
	  return (1);
      case 38:
	  printf ("&amp;");
	  return (1);
      case 60:
	  printf ("&lt;");
	  return (1);
      case 62:
	  printf ("&gt;");
	  return (1);
	  /*
	     german characters, im assured that this is the right way to handle them
	     by Markus Schulte <markus@dom.de>

	     As the output encoding for HTML was chosen as UTF-8, 
	     we don't need &Auml; etc. etc. I removed all but sz 
	     -- MV 6.4.2000
	   */

      case 0xdf:
	  printf ("&szlig;");
	  return (1);
	  /* end german characters */
      case 0x2026:
#if 0
/* 
this just looks awful in netscape 4.5, so im going to do a very foolish
thing and just put ... instead of this
*/
	  printf ("&#133;");
/*is there a proper html name for ... &ellipse;? Yes, &hellip; -- MV */
#endif
	  printf ("&hellip;");
	  return (1);
      case 0x2019:
	  printf ("'");
	  return (1);
      case 0x2215:
	  printf ("/");
	  return (1);
      case 0xF8E7:		/* without this, things should work in theory, but not for me */
	  printf ("_");
	  return (1);
      case 0x2018:
	  printf ("`");
	  return (1);

	  /* Windows specials (MV): */
      case 0x0160:
	  printf ("&Scaron;");
	  return (1);
      case 0x0161:
	  printf ("&scaron;");
	  return (1);
      case 0x2014:
	  printf ("&mdash;");
	  return (1);
      case 0x201c:
	  printf ("&ldquo;");	/* inverted double quotation mark */
	  return (1);
      case 0x201d:
	  printf ("&rdquo;");	/* double q.m. */
	  return (1);
      case 0x201e:
	  printf ("&bdquo;");	/* below double q.m. */
	  return (1);
      case 0x2020:
	  printf ("&dagger;");
	  return (1);
      case 0x2021:
	  printf ("&Dagger;");
	  return (1);
      case 0x2022:
	  printf ("&bull;");
	  return (1);
      case 0x0152:
	  printf ("&OElig;");
	  return (1);
      case 0x0153:
	  printf ("&oelig;");
	  return (1);
      case 0x0178:
	  printf ("&Yuml;");
	  return (1);
      case 0x2030:
	  printf ("&permil;");
	  return (1);
      case 0x20ac:
	  printf ("&euro;");
	  return (1);

	  /* Mac specials (MV): */

      case 0xf020:
	  printf (" ");
	  return (1);
      case 0xf02c:
	  printf (",");
	  return (1);
      case 0xf028:
	  printf ("(");
	  return (1);

      case 0xf03e:
	  printf ("&gt;");
	  return (1);
      case 0xf067:
	  printf ("&gamma;");
	  return (1);
      case 0xf064:
	  printf ("&delta;");
	  return (1);
      case 0xf072:
	  printf ("&rho;");
	  return (1);
      case 0xf073:
	  printf ("&sigma;");
	  return (1);
      case 0xf0ae:
	  printf ("&rarr;");	/* right arrow */
	  return (1);
      case 0xf0b6:
	  printf ("&part;");	/* partial deriv. */
	  return (1);
      case 0xf0b3:
	  printf ("&ge;");
	  return (1);

      }
    /* Debugging aid: */
    /* if (char16 >= 0x100) printf("[%x]", char16); */
    return (0);
}



int
wvConvertUnicodeToXml (U16 char16)
{
    switch (char16)
      {
      case 11:
	  printf ("<br />");
	  return (1);

      case 30:
      case 31:
      case 12:
      case 13:
      case 14:
      case 7:
	  return (1);

      /* Much simpler here, because XML supports only a few entities */
      case 34:
	  printf ("&quot;");
	  return (1);
      case 38:
	  printf ("&amp;");
	  return (1);
      case 39:
	  printf ("&apos;");
	  return (1);
      case 60:
	  printf ("&lt;");
	  return (1);
      case 62:
	  printf ("&gt;");
	  return (1);
      }
    
    return (0);
}

char *str_copy(char *d, size_t n, char *s)
{
    strncpy(d, s, n);
    d[n-1] = 0;
    return d;
}

char *str_append(char *d, size_t n, char *s)
{
    int max = n - strlen(d) - 1;
    strncat(d, s, max);
    d[n-1] = 0;
    return d;
}    

#define BUF_COPY(d,s) str_copy(d,sizeof(d),s)

char *
wvConvertStylename(char *stylename, char *outputtype)
{
    static char cached_outputtype[36];
    static GIConv g_iconv_handle = (GIConv)-1;
    /**FIXME: 100 is just the size of stylename[] from wv.h**/
    static char buffer[100];
    char *ibuf, *obuf;
    size_t ibuflen, obuflen, len;

    /* Destroy */
    if(!outputtype) 
    {
	if ((g_iconv_handle != (GIConv)-1))
	    g_iconv_close(g_iconv_handle);
	return NULL;
    }

    /* Initialize */
    if(!g_iconv_handle || strcmp(cached_outputtype, outputtype))
    {
	if ((g_iconv_handle != (GIConv)-1))
	    g_iconv_close(g_iconv_handle);

	/**FIXME: don´t know if ISO-8859-1 is really the correct
	 **charset for style names with eg umlauts.             **/
	g_iconv_handle = g_iconv_open(outputtype, "ISO-8859-1");
	if(g_iconv_handle == (GIConv)-1)
	{
	    wvError(("g_iconv_open fail: %d, cannot convert %s to %s\n",
		     errno, "ISO-8859-1", outputtype));
	    return stylename;
	}
    
	BUF_COPY(cached_outputtype, outputtype);
    }

    /* Convert */
    ibuf    = stylename;
    ibuflen = strlen(stylename);
    obuf    = buffer;
    obuflen = sizeof(buffer) - 1;
    len     = g_iconv (g_iconv_handle, &ibuf, &ibuflen, &obuf, &obuflen);
    *obuf   = 0;
    if(len == -1) 
    {
	wvError(("wvConfig.c: can´t iconv()\n"));
	return stylename;
    }
    
    return buffer;
}
