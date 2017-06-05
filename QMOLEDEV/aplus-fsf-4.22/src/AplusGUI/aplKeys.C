///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <MSGUI/MSKeyPress.H>

typedef struct 
{
  const char *_pTranslationString;
  const char *_pAPLString;
} APLKeyTranslationStruct;

static APLKeyTranslationStruct APLKeyTranslations[] = 
{ 
 { "<Key>KP_0",       "0" },
 { "<Key>KP_1",       "1" },
 { "<Key>KP_2",       "2" },
 { "<Key>KP_3",       "3" }, 
 { "<Key>KP_4",       "4" }, 
 { "<Key>KP_5",       "5" }, 
 { "<Key>KP_6",       "6" }, 
 { "<Key>KP_7",       "7" }, 
 { "<Key>KP_8",       "8" }, 
 { "<Key>KP_9",       "9" }, 
 { "<Key>KP_Decimal", "." }, 
 { "!Meta<Key>a", "Á" },
 { "!Meta<Key>b", "Â" },
 { "!Meta<Key>c", "Ã" },
 { "!Meta<Key>d", "Ä" },
 { "!Meta<Key>e", "Å" },
 { "!Meta<Key>f", "_" },
 { "!Meta<Key>g", "Ç" },
 { "!Meta<Key>h", "È" },
 { "!Meta<Key>i", "É" },
 { "!Meta<Key>j", "Ê" },
 { "!Meta<Key>k", "'" },
 { "!Meta<Key>l", "Ì" },
 { "!Meta<Key>m", "|" },
 { "!Meta<Key>n", "Î" },
 { "!Meta<Key>o", "Ï" },
 { "!Meta<Key>p", "*" },
 { "!Meta<Key>q", "?" },
 { "!Meta<Key>r", "Ò" },
 { "!Meta<Key>s", "Ó" },
 { "!Meta<Key>t", "~" },
 { "!Meta<Key>u", "Õ" },
 { "!Meta<Key>v", "Ö" },
 { "!Meta<Key>w", "×" },
 { "!Meta<Key>x", "Ø" },
 { "!Meta<Key>y", "Ù" },
 { "!Meta<Key>z", "Ú" },
 { "!Meta<Key>1", "¡" },
 { "!Meta<Key>2", "¢" },
 { "!Meta<Key>3", "<" },
 { "!Meta<Key>4", "¤" },
 { "!Meta<Key>5", "=" },
 { "!Meta<Key>6", "¦" },
 { "!Meta<Key>7", ">" },
 { "!Meta<Key>8", "¨" },
 { "!Meta<Key>9", "©" },
 { "!Meta<Key>0", "^" },
 { "!Shift Meta<Key>exclam",      "à" },
 { "!Shift Meta<Key>at",          "æ" },
 { "!Shift Meta<Key>numbersign",  "ç" },
 { "!Shift Meta<Key>dollar",      "è" },
 { "!Shift Meta<Key>percent",     "÷" },
 { "!Shift Meta<Key>asciicircum", "ô" },
 { "!Shift Meta<Key>ampersand",   "á" },
 { "!Shift Meta<Key>asterisk",    "ð" },
 { "!Shift Meta<Key>parenleft",   "¹" },
 { "!Shift Meta<Key>parenright",  "°" },
 { "!Shift Meta<Key>underscore",  "!" },
 { "!Meta<Key>minus",        "«" },
 { "!Meta<Key>equal",        "ß" },
 { "!Meta<Key>backslash",    "Ü" },
 { "!Meta<Key>quoteleft",    "þ" },
 { "!Meta<Key>comma",        "¬" },
 { "!Meta<Key>bracketleft",  "û" },
 { "!Meta<Key>bracketright", "ý" },
 { "!Meta<Key>semicolon",    "Û" },
 { "!Meta<Key>apostrophe",   "Ý" },
 { "!Meta<Key>period",       "Ü" },
 { "!Meta<Key>slash",        "¯" },
 { "!Meta<Key>braceleft",    "à" },
 { "!Meta<Key>at",           "æ" },
 { "!Meta<Key>numbersign",   "ç" },
 { "!Meta<Key>dollar",       "è" },
 { "!Meta<Key>percent",      "÷" },
 { "!Meta<Key>asciicircum",  "ô" },
 { "!Meta<Key>ampersand",    "á" },
 { "!Meta<Key>asterisk",     "ð" },
 { "!Meta<Key>parenleft",    "¹" },
 { "!Meta<Key>parenright",   "°" },
 { "!Meta<Key>underscore",   "!" },
 { "!Shift Meta <Key>plus",  "­" },
 { "!Shift Meta <Key>bar",   "ü" },
 { "Shift Meta<Key>o",       "ï" },
 { "Shift Meta<Key>j",       "ê" },
 { "Shift Meta<Key>f",       "½" },
 { "Shift Meta<Key>e",       "å" },
 { "Shift Meta<Key>i",       "é" },
 { "Shift Meta<Key>braceleft",  "Ý" },
 { "Shift Meta<Key>braceright", "Û" },
 { "Shift Meta<Key>s",          "¾" },
 { "Shift Meta<Key>g",          "ç" },
 { "Shift Meta<Key>h",          "è" },
 { "Shift Meta<Key>l",          "ì" },
 { "Shift Meta<Key>colon",      "¼" },
 { "Shift Meta<Key>quotedbl",   "»" },
 { "Shift Meta<Key>c",          "ã" },
 { "Shift Meta<Key>b",          "â" },
 { "Shift Meta<Key>n",          "î" },
 { "Shift Meta<Key>m",          "Í" },
 { "Shift Meta<Key>greater",    "®" },
 { "Shift Meta<Key>p",          "³" },
 { "Shift Meta<Key>y",          "´" },
 { "Shift Meta<Key>z",          "ú" },
 { "Shift Meta<Key>q",          "¿" },
 { "Shift Meta<Key>x",          "X" },
 { "Shift Meta<Key>v",          "V" },
 { "Shift Meta<Key>slash",      "?" },
 { 0,0 }
};

MSKeyPress *_keyPressTable=0;

// this class helps us clean up memory when
// the application exits.

class StaticInitClass
{
public:
  StaticInitClass(void);
  ~StaticInitClass(void);  
};

StaticInitClass::StaticInitClass(void) {}
StaticInitClass::~StaticInitClass(void)  
{
  if (_keyPressTable!=0)
   {
     delete [] (char *)_keyPressTable;
     _keyPressTable=0;
   }
}  

static StaticInitClass _staticInitClass;

static void initKeyPressTable(void)
{
  int n = sizeof(APLKeyTranslations)/sizeof(APLKeyTranslationStruct)-1;
  _keyPressTable = (MSKeyPress *)new char [n*sizeof(MSKeyPress)];
  for(int i=0;i<n;i++)
    {
      new (&_keyPressTable[i]) MSKeyPress(APLKeyTranslations[i]._pTranslationString);
    }
}

const char *aplKeyTranslationFunction(const XEvent *pEvent_)
{
  char    buf[16];
  KeySym  keysym;
  int     len=XLookupString((XKeyEvent *)pEvent_,buf,8,&keysym,NULL);
  buf[len]='\0';

  if(_keyPressTable==0) initKeyPressTable();
  int n = sizeof(APLKeyTranslations)/sizeof(APLKeyTranslationStruct)-1;
  for(int i=0; i<n;i++)
    {
      if (_keyPressTable[i].isMatch(keysym,pEvent_->xkey.state))
	{
	  return APLKeyTranslations[i]._pAPLString;
	}
    }
  return 0;
}






