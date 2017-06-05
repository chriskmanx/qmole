/********************************************************************************
*                                                                               *
*                       M e s s a g e   T r a n s l a t o r                     *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXTranslator.cpp,v 1.13 2006/01/22 17:58:49 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObject.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXTranslator.h"


/*
  Notes:
  - The input message string may be not UTF-8 but some other code
    page, the "developer's code page".  If we allow for explicit
    setting of the "developer's code page" we can perform another
    translation from code-page -> utf-8 here as well.  This will
    be convenient since this allows e.g. a russion programmer to
    just use his editor in koi8 setting.
  - The above does mean we need to return FXString instead of a
    pointer, perhaps...
*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXTranslator,FXObject,NULL,0)


// Construct translator
FXTranslator::FXTranslator(FXApp* a):app(a),codec(NULL){
  FXTRACE((100,"%p->FXTranslator::FXTranslator\n",this));
  }

/*
#ifdef WIN32
  LCID mylcid=GetUserDefaultLCID();
  TCHAR buffer[256];

  // ISO Standard 639 values language name
  GetLocaleInfo(mylcid,LOCALE_SISO639LANGNAME,buffer,sizeof(buffer)/sizeof(TCHAR));

  // ISO Standard 3166 country names
  GetLocaleInfo(mylcid,LOCALE_SISO3166CTRYNAME,buffer,sizeof(buffer)/sizeof(TCHAR));

  // ISO Standard 4217 currency
  GetLocaleInfo(mylcid,LOCALE_SINTLSYMBOL,buffer,sizeof(buffer)/sizeof(TCHAR)));

#else
  char *locale=setlocale(LC_ALL, NULL);
  char *lang=strstr(locale,"LANG=");
  if(!lang) lang=strstr(locale,"LC_MESSAGES=");
  if(!lang) lang=strstr(locale,"LC_CTYPE=");
  if(!lang){	// Try the LANG environment variable
    lang=getenv("LANG");
    if(!lang) break;
    }
  else{
    lang=strchr(lang, '=')+1;
    if('"'==*lang) lang++;
    }
  while(*lang!='_'){
    iso639.append(*lang);
    lang++;
    }
  lang++;
  while(isalpha(*lang) || '@'==*lang){
    iso3166.append(*lang);
    lang++;
    }
#endif
*/


// Translate a string
const FXchar* FXTranslator::tr(const FXchar* context,const FXchar* message,const FXchar* hint) const {
  FXTRACE((200,"tr context: '%s' message: '%s' hint: '%s'.\n",context,message,hint?hint:""));
  return message;
  }


// Save data
void FXTranslator::save(FXStream& store) const {
  FXObject::save(store);
  }


// Load data
void FXTranslator::load(FXStream& store){
  FXObject::load(store);
  }


// Destroy translator
FXTranslator::~FXTranslator(){
  FXTRACE((100,"%p->FXTranslator::~FXTranslator\n",this));
  app=(FXApp*)-1L;
  codec=(FXTextCodec*)-1L;
  }

}
