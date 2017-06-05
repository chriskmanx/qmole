///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <MSTypes/MSString.H>
#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSPrintFont.H>

#ifdef MS_WINDOWS
extern "C" int MSXGetFontDir(char*,int);
const char        *MSPrintFont::_defaultFontPath=0;
const char        *MSPrintFont::_altDefaultFontPath="/usr/local/lib/transcript";
#else
const char        *MSPrintFont::_defaultFontPath="/usr/local/lib/transcript";
const char        *MSPrintFont::_altDefaultFontPath="/usr/local/lib/transcript_4.0";
#endif

MSHashTable       *MSPrintFont::_fontHashTable=0;
MSStringHashTable *MSPrintFont::_afmfilesHashTable=0;
MSHashTable       *MSPrintFont::fontHashTable(void)     {return _fontHashTable;}
MSStringHashTable *MSPrintFont::afmfilesHashTable(void) {return _afmfilesHashTable;}
static const int   HashTableSize=128;
static MSBoolean  _initialized=MSFalse;
static int        _count=0;

MSPrintFont::MSPrintFont(void) 
{
 if (_initialized==MSFalse)
   {
     _initialized=MSTrue;
     _fontHashTable=new MSHashTable(HashTableSize);
     initFontHashTable();
   }
 _count++;
}

MSPrintFont::~MSPrintFont(void) 
{
  _count--;
  if (_count<=0)
   {
     delete _fontHashTable;
     delete _afmfilesHashTable;
     _fontHashTable=0;
     _afmfilesHashTable=0;
     _initialized=MSFalse;
   }
}

const char *MSPrintFont::defaultFontPath(void)
{ return _defaultFontPath; }

const char *MSPrintFont::altDefaultFontPath(void)
{ return _altDefaultFontPath; }

void MSPrintFont::initFontHashTable(void)
{
  fontHashTable()->notFound((unsigned long)0);
  fontHashTable()->add("avantgarde-book",(void*)"AvantGarde-Book");
  fontHashTable()->add("avantgarde-bookoblique",(void*)"AvantGarde-BookOblique");
  fontHashTable()->add("avantgarde-demi",(void*)"AvantGarde-Demi");
  fontHashTable()->add("avantgarde-demioblique",(void*)"AvantGarde-DemiOblique");
  fontHashTable()->add("bookman-demi",(void*)"Bookman-Demi");
  fontHashTable()->add("bookman-demiitalic",(void*)"Bookman-DemiItalic");
  fontHashTable()->add("bookman-light",(void*)"Bookman-Light");
  fontHashTable()->add("bookman-lightitalic",(void*)"Bookman-LightItalic");
  fontHashTable()->add("courier",(void*)"Courier");
  fontHashTable()->add("courier-normal",(void*)"Courier");
  fontHashTable()->add("courier-bold",(void*)"Courier-Bold");
  fontHashTable()->add("courier-boldoblique",(void*)"Courier-BoldOblique");
  fontHashTable()->add("courier-oblique",(void*)"Courier-Oblique");
  fontHashTable()->add("dithacks",(void*)"DIThacks");
  fontHashTable()->add("garamond-light",(void*)"Garamond-Light");
  fontHashTable()->add("garamond-lightitalic",(void*)"Garamond-LightItalic");
  fontHashTable()->add("garamond-bold",(void*)"Garamond-Bold");
  fontHashTable()->add("garamond-bolditalic",(void*)"Garamond-BoldItalic");
  fontHashTable()->add("helvetica",(void*)"Helvetica");
  fontHashTable()->add("helvetica-bold",(void*)"Helvetica-Bold");
  fontHashTable()->add("helvetica-boldoblique",(void*)"Helvetica-BoldOblique");
  fontHashTable()->add("helvetica-narrow",(void*)"Helvetica-Narrow");
  fontHashTable()->add("helvetica-narrow-bold",(void*)"Helvetica-Narrow-Bold");
  fontHashTable()->add("helvetica-narrow-boldoblique",(void*)"Helvetica-Narrow-BoldOblique");
  fontHashTable()->add("helvetica-narrow-oblique",(void*)"Helvetica-Narrow-Oblique");
  fontHashTable()->add("helvetica-oblique",(void*)"Helvetica-Oblique");
  fontHashTable()->add("lubalingraph-book",(void*)"LubalinGraph-Book");
  fontHashTable()->add("lubalingraph-bookoblique",(void*)"LubalinGraph-BookOblique");
  fontHashTable()->add("lubalingraph-demi",(void*)"LubalinGraph-Demi");
  fontHashTable()->add("lubalingraph-demioblique.",(void*)"LubalinGraph-DemiOblique.");
  fontHashTable()->add("newcenturyschlbk-bold",(void*)"NewCenturySchlbk-Bold");
  fontHashTable()->add("newcenturyschlbk-bolditalic",(void*)"NewCenturySchlbk-BoldItalic");
  fontHashTable()->add("newcenturyschlbk-italic",(void*)"NewCenturySchlbk-Italic");
  fontHashTable()->add("newcenturyschlbk-roman",(void*)"NewCenturySchlbk-Roman");
  fontHashTable()->add("optima-bold",(void*)"Optima-Bold");
  fontHashTable()->add("optima-boldoblique",(void*)"Optima-BoldOblique");
  fontHashTable()->add("optima-oblique",(void*)"Optima-Oblique");
  fontHashTable()->add("optima",(void*)"Optima");
  fontHashTable()->add("palatino-bold",(void*)"Palatino-Bold");
  fontHashTable()->add("palatino-bolditalic",(void*)"Palatino-BoldItalic");
  fontHashTable()->add("palatino-italic",(void*)"Palatino-Italic");
  fontHashTable()->add("palatino-roman",(void*)"Palatino-Roman");
  fontHashTable()->add("souvenir-demi",(void*)"Souvenir-Demi");
  fontHashTable()->add("souvenir-demiitalic",(void*)"Souvenir-DemiItalic");
  fontHashTable()->add("souvenir-light",(void*)"Souvenir-Light");
  fontHashTable()->add("souvenir-lightitalic",(void*)"Souvenir-LightItalic");
  fontHashTable()->add("symbol",(void*)"Symbol");
  fontHashTable()->add("times-bold",(void*)"Times-Bold");
  fontHashTable()->add("times-bolditalic",(void*)"Times-BoldItalic");
  fontHashTable()->add("times-italic",(void*)"Times-Italic");
  fontHashTable()->add("times-roman",(void*)"Times-Roman");
  fontHashTable()->add("time",(void*)"Times-Roman");
  fontHashTable()->add("times-normal",(void*)"Times-Roman");
  fontHashTable()->add("zapfchancery-mediumitalic",(void*)"ZapfChancery-MediumItalic");
  fontHashTable()->add("zapfdingbats",(void*)"ZapfDingbats");
  fontHashTable()->add("lucida-bright",(void*)"LucidaBright");
  fontHashTable()->add("lucida-brightdemibold",(void*)"LucidaBright-Demi");
  fontHashTable()->add("lucida-brightdemibolditalic",(void*)"LucidaBright-DemiItalic");
  fontHashTable()->add("lucida-brightitalic",(void*)"LucidaBright-Italic");
  fontHashTable()->add("lucidabright",(void*)"LucidaBright");
  fontHashTable()->add("lucidabright-demi",(void*)"LucidaBright-Demi");
  fontHashTable()->add("lucidabright-demiitalic",(void*)"LucidaBright-DemiItalic");
  fontHashTable()->add("lucidabright-italic",(void*)"LucidaBright-Italic");
  fontHashTable()->add("lucidasans-typewriter",(void*)"LucidaSans-Typewriter");
  fontHashTable()->add("lucidasans-typewriterbold",(void*)"LucidaSans-TypewriterBold");
  fontHashTable()->add("lucidasanstypewriter",(void*)"LucidaSans-Typewriter");
  fontHashTable()->add("lucidasanstypewriter-bold",(void*)"LucidaSans-TypewriterBold");
  fontHashTable()->add("lucidasans-bold",(void*)"LucidaSans-Bold");
  fontHashTable()->add("lucidasans",(void*)"LucidaSans");
  fontHashTable()->add("lucidasans-italic",(void*)"LucidaSans-Italic");
  fontHashTable()->add("lucidasans-bolditalic",(void*)"LucidaSans-Bold-Italic");
  fontHashTable()->add("lucidasans-roman",(void*)"LucidaSans-Roman");
  fontHashTable()->add("kaplscreen",(void*)"APL");
  fontHashTable()->add("kaplscreen-bold",(void*)"APL");
  fontHashTable()->add("kaplgallant",(void*)"APL");
  fontHashTable()->add("kaplgallant-bold",(void*)"APL");

  fontHashTable()->add("apl",(void*)"APL");
  fontHashTable()->add("Avantgarde-Book",(void*)"AvantGarde-Book");
  fontHashTable()->add("Avantgarde-BookOblique",(void*)"AvantGarde-BookOblique");
  fontHashTable()->add("Avantgarde-Demi",(void*)"AvantGarde-Demi");
  fontHashTable()->add("Avantgarde-DemiOblique",(void*)"AvantGarde-DemiOblique");
  fontHashTable()->add("Bookman-Demi",(void*)"Bookman-Demi");
  fontHashTable()->add("Bookman-DemiItalic",(void*)"Bookman-DemiItalic");
  fontHashTable()->add("Bookman-Light",(void*)"Bookman-Light");
  fontHashTable()->add("Bookman-LightItalic",(void*)"Bookman-LightItalic");
  fontHashTable()->add("Courier",(void*)"Courier");
  fontHashTable()->add("courier-Normal",(void*)"Courier");
  fontHashTable()->add("courier-Bold",(void*)"Courier-Bold");
  fontHashTable()->add("courier-BoldOblique",(void*)"Courier-BoldOblique");
  fontHashTable()->add("courier-Oblique",(void*)"Courier-Oblique");
  fontHashTable()->add("DIThacks",(void*)"DIThacks");
  fontHashTable()->add("Garamond-Light",(void*)"Garamond-Light");
  fontHashTable()->add("Garamond-LightItalic",(void*)"Garamond-LightItalic");
  fontHashTable()->add("Garamond-Bold",(void*)"Garamond-Bold");
  fontHashTable()->add("Garamond-BoldItalic",(void*)"Garamond-BoldItalic");
  fontHashTable()->add("Helvetica",(void*)"Helvetica");
  fontHashTable()->add("Helvetica-Bold",(void*)"Helvetica-Bold");
  fontHashTable()->add("Helvetica-Boldoblique",(void*)"Helvetica-BoldOblique");
  fontHashTable()->add("Helvetica-Narrow",(void*)"Helvetica-Narrow");
  fontHashTable()->add("Helvetica-Narrow-Bold",(void*)"Helvetica-Narrow-Bold");
  fontHashTable()->add("Helvetica-Narrow-BoldOblique",(void*)"Helvetica-Narrow-BoldOblique");
  fontHashTable()->add("Helvetica-Narrow-Oblique",(void*)"Helvetica-Narrow-Oblique");
  fontHashTable()->add("Helvetica-Oblique",(void*)"Helvetica-Oblique");
  fontHashTable()->add("LubalinGraph-Book",(void*)"LubalinGraph-Book");
  fontHashTable()->add("LubalinGraph-BookOblique",(void*)"LubalinGraph-BookOblique");
  fontHashTable()->add("LubalinGraph-Demi",(void*)"LubalinGraph-Demi");
  fontHashTable()->add("LubalinGraph-DemiOblique.",(void*)"LubalinGraph-DemiOblique.");
  fontHashTable()->add("NewCenturySchlbk-Bold",(void*)"NewCenturySchlbk-Bold");
  fontHashTable()->add("NewCenturySchlbk-BoldItalic",(void*)"NewCenturySchlbk-BoldItalic");
  fontHashTable()->add("NewCenturySchlbk-Italic",(void*)"NewCenturySchlbk-Italic");
  fontHashTable()->add("NewCenturySchlbk-roman",(void*)"NewCenturySchlbk-Roman");
  fontHashTable()->add("Optima-Bold",(void*)"Optima-Bold");
  fontHashTable()->add("Optima-BoldOblique",(void*)"Optima-BoldOblique");
  fontHashTable()->add("Optima-Oblique",(void*)"Optima-Oblique");
  fontHashTable()->add("Optima",(void*)"Optima");
  fontHashTable()->add("Palatino-Bold",(void*)"Palatino-Bold");
  fontHashTable()->add("Palatino-BoldItalic",(void*)"Palatino-BoldItalic");
  fontHashTable()->add("Palatino-Italic",(void*)"Palatino-Italic");
  fontHashTable()->add("Palatino-roman",(void*)"Palatino-Roman");
  fontHashTable()->add("Souvenir-Demi",(void*)"Souvenir-Demi");
  fontHashTable()->add("Souvenir-DemiItalic",(void*)"Souvenir-DemiItalic");
  fontHashTable()->add("Souvenir-Light",(void*)"Souvenir-Light");
  fontHashTable()->add("Souvenir-LightItalic",(void*)"Souvenir-LightItalic");
  fontHashTable()->add("Symbol",(void*)"Symbol");
  fontHashTable()->add("Times-Bold",(void*)"Times-Bold");
  fontHashTable()->add("Times-BoldItalic",(void*)"Times-BoldItalic");
  fontHashTable()->add("Times-Italic",(void*)"Times-Italic");
  fontHashTable()->add("Times-Roman",(void*)"Times-Roman");
  fontHashTable()->add("Times-Normal",(void*)"Times-Roman");
  fontHashTable()->add("ZapfChancery-mediumItalic",(void*)"ZapfChancery-MediumItalic");
  fontHashTable()->add("ZapfDingbats",(void*)"ZapfDingbats");
  fontHashTable()->add("Lucida-Bright",(void*)"LucidaBright");
  fontHashTable()->add("Lucida-BrightdemiBold",(void*)"LucidaBright-Demi");
  fontHashTable()->add("Lucida-BrightdemiBoldItalic",(void*)"LucidaBright-DemiItalic");
  fontHashTable()->add("Lucida-BrightItalic",(void*)"LucidaBright-Italic");
  fontHashTable()->add("LucidaBright",(void*)"LucidaBright");
  fontHashTable()->add("LucidaBright-Demi",(void*)"LucidaBright-Demi");
  fontHashTable()->add("LucidaBright-DemiItalic",(void*)"LucidaBright-DemiItalic");
  fontHashTable()->add("LucidaBright-Italic",(void*)"LucidaBright-Italic");
  fontHashTable()->add("LucidaSans-TypeWriter",(void*)"LucidaSans-Typewriter");
  fontHashTable()->add("LucidaSans-TypeWriterBold",(void*)"LucidaSans-TypewriterBold");
  fontHashTable()->add("LucidaSansTypeWriter",(void*)"LucidaSans-Typewriter");
  fontHashTable()->add("LucidaSansTypeWriter-Bold",(void*)"LucidaSans-TypewriterBold");
  fontHashTable()->add("LucidaSans-Bold",(void*)"LucidaSans-Bold");
  fontHashTable()->add("LucidaSans",(void*)"LucidaSans");
  fontHashTable()->add("LucidaSans-Italic",(void*)"LucidaSans-Italic");
  fontHashTable()->add("LucidaSans-BoldItalic",(void*)"LucidaSans-Bold-Italic");
  fontHashTable()->add("LucidaSans-Roman",(void*)"LucidaSans-Roman");
  fontHashTable()->add("KaplScreen",(void*)"APL");
  fontHashTable()->add("KaplScreen-Bold",(void*)"APL");
  fontHashTable()->add("KaplGallant",(void*)"APL");
  fontHashTable()->add("KaplGallant-Bold",(void*)"APL");
}

MSBoolean MSPrintFont::initAfmfilesHashTable(const char *path_)
{
  if (_afmfilesHashTable==0)
   {
     ifstream pin;
     MSString line;
#ifdef MS_WINDOWS
     if (defaultFontPath()==0)
       {
	 static char buf[256];
	 if(MSXGetFontDir(buf,256)) _defaultFontPath=buf;
	 else _defaultFontPath="C:\\MSTK\\Fonts";
	 //check for empty string, and make sure we go to default
	 //path in that case.
	 if(path_!=0&&path_[0]=='\0') path_=0;
       }
#endif
     MSString path(path_!=0?path_:defaultFontPath());
     if (path.last()!=MS_DIR_SEPARATOR) path<<MS_DIR_SEPARATOR;
     path<<"afmfiles.upr";
     pin.open(path);
     if (pin.fail()==ios::failbit)
      {
	// try default font path 
	if (path_!=0&&strcmp(path_,defaultFontPath())!=0)
	 {
	   path=defaultFontPath();
	   path<<"/afmfiles.upr";
	   pin.open(path);
	   if (pin.fail()==ios::failbit)
	    {
	      //try alternate default path
	      path=altDefaultFontPath();
	      path<<"/afmfiles.upr";
	      pin.open(path.string());
	      if (pin.fail()==ios::failbit)
	       {
		 //give up
		 MSMessageLog::errorMessage("MSPrintFont: Error opening file %s\n",path.string());
		 return MSFalse;
	       }
	    }
	 }
	else 
	 {
	   // if path was the default try the alternate
	   path=altDefaultFontPath();
	   path<<"/afmfiles.upr";
	   pin.open(path);
	   if (pin.fail()==ios::failbit)
	    {
 	      MSMessageLog::errorMessage("MSPrintFont: Error opening file %s\n",path.string());
	      return MSFalse;
	    }
	 }
      }
     line=MSString::lineFrom(pin);
     if (line.indexOf("PS-Resources")!=0)
      {
	MSMessageLog::errorMessage("MSPrintFont: incorrect file format %s\n",path.string());
	return MSFalse;
      }
     while(line!="."&&pin.eof()!=ios::eofbit) line=MSString::lineFrom(pin);
     if (pin.eof()==ios::eofbit)
      {
        MSMessageLog::errorMessage("MSPrintFont: incorrect file format %s\n",path.string());
	return MSFalse;
      }
     while(line!="FontAFM"&&pin.eof()!=ios::eofbit) line=MSString::lineFrom(pin);
     // load fonts into Hashtable 
     _afmfilesHashTable=new MSStringHashTable(HashTableSize);
     afmfilesHashTable()->notFound((unsigned long)0);
     int count=0;
     line=MSString::lineFrom(pin);
     while(line!="."&&pin.eof()!=ios::eofbit)
      {
	int index=line.indexOf("=")+1;
	MSString font(line.subString(0,index-1));
	MSString file(line.subString(index,line.indexOf(".")-index));
	char *cp=new char[file.length()+1];
	strcpy(cp,file.string());
	cp[file.length()]='\0';
	afmfilesHashTable()->add(font.string(),(void*)cp);
	line=MSString::lineFrom(pin);
	count++;
      }
     if (count==0)
      {
	delete _afmfilesHashTable;
	_afmfilesHashTable=0;
	MSMessageLog::errorMessage("MSPrintFont: unable to parse file %s\n",path.string());
	return MSFalse;
      }
     return MSTrue;
   }
  return MSTrue;
}



