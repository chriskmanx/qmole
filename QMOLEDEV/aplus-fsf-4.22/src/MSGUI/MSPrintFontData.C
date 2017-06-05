///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSGUI/MSPrintFont.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSTypes/MSMessageLog.H>
#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#endif
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#if HAVE_IOMANIP
#include <iomanip>
#else
#include <iomanip.h>
#endif

MSPrintFontData::MSPrintFontData(void)
{
  _ascent=0;  
  _descent=0; 
  _width=0;   
  _lbearing=0;
  _rbearing=0;
  _fontID=0;
  _fontPath=MSPrintFont::defaultFontPath();
  _fileName=MSPrintFont::defaultFontPath();
}  

MSPrintFontData::MSPrintFontData(const MSString& file_) :
_fileName(MSPrintFont::defaultFontPath())
{
  _ascent=0;  
  _descent=0; 
  _width=0;   
  _lbearing=0;
  _rbearing=0;
  _fontID=0;
  _fontPath=MSPrintFont::defaultFontPath();
  _fileName<<"/";
  _fileName<<file_;
  loadFont();
}  

MSPrintFontData::MSPrintFontData(const MSString& path_,const MSString& file_) :
_fileName(path_)
{
  _ascent=0;  
  _descent=0; 
  _width=0;   
  _lbearing=0;
  _rbearing=0;
  _fontID=0;
  _bufsize=0;
  _fontPath=path_;
  if (path_.last()!='/') _fileName<<"/";
  _fileName<<file_;
  loadFont();
}  

MSPrintFontData::~MSPrintFontData(void)
{
  if (_ascent!=0)   delete [] _ascent;  
  if (_descent!=0)  delete [] _descent; 
  if (_width!=0)    delete [] _width;   
  if (_lbearing!=0) delete [] _lbearing;
  if (_rbearing!=0) delete [] _rbearing;
}

void MSPrintFontData::loadFont(void)
{
  int         i=0;
  char        junk[_buflen];
  ifstream    pin;
  streampos   mark;
  strcpy(_pbuf,fileName().string());
  strcat(_pbuf,".afm");
  pin.open(_pbuf);
  if (pin.fail()==ios::failbit) { showFileError(); return; }
  mark=pin.tellg();
  pin>>setw(_buflen)>>_pbuf;
  if (strcmp(_pbuf,"StartFontMetrics")!=0) { showFontError(); return; }
  while(strcmp(_pbuf,"FontName")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_pbuf;
  fontName(_pbuf);
//  while(pin.eof()!=ios::eofbit&&strcmp(_pbuf,"EncodingScheme")!=0) pin>>_pbuf;
//  if (pin.eof()==ios::eofbit) { showFontError(); return; }
//  if (strcmp(_pbuf,"AdobeStandardEncoding")!=0)
//  if (strcmp(_pbuf,"ISOLatin1Encoding")!=0)
  pin.seekg(mark);
  while(strcmp(_pbuf,"IsFixedPitch")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  isFixedPitch(strcmp(_pbuf,"true")==0?MSTrue:MSFalse);
  pin.seekg(mark);
  while(strcmp(_pbuf,"UnderlinePosition")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_underlinePosition;
  pin.seekg(mark);
  while(strcmp(_pbuf,"UnderlineThickness")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_underlineThickness;
  pin.seekg(mark);
  while(strcmp(_pbuf,"FontBBox")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  else
   {
     int x;
     pin>>x; fontBox().x(x);
     pin>>x; fontBox().y(x);
     pin>>x; fontBox().width(x);
     pin>>x; fontBox().height(x);
   }
  pin.seekg(mark);
  while(strcmp(_pbuf,"CapHeight")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_capHeight;
  pin.seekg(mark);
  while(strcmp(_pbuf,"XHeight")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_xHeight;
  pin.seekg(mark);
  while(strcmp(_pbuf,"Descender")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_descender;
  pin.seekg(mark);
  while(strcmp(_pbuf,"Ascender")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_ascender;
  pin.seekg(mark);
  while(strcmp(_pbuf,"StartCharMetrics")!=0&&pin.eof()!=ios::eofbit) pin>>_pbuf;
  if (pin.eof()==ios::eofbit) { showFontError(); return; }
  pin>>_bufsize;
  _width=new int[bufsize()];
  for (int j=0; j<bufsize(); j++) _width[j]=0;
  pin>>_pbuf;
  if (*_pbuf!='C') { showFontError(); return; }
  pin>>_offset>>_pbuf>>_pbuf;
  if (*_pbuf!='W') { showFontError(); return; }
  pin>>_width[0];
  while(pin.getline(_pbuf,_buflen)&&strcmp(_pbuf,"EndCharMetrics")!=0&&pin.eof()!=ios::eofbit&&i>=0)
   {
#if HAVE_SSTREAM
     istringstream isl(_pbuf);
#else
     istrstream isl(_pbuf,strlen(_pbuf));
#endif
     isl>>junk>>i>>junk>>junk;
     if (i>0) isl>>_width[i-_offset];
   }     
}

double MSPrintFontData::fontOffset(int size_)
{
  return size_+(descender()*size_)/1000;
}

double MSPrintFontData::textWidth(int size_,const char *str_,int n_)
{
  double w=0;
  for (int i=0; i<n_; i++) w+=width((int)str_[i]-offset());
  return (w*size_)/1000.;
}

void MSPrintFontData::showFileError(void)
{
  MSMessageLog::errorMessage("MSPrintFontData: error opening file %s\n",fileName().string());
}

void MSPrintFontData::showFontError(void)
{
  MSMessageLog::errorMessage("MSPrintFontData: incorrect file format %s\n",fileName().string());
}

int MSPrintFontData::ascent(unsigned i_) const
{return i_<bufsize()?_ascent[i_]:0;}

int MSPrintFontData::descent(unsigned i_) const
{return i_<bufsize()?_descent[i_]:0;}

int MSPrintFontData::width(unsigned i_) const
{return i_<bufsize()?_width[i_]:0;}

int MSPrintFontData::lbearing(unsigned i_) const
{return i_<bufsize()?_lbearing[i_]:0;}

int MSPrintFontData::rbearing(unsigned i_) const
{return i_<bufsize()?_rbearing[i_]:0;}

int MSPrintFontData::height(unsigned i_) const
{return i_<bufsize()?ascent(i_)+descent(i_):0;}

