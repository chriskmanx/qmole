///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_IOMANIP
#include <iomanip>
#else
#include <iomanip.h>
#endif
#include <MSTypes/MSString.H>
#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSPostScript.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSGUI/MSPrintDisclaimer.H>
#include <MSGUI/MSStandardDisclaimer.H>

static const char *DefaultDisclaimer    ="/tmp/mstk.disclaimer";
static const char *AltDefaultDisclaimer ="/usr/local/lib/disclaimer";
static const char *DefaultDisclaimerFont="Times-Roman-6";

extern const int MSPointsPerInch;
extern const int MSPageSizeXTable[];
extern const int MSPageSizeYTable[];

MSPrintDisclaimer::MSPrintDisclaimer(MSPostScript *owner_) 
{
  _owner         =owner_;
  _style         =NoDisclaimer;
  _fontID   	 =0;
  _fontSize  	 =6;
  _orientation   =Default;
  _lineWidth	 =0;
  _leftPixel     =20;
  _rightPixel    =20;
  _topPixel      =20;
  _bottomPixel   =20;
  _rowCount      =0;
  _height        =0;
  _width         =0;
}

MSPrintDisclaimer::~MSPrintDisclaimer(void) 
{ pin.close(); }

MSPostScript *MSPrintDisclaimer::owner(void) const
{return _owner;}

ofstream& MSPrintDisclaimer::pout(void)
{return owner()->pout;}

void MSPrintDisclaimer::leftMargin(double x_)
{_leftPixel=(x_<=0.07)?5:(int)(MSPointsPerInch*x_);}

void MSPrintDisclaimer::rightMargin(double x_)
{_rightPixel=(x_<=0.07)?5:(int)(MSPointsPerInch*x_);}

void MSPrintDisclaimer::topMargin(double x_)
{_topPixel=(x_<=0.07)?5:(int)(MSPointsPerInch*x_);}

void MSPrintDisclaimer::bottomMargin(double x_)
{_bottomPixel=(x_<=0.07)?5:(int)(MSPointsPerInch*x_);}

double MSPrintDisclaimer::leftMargin(void)const
{return (_leftPixel==5)?0.0:(double)_leftPixel/MSPointsPerInch;}

double MSPrintDisclaimer::rightMargin(void)const
{return (_rightPixel==5)?0.0:(double)_rightPixel/MSPointsPerInch;}

double MSPrintDisclaimer::topMargin(void)const
{return (_topPixel==5)?0.0:(double)_topPixel/MSPointsPerInch;}

double MSPrintDisclaimer::bottomMargin(void)const
{return (_bottomPixel ==5)?0.0:(double)_bottomPixel/MSPointsPerInch;}

void MSPrintDisclaimer::font(const MSString& string_)
{
  if (string_.length()>0)
   {
     _fontName=string_;
     if (owner()!=0)
      {
	fontID(owner()->font(string_));
	_fontSize=owner()->fontSize();
      }
   }
}

void MSPrintDisclaimer::closeFile(void)
{
  pin.close();
#ifndef MS_WINDOWS
  if (system(MSString("rm -f ")+DefaultDisclaimer)!=0)
   {
     MSMessageLog::warningMessage("Warning: unable to delete temporary file %s\n",DefaultDisclaimer);
   }
#endif
}

void MSPrintDisclaimer::computeSize(void)
{
  ofstream dout;
  dout.open(DefaultDisclaimer);
  if (dout.fail()!=ios::failbit)
   {
     dout<<Disclaimer;
     dout.close();
#ifndef MS_WINDOWS
     system(MSString("chmod +w ")+DefaultDisclaimer);
#endif
   }
  else MSMessageLog::warningMessage("Warning: unable to open %s\n",DefaultDisclaimer); 
  char buf[buflen];
  if (fontName().length()==0) font(DefaultDisclaimerFont);
  
  PageOrientation orient=orientation()==Default?owner()->pageOrientation():orientation();
  
  if (style()!=NoDisclaimer)
   {
     int 		ct=0;
     double 		w=0;
     streampos 		mark0,mark1,mark2;
     _rowCount=0;

     if (orient==Portrait) width(MSPageSizeXTable[owner()->pageSize()-Letter]-leftPixel()-rightPixel());
     else width(MSPageSizeYTable[owner()->pageSize()-Letter]-leftPixel()-rightPixel());
     MSPrintFontData *fdata=owner()->fontStruct(fontID());
     if (fdata!=0)
      {
	MSString file(fileName().length()!=0?fileName().string():DefaultDisclaimer);
	pin.open(file);
	if (pin.fail()==ios::failbit)
	 {
	   pin.clear();
	   if (fileName().length()!=0)
	    {
	      pin.open(DefaultDisclaimer);
	      if (pin.fail()==ios::failbit)
	       {
		 pin.clear();
		 pin.open(AltDefaultDisclaimer);
		 if (pin.fail()==ios::failbit)
		  {
		    MSMessageLog::errorMessage("Error: opening file %s\n"
                                               "Error: opening one of the standard disclaimers:\n"
                                               "\t%s\t%s\n*** disclaimer not printed ***\n",
                                               fileName().string(),DefaultDisclaimer,AltDefaultDisclaimer);
		    return;
		  }
		 else
		  {
                    MSMessageLog::errorMessage("Error: opening file %s, using standard disclaimer\n",fileName().string());
		  }
	       }
	      else 
	       {
                 MSMessageLog::errorMessage("Error: opening file %s, using standard disclaimer\n",fileName().string());
	       }
	    }
	   else
	    {
	      pin.open(AltDefaultDisclaimer);
	      if (pin.fail()==ios::failbit)
	       {
                 MSMessageLog::errorMessage("Error: opening one of the standard disclaimers:\n"
                                            "\t%s\t%s\n*** disclaimer not printed ***\n",
                                            DefaultDisclaimer,AltDefaultDisclaimer);
		 return;
	       }
	      file=AltDefaultDisclaimer;
	    }
	 }
	fileName(file);
	mark0=pin.tellg();
	double spaceChar=fdata->textWidth(fontSize()," ",1);
	double pageWidth=width();
	if (style()==Box||style()==AppendBox)
	 {
	   int maxBounds=(int)(fdata->textWidth(fontSize(),"M",1));
	   pageWidth-=2*(maxBounds+lineWidth());
	 }
	while(pin.eof()!=ios::eofbit&&ct<buflen)
	 {
	   mark1=pin.tellg();
	   pin>>setw(buflen)>>buf;
	   mark2=pin.tellg();
	   long len=strlen(buf);
	   double temp=fdata->textWidth(fontSize(),buf,len);
	   if ((mark2-mark1-len)>1)
	    {
	      if (w==0) _residualSpace[_rowCount-1]+=_spaceWidth[_rowCount-1];
	      else w+=(int)(spaceChar*(mark2-mark1-len-1L));
	    }
	   if ((w+temp+spaceChar*ct)>pageWidth)
	    {
	      int space=(int)(pageWidth-w-temp)/ct;
	      if (space<=1)
	       {
		 temp=0;
		 pin.seekg(mark1);
	       }	  
	      else ct++;
	      _spaceWidth[rowCount()]=(int)(pageWidth-w-temp)/(ct-1);
	      _residualSpace[rowCount()]=(int)(pageWidth-w-temp-spaceWidth(rowCount())*(ct-1));
	      _wordCount[_rowCount++]=ct;
	      w=ct=0;
	    }
	   else 
	    {
	      w+=temp;
	      ct++;
	    }
	 }
	_spaceWidth[rowCount()]=(int)(pageWidth-w)/(ct-1);
	_residualSpace[rowCount()]=(int)(pageWidth-w-spaceWidth(rowCount())*ct);
	if (_spaceWidth[rowCount()]>spaceChar)
	 {
	   _spaceWidth[rowCount()]=(int)spaceChar;
	   _residualSpace[rowCount()]=0;
	 }
	_wordCount[_rowCount++]=ct-1;
	pin.clear();
	pin.seekg(mark0);
	ct=rowCount()>2?rowCount()-2:rowCount();
	if (style()==Box||style()==Rule) if (rowCount()>2) ct+=2;
	else if (style()==Toprule) ct++;
      }
     else 
      {
        MSMessageLog::errorMessage("***disclaimer not printed***\n");
      }
     height(ct*fontSize());
     if (orient==Portrait)
      {
	xTrans(leftPixel());
	yTrans(bottomPixel()+height());
      }
     else 
      {
	xTrans(MSPageSizeXTable[owner()->pageSize()-Letter]-bottomPixel()-height());
	yTrans(leftPixel());
      }
   }
  else height(0);
}

void MSPrintDisclaimer::print(void)
{ 
  if (height()>0)
   {
     static char  buf[buflen],buf1[buflen];
     streampos 	  mark1,mark2;
     double	  w=0.;
     MSPrintFontData *fdata=owner()->fontStruct(fontID());
     int maxBounds=(int)(fdata->textWidth(fontSize(),"M",1));
     PageOrientation orient=orientation()==Default?owner()->pageOrientation():orientation();
  
     pout()<<"gr gr gs";
     pout()<< " ";
     pout()<< xTrans();
     pout()<< " ";
     pout()<< yTrans();
     pout()<< " ";
     pout()<<"translate";
     pout()<< " ";
     if (orient==Landscape) pout()<<"90 rotate";
     pout()<< " ";
     pout()<< fontSize();
     pout()<< "/";
     pout()<< fdata->fontName();
     pout()<< " ";
     pout()<< "font";
     pout()<< endl;
     int x_start=style()==Box||style()==AppendBox?maxBounds+lineWidth():0;
     for (int i=0; i<rowCount(); i++)
      {
	int rSpace=residualSpace(i);
	pout()<<x_start<<" "<<-i*fontSize()<<" "<<"M ";
	for (int j=0; j<wordCount(i); j++)
	 {
	   if (i==0&&j==0)
	    {
	      pin>>setw(buflen)>>buf1;
	    }
	   else if (j!=0)
	    {
	      pout()<<w+spaceWidth(i)+(rSpace>0?1:0);
	      rSpace--;
	      pout()<<" s ";
	    }
	   mark1=pin.tellg();
	   pin>>setw(buflen)>>buf;
	   mark2=pin.tellg();
	   long len=strlen(buf);
	   if ((mark2-mark1-len)>1) if (j!=wordCount(i)-1) strcat(buf1," ");
	   pout()<<"("<<buf1<<")"<<"S"<<" ";
	   strcpy(buf1,buf);
	 }
	pout()<<endl;
      }
   }
  pin.clear();
  pin.seekg(ios::beg);
}

void MSPrintDisclaimer::printRule(void)
{
  if (style()>Text)
   {
     int ruleOffset=(int)(fontSize()*1.5);
     pout()<< (style()==AppendBox?0:lineWidth());
     pout()<< " ";
     pout()<< "w";
     pout()<< " ";
     pout()<< "2";
     pout()<< " ";
     pout()<< "lc";
     pout()<< " ";
     owner()->printLine(0,ruleOffset,width(),ruleOffset);
     if (style()==Rule)
      {
	owner()->printLine(0,-height(),width(),-height());
      }
     if (style()==Box)
      {
	owner()->printLine(0,-height(),width(),-height());
	owner()->printLine(0,ruleOffset,0,-height());
	owner()->printLine(width(),-height(),width(),ruleOffset);
      }
   }
}


