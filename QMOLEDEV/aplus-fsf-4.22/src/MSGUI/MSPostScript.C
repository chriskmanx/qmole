///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <time.h>
#include <pwd.h>
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include <MSGUI/MSPostScript.H>
#include <MSGUI/MSPrintFontData.H>
#include <MSTypes/MSMessageLog.H>

static const char  *DefaultPrintFont     ="LucidaSans-TypewriterBold";
static const char  *AltDefaultPrintFont  ="Courier";
static const char  *DefaultScreenFont    ="Courier";

static const int    MSPrintFontTableSize = 256;
/*static const int    DefaultLineWidth     = 1;*/
static const int    DefaultLeftPixel     = 72;
static const int    DefaultRightPixel    = 72;
static const int    DefaultTopPixel      = 72;
static const int    DefaultBottomPixel   = 72;
static const int    BadFontName          = 0xFF55;
static MSBoolean    _afmInitialized      = MSFalse;

unsigned long MSPostScript::_blackPixel  = LONG_MAX;
unsigned long MSPostScript::_whitePixel  = LONG_MAX;

extern const double PSFactor	    = 0.85;
extern const int MSPointsPerInch    = 72;
extern const int MSPageSizeXTable[] = {612,612,595,516,612,792 };
extern const int MSPageSizeYTable[] = {792,1008,842,728,792,1224};

extern MSString applicationVersionString(void);

MSPrintFontHashTable::~MSPrintFontHashTable(void)
{
  for (int i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     MSPrintFontData *data; 
     while (entry!=0)
      {
	_bucket[i]=entry->next();
        data=(MSPrintFontData *) entry->value();
        if (data!=0) delete data;   
	delete entry;
	entry=bucket(i);
      }
   }
}

MSPostScript::MSPostScript(void) 
{
  _pageOrientation  =Landscape;
  _pageSize  	    =Letter;
  _printMode  	    =Mono;
  _pageLayout       =MSCenter;
  _outputMode       =PS;
  _leftPixel        =DefaultLeftPixel;
  _rightPixel       =DefaultRightPixel;
  _topPixel         =DefaultTopPixel;
  _bottomPixel      =DefaultBottomPixel;
  _lineWidth	    =-1;		
  _lineStyle	    =-1;		
  _capStyle	    =-1;		
  _joinStyle	    =-1;		
  _fontCount        =0;
  _defaultFontSize  =10;

// init gcValues structure

  _gcValues.arc_mode=ArcPieSlice;
  _gcValues.foreground=0;
  _gcValues.background=0;
  _gcValues.line_width=0;
  _gcValues.line_style=0;
  _gcValues.dash_offset=0;
  _gcValues.cap_style=0;
  _gcValues.join_style=0;
  _gcValues.font=0;
  
  pout.precision(4);
  fontString(DefaultScreenFont);
  _defaultFontString=DefaultScreenFont;
  printFontIDHashTable().init(MSPrintFontTableSize);
  fontDataHashTable().init(MSPrintFontTableSize);
  printFontIDHashTable().notFound(0x55FF);
  fontDataHashTable().notFound(0x0);
  _fontPath=defaultFontPath();
  _disclaimer=new MSPrintDisclaimer(this);
  init();
}

MSPostScript::~MSPostScript(void) 
{
  _afmInitialized=MSFalse;
  pout.close();
  delete _disclaimer;
}

void MSPostScript::init(void)
{
  _pages       =1;
  _fontID      =0;
  _foreground  =LONG_MAX;		
  _background  =LONG_MAX;		
  _gscale      =1;
  _lineStyle   =-1;
  _lineWidth   =-1;
  _capStyle    =-1;		
  _joinStyle   =-1;		
  _bwidth      =0;
  _bheight     =0;
  _x_org       =0;
  _y_org       =0;
  _x_printScale=1;
  _y_printScale=1;
  documentFonts().removeAll();
}

void MSPostScript::leftMargin(double x_)
{_leftPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}

void MSPostScript::rightMargin(double x_)
{_rightPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}

void MSPostScript::topMargin(double x_)
{_topPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}

void MSPostScript::bottomMargin(double x_)
{_bottomPixel=x_<=0.07?5:(int)(MSPointsPerInch*x_);}

double MSPostScript::leftMargin(void) const
{return _leftPixel==5?0.0:(double)_leftPixel/MSPointsPerInch;}

double MSPostScript::rightMargin(void) const
{return _rightPixel==5?0.0:(double)_rightPixel/MSPointsPerInch;}

double MSPostScript::topMargin(void) const
{return _topPixel==5?0.0:(double)_topPixel/MSPointsPerInch;}

double MSPostScript::bottomMargin(void) const
{return _bottomPixel ==5?0.0:(double)_bottomPixel/MSPointsPerInch;}

void MSPostScript::initFont(void)
{
  if (fontCount()==0)
   {
     if (_afmInitialized==MSFalse&&afmfilesHashTable()==0)
      {
	_afmInitialized=MSTrue;
	initAfmfilesHashTable(fontPath());
      }
     if (printFontID((char *)DefaultPrintFont)==BadFontName)
      {
	if (printFontID((char*)AltDefaultPrintFont)==BadFontName)
	 {
	   MSMessageLog::warningMessage("Warning: unable to initialize fonts.\n");
	 }
      }
   }
}

void MSPostScript::fontPath(const char *path_)
{ 
  DIR *dp;
  
  if (path_!=0&&(dp=opendir(path_))!=0)
   {
     _fontPath=path_; 
     closedir(dp);
   }
  else
   {
     MSMessageLog::errorMessage("Error: directory %s: not found\n",path_);
   }
}

void MSPostScript::defaultFontName(const char *str_)
{
  const char *tempStr,*fontStr;
  
  if ((tempStr=formatFontString(str_))!=0&&
      (fontStr=(char *)fontHashTable()->lookup(tempStr))!=0)
   {
     _defaultFontString=fontStr;
   }
  else
   {
     MSMessageLog::warningMessage("Warning: Invalid font specification - using %s\n",defaultFontString().string());
   }
}

Font MSPostScript::printFontID(const char *fontString_)
{
  Font  fid=0;
  const char *fontStr,*tempStr;

  if ((tempStr=extractFontString(fontString_))!=0)
   {
     // see if its already allocated
     if ((fid=(Font)printFontIDHashTable().lookup(tempStr))==printFontIDHashTable().notFound())
      {
	// see if it exists as specified 
	if (findFont(tempStr)!=MSTrue)
	 {
	   // look it up in the font table
	   MSString str(tempStr);
	   str.lower();
	   if ((fontStr=(char *)fontHashTable()->lookup(str))!=0)
	    {
	      // see if the converted font is allocated
	      if ((fid=(Font)printFontIDHashTable().lookup(fontStr))==printFontIDHashTable().notFound())
	       {
		 // try to find the converted font
		 if (findFont(fontStr)!=MSTrue)
		  {
		    MSMessageLog::warningMessage("Warning: invalid font specification %s\n",tempStr);
		    printFontIDHashTable().add(fontStr,(void *)BadFontName);
		    if (strcmp(fontStr,tempStr)!=0) printFontIDHashTable().add(tempStr,(void *)BadFontName);
		  }
		 else
		  {
		    addFont(fontStr);
		    printFontIDHashTable().add(tempStr,(void *)fontCount());
		  }
	       }
	      else
	       {
                 printFontIDHashTable().add(tempStr,(void *)fid);
		 fontSize(extractFontSize(fontString_));
		 return fid;
	       }
	    }
	   else
	    {
	      MSMessageLog::warningMessage("Warning: invalid font specification %s\n",tempStr);
	      printFontIDHashTable().add(tempStr,(void *)BadFontName);
	    }
	 }
	else addFont(tempStr);
	fid=(Font)printFontIDHashTable().lookup(tempStr);
      }
     fontSize(extractFontSize(fontString_));
   }
  else fontSize(defaultFontSize());
  return fid;
}

MSBoolean MSPostScript::findFont(const char *fontString_)
{
  DIR           *dp;
  struct dirent *fp;
  MS_DECLARE_DIRENT(dirEntry);  //see comment in MSDefines.H on usage of this macro
  MSString       str(fontString_);
  MSBoolean      status=MSFalse;
  _path=fontPath();
  fontFileName(fontString_);
  
  if (str.length()!=0)
   {
     str<<".afm";
     if ((dp=opendir(fontPath()))!=0)
      {
	while (MS_READDIR(dp,dirEntry,fp)!=0) if (str==fp->d_name)
	 {
	   status=MSTrue;
	   break;
	 }
	closedir(dp);
      }
     if (status!=MSTrue&&(dp=opendir(defaultFontPath()))!=0)
      {
        if (fontPath()!=defaultFontPath())
         {
           while (MS_READDIR(dp,dirEntry,fp)!=0) if (str==fp->d_name)
            {
              _path=defaultFontPath();
              status=MSTrue;
              break;
            }
         }
	closedir(dp);
	if (status!=MSTrue&&afmfilesHashTable()!=0&&(dp=opendir(defaultFontPath()))!=0)
	 {
	   MSString afmfile((char *)afmfilesHashTable()->lookup(fontString_));
	   MSString tmp=afmfile;
	   tmp<<".afm";
	   while (MS_READDIR(dp,dirEntry,fp)!=0) if (tmp==fp->d_name)
	    {
	      _path=defaultFontPath();
	      fontFileName(afmfile);
	      status=MSTrue;
	      break;
	    }
	   closedir(dp);
	 }
      }
     if (status!=MSTrue&&(dp=opendir(altDefaultFontPath()))!=0)
      {
	while (MS_READDIR(dp,dirEntry,fp)!=0) if (str==fp->d_name)
	 {
	   _path=altDefaultFontPath();
	   status=MSTrue;
	   break;
	 }
	closedir(dp);
	if (status!=MSTrue&&afmfilesHashTable()!=0&&
	 (dp=opendir(altDefaultFontPath()))!=0)
	 {
	   MSString afmfile((char *)afmfilesHashTable()->lookup(fontString_));
	   MSString tmp=afmfile;
	   tmp<<".afm";
	   while (MS_READDIR(dp,dirEntry,fp)!=0) if (tmp==fp->d_name)
	    {
	      _path=altDefaultFontPath();
	      fontFileName(afmfile);
	      status=MSTrue;
	      break;
	    }
	   closedir(dp);
	 }
      }
   }
  return status;
}
	
void MSPostScript::addFont(const char *fontString_)
{
  MSPrintFontData *data=0;
  
  if (fontString_!=0)
   {
     if ((MSPrintFontData *)printFontIDHashTable().lookup(fontString_)==
	 (MSPrintFontData *)printFontIDHashTable().notFound())
      {
	data=new MSPrintFontData(path(),fontFileName());
	if (data->bufsize()!=0)
	{
	  data->fontID(++_fontCount);
	  fontDataHashTable().add(data->fontID(),(void *)data); 
	  printFontIDHashTable().add(fontString_,(void *)data->fontID());
	}
	else 
	{
	  printFontIDHashTable().add(fontString_,(void *)BadFontName);
	  delete data;
	}
      }
     documentFonts()<<data->fontName();
   }
}

MSPrintFontData *MSPostScript::fontStruct(Font fid_)
{
  MSPrintFontData *data=0;
  if ((data=(MSPrintFontData *)fontDataHashTable().lookup(fid_))==
      (MSPrintFontData *)fontDataHashTable().notFound())
   {
     if ((data=(MSPrintFontData *)fontDataHashTable().lookup(1))==
	 (MSPrintFontData *)fontDataHashTable().notFound())
      {
	MSMessageLog::warningMessage("Warning: no font information available.\n");
      }
   }
  return data;
}

Font MSPostScript::font(const char *font_)
{
  initFont();
  Font fid=printFontID(font_);
  if (fid!=0&&fid!=printFontIDHashTable().notFound())
   {
     fontID(fid);
     fontString(font_);
   }
  return fid;
}

const char *MSPostScript::fontName(Font fid_)
{
  MSPrintFontData *data=(MSPrintFontData *)fontStruct(fid_);
  return data!=0?data->fontName().string():DefaultPrintFont;
}

MSBoolean MSPostScript::printOpen()
{ 
  MSBoolean status=MSTrue;
  MSString file(fileName());
  if (outputMode()==EPS) file.change(".ps",".eps",0,1);
  if (outputMode()==PPM) file.change(".ps",".ppm",0,1);
  pout.open(file);
  if (pout.fail()==ios::failbit)
   {
     MSMessageLog::warningMessage("Error: opening file %s\n",file.string());
     status=MSFalse;
   }
  return status;
}

MSBoolean MSPostScript::printClose(void)
{ 
  printEpilog(); 
  pout.close();
  disclaimer().closeFile();
  return MSTrue;
}

void MSPostScript::printProlog(void)
{
  unsigned n;
  struct tm *tp;
  struct timeval tv;
  struct passwd *pwd;
  gettimeofday(&tv,NULL);
  const time_t *tmp=(const time_t*)&tv.tv_sec;
  
#if defined(MS_THREAD_SAFE_FUNCTIONS)
  //see comment in MSDefines.H on mt-safe system function usage.
  struct tm tmStruct;
  struct passwd pwdStruct;
  char charBuf[1024];
#endif
  tp=MS_LOCALTIME(tmp,&tmStruct);

  pout<<"%!PS-Adobe-3.0 "<<(outputMode()==EPS?"EPSF-3.0":"MSPostScript_1.0")	<< endl;
  pout<<"%%Creator: ";
#if defined(MS_WINDOWS)
  pout<<"winnt";
#else
  MS_GETPWUID(geteuid(),&pwdStruct,charBuf,1024,pwd);
  pout<<((pwd!=0)?pwd->pw_name:"");
#endif
  pout<<" - "<<"MStk Release "<<applicationVersionString()<<endl;
  pout<<"%%Title: "								<< endl;
  pout<<"%%CreationDate: "<<MS_ASCTIME(tp,charBuf,1024);
  pout<<"%%BoundingBox: ";
  if (outputMode()==EPS) pout<<"0 0 "<<bwidth()<<" "<<bheight()<<endl; else pout<< endl;
  pout<<"%%Orientation: "<<(pageOrientation()==Landscape?"Landscape":"Portrait")<< endl;
  pout<<"%%DocumentNeededResources: ";
  if ((n=documentFonts().length())>0)
   {
     pout<<"font "<<documentFonts()(0)<<endl;
     for (unsigned i=1;i<n;i++) pout<<"%%+ font "<<documentFonts()(i)<<endl;
   }
  else pout<<endl;
  pout<<"%%DocumentSuppliedResources:"  					<< endl;
  pout<<"%%Pages: "<<pages()          						<< endl;
  pout<<"%%EndComments"     							<< endl;
  pout<<"statusdict begin /waittimeout 0 def end"				<< endl;
  pout<<"/MSPostScript_1.0 500 dict def MSPostScript_1.0 begin"			<< endl;
  pout<<"/bd{bind def}bind def/xd{exch def}bd/ld{load def}bd/ex/exch ld"	<< endl;
  pout<<"/M/moveto ld/m/rmoveto ld/L/lineto ld/l/rlineto ld/w/setlinewidth ld"	<< endl;
  pout<<"/n/newpath ld/P/closepath ld/d/setdash ld/tr/translate ld/gs/gsave ld" << endl;
  pout<<"/gr/grestore ld/lc/setlinecap ld/lj/setlinejoin ld/ml/setmiterlimit ld"<< endl;
  pout<<"/f/fill ld/bz/curveto ld/_m matrix def/_cm matrix def/st/stroke ld"	<< endl;
  pout<<"/c{3{255 div 3 1 roll}repeat}bd /tc{c setrgbcolor}bd /sg/setgray ld"	<< endl;
  pout<<"/v {M L st}bd /C{0 360 arc}bd /D{{l}repeat}bd /DL{{L}repeat}bd"	<< endl;
  pout<<"/ST {gs dup stringwidth pop 3 2 roll ex div 1 scale show gr}bd"	<< endl;
  pout<<"/S/show ld /s {0 m}bd /sp {gr showpage}bd /sh {true charpath}bd"                  << endl;
  pout<<"/font {findfont ex scalefont setfont}bd "                              << endl;
  pout<<"/beginpage {MSPostScript_1.0 begin /state save def gs}def"             << endl;
  pout<<"/endpage {gr state restore end showpage}def"                           << endl;
  pout<<"%%EndProlog" 								<< endl;
}

void MSPostScript::printSetup(void)
{
  pout<<"%%BeginSetup"<< endl;
  if (outputMode()!=EPS&&pageSize()==Legal)
   {
     pout<<"%%BeginPaperTray"<< endl;
     pout<<"statusdict /legaltray known {statusdict begin legaltray end} if"<< endl;
   }
  pout<<"%%EndSetup"<< endl;
}

void MSPostScript::printEpilog(void)
{
  disclaimer().print();
  disclaimer().printRule();
  pout<< " sp" <<endl;
  pout<< "%%Trailer" <<endl;
  if (outputMode()==EPS) pout<<"end"<<endl<<"%%EOF" <<endl;
}

const char *MSPostScript::extractFontString(const char *fontStr_)
{
  const char   *numbers="0123456789";
  static char 	buf[64];
  char 	       *n;
  int		i,j;

  *buf='\0';
  if (fontStr_!=0)
   {
     // cast is for bug in Borland
     if ((n=strpbrk((char*)(void*)fontStr_,numbers))!=0)
      {
	i=n-(char*)fontStr_;
	for (j=0; j<i-1; j++)
	 {
	   buf[j]=fontStr_[j];
	 }
	buf[i-1]='\0';
      }
     else
      {
	for (j=0; j<strlen(fontStr_); j++)
	 {
	   buf[j]=fontStr_[j];
	 }
	buf[j]='\0';
      }
   }
  return strlen(buf)!=0?buf:0;
}

const char *MSPostScript::formatFontString(const char *fontStr_)
{
  static char buf[64];
  const char *fontStr=extractFontString(fontStr_);

  *buf='\0';
  if (fontStr!=0)
   {
     int i;
     for (i=0;i<strlen(fontStr)&&i<64;i++)
      {
	buf[i]=tolower(fontStr_[i]);
      }
     buf[i]='\0';
   }
  return strlen(buf)!=0?buf:0;
}

void MSPostScript::updateForeground(int)
{
  if (fgColor()!=gcValues().foreground)
   {
     fgRGB().pixel=gcValues().foreground;
     fgColor(gcValues().foreground);
     bgColor(0);
   }
}

void MSPostScript::updateBackground(void)
{
  if (bgColor()==0||bgColor()!=gcValues().background)
   {
     bgRGB().pixel=gcValues().background;
     bgColor(gcValues().background);
     fgColor(0);
   }
}

int MSPostScript::extractFontSize(const char *fontStr_)
{
  const char   *numbers="0123456789";
  char 	       *n;
  int 		ret=6;
  
  // cast is for bug in Borland
  if (fontStr_!=0&&(n=strpbrk((char*)(void*)fontStr_,numbers))!=0)
   {
     ret=atoi((const char *)n);
   }
  return ret;
}

void MSPostScript::updateFont(void)
{}     

void MSPostScript::setAttributes(void)
{
  MSBoolean	update=MSFalse;
  
  if (setForeground()==MSTrue) update=MSTrue;
  if (setLineAttributes()==MSTrue) update=MSTrue;	
  if (update==MSTrue) pout<<endl;
}

MSBoolean MSPostScript::setForeground(void)
{
  MSBoolean 	status=MSFalse;
  
  switch(printMode())
   {
   case Mono:
     if (gscale()!=0)
      {
	gscale(0);
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
	status=MSTrue;
      }
     break;
	
   case Reverse:
     if (gscale()!=1)
      {
	gscale(1);
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	pout<< " ";
	status=MSTrue;
      }
     break;
	
   case Colorfg:
     if (gcValues().foreground==whitePixel()) gcValues().foreground=blackPixel();
   case Color:
     status=setFGColor();
     break;
   }
  return status;
}

void MSPostScript::setBackground(int mode_)
{
  MSBoolean status=MSFalse;
  
  switch(printMode())
   {
   case Mono:
     if ((mode_==0 && gscale()!=1)||(mode_==1 && gscale()!=0))
      {
	if (mode_==1) gscale(0);
	else gscale(1);
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	status=MSTrue;
      }
     break;
	
   case Reverse:
     if ((mode_==0 && gscale()==1)||(mode_==1 && gscale()==0))
      {
	if (mode_==1) gscale(1);
	else gscale(0);
	pout<< gscale();
	pout<< " ";
	pout<< "sg";
	status=MSTrue;
      }
     break;
	
   case Colorfg:
     if (mode_==-1) status=setBGColor();
     else 
      {
	if (gcValues().background==whitePixel()) gcValues().background=blackPixel();
	status=setFGColor();
      }
     break;
     
   case Color:
     if (mode_==-1) status=setBGColor();
     else status=setFGColor();
     break;
   }
  if (status==MSTrue) pout<<endl;
}

MSBoolean MSPostScript::setFGColor(int)
{
  MSBoolean 	status=MSFalse;

  if (fgColor()!=gcValues().foreground)
   {
     updateForeground();
     if (fgRGB().red==fgRGB().green&&fgRGB().green==fgRGB().blue)
      {
	pout<< (double)(fgRGB().red)/65536.0;
	pout<< " ";
	pout<< "sg";
      }
     else
      {
	pout<< (double)(fgRGB().red>>8);
	pout<< " ";
	pout<< (double)(fgRGB().green>>8);
	pout<< " ";
	pout<< (double)(fgRGB().blue>>8);
	pout<< " ";
	pout<< "tc";
      }
     pout<< " ";
     status=MSTrue;
   }
  return status;
}   

MSBoolean MSPostScript::setBGColor(void)
{
  MSBoolean 	status=MSFalse;

  if (bgColor()!=gcValues().foreground)
   {
     updateBackground();
     if (bgRGB().red==bgRGB().green&&bgRGB().green==bgRGB().blue)
      {
	pout<< (double)(bgRGB().red)/65536.0;
	pout<< " ";
	pout<< "sg";
      }
     else
      {
	pout<< (double)(bgRGB().red>>8);
	pout<< " ";
	pout<< (double)(bgRGB().green>>8);
	pout<< " ";
	pout<< (double)(bgRGB().blue>>8);
	pout<< " ";
	pout<< "tc";
      }
     pout<< " ";
     status=MSTrue;
   }
  return status;
}   

MSBoolean MSPostScript::setLineAttributes(void)
{
  MSBoolean	status=MSFalse;
  
  if (lineWidth()!=gcValues().line_width)
   {
     lineWidth(gcValues().line_width);
     pout<< ((lineWidth()>2)?lineWidth()-1:lineWidth());
     pout<< " ";
     pout<< "w";
     pout<< " ";
     status=MSTrue;
   }
  if (lineStyle()!=gcValues().line_style)
   {
     dashOffset(gcValues().dash_offset);
     switch (gcValues().line_style)
      { 
      case LineOnOffDash:
      case LineDoubleDash:
	pout<<"[ ";
	pout<<"]";
	pout<<" ";
	pout<<dashOffset();
	pout<<" ";
	pout<<"d";
	status=MSTrue;
	break;

      case LineSolid:
      default:
	pout<< "[] 0 d";
	status=MSTrue;
      }
     pout<< " ";
     lineStyle(gcValues().line_style);
   }
  if (capStyle()!=gcValues().cap_style)
   {
     capStyle(gcValues().cap_style);
     pout<< capStyle()-(capStyle()!=0?1:0);
     pout<< " ";
     pout<< "lc";
     pout<< " ";
     status=MSTrue;
   }
  if (joinStyle()!=gcValues().join_style)
   {
     joinStyle(gcValues().join_style);
     pout<< joinStyle();
     pout<< " ";
     pout<< "lj";
     status=MSTrue;
   }
  return status;
}

void MSPostScript::setFontAttributes(void)
{
  if (fontID()==0||fontID()!=gcValues().font)
   {
     updateFont();
     pout<< fontSize()*((y_printScale()==PSFactor)?y_printScale():1);
     pout<< "/";
     pout<< fontString();
     pout<< " ";
     pout<< "font";
     pout<< endl;
   }
}

void MSPostScript::setFillStyle(void)
{
}

void MSPostScript::setFillRule(void)
{
}

void MSPostScript::printArc(int x_,int y_,int w_,int h_,int angle1_,int angle2_,int arcMode_,DrawMode mode_)
{
  static short circle=64*360;

  double yscale=w_>h_?(double)h_/w_:1.0;
  double xscale=h_>w_?(double)w_/h_:1.0;
  double r=(w_>h_?w_:h_)/2;
  double x=x_+r;
  double y=y_-r;
  double xt=x/xscale;
  double yt=y/yscale;
  
  if (xscale<1||yscale<1) 
   {
     pout<< "gs";
     pout<< " ";
     pout<< (xscale<1?r*xscale-r:0);
     pout<< " ";
     pout<< (yscale<1?r-r*yscale:0);
     pout<< " ";
     pout<< "translate";
     pout<< " ";
     pout<< xscale;
     pout<< " ";
     pout<< yscale;
     pout<< " ";
     pout<< "scale";
     pout<< " ";
   }
  if (arcMode_==ArcPieSlice&&mode_==Fill)
   {
     pout<< xt;
     pout<< " ";
     pout<< yt;
     pout<< " ";
     pout<< "M";
     pout<< " ";
   }
  pout<< xt;
  pout<< " ";
  pout<< yt;
  pout<< " ";
  pout<< r;
  pout<< " ";
  if (angle2_==circle)
   {
     pout<< "C";
   }
  else
   {
     double angle1=(angle1_<0?angle1_+circle:angle1_)/64.0;
     double angle2=angle1+angle2_/64.0;
     if (angle2_<0)
      {
        pout<< angle2;
        pout<< " ";
        pout<< angle1;
      }
     else
      {
        pout<< angle1;
        pout<< " ";
        pout<< angle2;
      }
     pout<< " ";
     pout<< "arc";
   }
  pout<< " ";
  pout<< (mode_==Fill?"f":"st");
  pout<< " ";
  if (xscale<1||yscale<1) pout<< "gr";
  pout<< endl;
}

void MSPostScript::printLine(double x1_,double y1_,double x2_,double y2_)
{
  pout<< x1_;
  pout<< " ";
  pout<< y1_;
  pout<< " ";
  pout<< x2_;
  pout<< " ";
  pout<< y2_;
  pout<< " ";
  pout<< "v";
  pout<< endl;
}

void MSPostScript::printRectangle(double x_,double y_,double w_,double h_)
{
  pout<< "n";
  pout<< " ";
  pout<< x_;
  pout<< " ";
  pout<< y_;
  pout<< " ";
  pout<< "M";
  pout<< " ";
  pout<< -w_;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< -h_;
  pout<< " ";
  pout<< w_;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "3";
  pout<< " ";
  pout<< "D";
  pout<< " ";
  pout<< "P";
  pout<< " ";
}

void MSPostScript::printString(int x_,int y_,const char *string_,int n_)
{
  MSString aString(string_,n_);
  aString.change("\\","\\\\").change("(","\\(").change(")","\\)");
  
  if (aString.length()>0)
   {
     pout<< x_;
     pout<< " ";
     pout<< y_;
     pout<< " ";
     pout<< "M";
     pout<< " ";
     pout<< "(";
     pout<< aString;
     pout<< ")";
     pout<< " ";
     pout<< "show";
     pout<< endl;
   } 
}

void MSPostScript::setClipRectangle(int x_,int y_,int width_,int height_)
{
  pout<< x_;
  pout<< " ";
  pout<< y_;
  pout<< " ";
  pout<< "M";
  pout<< " ";
  pout<< -width_;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< -height_;
  pout<< " ";
  pout<< width_;
  pout<< " ";
  pout<< "0";
  pout<< " ";
  pout<< "3";
  pout<< " ";
  pout<< "D";
  pout<< " ";
  pout<< "clip";
  pout<< " ";
  pout<< "n";
}


MSString MSPostScript::adjustedFileName(void) const
{
  MSString file(fileName());
  if (outputMode()==EPS) file.change(".ps",".eps",0,1);
  if (outputMode()==PPM) file.change(".ps",".ppm",0,1);
  return file;
}

