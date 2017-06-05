///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSDefaults.H>
#include <MSGUI/MSFontManager.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSMessageLog.H>

static const double MMPerInch=25.4;
static const int DefaulFontSize=12;
static const int MSFontManagerTableSize=256;
extern const char *MSAltDefaultFont;
extern Font MSDefaultFontID;

class MSFontConverter
{
public:
  static MSString isoName(MSDisplayServer *server_,const char *name_);
};

class MSFontData
{
public:
  MSFontData(MSDisplayServer *server_,const char *fontString_,XFontStruct *fontStruct_);
  ~MSFontData(void);

  MSDisplayServer *_server;
  MSString         _fontString;
  XFontStruct     *_fontStruct;

  MSDisplayServer *server(void) const {return _server;}
  const char *fontString(void)  const {return _fontString.string();}
  XFontStruct *fontStruct(void) const {return _fontStruct;}
};

MSFontHashTable::MSFontHashTable(void) :
MSHashTable()
{}

MSFontHashTable::MSFontHashTable(int size_) :
MSHashTable(size_)
{}

// need to delete the font data structures that are stored as
// data in the hash table
MSFontHashTable::~MSFontHashTable(void)
{
  for (int i=0;i<size();i++)
   {
     MSHashEntry *entry=bucket(i);
     MSFontData *data;
     while (entry!=0)
      {
	_bucket[i]=entry->next();
        data=(MSFontData *) entry->value();
        if (data!=0) delete data;   
	delete entry;
	entry=bucket(i);
      }
   }
}

MSFontData::MSFontData(MSDisplayServer *server_,const char *fontString_,XFontStruct *fontStruct_)
{
  _server=server_;
  _fontString=fontString_;
  _fontStruct=fontStruct_;
}

MSFontData::~MSFontData(void)
{ if (fontStruct()!=0) XFreeFontInfo(0,_fontStruct,1); }

MSFontManager::MSFontManager(MSDisplayServer& server_) :
_fontIDHashTable(MSFontManagerTableSize),_fontDataHashTable(MSFontManagerTableSize)
{
  _server=&server_;
  init();
}

MSFontManager::~MSFontManager(void)
{}

void MSFontManager::init(void)
{
  fontIDHashTable().notFound(0x55FF);
  fontDataHashTable().notFound(0x0);

  addFont(MSDefaultFont);
  addFont(MSAltDefaultFont);
  _defaultFontID=fontID(MSDefaultFont);
  MSDefaultFontID=defaultFontID();
}

void MSFontManager::addFont(const char *fontString_)
{
  Font fontID=0;
  if (fontString_!=0&&server()!=0)
   {
     XFontStruct *nfs=XLoadQueryFont(display(),fontString_);
     if (nfs==0)
      {
        MSString isoName(MSFontConverter::isoName(server(),fontString_));
        if (isoName.length()>0&&isoName!=fontString_) nfs=XLoadQueryFont(display(),isoName.string());
        if (nfs==0)
	 {
	   MSMessageLog::warningMessage("Warning - loading font '%s' failed - not available on this system.\n",
                                     fontString_);
           return;
         }
        else fontID=XLoadFont(display(),isoName.string());
      }
     else fontID=XLoadFont(display(),fontString_);
     if (nfs!=0&&fontID!=0)
      {
	MSFontData *data=0;
	if (fontID!=0)
	 {
	   if ((Font)fontIDHashTable().lookup(fontString_)==(Font)fontIDHashTable().notFound())
	    {
	      fontIDHashTable().add(fontString_,(void *)fontID);
	    }
	   if ((MSFontData *)fontDataHashTable().lookup(fontID)==(MSFontData *)fontDataHashTable().notFound())
	    {
	      data=new MSFontData(server(),fontString_,nfs);
	      fontDataHashTable().add(fontID,(void *)data); 
	    }
	 }
	if (data==0) XFreeFontInfo(NULL,nfs,1);
      }
   }
}

// get a font id from the hash table
// 1. if not already in the hash table,then
//    a. get the defaultfont
//    b. if the defaultfont was not loaded-because it doesn't exist on
//       this system,then use the alternate default,which should be
//       on every system
Font MSFontManager::fontID(const char *fontString_)
{
  Font fontID=(Font)fontIDHashTable().lookup(fontString_);
  if (fontID==fontIDHashTable().notFound())
   {
     addFont(fontString_);
     fontID=(Font) fontIDHashTable().lookup(fontString_);
     if (fontID==fontIDHashTable().notFound())
      {
        fontID=(Font)fontIDHashTable().lookup(MSDefaultFont); // default if notFound
        if (fontID==fontIDHashTable().notFound())
         {
           fontID=(Font)fontIDHashTable().lookup(MSAltDefaultFont); // alternate default
	 }
      }
   }
  return fontID;
}

const char *MSFontManager::fontName(Font fontID_) const
{
  MSFontData *data=(MSFontData *)fontDataHashTable().lookup(fontID_);
  return (char *)(data!=0)?data->fontString():0;
}

const XFontStruct *MSFontManager::fontStruct(Font fontID_) const
{
  MSFontData *data=(MSFontData *)fontDataHashTable().lookup(fontID_);
  return (XFontStruct *)(data!=0)?data->fontStruct():0;
}

MSString MSFontConverter::isoName(MSDisplayServer *server_,const char *name_)
{
  if (name_[0]=='-') return MSString(name_);
  else
   {
     char *pString=strchr((char*)(void*)name_,'-'); // cast: bug in Borland
     int i,count=0;
     int len,size=DefaulFontSize;
     MSString family;
     MSString weight("*");
     if (pString==0) family=name_;
     else
      {
        len=(pString-(char *)name_);
        family=MSString(name_,len);        
	for (i=0;name_[i]!='\0';i++) if (name_[i]=='-') count++;
	if (count==1)
	 {
	   if (pString[1]>='1'&&pString[1]<='9') sscanf(pString+1,"%d",&size);
	   else weight=(pString+1);
	 }
	else if (count==2)
	 {
	   char *cpnext=strchr(pString+1,'-');
	   len=cpnext-(pString+1);
	   weight=MSString(pString+1,len);
	   if (cpnext[1]>='1'&&cpnext[1]<='9') sscanf(cpnext+1,"%d",&size);      
	 }
      }
     int res_x=(int)(server_->width()/(server_->widthMM()/MMPerInch));
     int res_y=(int)(server_->height()/(server_->heightMM()/MMPerInch));
     char *buf=new char[50+family.length()+weight.length()];
     sprintf(buf,"-*-%s-%s-r-*-*-*-%d-%d-%d-*-*-iso8859-1",
             family.string(),weight.string(),size*10,res_x,res_y);
     MSString name(buf); 
     delete [] buf;
     return name;
   }
}






