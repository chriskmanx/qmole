///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSDisplayServer.H>
#include <MSGUI/MSColorManager.H>
#include <MSGUI/MSPixmap.H>

#ifndef MSWidgetHEADER
#include <MSGUI/MSWidget.H>
#endif

#include <MSGUI/MSShadow.H>

static const unsigned long MaxIntensity=65535;

// Contributions of each primary to overall luminosity,sum to 1.0 

static const double RedLuminosity  =0.3;
static const double GreenLuminosity=0.59;
static const double BlueLuminosity =0.11;

// Percent effect of intensity,light,and luminosity & on brightness,

static const unsigned long IntensityFactor =25;
static const unsigned long LightFactor     =0;
static const unsigned long LuminosityFactor=75;

// Thresholds for brightness
// above Lite threshold,Lite color model is used
// below Dark threshold,Dark color model is be used
// use Std color model in between 

static const unsigned long ColorLiteThreshold =(unsigned long)(0.77*MaxIntensity);
static const unsigned long ForegroundThreshold=(unsigned long)(0.35*MaxIntensity);
static const unsigned long ColorDarkThreshold =(unsigned long)(0.15*MaxIntensity);

static const unsigned long ColorLiteSelFactor=15;
static const unsigned long ColorLiteBsFactor =45;
static const unsigned long ColorLiteTsFactor =20;

static const unsigned long ColorDarkSelFactor=15;
static const unsigned long ColorDarkBsFactor =30;
static const unsigned long ColorDarkTsFactor =50;

static const unsigned long ColorHiSelFactor=15;
static const unsigned long ColorHiBsFactor =35;
static const unsigned long ColorHiTsFactor =70;

static const unsigned long ColorLoSelFactor =15;
static const unsigned long ColorLoBsFactor  =60;
static const unsigned long ColorLoTsFactor  =40;

#ifdef MS_MULTI_THREAD
MSMutex MSShadow::_shadowHashTableMutex;
#endif

MSShadow::ShadowColors::ShadowColors(void)
{ _count = 0; }

MSShadow::ShadowColors::~ShadowColors(void) {}
 
MSShadow::MSShadow(void) :
_server(0),_shadowColors(0)
{}

MSShadow::MSShadow(MSWidget *pWidget_) :
_server(pWidget_->server()),_shadowColors(0)
{ if (pWidget_!=0) color(pWidget_->background()); }

MSShadow::MSShadow(MSDisplayServer *server_) :
_server(server_),_shadowColors(0)
{ if (_server!=0) color(server_->defaultBackground()); }

MSShadow::MSShadow(const MSShadow& aShadow_) :
_server(aShadow_._server),_shadowColors(aShadow_._shadowColors)
{
  MSGUARD(_shadowHashTableMutex);
  if (_shadowColors!=0) _shadowColors->increment(); 
}

MSShadow::~MSShadow(void)
{ 
   MSGUARD(_shadowHashTableMutex);
   deleteColors(); 
}

void MSShadow::displayServer(MSDisplayServer *server_)
{
  _server=server_;
  if (_server!=0) color(server_->defaultBackground());
}

MSShadow& MSShadow::operator=(const MSShadow& aShadow_)
{
  if (this!=&aShadow_)
   {
     MSGUARD(_shadowHashTableMutex);
     deleteColors();
     _server=aShadow_._server;
     _shadowColors=aShadow_._shadowColors;
     if (_shadowColors!=0) _shadowColors->increment();
   }
  return *this;
}

void MSShadow::deleteColors(void)
{
  if (_shadowColors!=0)
   {
      if (_shadowColors->count()==1)
      {
        MSHashTable *sht=_server->shadowHashTable();
        sht->remove(_shadowColors->_bgColor.pixel);     
        XFreeGC(_server->display(),_shadowColors->_bgGC);
        XFreeGC(_server->display(),_shadowColors->_tsGC);
        XFreeGC(_server->display(),_shadowColors->_bsGC);
        XFreeGC(_server->display(),_shadowColors->_selectGC);
      }
     _shadowColors->decrement();
     _shadowColors=0;
   }
}

void MSShadow::color(unsigned long pixel_)
{
  if (_server!=0)
   {
     MSGUARD(_shadowHashTableMutex);
     ShadowColors *c;
     MSHashTable *sht=_server->shadowHashTable();
     int depth=DefaultDepthOfScreen(_server->screen());
     if (depth==1)
      {
        unsigned long pixel=WhitePixelOfScreen(_server->screen());
        if ((unsigned long)(c=(ShadowColors *)sht->lookup(pixel))==0)
         {
           if (_shadowColors!=0) deleteColors();
           _shadowColors=new ShadowColors;
           sht->add(pixel,(void *)_shadowColors);
           _shadowColors->_bgColor.pixel=pixel;
           _server->colorManager()->query(&_shadowColors->_bgColor);
           setMonochromeColors();
           _server->colorManager()->allocate(&_shadowColors->_bgColor);
           _server->colorManager()->allocate(&_shadowColors->_fgColor);
           _server->colorManager()->allocate(&_shadowColors->_selectColor);
           _server->colorManager()->allocate(&_shadowColors->_bsColor);
           _server->colorManager()->allocate(&_shadowColors->_tsColor);
           createGCs();
         } 
        else if (_shadowColors!=c)
         {
           if (_shadowColors!=0) deleteColors();
           _shadowColors=c;
         }
      }
     else
      {
        if ((unsigned long)(c=(ShadowColors *)sht->lookup(pixel_))==0)
         {
           if (_shadowColors!=0) deleteColors();
           _shadowColors=new ShadowColors;
           sht->add(pixel_,(void *)_shadowColors);
           _shadowColors->_bgColor.pixel=pixel_;
           _server->colorManager()->query(&_shadowColors->_bgColor);
           calculateRGBColors();
           _server->colorManager()->allocate(&_shadowColors->_bgColor);
           _server->colorManager()->allocate(&_shadowColors->_fgColor);
           _server->colorManager()->allocate(&_shadowColors->_selectColor);
           _server->colorManager()->allocate(&_shadowColors->_bsColor);
           _server->colorManager()->allocate(&_shadowColors->_tsColor);
           createGCs();
         } 
        else if (_shadowColors!=c)
         {
           if (_shadowColors!=0) deleteColors();
           _shadowColors=c;
         }
      }
     _shadowColors->increment();
   }
}   

void MSShadow::createGCs(void)
{
  XGCValues gcvalues;
  unsigned long valueMask=GCForeground|GCBackground|GCGraphicsExposures;
  Window root=_server->root();

  gcvalues.background=_shadowColors->_bgColor.pixel;
  gcvalues.foreground=_shadowColors->_bgColor.pixel;   
  gcvalues.graphics_exposures=False;
  _shadowColors->_bgGC=XCreateGC(_server->display(),root,valueMask,&gcvalues);   

  gcvalues.foreground=_shadowColors->_selectColor.pixel;   
  _shadowColors->_selectGC=XCreateGC(_server->display(),root,valueMask,&gcvalues);

  gcvalues.foreground=_shadowColors->_bsColor.pixel;   
  _shadowColors->_bsGC=XCreateGC(_server->display(),root,valueMask,&gcvalues);   

  gcvalues.foreground=_shadowColors->_tsColor.pixel;   
  MSPixmap *tsPixmap=0;

  if (_shadowColors->_tsColor.pixel==_shadowColors->_bgColor.pixel)
   {
     tsPixmap=new MSPixmap(_server,MSPixmap::ForegroundFiftyPixmap,
			   _shadowColors->_tsColor.pixel,_shadowColors->_fgColor.pixel);
   }
  else if (DefaultDepthOfScreen(_server->screen())==1)
   {
     tsPixmap=new MSPixmap(_server,MSPixmap::ForegroundFiftyPixmap,
			   _shadowColors->_tsColor.pixel,_shadowColors->_bgColor.pixel);
   }

  if (tsPixmap!=0)
   {
     valueMask|=GCFillStyle|GCTile;
     gcvalues.fill_style=FillTiled;
     gcvalues.tile=tsPixmap->pixmap();
   }
  _shadowColors->_tsGC=XCreateGC(_server->display(),root,valueMask,&gcvalues);   
}

void MSShadow::setMonochromeColors(void)
{
  if (_shadowColors->_bgColor.pixel==BlackPixelOfScreen(_server->screen()))
   {
     _shadowColors->_fgColor.pixel=WhitePixelOfScreen (_server->screen());
     _shadowColors->_fgColor.red=_shadowColors->_fgColor.green=
     _shadowColors->_fgColor.blue=(unsigned short)MaxIntensity;
     
     _shadowColors->_bsColor.pixel=WhitePixelOfScreen(_server->screen());
     _shadowColors->_bsColor.red=_shadowColors->_bsColor.green=
     _shadowColors->_bsColor.blue=(unsigned short)MaxIntensity;
     
     _shadowColors->_selectColor.pixel=WhitePixelOfScreen(_server->screen());
     _shadowColors->_selectColor.red=_shadowColors->_selectColor.green=
     _shadowColors->_selectColor.blue=(unsigned short)MaxIntensity;
     
     _shadowColors->_tsColor.pixel=BlackPixelOfScreen(_server->screen());
     _shadowColors->_tsColor.red=_shadowColors->_tsColor.green=
     _shadowColors->_tsColor.blue=0;
   }
  else if (_shadowColors->_bgColor.pixel==WhitePixelOfScreen(_server->screen()))
   {
     _shadowColors->_fgColor.pixel=BlackPixelOfScreen (_server->screen());
     _shadowColors->_fgColor.red=_shadowColors->_fgColor.green=
     _shadowColors->_fgColor.blue=0;
     
     _shadowColors->_bsColor.pixel=BlackPixelOfScreen(_server->screen());
     _shadowColors->_bsColor.red=_shadowColors->_bsColor.green=
     _shadowColors->_bsColor.blue=0;

     _shadowColors->_selectColor.pixel=BlackPixelOfScreen(_server->screen());
     _shadowColors->_selectColor.red=_shadowColors->_selectColor.green=
     _shadowColors->_selectColor.blue=0;

     _shadowColors->_tsColor.pixel=WhitePixelOfScreen(_server->screen());
     _shadowColors->_tsColor.red=_shadowColors->_tsColor.green=
     _shadowColors->_tsColor.blue=(unsigned short)MaxIntensity;
   }
}
  
unsigned long MSShadow::brightness(void)
{
  unsigned long red=(unsigned long)_shadowColors->_bgColor.red;
  unsigned long green=(unsigned long)_shadowColors->_bgColor.green;
  unsigned long blue=(unsigned long)_shadowColors->_bgColor.blue;
  unsigned long rgbsum=(red+green+blue);
  unsigned long intensity=(rgbsum)/3;
  unsigned long luminosity=(unsigned long)
                 ((RedLuminosity*(double)red)+
                  (GreenLuminosity*(double)green)+ 
                  (BlueLuminosity*(double)blue));
  unsigned long maxprimary=((red>green)?((red>blue)?red:blue):((green>blue)?green:blue));
  unsigned long minprimary=((red<green)?((red<blue)?red:blue):((green<blue)?green:blue));
  unsigned long light=(minprimary+maxprimary)>>1;
  return (unsigned long)
          (((intensity*IntensityFactor)+
           (light*LightFactor)+
           (luminosity*LuminosityFactor))/100);
}

void MSShadow::calculateColorsForLightBackground(void)
{
  _shadowColors->_fgColor.red=0;
  _shadowColors->_fgColor.green=0;
  _shadowColors->_fgColor.blue=0;
  
  unsigned long color_value=(unsigned long)_shadowColors->_bgColor.red;
  color_value-=(color_value*ColorLiteSelFactor)/100;
  _shadowColors->_selectColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value-=(color_value*ColorLiteSelFactor)/100;
  _shadowColors->_selectColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value-=(color_value*ColorLiteSelFactor)/100;
  _shadowColors->_selectColor.blue=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.red;
  color_value-=(color_value*ColorLiteBsFactor)/100;
  _shadowColors->_bsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value-=(color_value*ColorLiteBsFactor)/100;
  _shadowColors->_bsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value-=(color_value*ColorLiteBsFactor)/100;
  _shadowColors->_bsColor.blue=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.red;
  color_value-=(color_value*ColorLiteTsFactor)/100;
  _shadowColors->_tsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value-=(color_value*ColorLiteTsFactor)/100;
  _shadowColors->_tsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value-=(color_value*ColorLiteTsFactor)/100;
  _shadowColors->_tsColor.blue=(unsigned short)color_value;
}
	
void MSShadow::calculateColorsForDarkBackground(void)
{
  _shadowColors->_fgColor.red=(unsigned short)MaxIntensity;
  _shadowColors->_fgColor.green=(unsigned short)MaxIntensity;
  _shadowColors->_fgColor.blue=(unsigned short)MaxIntensity;
  
  unsigned long color_value=(unsigned long)_shadowColors->_bgColor.red;
  color_value+=ColorDarkSelFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_selectColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value+=ColorDarkSelFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_selectColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value+=ColorDarkSelFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_selectColor.blue=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.red;
  color_value+=ColorDarkBsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_bsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value+=ColorDarkBsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_bsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value+=ColorDarkBsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_bsColor.blue=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.red;
  color_value+=ColorDarkTsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value+=ColorDarkTsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value+=ColorDarkTsFactor*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.blue=(unsigned short)color_value;
}

void MSShadow::calculateColorsForMediumBackground(void)
{
  unsigned long b=brightness();
  if (b>ForegroundThreshold)
   {
     _shadowColors->_fgColor.red=0;
     _shadowColors->_fgColor.green=0;
     _shadowColors->_fgColor.blue=0;
   }
  else
   {
     _shadowColors->_fgColor.red=(unsigned short)MaxIntensity;
     _shadowColors->_fgColor.green=(unsigned short)MaxIntensity;
     _shadowColors->_fgColor.blue=(unsigned short)MaxIntensity;
   }
  
  unsigned long f=ColorLoSelFactor+(b*(ColorHiSelFactor-ColorLoSelFactor)/MaxIntensity);
  unsigned long color_value=_shadowColors->_bgColor.red;
  color_value-=(color_value*f)/100;
  _shadowColors->_selectColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value-=(color_value*f)/100;
  _shadowColors->_selectColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value-=(color_value*f)/100;
  _shadowColors->_selectColor.blue=(unsigned short)color_value;
  
  f=ColorLoBsFactor-(b*(ColorLoBsFactor-ColorHiBsFactor)/MaxIntensity);
  
  color_value=_shadowColors->_bgColor.red;
  color_value-=(color_value*f)/100;
  _shadowColors->_bsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value-=(color_value*f)/100;
  _shadowColors->_bsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value-=(color_value*f)/100;
  _shadowColors->_bsColor.blue=(unsigned short)color_value;
  
  f=ColorLoTsFactor+(b*(ColorHiTsFactor-ColorLoTsFactor)/MaxIntensity);
  
  color_value=_shadowColors->_bgColor.red;
  color_value+=f*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.red=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.green;
  color_value+=f*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.green=(unsigned short)color_value;
  
  color_value=_shadowColors->_bgColor.blue;
  color_value+=f*(MaxIntensity-color_value)/100;
  _shadowColors->_tsColor.blue=(unsigned short)color_value;
}      
   
void MSShadow::calculateRGBColors(void)
{
  unsigned long b=brightness();
  if (b<ColorDarkThreshold) calculateColorsForDarkBackground();
  else if (b>ColorLiteThreshold) calculateColorsForLightBackground();
  else calculateColorsForMediumBackground();
}







