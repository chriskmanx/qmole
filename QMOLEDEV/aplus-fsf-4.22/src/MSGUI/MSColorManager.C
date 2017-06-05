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
#include <MSTypes/MSIndexVector.H>
#include <MSGUI/MSColorManager.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMessageLog.H>

static const int MSColorManagerSize=256;

MSColorManager::MSColorManager(void)
{
  _server=MSDisplayServer::defaultDisplayServer();
  init();
}

MSColorManager::MSColorManager(MSDisplayServer& server_) :
_colorPixelHashTable(MSColorManagerSize),
_colorStringHashTable(MSColorManagerSize),
_rgbHashTable(MSColorManagerSize)
{
  _server=&server_;
  init();
}

MSColorManager::~MSColorManager(void)
{}

void MSColorManager::init(void)
{
  const unsigned short defaultRGBCloseness=USHRT_MAX/2;

  _warned=MSFalse;
  _bestMatch=MSTrue;
  _redCloseness=defaultRGBCloseness;
  _greenCloseness=defaultRGBCloseness;
  _blueCloseness=defaultRGBCloseness;
  _defaultFg=0;
  _defaultBg=0;
  _cmap=DefaultColormap(display(),DefaultScreen(display()));
  colorPixelHashTable().notFound(0x55FF);
  colorStringHashTable().notFound(0x0);
  rgbHashTable().notFound(ULONG_MAX);

  addColor(MSDefaultBackgroundColorSpec);
  addColor(MSDefaultForegroundColorSpec);
  _defaultBg=pixel(MSDefaultBackgroundColorSpec);
  _defaultFg=pixel(MSDefaultForegroundColorSpec);
  if (MSDefaultBackground==0) MSDefaultBackground=defaultBg();
  if (MSDefaultForeground==0) MSDefaultForeground=defaultFg();  
}

unsigned long MSColorManager::addColor(const char *color_)
{
  if (color_!=0)
   {
     XColor hardware_def;
     Status s=XParseColor(display(),colormap(),color_,&hardware_def);
     if (s!=0) s=XAllocColor(display(),colormap(),&hardware_def);
     else
      {
	MSMessageLog::warningMessage("Error: Bad Color Specification '%s'.\n",color_);
	return defaultFg();
      }
     if (s!=0) 
      {
	cacheColor(color_,hardware_def.pixel);
	return hardware_def.pixel;
      }
     else
      {
	Visual *visual=server()->visual();
	// We are only going to implement best match for psuedo color visual
	if (bestMatch()==MSTrue&&(visual->c_class==PseudoColor||visual->c_class==GrayScale))
	 {
	   bestMatchWarning(color_);
	   if (matchColor(hardware_def)==MSTrue)
	    {
	      cacheColor(color_,hardware_def.pixel);
	      return hardware_def.pixel;
	    }
	   
	   else
	    {
	      MSMessageLog::warningMessage("Warning - Failed to find a best match for requested color.  Using default.\n");
 	      return defaultFg();
	    }
	 }
	else
	 {
	   MSMessageLog::warningMessage("Warning - Cannot allocate color '%s'.\n"
	    "Possible blown colormap resources - i.e. more than 255 colors in use\n",color_);
	   return defaultFg();
	 }
      }
   }
  else
   { 
     MSMessageLog::warningMessage("Empty Color Specification.\n");
     return defaultFg();
   }
}

unsigned long MSColorManager::pixel(const char *color_)
{
  unsigned long pixel=(unsigned long) colorPixelHashTable().lookup(color_);
  if (pixel==colorPixelHashTable().notFound()) return addColor(color_);
  return pixel;
}

const char *MSColorManager::colorName(unsigned long pixel_) const
{ return (char *)colorStringHashTable().lookup(pixel_);}

void MSColorManager::query(XColor *colorcell_) const
{ XQueryColor(display(),colormap(),colorcell_); }

MSBoolean MSColorManager::allocate(XColor *colorcell_)
{
  char key[256];
  sprintf(key,"%u.%u.%u",colorcell_->red,colorcell_->green,colorcell_->blue);
  unsigned long pixel=(unsigned long) rgbHashTable().lookup(key);
  if (pixel!=rgbHashTable().notFound())
   {
     colorcell_->pixel=pixel;
     return MSTrue;
   }
  else if (XAllocColor(display(),colormap(),colorcell_)!=0)
   {
     rgbHashTable().add(key,(void *)colorcell_->pixel);
     return MSTrue;
   }
  else
   {
     Visual *visual=server()->visual();
     if (bestMatch()==MSTrue&&(visual->c_class==PseudoColor||visual->c_class==GrayScale))
      {
	MSString colorString;
	colorString<<"("<<MSString(colorcell_->red)<<"."<<MSString(colorcell_->green)<<"."<<MSString(colorcell_->blue)<<")";
	bestMatchWarning(colorString);
	if (matchColor(*colorcell_)==MSFalse)
	 {
	   MSMessageLog::warningMessage("Failed to find a best match for requested color.  Using default.\n");
	   colorcell_->pixel=defaultFg();
	   return MSFalse;
	 }
	else
         {
           rgbHashTable().add(key,(void *)colorcell_->pixel);
           return MSTrue;
         }
      }
     else
      {
        MSMessageLog::warningMessage("Warning - Cannot allocate  RGB of %d.%d.%d\n"
	    "Possible blown colormap resources - i.e. more than 255 colors in use\n",
                                     colorcell_->red,colorcell_->green,colorcell_->blue);
	colorcell_->pixel=defaultFg();
	return MSFalse;
      }
   }
}

static unsigned indexCompareUp(long *p_,unsigned i_,unsigned j_)
{ return (p_[i_]!=p_[j_])?((p_[i_])<(p_[j_])):(i_<j_); }

static unsigned mergeSortUp(unsigned n_,long *sp_,unsigned *p_,unsigned low_,unsigned high_)
{
  unsigned t,m=(low_+high_+1)>>1;
  if (high_==m) {p_[low_]=UINT_MAX;return low_;}
  high_=mergeSortUp(n_,sp_,p_,m,high_);
  low_=mergeSortUp(n_,sp_,p_,low_,m);
  if (indexCompareUp(sp_,high_,low_)) {m=low_;low_=high_;high_=m;}
  for (t=low_;;low_=p_[low_]=high_,high_=m)
   {
L:m=p_[low_];
    if (UINT_MAX==m) {p_[low_]=high_;return t;}
    if (indexCompareUp(sp_,m,high_)) {low_=m;goto L;}
   }
}

// this is done here, so that we do need to pull in MSLongVector
static MSIndexVector gradeUp(long *sp_,unsigned length_)
{
  unsigned n=length_;
  if (n>0)
   {
     unsigned *p=new unsigned[n];
     MSIndexVector::Data *d=MSIndexVector::Data::allocateWithLength(length_);
     unsigned *dp=d->data();
     dp[0]=mergeSortUp(n,sp_,p,0,n);
     for (unsigned i=0;i<n-1;++i) dp[i+1]=p[dp[i]];
     delete [] p;
     return MSIndexVector(d,n);
   }
  return MSIndexVector();
}

MSBoolean MSColorManager::matchColor(XColor& color_)
{
  const int colorFactor=3;
  const int brightnessFactor=1;
  const int bestMatchIteration=3;

  int ncols=server()->visual()->map_entries;
  XColor *colors=new XColor[ncols];
  for (int n=0;n<ncols;n++) colors[n].pixel=n;
  
  MSIndexVector colorIndex(ncols);
  long *closeness=new long[ncols];
  // Only going to do 3 iterations. It turns out that it rarely needs more than 1 try.
  for (int i=1;i<=bestMatchIteration;i++)
   {
     // First get every color in the color map.
     XQueryColors(display(),colormap(),colors,ncols);
     
     // Calculate a vector of closeness value for every entry inside the colormap.
     // Closeness value is based on the RGB of the color we want to match
     // and the RGB of the entry inside the colormap.
     // And then we sort the vector in an ascending order by the closeness values.
     for (int j=0;j<ncols;j++)
      {
	colorIndex[j]=j;
	closeness[j]=colorFactor*(MSUtil::abs((long)color_.red-(long)colors[j].red)+
				  MSUtil::abs((long)color_.green-(long)colors[j].green)+
				  MSUtil::abs((long)color_.blue-(long)colors[j].blue))+
	brightnessFactor*(MSUtil::abs((long)color_.red+(long)color_.green+(long)color_.blue)-
	  	                     ((long)colors[j].red+(long)colors[j].green+(long)colors[j].blue));
      }
     colorIndex.permute(gradeUp(closeness,ncols));
     int index=0;
     unsigned cIndex=colorIndex(index);
     // Only look at those colors inside the colormap that satisfy our criteria
     while ((long)colors[cIndex].red>=(long)color_.red-(long)redCloseness()&&
	    (long)colors[cIndex].red<=(long)color_.red+(long)redCloseness()&&
	    (long)colors[cIndex].green>=(long)color_.green-(long)greenCloseness()&&
	    (long)colors[cIndex].green<=(long)color_.green+(long)greenCloseness()&&
	    (long)colors[cIndex].blue>=(long)color_.blue-(long)blueCloseness()&&
	    (long)colors[cIndex].blue<=(long)color_.blue+(long)blueCloseness()) 
      {
	if (XAllocColor(display(),colormap(),&colors[cIndex]))
	 {
	   if (i==bestMatchIteration) XUngrabServer(display());
	   color_=colors[cIndex];
           delete [] closeness; 
	   delete [] colors;
	   return MSTrue;
	 }
	if (++index!=ncols) cIndex=colorIndex(index);
	else break;
      }
     
     if (i==bestMatchIteration) XUngrabServer(display());
     if (index==0||index==ncols)
      {
	// If we get here, that means nothing in the colormap is close enough
	// for use, or everything in the colormap is r/w.
        delete [] closeness; 
	delete [] colors;
	return MSFalse;
      }
     // Try the original color again cause the colormap might have changed.
     if (XAllocColor(display(),colormap(),&color_))
      {
        delete [] closeness; 
	delete [] colors;
	return MSTrue;
      }
     else
      {
	// For the last iteration, we want to grab the server to make sure
	// nobody else can touch the color map.
	if (i==bestMatchIteration-1) XGrabServer(display());
      }
   }
  delete [] closeness; 
  delete [] colors;
  return MSFalse;
}

void MSColorManager::cacheColor(const char *colorName_,unsigned long pixel_)
{
  if ((unsigned long)colorPixelHashTable().lookup(colorName_)==colorPixelHashTable().notFound())
   {
     colorPixelHashTable().add(colorName_,(void *)pixel_);
   }
  if ((char *)colorStringHashTable().lookup(pixel_)==(char *)colorStringHashTable().notFound())
   {
     int len=strlen((char *)colorName_);
     char *cp=new char[len+1];
     strcpy(cp,colorName_);
     cp[len]='\0';
     colorStringHashTable().add(pixel_,(void *)cp); 
   }
}

void MSColorManager::bestMatchWarning(const char *)
{
  // We don't look at the color string passed in, but if we
  // ever need it, it is available.
  if (warned()==MSFalse)
   {
     _warned=MSTrue;
     MSMessageLog::warningMessage("Warning - Color allocation failed.  Will be searching for a best match.\n"
                                  "          Some colors may appear slightly different from their specifications.\n");
   }
}
