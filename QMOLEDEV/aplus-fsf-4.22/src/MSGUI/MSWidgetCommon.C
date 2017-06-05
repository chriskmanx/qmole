///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSShadow.H>

#ifdef MS_NO_INLINES
#include <MSGUI/MSWidgetCommonInlines.C>
#endif

extern MSBoolean applicationBackingStoreOption(void);

MSWidgetCommon::MSWidgetCommon(void) 
{ init(); }

MSWidgetCommon::MSWidgetCommon(MSDisplayServer *server_) :
MSWidgetView(server_)
{ init(); }

MSWidgetCommon::MSWidgetCommon(MSWidget *owner_,const char *title_) :
_title(title_),
MSWidgetView(owner_)
{ init(); }

MSWidgetCommon::MSWidgetCommon(MSWidget *owner_,const MSStringVector& title_) :
_title(title_),
MSWidgetView(owner_)
{ init(); }

MSWidgetCommon::~MSWidgetCommon()
{}

void MSWidgetCommon::init(void)
{
  _titleAlignment=MSCenter|MSBottom;
  create();

  if (owner()==0)
   {
     _titleFg=server()->defaultForeground();
     _titleFont=server()->defaultFont();
   }     
  else
   {
     _titleFg=owner()->foreground();
     _titleFont=owner()->font();
   }
}

void MSWidgetCommon::create(void)
{
  XSetWindowAttributes attributes;
  attributes.background_pixel=background();
  attributes.border_pixel=foreground();
  attributes.event_mask=ExposureMask;
  attributes.backing_store=
  (owner()==0)?NotUseful:((applicationBackingStoreOption()==MSTrue)?WhenMapped:NotUseful);
  _eventMask=ExposureMask;
  _window=(Window)XCreateWindow(display(),
                    (owner()==0)?server()->root():owner()->window(),
                    MSRect::x(),MSRect::y(),MSRect::width(),MSRect::height(),
                    MSDefaultBorderWidth,
                    (int)CopyFromParent,InputOutput,CopyFromParent,
                    (unsigned long)(CWBackPixel|CWBorderPixel|CWBackingStore|CWEventMask),
                    (XSetWindowAttributes *)&attributes);  
  
  server()->widgetHashTable()->add(window(),this);
  childCreateNotify();
}

void MSWidgetCommon::title(const char *title_) 
{ 
  _title=title_;
  updateTitle(); 
}

void MSWidgetCommon::title(const MSStringVector& title_) 
{ 
  if (title()!=title_)
   {
     _title=title_;
     updateTitle(); 
   }
}

void MSWidgetCommon::titleForeground(const char *fg_)
{ titleForeground(server()->pixel(fg_)); }

void MSWidgetCommon::titleForeground(unsigned long fg_) 
{ 
  if (titleForeground()!=fg_)
   {
     _titleFg=fg_;
     updateTitle();
   }
}

void MSWidgetCommon::titleFont(const char *fid_) 
{ titleFont(server()->fontID(fid_)); }

void MSWidgetCommon::titleFont(Font fid_) 
{ 
  if (titleFont()!=fid_)
   {
     _titleFont=fid_;
     updateTitle();
   }
}

void MSWidgetCommon::titleAlignment(unsigned long alignment_)
{
  if (titleAlignment()!=alignment_)
   {
     _titleAlignment=alignment_;
     updateTitle();
   }
}

//#############################################################################################
// set and get methods to support the application builder

void MSWidgetCommon::set(MSAttrValueList& avList_)
{
  MSWidgetView::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="title")
       title(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="titleForeground")
       titleForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="titleFont")
       titleFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="titleAlignment")
       titleAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
   } 
  avList_.remove(index);
}

MSAttrValueList& MSWidgetCommon::get(MSAttrValueList& avList_)
{
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight");
  avList_<<MSAttrValue("title",MSAttrValue::stringVectorToString(title()),MSAttrValue::String);
  avList_<<MSAttrValue("titleForeground",server()->colorName(titleForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("titleFont",server()->fontName(titleFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("titleAlignment",MSAttrValue::alignmentToString(titleAlignment()),
                       alignmentVector, MSAttrValue::List);
  return MSWidgetView::get(avList_);
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSWidgetCommon::computeSize(void) {}
void MSWidgetCommon::updateTitle(void) {}





