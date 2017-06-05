///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSSeparator.H>

static const int MSSeparatorDefaultHighlightThickness=0;
static const int MSSeparatorDefaultShadowThickness=0;
static const int MSSeparatorEventMask=ExposureMask;
static const int MSSeparatorDefaultThickness=1;

MSSeparator::MSSeparator(MSWidget *owner_):
MSWidgetCommon(owner_)
{ init(); }

void MSSeparator::init(void)
{
  _highlightThickness=MSSeparatorDefaultHighlightThickness;
  _shadowThickness=MSSeparatorDefaultShadowThickness;
  _thickness=MSSeparatorDefaultThickness;
  acceptFocus(MSFalse);
  dynamic(MSTrue);
  selectInput(MSSeparatorEventMask);
}

void MSSeparator::marginWidth(int marginWidth_)
{
  if (_marginWidth!=marginWidth_)
   {
     _marginWidth=marginWidth_;
     computeSize();
   }
}

void MSSeparator::marginHeight(int marginHeight_)
{
  if (_marginHeight!=marginHeight_)
   {
     _marginHeight=marginHeight_;
     computeSize();
   }
}

void MSSeparator::thickness(int thickness_)
{
  if (_thickness!=thickness_)
   {
     _thickness=thickness_;
     computeSize();
   }
}

void MSSeparator::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_);
  redraw();
}

void MSSeparator::naturalSize(void)
{
  resize((marginWidth()+shadowThickness()+highlightThickness()+thickness())*2,
	 (marginHeight()+shadowThickness()+highlightThickness()+thickness())*2);
}

MSAttrValueList& MSSeparator::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("marginWidth", MSString(marginWidth()));
  avList_<<MSAttrValue("marginHeight", MSString(marginHeight()));
  avList_<<MSAttrValue("thickness", MSString(thickness()));
  return MSWidgetCommon::get(avList_);
}

void MSSeparator::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="marginWidth")
      {
        marginWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="marginHeight")
      {
        marginHeight(avList_[i].value().asInt());
	index<<i;
      }
     if (avList_[i].attribute()=="thickness")
      {
        thickness(avList_[i].value().asInt());
	index<<i;
      }
   }
  avList_.remove(index);
}
