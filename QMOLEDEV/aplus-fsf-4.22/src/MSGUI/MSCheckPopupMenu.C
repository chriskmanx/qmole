///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1999-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCheckMenuItem.H>
#include <MSGUI/MSCheckPopupMenu.H>

MSCheckPopupMenu::MSCheckPopupMenu(MSDisplayServer *server_) :
MSStringPopupMenu(server_)
{ init(); }

MSCheckPopupMenu::MSCheckPopupMenu(MSDisplayServer *server_,MSStringVector& aModel_) :
MSStringPopupMenu(server_)
{ 
  model(aModel_);
  init();
 }

MSCheckPopupMenu::~MSCheckPopupMenu(void)
{}

void MSCheckPopupMenu::init(void)
{
  _selectColor=server()->pixel("violet red");
}

void MSCheckPopupMenu::selectColor(const char *color_) 
{ selectColor(server()->pixel(color_)); }

void MSCheckPopupMenu::selectColor(unsigned long pixel_)
{ 
  if (_selectColor!=pixel_)
   { 
     _selectColor=pixel_; 
     MSWidgetVector itemVector(children());
     MSCheckMenuItem *pMenuItem;
     for (unsigned i=0, len=itemVector.length();i<len;i++)
       {
	 pMenuItem=(MSCheckMenuItem *)itemVector(i);
	 pMenuItem->selectColor(_selectColor);
       }
   }
}

void MSCheckPopupMenu::selection(const MSStringVector &selection_)
{
  _selection=selection_;
  resetSelection();
}

void MSCheckPopupMenu::resetSelection(void)
{
  MSWidgetVector itemVector(children());
  MSCheckMenuItem *pMenuItem;
  for (unsigned i=0, len=itemVector.length();i<len;i++)
    {
      pMenuItem=(MSCheckMenuItem *)itemVector(i);
      if (_selection.indexOf(pMenuItem->label())<_selection.length())
	{
	  pMenuItem->state(MSTrue);
	}
      else
	{
	  pMenuItem->state(MSFalse);
	}
    }
}

void MSCheckPopupMenu::updateData(void)
{
  if (MSView::model()!=0)
   {
     freeze();
     const MSStringVector& aStringVector=stringVector();
     unsigned currentCount(itemCount());
     unsigned i;
     MSWidgetVector itemVector(children());
     MSCheckMenuItem *pMenuItem;
     for (i=0;i<aStringVector.length();i++)
      {
	if (i<itemVector.length())
	 {
           pMenuItem=(MSCheckMenuItem *)itemVector(i);
	   pMenuItem->label(aStringVector(i));
	   pMenuItem->state(MSFalse);
	 }
	else 
	  {
	    pMenuItem=new MSCheckMenuItem(this,aStringVector(i),0,i);
	    pMenuItem->selectColor(_selectColor);
	  }
	setItem(pMenuItem,i);
      }
     for (i=aStringVector.length();i<itemVector.length();i++)
      {
	pMenuItem=(MSCheckMenuItem *)itemVector(i);
	delete pMenuItem;
      }
     unfreeze();
     computeSize();
   }
  else removeAllItems();
}


void MSCheckPopupMenu::receiveEvent(MSEvent& aEvent_)
{
  _selection.removeAll();
  MSStringPopupMenu::receiveEvent(aEvent_);
}

void MSCheckPopupMenu::activate(void)
{
  MSCheckMenuItem *item=(MSCheckMenuItem *)activeMenuItem();
  if (item->state()==MSTrue)
    {
      _selection.removeAt(_selection.indexOf(item->label()));
    }
  else
    {
      _selection<<item->label();
    }
  MSStringPopupMenu::activate();
}
