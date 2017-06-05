///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCallback.H>
#include <MSGUI/MSWidget.H>
#include <MSGUI/MSWidgetResourceMapper.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSIHashKeySet.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSIHashKeySet<MSWidgetResourceMapper::MapperItem,MSString>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSIHashKeySet<MSWidgetResourceMapper::MapperItem,MSString>;
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSIHashKeySet<MSWidgetResourceMapper::MapperItem,MSString>)
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSIHashKeySet<MSWidgetResourceMapper::MapperItem,MSString>;
#endif

#endif

void MSWidgetResourceMapper::WidgetDestroyCallback::process(void)
{
  mapper().widgetDestroyed(this);
}

MSWidgetResourceMapper::MapperItem::MapperItem(const MSString &logicalValue_,
                                               const MSString &realValue_)
:_logicalValue(logicalValue_),_realValue(realValue_)
{}

void MSWidgetResourceMapper::MapperItem::doCallback(MSCallback *pCallback_)
{
  MSResourceChangeCallback *cb=(MSResourceChangeCallback *)pCallback_;
  cb->resourceChanged(realValue());
}

MSWidgetResourceMapper::MSWidgetResourceMapper(void)
{}

MSWidgetResourceMapper::~MSWidgetResourceMapper(void)
{
  ResourceSet::Cursor cursor(resourceSet());
  for (cursor.setToFirst();cursor.isValid();cursor.setToNext())
   {
     MapperItem &item=resourceSet().elementAt(cursor);
     MSUnsignedLongVector &cbList=item.destroyCBList();
     unsigned len=cbList.length();
     for (unsigned i=0;i<len;i++)
      {
        WidgetDestroyCallback *cb=(WidgetDestroyCallback *)cbList(i);
        cb->widget()->removeCallback(MSWidgetCallback::destroy,cb);
      }
   }
}

void MSWidgetResourceMapper::associate(const char *logicalValue_,const char *realValue_)
{
  ResourceSet::Cursor cursor(resourceSet());
  if (resourceSet().locateElementWithKey(logicalValue_,cursor))
   {
     MapperItem &item=resourceSet().elementAt(cursor);
     item.realValue(realValue_);
     item.activateCallback(MSSymbol(logicalValue_));
   }
  else
   {
     resourceSet().add(MapperItem(logicalValue_,realValue_));
   }
}

const char *MSWidgetResourceMapper::value(const char *logicalValue_) const
{
  ResourceSet::Cursor cursor(resourceSet());
  if (resourceSet().locateElementWithKey(logicalValue_,cursor))
   {
     const MapperItem &item=resourceSet().elementAt(cursor);
     return item.realValue();
   }
  else return logicalValue_;
}

void MSWidgetResourceMapper::addCallback(const char *logicalValue_,
                                         MSResourceChangeCallback *callback_)
{
  MapperItem *item=0;
  ResourceSet::Cursor cursor(resourceSet());
  if (resourceSet().locateElementWithKey(logicalValue_,cursor))
   {
     item=&resourceSet().elementAt(cursor);
   }
  else
   {
     resourceSet().add(MapperItem(logicalValue_,""));
     item=&resourceSet().elementWithKey(logicalValue_);
   }
  // First add the resource change callback on this item, using the pointer to
  // the callback object as the void * ID.
  item->addCallback(MSSymbol(logicalValue_),callback_,callback_);

  // Create a WidgetDestroyCallback object and add it as a destroy callback 
  // onto the widget.  Use the WidgetDestroyCallback object pointer as the
  // void * ID.
  if (callback_->widget()!=0)
   {
     WidgetDestroyCallback *cb=new WidgetDestroyCallback(*this,*item,
                                                         callback_->widget(),callback_);
     callback_->widget()->addCallback(MSWidgetCallback::destroy,cb,cb);

     // Add the pointer of the WidgetDestroyCallback into or destroy callback list
     item->destroyCBList()<<(unsigned long)cb;
   }
}

  
void MSWidgetResourceMapper::removeCallback(const char *logicalValue_,
                                            MSResourceChangeCallback *callback_)
{
  ResourceSet::Cursor cursor(resourceSet());
  if (resourceSet().locateElementWithKey(logicalValue_,cursor))
   {
     MapperItem &item=resourceSet().elementAt(cursor);
     item.removeCallback(MSSymbol(logicalValue_),callback_);
   }
}

void MSWidgetResourceMapper::widgetDestroyed(WidgetDestroyCallback *callback_)
{
  MapperItem &item=callback_->mapperItem();
  item.removeCallback(MSSymbol(item.logicalValue()),callback_->id());
  unsigned i;
  if ((i=item.destroyCBList().indexOf((unsigned long)callback_))!=item.destroyCBList().length())
   {
     item.destroyCBList().removeAt(i);
   }
}

MSStringVector MSWidgetResourceMapper::logicalValues(void) const
{
  MSStringVector values;
  ResourceSet::Cursor cursor(resourceSet());
  for (cursor.setToFirst();cursor.isValid();cursor.setToNext())
   {
     const MapperItem &item=resourceSet().elementAt(cursor);
     values<<item.logicalValue();
   }
  return values;
}
