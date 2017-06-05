#ifndef MSWidgetResourceMapperHEADER
#define MSWidgetResourceMapperHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCallbackBehavior.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSGUI/MSResourceChangeCallback.H>
#include <MSTypes/MSIHashKeySet.H>

// 4 situations that require fast lookup:
// a) when the real value is needed from a logical value.
// b) when the mapping between a pair of logical and real values is changed - need to update
//    the mapping and notify all widgets that registered for this change.
// c) when the mapper is destroyed - need to remove destroy callback from widgets.
// d) when a widget is destroyed - need to remove all resource-change callbacks registered
//    by this widget from our internal callback list.

class MSWidget;

class MSGUIExport MSWidgetResourceMapper 
{
public:
  class MSGUIExport MapperItem : public MSCallbackBehavior
  {
  private:
    MSString                _logicalValue;
    MSString                _realValue;
    MSUnsignedLongVector    _destroyCBList;
  protected:
    virtual void doCallback(MSCallback *);
  public:
    MapperItem() {}
    MapperItem(const MSString &logical_,const MSString &real_);

    MSUnsignedLongVector &destroyCBList(void) {return _destroyCBList;}
    const MSString &logicalValue(void) const {return _logicalValue;}
    const MSString &realValue(void) const {return _realValue;}
    void realValue(const MSString &realValue_) {_realValue=realValue_;}
  };
  class MSGUIExport WidgetDestroyCallback : public MSCallback
  {
  private:
    MSWidgetResourceMapper             &_mapper;
    MapperItem                         &_mapperItem;
    MSWidget   *_widget;
    void       *_id;
  public:
    WidgetDestroyCallback(MSWidgetResourceMapper &mapper_,MapperItem &mapperItem_,MSWidget *widget_,void *id_)
        :_mapper(mapper_),_mapperItem(mapperItem_),_widget(widget_),_id(id_) {}
    ~WidgetDestroyCallback(void) {}
    MSWidgetResourceMapper &mapper(void) {return _mapper;}
    MapperItem &mapperItem(void) {return _mapperItem;}
    MSWidget *widget(void) {return _widget;}
    void *id(void) {return _id;}
    virtual void process(void);
  };
  friend class WidgetDestroyCallback;

  inline friend MSGUIExport const MSString &key(const MapperItem &item_)
  { return item_.logicalValue(); }
  
  typedef MSIHashKeySet<MapperItem,MSString> ResourceSet;

protected:
  ResourceSet           _resourceSet;

  ResourceSet &resourceSet(void) {return _resourceSet;}
  const ResourceSet &resourceSet(void) const {return _resourceSet;}

  void widgetDestroyed(WidgetDestroyCallback *);

public:
  MSWidgetResourceMapper(void);
  ~MSWidgetResourceMapper(void);

  // Associate a logical value with a real value
  void associate(const char *logicalValue_,const char *realValue_);

  // Return the real value of the corresponding logical value.
  const char *value(const char *logicalValue_) const;

  // Return a list of logical values that are current registered.
  MSStringVector logicalValues(void) const;

  // Add and remove a resource change callback.
  void addCallback(const char *,MSResourceChangeCallback *);
  void removeCallback(const char *,MSResourceChangeCallback *);
};

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSIHashKeySet<MSWidgetResourceMapper::MapperItem,MSString>
#pragma do_not_instantiate MSIHashKeySetNode<MSWidgetResourceMapper::MapperItem>
#pragma do_not_instantiate MSIHashKeySetCursor<MSWidgetResourceMapper::MapperItem,MSString>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

DECLARE_ELEMENT_FOR_OPS_TYPE(MSWidgetResourceMapper::MapperItem)
#endif
