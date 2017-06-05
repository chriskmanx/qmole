///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSTable.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSTableColumnGroup.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSGenericVector.C>
#include <MSTypes/MSManagedPointer.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSGenericVector<MSTableColumnGroup::Node>
#pragma instantiate MSGenericVector<const MSTableColumnGroup *>
#pragma instantiate MSGenericVector<MSManagedPointer<MSTableColumnGroup> >
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSGenericVector<MSTableColumnGroup::Node>)
#pragma define(MSGenericVector<const MSTableColumnGroup *>)
#pragma define(MSGenericVector<MSManagedPointer<MSTableColumnGroup> >)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSGenericVector<MSTableColumnGroup::Node>;
template class MSGenericVector<const MSTableColumnGroup *>;
template class MSGenericVector<MSManagedPointer<MSTableColumnGroup> >;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template  MSGenericVector<MSTableColumnGroup::Node>;
template  MSGenericVector<const MSTableColumnGroup *>;
template  MSGenericVector<MSManagedPointer<MSTableColumnGroup> >;
#endif

#endif //MSTK_MANUAL_INSTANTIATION


ostream &operator<<(ostream &os_,MSManagedPointer<MSTableColumnGroup> const &group_)
{
  os_<<*group_;
  return os_;
}

ostream &operator<<(ostream &os_,MSTableColumnGroup::Node &node_)
{
  if (node_.type()==MSTableColumnGroup::Node::Column) os_<<"Column Name: "<<node_.column()->heading()<<endl;
  else if (node_.type()==MSTableColumnGroup::Node::Group) os_<<*(node_.group())<<endl;
  else os_<<"Undefined Node Type"<<endl;
  return os_;
}

ostream &operator<<(ostream &os_,MSTableColumnGroup const &group_)
{
  group_.print(os_,0);
  return os_;
}

ostream &operator<<(ostream &os_,const MSTableColumnGroup * const &group_)
{
  os_<<group_<<endl;
  return os_;
}

MSTableColumnGroup::Node::Node(MSTableColumnGroup &tableColumnGroup_)
:_type(Group),_column(0)
{
  _group=new MSTableColumnGroup(tableColumnGroup_);
}

MSTableColumnGroup::Node::Node(MSTableColumn *tableColumn_)
:_type(Column),_column(tableColumn_),_group(0)
{}

MSTableColumnGroup::Node::Node(void)
:_type(Undefined),_column(0),_group(0)
{}

MSTableColumnGroup::Node::Node(const Node &node_)
:_type(node_._type),_column(node_._column),_group(0)
{
  if (node_._group!=0) _group=new MSTableColumnGroup(*(node_._group));
}

MSTableColumnGroup::Node &MSTableColumnGroup::Node::operator=(const Node &node_)
{
  if (&node_!=this)
   {
     if (_group!=0) delete _group;
     _type=node_._type;
     _column=node_._column;
     if (node_._group!=0) _group=new MSTableColumnGroup(*(node_._group));
   }
  return *this;
}

MSTableColumnGroup::Node::~Node(void)
{
  if (_group!=0) delete _group;
}

MSTableColumnGroup::MSTableColumnGroup(MSTable *table_,const MSStringVector &heading_,const MSSymbol &tag_)
:_table(table_),
 _heading(heading_),
 _tag(tag_),
 _font(0),
 _foreground(0)
{
  init();
}

MSTableColumnGroup::MSTableColumnGroup(MSTable *table_,const char *heading_,const MSSymbol &tag_)
:_table(table_),
 _heading(heading_),
 _tag(tag_),
 _font(0),
 _foreground(0)
{
  init();
}

void MSTableColumnGroup::init(void)
{
  if (table()!=0)
   {
     _font=table()->font();
     _foreground=table()->foreground();
   }
}

MSTableColumnGroup::MSTableColumnGroup(const MSTableColumnGroup &tableColumnGroup_)
:_table(tableColumnGroup_._table),
 _tag(tableColumnGroup_._tag),
 _heading(tableColumnGroup_._heading),
 _font(tableColumnGroup_._font),
 _foreground(tableColumnGroup_._foreground),
 _nodeList(tableColumnGroup_._nodeList)
{}

MSTableColumnGroup::~MSTableColumnGroup(void)
{}
  
MSTableColumnGroup &MSTableColumnGroup::operator=(const MSTableColumnGroup &tableColumnGroup_)
{
  if (&tableColumnGroup_!=this)
   {
     return deepCopy(tableColumnGroup_);
   }
  return *this;
}

MSTableColumnGroup &MSTableColumnGroup::operator<<(MSTableColumn *tableColumn_)
{
  if (isOkToAdd(tableColumn_)==MSTrue) _nodeList<<Node(tableColumn_);
  return *this;
}

MSTableColumnGroup &MSTableColumnGroup::operator<<(MSTableColumnGroup &tableColumnGroup_)
{
  if (isOkToAdd(tableColumnGroup_)==MSTrue) _nodeList<<Node(tableColumnGroup_);
  return *this;
}

MSBoolean MSTableColumnGroup::isOkToAdd(MSTableColumnGroup &group_)
{
  unsigned len=group_.nodeList().length();
  for (unsigned i=0;i<len;i++)
   {
     Node &node=group_.nodeList()[i];
     if (node.type()==Node::Column)
      {
        if (isOkToAdd(node.column())==MSFalse) return MSFalse;
      }
     else if (node.type()==Node::Group)
      {
        if (isOkToAdd(*node.group())==MSFalse) return MSFalse;
      }
   }
  return MSTrue;
}

MSBoolean MSTableColumnGroup::isOkToAdd(MSTableColumn *column_)
{
  if (column_!=0)
   {
     if (column_->table()==table())
      {
        unsigned len=nodeList().length();
        for (unsigned i=0;i<len;i++)
         {
           Node &node=nodeList()[i];
           if (node.type()==Node::Column)
            {
              if (column_==node.column())
               {
                  MSMessageLog::warningMessage("Warning: MSTableColumnGroup - Column already exists, append fails");
                 return MSFalse;
               }
            }
           else if (node.type()==Node::Group)
            {
              if (node.group()->isOkToAdd(column_)==MSFalse) return MSFalse;
            }
         }
        return MSTrue;
      }
     else
      {
        MSMessageLog::warningMessage("Warning: MSTableColumnGroup - Try to append column from a different table, append fails");
        return MSFalse;
        
      }
   }
  else
   {
      MSMessageLog::warningMessage("Warning: MSTableColumnGroup - Null column pointer, append fails");
     return MSFalse;
   }
}

void MSTableColumnGroup::tag(const MSSymbol &tag_)
{ _tag=tag_; }

void MSTableColumnGroup::heading(const MSStringVector &heading_)
{ _heading=heading_;}

void MSTableColumnGroup::font(Font font_)
{ _font=font_; }

void MSTableColumnGroup::font(const char *font_)
{
  if (table()!=0)
   {
     font(table()->server()->fontID(font_));
   }
}

void MSTableColumnGroup::foreground(unsigned long foreground_)
{ _foreground=foreground_; }

void MSTableColumnGroup::foreground(const char *foreground_)
{
  if (table()!=0)
   {
     foreground(table()->server()->pixel(foreground_));
   }
}

MSTableColumnGroup &MSTableColumnGroup::deepCopy(const MSTableColumnGroup &tableColumnGroup_)
{
  if (&tableColumnGroup_!=this)
   {
     _table=tableColumnGroup_._table;
     _tag=tableColumnGroup_._tag;
     _heading=tableColumnGroup_._heading;
     _font=tableColumnGroup_._font;
     _foreground=tableColumnGroup_._foreground;
     _nodeList=tableColumnGroup_._nodeList;
   }
  return *this;
}

MSTableColumnGroup &MSTableColumnGroup::shallowCopy(const MSTableColumnGroup &tableColumnGroup_)
{
  if (&tableColumnGroup_!=this&&table()==tableColumnGroup_._table)
   {
     _tag=tableColumnGroup_.tag();
     _heading=tableColumnGroup_.heading();
     _font=tableColumnGroup_.font();
     _foreground=tableColumnGroup_.foreground();
   }
  return *this;
}

void MSTableColumnGroup::allNodesDo(Iterator &iterator_)
{
  ColumnGroupList groupList;
  depthFirstNodeIteration(iterator_,groupList);
}

MSBoolean MSTableColumnGroup::depthFirstNodeIteration(Iterator &iterator_,
                                                      ColumnGroupList &groupList_)
{
  groupList_.append(this);
  unsigned len=nodeList().length();
  for (unsigned i=0;i<len;i++)
   {
     Node &node=nodeList()[i];
     if (node.type()==Node::Column)
      {
        if (iterator_.applyTo(node.column(),groupList_)==MSFalse) return MSFalse;
      }
     else if (node.type()==Node::Group)
      {
        if (node.group()->depthFirstNodeIteration(iterator_,groupList_)==MSFalse) return MSFalse;
      }
   }
  MSBoolean ret=iterator_.applyTo(*this,groupList_);
  groupList_.removeAt(groupList_.length()-1);
  return ret;
}

void MSTableColumnGroup::allNodesDo(ConstIterator &iterator_) const
{
  ColumnGroupList groupList;
  depthFirstNodeIteration(iterator_,groupList);
}

MSBoolean MSTableColumnGroup::depthFirstNodeIteration(ConstIterator &iterator_,
                                                      ColumnGroupList &groupList_) const
{
  groupList_<<this;
  unsigned len=nodeList().length();
  for (unsigned i=0;i<len;i++)
   {
     const Node &node=nodeList()[i];
     if (node.type()==Node::Column)
      {
        if (iterator_.applyTo(node.column(),groupList_)==MSFalse) return MSFalse;
      }
     else if (node.type()==Node::Group)
      {
        if (node.group()->depthFirstNodeIteration(iterator_,groupList_)==MSFalse) return MSFalse;
      }
   }
  MSBoolean ret=iterator_.applyTo(*this,groupList_);
  groupList_.removeAt(groupList_.length()-1);
  return ret;
}

MSBoolean MSTableColumnGroup::shallowCompare(const MSTableColumnGroup &group_) const
{
  if (&group_!=this)
   {
     if ((_table==group_._table)&&
         (_tag==group_._tag)&&
         (_heading==group_._heading)&&
         (_font==group_._font)&&
         (_foreground==group_._foreground))
      {
        return MSTrue;
      }
     else return MSFalse;
   }
  return MSTrue;
}

MSBoolean MSTableColumnGroup::deepCompare(const MSTableColumnGroup &group_) const
{
  if (&group_!=this)
   {
     if ((_table==group_._table)&&
         (_tag==group_._tag)&&
         (_heading==group_._heading)&&
         (_font==group_._font)&&
         (_foreground==group_._foreground)&&
         (_nodeList==group_._nodeList))
      {
        return MSTrue;
      }
     else return MSFalse;
   }
  return MSTrue;
}

MSBoolean operator==(const MSTableColumnGroup &group1_,const MSTableColumnGroup &group2_)
{
  return group1_.deepCompare(group2_);
}


void MSTableColumnGroup::print(ostream &os_,unsigned level_) const
{
  unsigned i;
  for (i=0;i<level_;i++) os_<<'\t';
  os_<<"(GROUP) ";
  if (heading().length()==0) os_<<endl;
  else os_<<heading();
  for (unsigned j=0;j<nodeList().length();j++)
   {
     const Node &node=nodeList()[j];
     if (node.type()==Node::Group) node.group()->print(os_,level_+1);
     else if (node.type()==Node::Column)
      {
        for (i=0;i<=level_;i++) os_<<'\t';
        os_<<node.column()->heading();
      }
   }
}
