#ifndef MSTableColumnGroupHEADER
#define MSTableColumnGroupHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSGenericVector.H>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSStringVector.H>
#include <X11/X.h>
class MSTable;
class MSTableColumn;
#if HAVE_IOSTREAM
#include <iostream>
#else
class ostream;
using namespace std;
#endif

class MSGUIExport MSTableColumnGroup
{
public:
  typedef MSGenericVector<const MSTableColumnGroup *> ColumnGroupList;
//  typedef MSGenericVector<MSTableColumnGroup *> ColumnGroupList;
  class MSGUIExport Iterator 
  {
  public:
    virtual MSBoolean applyTo(MSTableColumnGroup &,const ColumnGroupList &)=0;
    virtual MSBoolean applyTo(MSTableColumn *,const ColumnGroupList &)=0;
  };
  class MSGUIExport ConstIterator 
  {
  public:
    virtual MSBoolean applyTo(const MSTableColumnGroup &,const ColumnGroupList &)=0;
    virtual MSBoolean applyTo(const MSTableColumn *,const ColumnGroupList &)=0;
  };

public:
  MSTableColumnGroup(MSTable *,const MSStringVector &headings_,
                     const MSSymbol &tag_=MSSymbol::nullSymbol());
  MSTableColumnGroup(MSTable *,const char *headings_=0,
                     const MSSymbol &tag_=MSSymbol::nullSymbol());
  MSTableColumnGroup(const MSTableColumnGroup &);
  ~MSTableColumnGroup(void);
  
  MSTableColumnGroup &operator=(const MSTableColumnGroup &);
  MSTableColumnGroup &operator<<(MSTableColumn *);
  MSTableColumnGroup &operator<<(MSTableColumnGroup &);

  MSTable *table(void) const {return _table;}

  const MSSymbol &tag(void) const {return _tag;}
  void tag(const MSSymbol &);

  const MSStringVector &heading(void) const {return _heading;}
  void heading(const MSStringVector &);

  Font font(void) const {return _font;}
  void font(Font);
  void font(const char *);

  unsigned long foreground(void) const {return _foreground;}
  void foreground(unsigned long);
  void foreground(const char *);

  void allNodesDo(Iterator &);
  void allNodesDo(ConstIterator &) const;
  
  MSTableColumnGroup &deepCopy(const MSTableColumnGroup &);
  MSTableColumnGroup &shallowCopy(const MSTableColumnGroup &);
  MSBoolean shallowCompare(const MSTableColumnGroup &) const;
  MSBoolean deepCompare(const MSTableColumnGroup &) const;

  friend MSGUIExport MSBoolean operator==(const MSTableColumnGroup &,const MSTableColumnGroup &);

public:
  class MSGUIExport Node
  {
  public:
    enum NodeType {Undefined,Group,Column};

    Node(MSTableColumnGroup&);
    Node(MSTableColumn *);
    Node(const Node &);
    Node();
    Node &operator=(const Node &);
    ~Node();

    NodeType type(void) const {return _type;}

    const MSTableColumnGroup *group(void) const {return _group;}
    MSTableColumnGroup *group(void) {return _group;}

    const MSTableColumn *column(void) const {return _column;}
    MSTableColumn *column(void) {return _column;}
  
  private:
    MSTableColumnGroup *_group;
    MSTableColumn *_column;
    NodeType _type;
  };
  typedef MSGenericVector<Node> NodeList;
protected:
  MSTable       *_table;
  MSSymbol       _tag;
  MSStringVector _heading;
  Font           _font;
  unsigned long  _foreground;
  NodeList       _nodeList;

  void init(void);

  const NodeList &nodeList(void) const {return _nodeList;}
  NodeList &nodeList(void) {return _nodeList;}

  MSBoolean isEmpty(void) const {return MSBoolean(_nodeList.length()==0);}
  MSBoolean isOkToAdd(MSTableColumn *);
  MSBoolean isOkToAdd(MSTableColumnGroup &);

  MSBoolean depthFirstNodeIteration(Iterator &,ColumnGroupList &);
  MSBoolean depthFirstNodeIteration(ConstIterator &,ColumnGroupList &) const;

  void print(ostream &,unsigned) const;

  friend class MSTable;
  friend MSGUIExport ostream &operator<<(ostream &,MSTableColumnGroup const &);

  //Required for MSGenericVector template
  friend MSGUIExport ostream &operator<<(ostream &,MSTableColumnGroup::Node const &);
  friend MSGUIExport ostream &operator<<(ostream &,const MSTableColumnGroup * const &);
};

inline long compare(MSTableColumnGroup::Node const &,MSTableColumnGroup::Node const &)
{return 0;}

inline long compare(MSTableColumnGroup const &,MSTableColumnGroup const &)
{return 0;}


// SUNPRO complains when elementForOps is specified for const arguments.
// Provide specific function when const elements are used.
#if defined(MS_CONST_ELEMENT_FOR_OPS_BUG)
inline const MSTableColumnGroup* const& elementForOps (const MSTableColumnGroup* const & e) { return e; }
#endif

DECLARE_ELEMENT_FOR_OPS_TYPE(MSTableColumnGroup::Node)
DECLARE_ELEMENT_FOR_OPS_TYPE(const MSTableColumnGroup*)


// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSGenericVector<MSTableColumnGroup::Node>
#pragma do_not_instantiate MSGenericVectorOps<MSTableColumnGroup::Node>
#pragma do_not_instantiate MSGenericData<MSTableColumnGroup::Node>

#pragma do_not_instantiate MSGenericVector<const MSTableColumnGroup *>
#pragma do_not_instantiate MSGenericVectorOps<const MSTableColumnGroup *>
#pragma do_not_instantiate MSGenericData<const MSTableColumnGroup *>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
