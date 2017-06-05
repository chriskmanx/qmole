#ifndef AplusTreeViewHEADER
#define AplusTreeViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusEvent.H>
#include <MSTypes/MSStack.H>
#include <MSGUI/MSTreeView.H>

class AplusTreeItem
{
private:
  S _aSymbol;

public:
  AplusTreeItem(void) : _aSymbol(0) {}
  AplusTreeItem(const AplusTreeItem& aTreeItem_) : _aSymbol(aTreeItem_._aSymbol) {}
  AplusTreeItem(S aSymbol_) : _aSymbol(aSymbol_) {}
  ~AplusTreeItem(void) {_aSymbol=0;}

  AplusTreeItem& operator=(const AplusTreeItem& aTreeItem_)
  {if (&aTreeItem_!=this) _aSymbol=aTreeItem_._aSymbol; return *this;}

  MSString name(void) const
  {
    if (_aSymbol!=0) return MSString((const char *)_aSymbol->n);
    return MSString();
  }

  S symbol(void) const    {return _aSymbol;}
  void symbol(S aSymbol_) {_aSymbol=aSymbol_;}

  MSError::ErrorStatus set(const char *);
  const char *format(MSString&) const;
};


class AplusTreeView : public MSTreeView<AplusTreeItem>
{
public:
  AplusTreeView(MSWidget *);
  ~AplusTreeView(void);

  virtual void receiveEvent(MSEvent &);
  virtual void addSenderNotify(MSEventSender *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  virtual void update(V v_,int r_,int c_,UpdateType type_);
  virtual void update(V v_,A index_,A pick_,I ravel_);
  virtual void update(const MSIndexVector&);
    
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  virtual MSBoolean verifyData(V,A);
  MSBoolean isTree(A);

  inline AplusModel *model(void) const;

  A selectedNodeA(void) const;
  void selectedNodeA(A);

protected:
  MSStack _updateStack;
  TreeModel _modelTree;

  MSStack& updateStack(void) {return _updateStack;}

  void rebuildTree(TreeModelCursor&,A attr_,A val_);
  void buildTree(TreeModelCursor&,A attr_,A val_);
  void updateTree(TreeModelCursor&,A attr_,A val_,A pick_,int index_);

  virtual TreeModel& modelTree(void); 
  virtual const TreeModel& modelTree(void) const;

  virtual MSBoolean validate(TreeModelCursor &,const char *);
  virtual void nodeAttribute(const TreeModelCursor&, NodeAttribute&);

  virtual MSBoolean isNodeProtected(const TreeNode *);

  static MSBoolean checkValues(A);

  void referenceCB(void);

  STANDARD_WIDGET_METHODS_H
};

inline AplusModel *AplusTreeView::model(void) const { return (AplusModel *)_model; }

#endif // AplusTreeViewHEADER

