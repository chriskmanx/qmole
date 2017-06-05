///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSDefines.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusModel.H>
#include <MSTypes/MSEventBlocker.H>
#include <AplusGUI/AplusTreeView.H>

#include <MSTypes/MSMethodCallback.H>
#include <MSTypes/MSTabularTree.C>
#include <MSTypes/MSTabularTreeCursor.C>
#include <MSTypes/MSObservableTree.C>
#include <MSTypes/MSObservableTreeEvent.C>
#include <MSGUI/MSTreeView.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTabularTree<AplusTreeItem>
#pragma instantiate MSTabularTreeCursor<AplusTreeItem>
#pragma instantiate MSObservableTree<AplusTreeItem>
#pragma instantiate MSObservableTreeEvent<AplusTreeItem>
#pragma instantiate MSTreeView<AplusTreeItem>
#endif // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTabularTree<AplusTreeItem>;
template class MSTabularTreeCursor<AplusTreeItem>;
template class MSObservableTree<AplusTreeItem>;
template class MSObservableTreeEvent<AplusTreeItem>;
template class MSTreeView<AplusTreeItem>;
#endif // MS_STD_TEMPLATE_INSTANTIATION

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSTabularTree<AplusTreeItem>)
#pragma define(MSTabularTreeCursor<AplusTreeItem>)
#pragma define(MSObservableTree<AplusTreeItem>)
#pragma define(MSObservableTreeEvent<AplusTreeItem>)
#pragma define(MSTreeView<AplusTreeItem>)
#endif // MS_XLC_TEMPLATE_INSTANTIATION

extern int safeAset(V,A,A,A);

static const int AplusTreeViewDefaultHighlightThickness=2;
extern long dbg_tmstk;

class Update
{
public:
  V _aplusVar;
  A _data;
  A _index;
  A _pick;
  I _ravel;
 
  Update(void) : _aplusVar(0),_data(0),_index(0),_pick(0),_ravel(0)
  {}
  Update(const Update& aUpdate_) :
    _aplusVar(aUpdate_._aplusVar),_data((A)ic(aUpdate_._data)),
    _index((A)ic(aUpdate_._index)),
    _pick((A)ic(aUpdate_._pick)),_ravel(aUpdate_._ravel)
  {}
  Update(V v_,A data_,A index_,A pick_,I ravel_)
  {
    _aplusVar=v_;
    _data=(data_!=0)?(A)ic(data_):data_;   
    _index=(index_!=0)?(A)ic(index_):index_;   
    _pick=(pick_!=0)?(A)ic(pick_):pick_;   
    _ravel=ravel_;   
  }
  ~Update(void)
  {
    if (_data!=0)  dc(_data);
    if (_index!=0) dc(_index);
    if (_pick!=0)  dc(_pick);  
  }

  Update& operator=(const Update& aUpdate_)
  {
    if (this!=&aUpdate_)
     {
       A d=_data,i=_index,p=_pick;
      _aplusVar=aUpdate_._aplusVar;
      _data=(A)ic(aUpdate_._data);
      _index=(A)ic(aUpdate_._index);
      _pick=(A)ic(aUpdate_._pick);
      _ravel=aUpdate_._ravel;
      if (d!=0) dc(d);
      if (i!=0) dc(i);
      if (p!=0) dc(p);  
     }
    return *this;
  }

  V aplusVar(void)     {return _aplusVar;}
  A data(void)  {return _data;}
  A index(void) {return _index;}
  A pick(void)  {return _pick;}
  I ravel(void) {return _ravel;}
};


MSError::ErrorStatus AplusTreeItem::set(const char *pString_)
{
  return MSError::MSFailure;
}


const char *AplusTreeItem::format(MSString& str_) const
{
  str_.removeAll();

  if (_aSymbol!=0)
    {
      str_=_aSymbol->n;
    }

  return str_.string();
}


AplusTreeView::AplusTreeView(MSWidget *pOwner_) : MSTreeView<AplusTreeItem>(pOwner_)
{
  _margin=0;
  readOnly(MSTrue);
  highlightThickness(AplusTreeViewDefaultHighlightThickness);
  shadowThickness(0);
  horizontalSpacing(15);
  verticalSpacing(5);
  showRootNode(MSFalse);
  //
  // _modelTree is a mirror image of the A+ tree data model.  It is used for the
  // interaction with MSTreeView widget.  This is the easiest way of making MSTreeView
  // work with A+ data models.  Because MSTreeView does not abstract its data model
  // representation (it always assumes it's MSObservableTree), unless we keep this
  // mirror observable tree, we would have to override most of MSTreeView to make them
  // accept the A+ slotfiller tree.
  //
  _modelTree.addAsRoot(AplusTreeItem());
  _modelTree.addReceiver(this);

  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
  //
  // Translating MSCallbacks to AplusCallbacks.
  //
  callback(MSWidgetCallback::doubleclick, new MSMethodCallback<AplusTreeView>(this, &AplusTreeView::referenceCB));      
}

AplusTreeView::~AplusTreeView(void)
{}

void AplusTreeView::addSenderNotify(MSEventSender *m_)
{
  MSModel *pModel=(MSModel *)m_;

  if (pModel->type()==AplusModel::symbol())
    {
      AplusModel *pAplusModel=(AplusModel *)pModel;
      INTERNAL_COUPLE(pAplusModel);
      update(pAplusModel->aplusVar(),0,0,0);
    }
}

void AplusTreeView::receiveEvent(MSEvent& event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) cout<<"Received UpdateEvent in AplusTreeView" <<endl;
     AplusEvent *ave=(AplusEvent *)&event_;
     V v    =((AplusModel *)model())->aplusVar();
     A index=ave->index();
     A pick =ave->pick();
     I ravel=ave->ravel();;
     update(v,index,pick,ravel);
   }
  else if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout<<"Received VerifyEvent in AplusTreeView" <<endl;

     AplusVerifyEvent *ave=(AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(),ave->a()));
   }
  else
    {
      MSTreeView<AplusTreeItem>::receiveEvent(event_);
    }
}


void AplusTreeView::rebuildTree(AplusTreeView::TreeModelCursor& aCursor_, A attr_, A val_)
{
  _modelTree.removeAll();
  _modelTree.addAsRoot(AplusTreeItem());
  aCursor_.setToRoot();
  buildTree(aCursor_,attr_,val_);  
}


void AplusTreeView::buildTree(AplusTreeView::TreeModelCursor& aCursor_, A attr_, A val_)
{
  if (attr_!=0&&val_!=0)
    {
      A value,attr;
      P p;
      for (int i=0;i<attr_->n;i++)
	{
	  attr=(A)attr_->p[i];  // used as the pick
	  value=(A)val_->p[i];
	  AplusTreeItem aTreeItem((S)XS(attr));
	  unsigned aPosition=i+1;

	  _modelTree.addAsChild(aCursor_,aPosition,aTreeItem);

	  if (isTree(value)==MSTrue)
	    {
	      p.i=value->p;
	      aCursor_.setToChild(aPosition);
	      buildTree(aCursor_,p.a[0],p.a[1]);
	      aCursor_.setToParent();
	    }
	}
    }
}


MSBoolean AplusTreeView::checkValues(A vals_)
{
  MSBoolean result=MSTrue;
  for (int i=0; result && i<vals_->n; ++i) 
    {
      A vali=(A)vals_->p[i];
      if (isSlotFiller(vali))
	{
	  result=checkValues((A)vali->p[1]);
	}
    }

  return result;
}


void AplusTreeView::updateTree(AplusTreeView::TreeModelCursor& aCursor_,
			       A attr_, A val_, A pick_, int index_)
{
  if (pick_->p[index_]==1)
   {
     if (pick_->n==index_)
       {
	 rebuildTree(aCursor_,attr_,val_);
       }
     else
       {
	 int item=(int)pick_->p[index_+1];
	 A attr=(A)attr_->p[item];
	 A val=(A)val_->p[item];
	 S aSymbol=XS(attr);
	 for (; aCursor_.isValid(); _modelTree.setToNext(aCursor_,MSPreorder))
	   {
	     if (aCursor_.element().symbol()==aSymbol)
	       {
		 break;
	       }
	   }

        P p; p.i=val->p;
	if (val->t!=Et || val->n<2 || checkValues(p.a[1])==MSFalse)
	  {
	    return;
	  }

	if (pick_->n==index_+2)
	  {
	    rebuildTree(aCursor_,p.a[0],p.a[1]);
	  }
	else
	  {
	    updateTree(aCursor_,p.a[0],p.a[1],pick_,index_+2);
	  }
      }
   }
  else
    {
      rebuildTree(aCursor_,attr_,val_);
    }
}


void AplusTreeView::update(const MSIndexVector& v_)
{ MSView::update(v_); }

void AplusTreeView::update(V,int r_,int c_,UpdateType type_)
{ if (r_==-1&&c_==-1&&type_==ValueUpdate) updateForeground(foreground()); }

extern "C" A gpix(A,A);

void AplusTreeView::update(V v_,A index_,A pick_,I ravel_)
{ 
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  if (v!=0)
    {
      P p; p.i=((AplusModel*)model())->data();
      A attr=p.a[0];
      A val=p.a[1];
      TreeModelCursor aTreeCursor(_modelTree);
      aTreeCursor.setToRoot();

      MSEventBlocker b(&_modelTree);
      rebuildTree(aTreeCursor,attr,val);
    }

  /***
  if (frozen()==MSFalse)
   {
     updateData();
//     if (pick_==0) updateData();     
//     else if (QA(pick_))
//      {
//	A pick=(A)gpix(pick_,(A)v_->a);
//	if (pick!=0&&QA(pick)&&pick->t==It&&pick->r<=1)
//	 {
//	   P p; p.i=((AplusModel*)model())->data();
//           Cursor aTreeCursor(elementTree());
//           aTreeCursor.setToRoot();
//	   updateTree(aTreeCursor,p.a[0],p.a[1],pick,0);
//	 }
//	else if (pick==0) cerr<<"tree: pick assignment error in update."<<endl;
//	else cerr<<"tree: pick assignment error in update."<<endl;
//	if (pick!=0) dc(pick);
//      }
   }
  else
   {
     Update *pUpdate=new Update(v_,aplus_nl,index_,pick_,ravel_);
     MSNodeItem *np=new MSNodeItem((void *)pUpdate);
     updateStack().push(np);
   }
  ***/
}

void AplusTreeView::updateFont(Font oldfid_)
{
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a=(model()!=0)?((AplusModel*)model())->a():0;
  if (v!=0) 
   {
     AFontFunction *fontFunc=AplusModel::getFontFunc(v);
     Font fid=(fontFunc!=0)?fontFunc->invoke(v,a):font();
     if (fid!=oldfid_) MSTreeView<AplusTreeItem>::updateFont(oldfid_);
   }
}

void AplusTreeView::updateForeground(unsigned long oldfg_)
{
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a=(model()!=0)?((AplusModel*)model())->a():0;
  if (v!=0)
   {
     AColorFunction *fgFunc=AplusModel::getFgFunc(v);
     unsigned long fg=(fgFunc!=0)?fgFunc->invoke(v,a):foreground();
     if (fg!=oldfg_)  MSTreeView<AplusTreeItem>::updateForeground(oldfg_);
   }
}

void AplusTreeView::updateBackground(unsigned long oldbg_)
{
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a=(model()!=0)?((AplusModel*)model())->a():0;
  if (v!=0)
   {
     AColorFunction *bgFunc=AplusModel::getBgFunc(v);
     unsigned long bg=(bgFunc!=0)?bgFunc->invoke(v,a):background();
     if (bg!=oldbg_)  MSTreeView<AplusTreeItem>::updateBackground(oldbg_);
   }
}

MSBoolean AplusTreeView::isTree(A a_)
{ return isSlotFiller(a_); }

MSBoolean AplusTreeView::verifyData(V,A a_)
{ return (isTree(a_)&&checkValues((A)a_->p[1]))?MSTrue:MSFalse; }

AplusTreeView::TreeModel& AplusTreeView::modelTree(void) { return _modelTree; }
const AplusTreeView::TreeModel& AplusTreeView::modelTree(void) const { return _modelTree; }


MSBoolean AplusTreeView::validate(AplusTreeView::TreeModelCursor &cursor_,const char *pString_)
{
  //
  // Interactive editing of the tree is not supported now.  See the comment
  // in isProtected() method.
  //
  return MSTreeView<AplusTreeItem>::validate(cursor_,pString_);
  /***
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;

  busyOn();
  int depth=1;
  TreeModelCursor aTreeCursor(cursor_);
  aTreeCursor.setToParent();
  while (_modelTree.isRoot(aTreeCursor)==MSFalse)
    {
      depth++;
      aTreeCursor.setToParent();
    }

  aTreeCursor=cursor_;
  
  A pick=gv(Et,depth);
  P ppick; ppick.i=pick->p;
  
  int i=depth,j;
  while (_modelTree.isRoot(aTreeCursor)==MSFalse)
    {
      S aSymbol=aTreeCursor.element().symbol();
      ppick.a[i-1]=(A)MS(aSymbol);
      aTreeCursor.setToParent();
      --i;        
    }
  
  A vals,syms,val;
  S psym,dsym;
  P p; p.i=((AplusModel*)model())->data();
  for (i=0;i<pick->n;i++)
    {
      syms=p.a[0];
      vals=p.a[1];
      psym=XS(pick->p[i]);
      for (j=0;j<syms->n;j++)
	{
	  dsym=XS(syms->p[j]);
	  if (dsym==psym) 
            { 
              val=(A)vals->p[j]; 
              break;
            }	
	}		
      p.i=val->p;
    }	   
  
  freeze();
  val=(A)ic(val);
  if (safeAset(v,(A)ic(val),0,pick)==0) showError(qs);
  else AplusModel::doneCB(v,val,0,pick);
  dc(val);
  unfreeze();
  // pull our update off the stack
  MSNodeItem *np=updateStack().pop();  
  Update *pUpdate;
  if (np!=0)
    {
      pUpdate=(Update *)np->data();
      delete np;
      delete pUpdate;
    }

  while ((np=updateStack().pop())!=0)
    {
      pUpdate=(Update *)np->data();
      delete np;
      update(pUpdate->aplusVar(),pUpdate->index(),pUpdate->pick(),pUpdate->ravel());
      delete pUpdate;
    }

  dc(pick);
  busyOff();

  return MSTrue;
  ***/
}


void AplusTreeView::nodeAttribute(const TreeModelCursor& cursor_, NodeAttribute& nodeAttr_)
{
  if (_modelTree.isLeaf(cursor_)==MSTrue)
    {
      nodeAttr_.expandable(MSFalse);
      nodeAttr_.expandedState(MSFalse);
    }
  else
    {
      nodeAttr_.expandable(MSTrue);
      nodeAttr_.expandedState(MSTrue);
    }
}


MSBoolean AplusTreeView::isNodeProtected(const TreeNode *)
{
  // For now, only support read-only mode, just like version 2.
  // If editing of the nodes is to be supported, the code below
  // needs to be uncommented and the validate() method needs to
  // be fixed.
  //
  return MSTrue;
  /***
  V v=model()->aplusVar();
  if (v!=0)
    {
      AVariableData *varData = ::pAVarDataFromV(v);  
      return varData->readOnly();
    }

  return MSFalse;
  ***/
}


void AplusTreeView::referenceCB(void)
{
  activateCallback(MSWidgetCallback::reference);
}


A AplusTreeView::selectedNodeA(void) const
{
  TreeModelCursor cursor(selectedNode());

  if (cursor.isValid()==MSFalse)
    {
      return aplus_nl;
    }

  MSUnsignedLongVector nodePath(1,(unsigned long)cursor.element().symbol());
  S sym;

  while (cursor.setToParent()==MSTrue)   // while the parent is valid - i.e., if we haven't reached the root
    {
      sym = cursor.element().symbol();
      //
      // since the root in an A+ tree is a special null node, we shouldn't include
      // it in the path
      //
      if (sym==0)
	{
	  break;
	}

      nodePath << (unsigned long)sym;
    }

  I pathLen=nodePath.length();
  if (pathLen==0)
    {
      return aplus_nl;
    }

  A svPath = gv(Et,pathLen);
  P p; p.i=svPath->p;

  for (I i=0; i<pathLen; ++i)
    {
      p.a[i]=(A)MS(nodePath(pathLen-i-1));
    }

  return svPath;
}


void AplusTreeView::selectedNodeA(A aNode_)
{
  if (isNull(aNode_))
    {
      selectedNode(TreeModelCursor());  // unselect a node
    }
  else	// aNode_ contains the path to a tree node
    {
      if (!QS(aNode_) && aNode_->t==Et && aNode_->n>0)	// if this is a symbol vector
	{
	  int pathLen=aNode_->n;
	  TreeModelCursor cursor(_modelTree);
	  cursor.setToRoot();

	  for (int i=0; i<pathLen; ++i)
	    {
	      // We have to do a couple of checks first:
	      //
	      // 1) if the cursor is invalid, it means that either the tree is empty (this is the
	      //    first iteration of the loop) or no matching nodes were found on the previous
	      //    iteration;
	      //
	      // 2) if the cursor has no children, it means that the given node path is longer
	      //    than what the tree has.
	      //
	      // In either case, the node could not be found in the tree.
	      //
	      if (cursor.isValid()==MSFalse || cursor.setToFirstExistingChild()==MSFalse)
		{
		  if (dbg_tmstk) showError("Node with the given name not found in the tree!",2);
		  return;
		}

	      if (!QS(aNode_->p[i]))   // make sure it's a symbol
		{
		  if (dbg_tmstk) showError("Incorrect format for the node path - it must be a symbol vector",2);
		  return;
		}

	      do
		{
		  if (cursor.element().symbol()==(S)XS(aNode_->p[i]))  // if we found the node
		    {
		      break;
		    }
		}
	      while (cursor.setToNextExistingChild()==MSTrue);  // iterate over all children
	    }

	  if (cursor.isValid()==MSTrue)
	    {
	      selectedNode(cursor);
	    }
	  else
	    {
	      if (dbg_tmstk) showError("Node with the given name not found in the tree!",2); // print a warning message
	      return;
	    }
	}
      else
	{
	  if (dbg_tmstk) showError("Incorrect format for the node path - it must be a symbol vector",2);
	  return;
	}
    }
}


const MSSymbol& AplusTreeView::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusTreeView::symbol(void)
{
  static MSSymbol sym("AplusTreeView");
  return sym;
}
