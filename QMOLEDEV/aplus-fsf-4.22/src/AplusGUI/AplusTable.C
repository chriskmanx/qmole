///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSMethodCallback.H>
#include <MSTypes/MSUtil.H>
#include <MSGUI/MSFontObject.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusTableColumn.H>
#include <AplusGUI/AplusCommon.H>

extern ::A convertToPixels(const MSWidgetCommon *, ::A);
extern long dbg_tmstk;

AplusTable::AplusTable(MSWidget *widget_) : MSTable(widget_)
{
  if (dbg_tmstk) cout << "Creating AplusTable" << endl;
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
  columnResize(MSFalse);
  columnDragDrop(MSFalse);
  //
  // Translating MSCallbacks to AplusCallbacks.
  //
  callback(MSWidgetCallback::doubleclick, new MSMethodCallback<AplusTable>(this, &AplusTable::referenceCB));      
}

AplusTable::~AplusTable(void)
{}

void AplusTable::receiveEvent(MSEvent &event_)
{
  const MSSymbol& eventType=event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusTable"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = ((AplusModel *)model())->aplusVar();
      ::A index = ave->index();
      ::A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusTable"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateDataEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusTable"  << endl;
      updateData();
    }
  else if (eventType==AplusProtectEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received an AplusProtectEvent in AplusTable" << endl;
      V v = ((AplusModel *)model())->aplusVar();
      AVariableData *varData = ::pAVarDataFromV(v);
      if (varData)
	{
	  readOnly(varData->readOnly());
	}
    }
}

MSBoolean AplusTable::verifyData(V v_,::A a_)
{
  MSBoolean r=MSFalse;
  if (a_->t==Et) 
   {
     r=MSTrue;
     ::A as;
     V sv;
     ::A *p=(::A *)a_->p;
     
     for (int i=0;i<a_->n&&r==MSTrue;i++)
      {
	if (QS(p[i]))
	 {
  	   sv=(V) getVFromSym(v_->cx,(S)XS((::A)p[i]));
	   as=(::A)sv->a;
	   r=verifyColumn(as);
	 }
	else r=MSFalse;
      }
   }
  return r;
}

MSBoolean AplusTable::verifyColumn(::A a_)
{
  if (a_!=0&&!QS(a_))
   {
     if (a_->t==Et) return MSTrue;
     else if (a_->r==1&&a_->t!=Ct) return MSTrue;
     else if (a_->t==Ct&&a_->r>=1&&a_->r<=2) return MSTrue;
   }
  return MSFalse;
}

void AplusTable::addSenderNotify(MSEventSender *m_)
{
  if (dbg_tmstk) cout << "AplusTable::addSenderNotify" << endl;
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusTable::updateData(void) 
{
  V var = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  MSBoolean wasFrozen=frozen();
  if (editor()->mapped()==MSTrue) unmapEditor();

  if (var)
   {
     if (MSFalse==wasFrozen)freeze();
     ::A a=(::A)var->a;
     int nVars=(int)a->n;
     if (nVars==0) variables(0,0);
     else
      {  
    	::A *p=(::A *)a->p;
    	V *v=new V[nVars];
    	for (int i=0;i<nVars;i++) v[i]=(V)getVFromSym(var->cx,(S)XS((::A)p[i]));
    	variables(v,nVars);
    	delete [] v;
      }

     calculateRowHeight();
     calculateHeadingsHeight();
     adjustNumVisible();
     if(MSFalse==wasFrozen)unfreeze();
     redrawImmediately();
  }
}

void AplusTable::update(V,int r_,int c_,UpdateType t_)
{ 
  if (r_==-1&&c_==-1&&t_==ValueUpdate) redrawImmediately(); 
  else if (t_==ShapeUpdate||t_==AppendUpdate) updateData();
}

void AplusTable::update(V v_,::A index_,::A,I ravel_)
{ 
  if(index_==(::A)MP(22)) update(v_,-1,-1,AppendUpdate);
  else if(!index_) update(v_,-1,-1,ShapeUpdate);
  else if(ravel_) // ravel update
   {
     ::A a=(::A)v_->a;
     if(a->r==2&&a->n==1) 
      { 
        int n=(int)a->d[1];
        int k=(int)index_->p[0];
        int j=k/n;
        update(v_,j,k-n*j,ValueUpdate); 
      }
     else update(v_,-1,-1,ValueUpdate);
   }
  else 
   {
     ::A r=index_->t==It?index_:index_->n?(::A)*index_->p:aplus_nl;
     ::A c=index_->t==Et&&index_->n>1?(::A)index_->p[1]:aplus_nl;
     if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
      {     
        if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
        else 
         {      
           for(int i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
         }
      }
     else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
      { 
        for(int i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
      }
     else 
      {
	for(int j=0;j<(int)r->n;j++)
	 {
	   for(int i=0;i<(int)c->n;i++)
	    {
	      update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
	    }
	 }
      }
   }
}


void AplusTable::variables(V *v_,int numVars_) 
{
  int i,j;
  int nc=numColumns();
  AplusTableColumn *var;
  if (numVars_>0)
    {
      AplusTableColumn **old_columns = 0;
      AplusTableColumn **vars=new AplusTableColumn*[numVars_];
      MSBoolean found;
      
      if (nc > 0)  // Copy all existing columns to old_columns
	{
	  old_columns=new AplusTableColumn*[nc];
	  for(i=0; i < nc; i++)
	    old_columns[i] = (AplusTableColumn *)tableColumn(i);
	}
     
      for (i=0;i<numVars_;i++) vars[i]=0;
      for (i=0;i<nc;i++)
	{
	  var=old_columns[i];
	  found=MSFalse;
	  for (j=0;j<numVars_;j++) 
	    { 
	      if (var->model()&&((AplusModel*)var->model())->aplusVar()==v_[j]) 
		{
		  vars[j]=var;
		  found=MSTrue;
		  break;
		}
	    }
	  removeChild(var);
	  if (found==MSFalse)
	    {
	      // Removing object association from the V of the column
	      AplusModel *pOldModel = (AplusModel*)var->model();
	      if (pOldModel && pOldModel->aplusVar() && pOldModel->pAVarData())
		{
		  pOldModel->pAVarData()->pWidgetView(0);
		  pOldModel->aplusVar()->o = 0;
		  safeDestroy(var);
		}
	    }
	
	}
      for (i=0;i<numVars_;i++) 
	{
	  if (vars[i]==0) 
	    {
	      vars[i] = new AplusTableColumn(this);
	      AplusModel *apm=new AplusModel(v_[i]);  // Setting the model for AplusTableColumn
	      apm->coupleWidgetView(vars[i]);
	    }
	  else // insertChild doesn't set the column value, but removeChild sets it to zero.  Bug?
	    {
	      vars[i]->column(i);
	    }
	  insertChild(vars[i]);
	}
      delete [] vars;
      if (old_columns) delete [] old_columns;
    }
  else 
    {
      for (i=0;i<nc;i++) 
	{
	  var=(AplusTableColumn*)tableColumn(i);
	  removeChild(var);
	  safeDestroy(var);
	}
    }
}

Font AplusTable::getVFont(V v_)
{
  Font fid;
  AVariableData *varData;
  varData = ::pAVarDataFromV(v_);     
  if (varData && varData->pWidgetView()) fid=varData->pWidgetView()->font();
  else fid=server()->defaultFont();
  return (server()->defaultFont()==fid)?font():fid;
}

void AplusTable::updateBackground(unsigned long oldbg_)
{ 
  MSArrayView::updateBackground(oldbg_);
  unsigned i,n=numColumns();
  for (i=0;i<n;i++) ((AplusTableColumn *)tableColumn(i))->setBg(background());
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--)
  {
     MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
     ((AplusTableColumn*)tc)->setBg(background());
  }
}

void AplusTable::updateFont(Font oldfont_)
{ 
  MSCompositeText::updateFont(oldfont_);
  freeze();
  unsigned i,n=numColumns();
  for (i=0;i<n;i++) ((AplusTableColumn *)tableColumn(i))->setFont(font());
  n=hiddenColumnList()->count();
  for (i=n-1;i<n;i--)
  {
     MSTableColumn *tc=(MSTableColumn *)hiddenColumnList()->array(i);
     ((AplusTableColumn*)tc)->setFont(font());
  }
  unfreeze();
}

void AplusTable::updateTitle(void)
{
  Font fid = titleFont();
  int n = numColumns();
  for (int i = 0; i < n; i++)
    {
      AplusModel *pModel = (AplusModel *)tableColumn(i)->model();
      if (pModel)
	{
	  AVariableData *vd = ::pAVarDataFromV(pModel->aplusVar());
	  if (isNull(vd->titleAFg())==MSTrue)
	    {
	      tableColumn(i)->headingForeground(titleForeground());
	    }
	  if (isNull(vd->titleAFont())==MSTrue)
	    {
	      tableColumn(i)->headingFont(fid);
	    }
	}
    }

  MSTable::updateTitle();
}

void AplusTable::updateForeground(unsigned long oldfg_)
{
  MSCompositeText::updateForeground(oldfg_);
  redrawImmediately();
}

// Cellforeground will attempt to get the color from the table column.
// if the table column doesn't have a colorfunc, it will try to evaluate
// it's own colorfunc.

unsigned long AplusTable::cellForeground(unsigned row_,unsigned column_) 
{
  AplusTableColumn *pTableColumn = (AplusTableColumn *) tableColumn(column_);
  if (pTableColumn==0) 
  {
     if (foregroundColors().length()==0) return foreground();
     else return foregroundColors()(row_%foregroundColors().length());
  }
  if (pTableColumn->model()!=0)
   {
     V v = ((AplusModel *)pTableColumn->model())->aplusVar();
     AVariableData *pAVarData = ::pAVarDataFromV(v);
     // If a color was set before or if there's a color function
     if ((pAVarData && isNull(pAVarData->fgA())==MSFalse)  
	 || AplusModel::getFgFunc(v))
       return pTableColumn->cellForeground(row_);
     else
      {
	v = ((AplusModel*)model())->aplusVar();
	::A a = ((AplusModel*)model())->a();
	AColorFunction *fgFunc = AplusModel::getFgFunc(v);
	return (fgFunc!=0)?fgFunc->invoke(v,a):foreground();
      }
   }
  return server()->defaultForeground();
}

// When functional background comes around, edit this function

unsigned long AplusTable::cellBackground(unsigned row_, unsigned column_)
{
  return MSTable::cellBackground(row_,column_);
}


// Cyclecolors with Table is quite complicated.  
//
// If the TableColumn has a cycle color function, it is used
// else, if it has a colors vector, it is used
// else, if the Table has a cycle color function, it is used
// else, if the Table has a colors vector, it is used
// else do not cycle
//
void AplusTable::createCycle(int row_,int column_)
{
  if (column_<0)
    {
      return;
    }

  AplusModel *pModel=(AplusModel *)model();
  if (pModel==0)
    {
      return;
    }

  V v = pModel->aplusVar();
  if (v==0)
    {
      return;
    }

  if (pModel->numElmts()>0)
    {
      ACycleFunction *tableCycleFunc=AplusModel::getCycleFunc(v);
      AplusTableColumn *pColumn = (AplusTableColumn *)tableColumn(column_);

      if (pColumn!=0)
	{
	  V columnV = (pColumn->model()!=0)?((AplusModel*)pColumn->model())->aplusVar():0;
	  if (columnV==0)
	    {
	      return;
	    }

	  ACycleFunction *columnCycleFunc = AplusModel::getCycleFunc(columnV);

	  if (columnCycleFunc!=0 && columnCycleFunc->func()!=0)
	    {
	      pColumn->cycleColors(pColumn->getCycleColors(row_));	// use the column's cycle colors function
	    }
	  else if (pColumn->cycleColors().length()==0 && tableCycleFunc!=0 && tableCycleFunc->func()!=0)
	    {
	      cycleColors(getCycleColors(row_, column_)); 	// use the table's cycle colors function
	    }

	  MSTable::createCycle(row_,column_);
	}
    }
}


MSUnsignedLongVector AplusTable::getCycleColors(int row_, int col_) const
{
  char *buf;
  ::A r=aplus_nl;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  ACycleFunction *cycleFunc = AplusModel::getCycleFunc(v);
  
  if (cycleFunc && cycleFunc->func()!=0&&(row_<numRows()||row_==0))
   {
     P p; p.i=((AplusModel *)model())->data();
     int rank = ((AplusModel *)model())->rank();
     int type = ((AplusModel *)model())->a_type();
     int w=rank==2?numColumns():1;
     int offset=row_*w+col_;
     int n=0;
     
     switch(type)
      {
	case It:
	  r=cycleFunc->invoke(v,(int)p.i[offset],row_,col_);
	  break;
	case Ft:
	  r=cycleFunc->invoke(v,(double)p.f[offset],row_,col_);
	  break;
	case Ct:
	  n=((AplusModel*)model())->charLength();
	  offset=row_*n;
          buf=new char[n+1];
	  strncpy(buf,p.c+(offset),n);
	  buf[n]= '\0';
	  r=cycleFunc->invoke(v,(char *)buf,row_,col_);   
          delete [] buf;
	  break;
	case Et:
	  if (((AplusModel*)model())->numElmts()>0)
           {
             ::A d=gs(Et);
	     *d->p=ic(p.a[offset]);
             r=cycleFunc->invoke(v,d,row_,col_);   
             dc(d);
	   }
	  break;
      }
   }
  ::A pixelColors = (isNull(r)==MSFalse) ? convertToPixels(this,r) : r;
  MSUnsignedLongVector uv;

  if (isNull(pixelColors)==MSFalse)
   {
     for (unsigned i = 0; i < pixelColors->n; i++) 
       uv << (unsigned long) pixelColors->p[i];
     dc(pixelColors);
   }
  return uv;
}


void AplusTable::shuffleColumns(MSIndexVector &aIndexVector_)
{
  extern int safeAset(V,::A,::A,::A);
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  if (v)
  {
    ::A old = ((AplusModel*)model())->a();
    ::A r = aplus_nl;
    long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    d[0] = aIndexVector_.length();
    r = ga(Et, 1, aIndexVector_.length(), d);
    for (int i = 0; i < aIndexVector_.length(); i++)
      r->p[i] = (long) ic((::A) old->p[aIndexVector_(i)]);
    
    if (safeAset(v, r, 0, 0)==0)
     {
       showError(qs);
     }
    else
     {
       activateCallback(MSWidgetCallback::permutecolumns);
     }
  }
}

const MSSymbol& AplusTable::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusTable::symbol(void)
{
  static MSSymbol sym("AplusTable");
  return sym;
}


void AplusTable::referenceCB(void)
{
  activateCallback(MSWidgetCallback::reference);
}
