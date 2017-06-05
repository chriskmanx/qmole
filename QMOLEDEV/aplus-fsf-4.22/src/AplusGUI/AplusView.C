///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMethodCallback.H>
#include <AplusGUI/AplusView.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/Macros.H>

extern long dbg_tmstk;
static const char *DefaultSelectedRowBackground="lightsteelblue3";

AplusView::AplusView(MSWidget *widget_) : MSList(widget_)
{
  AplusModel *am = new AplusModel(0);
  INTERNAL_COUPLE(am);
  
  _rowSpacing = 0;
  _rowHeight = textHeight();
  selectedRowBackground(server()->pixel(DefaultSelectedRowBackground));
  
  // Translating MSCallbacks to AplusCallbacks
  //
  callback(MSWidgetCallback::doubleclick,
	   new MSMethodCallback<AplusView>(this, &AplusView::referenceCB));
}


AplusView::~AplusView(void)
{
}


MSBoolean AplusView::verifyData(V,A a_)
{
  return (0!=a_&&QA(a_)&&a_->t==Ct&&a_->r==2)?MSTrue:MSFalse;
}


void AplusView::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel*)m_));
  updateData();
}


void AplusView::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusView"  << endl;
     AplusEvent *ave = (AplusEvent *) &event_;
     V v     = ((AplusModel *)model())->aplusVar();
     A index = ave->index();
     A pick  = ave->pick();
     I ravel = ave->ravel();;
     update(v,index, pick, ravel);
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusView"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


const char *AplusView::formatOutput(MSString& str_, unsigned row_)
{
  if (model()!=0)
   {
     P p; p.i=((AplusModel*)model())->data();
     int cl=((AplusModel*)model())->charLength();
     int offset=row_*cl;
     
     str_=MSString((const char *)(p.c+offset),cl);
   }
  return str_;
}


void AplusView::update(V,int row_,int,UpdateType type_)
{
  if (type_==ValueUpdate)
   {
     if (row_) redrawImmediately();
     else drawRow(row_);
   }
  else if (type_==ShapeUpdate||type_==AppendUpdate)
    {
      shapeUpdate();
      redrawImmediately();
    }
}


void AplusView::update(V v_,A index_,A,I ravel_)
{ 
  if(!index_)
    {
      shapeUpdate();
      redrawImmediately();
    }
  else if(ravel_) // ravel update
   {
     A a=(A)v_->a;
     if(a->r==2&&a->n==1) 
      { 
        int n=(int)a->d[1];
        int k=(int)index_->p[0];
        int j=k/n;
        update(v_,j,k-n*j,ValueUpdate); 
      }
     else redrawImmediately(); 
   }
  else 
   {
     A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
     A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
     int i,j;
     if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
      {     
        if(isNull(r)==MSTrue) redrawImmediately(); 
        else 
         {      
	   // *** need to supply a real index vector to appendUpdate() ***
	   //
           for(i=0;i<(int)r->n;i++) if ((int)r->p[i]>=vsb()->max()) appendUpdate(MSIndexVector::nullVector());
           for(i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
         }
      }
     else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
      { 
        for(i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
      }
     else 
      {
	// *** need to supply a real index vector to appendUpdate() ***
	//
	for(i=0;i<(int)r->n;i++) if ((int)r->p[i]>=vsb()->max()) appendUpdate(MSIndexVector::nullVector());
	for(j=0;j<(int)r->n;j++)
	 {
	   for(i=0;i<(int)c->n;i++)
	    {
	      update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
	    }
	 }
      }
   }
}


unsigned long AplusView::selectedRowForegroundColor(int row_)
{
  return rowForeground(row_);
}


void AplusView::drawSelectOutline(Window window_,int row_,MSBoolean select_,MSBoolean highlightedRow_)
{
  // override drawSelectOutline() to do nothing as we don't want the rows to have any outline
}

unsigned AplusView::numRows(void) const
{ return (model()==0)?0:((AplusModel*) model())->numRows(); }

unsigned AplusView::numColumns(void) const
{ return (model()==0)?0:((AplusModel*) model())->charLength(); }


unsigned AplusView::maxLength(void) const
{
  return numColumns();
}

unsigned AplusView::rowLength(unsigned) const
{
  return numColumns();
}

unsigned long AplusView::rowForeground(unsigned row_)
{
  AplusModel *m=(AplusModel *)model();
  unsigned long fgcolor=foreground();
  if(m)
  {
    V v = m->aplusVar();
    AColorFunction *fgFunc=AplusModel::getFgFunc(v);
    P p;
    p.i = m->data();
    
    if (fgFunc!=0&&row_<numRows()&&Ct==m->a_type())
    {
      // `view uses only char matrices, so other types not considered;
      // (plus we checked in if statement above cuz we are paranoid);
      int len=m->charLength();
      char *buf=new char[len+1];
      memcpy(buf,p.c+(row_*len),len);
      buf[len]= '\0';
      fgcolor=(unsigned long) fgFunc->invoke(v,(char *)buf,row_,0);   
      delete [] buf;
    }
  }
  return fgcolor;
}

unsigned long AplusView::rowBackground(unsigned row_)
{
  AplusModel *m=(AplusModel *)model();
  unsigned long bgcolor=background();
  if(m)
  {
    V v = m->aplusVar();
    AColorFunction *bgFunc=AplusModel::getBgFunc(v);
    P p;
    p.i = m->data();
    
    if (bgFunc!=0&&row_<numRows()&&Ct==m->a_type())
    {
      // `view uses only char matrices, so other types not considered;
      // (plus we checked in if statement above 'cause we are paranoid);
      int len=m->charLength();
      char *buf=new char[len+1];
      memcpy(buf,p.c+(row_*len),len);
      buf[len]= '\0';
      bgcolor=(unsigned long) bgFunc->invoke(v,(char *)buf,row_,0);   
      delete [] buf;
    }
  }
  return bgcolor;
}

void AplusView::referenceCB(void)
{
  activateCallback(MSWidgetCallback::reference);
}


const MSSymbol& AplusView::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusView::symbol(void)
{
  static MSSymbol sym("AplusView");
  return sym;
}
