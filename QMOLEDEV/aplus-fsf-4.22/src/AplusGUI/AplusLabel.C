///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusLabel.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

extern long dbg_tmstk;

AplusLabel::AplusLabel(MSWidget *widget_) : MSLabel(widget_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusLabel::~AplusLabel(void) 
{}

int AplusLabel::numRows(void) const
{
  if (_model == 0) return 0;
  
  A a = ((AplusModel *)_model)->a();
  int type = ((AplusModel *)_model)->a_type();
  int rank = ((AplusModel *)_model)->rank();
  int n = ((AplusModel *)_model)->numElmts();
  
  if (a!=0)
   {
     if (type==Et) return (int)n;
     else if (type==Ct) return (rank<=1)?1:(int)a->d[0];
   }
  return 0;
}


int AplusLabel::numColumns(int row_) const
{
  A a = ((AplusModel *)_model)->a();
  int type = ((AplusModel *)_model)->a_type();
  int rank = ((AplusModel *)_model)->rank();
  int n = ((AplusModel *)_model)->numElmts();
  I *data =  ((AplusModel *)_model)->data();

  if (a!=0&&row_<numRows())  
   {
     if (type==Et)
      {
        P p; p.i=data;
	A ra=p.a[row_];
	return (ra->r==0)?1:(int)ra->d[0];
      }
     else if (type==Ct)
      {
        if (rank==0) return 1;
        else if (rank==1) return (int)a->d[0];
	else return (int)a->d[1];
      }
   }
  return 0;
}

int AplusLabel::numColumns(void) const
{
  if (_model == 0) return 0;

  A a = ((AplusModel *)_model)->a();
  int type = ((AplusModel *)_model)->a_type();
  int rank = ((AplusModel *)_model)->rank();
  int n = ((AplusModel *)_model)->numElmts();
  I *data =  ((AplusModel *)_model)->data();

  if (a!=0)  
   {
     if (type==Et)
      {
        P p; p.i=data;
	int r=0;
        for (int i=0;i<n;i++)
	 {
           if (p.a[i]->r==0) r=(1>r)?1:r;
           else r=(p.a[i]->d[0]>r)?(int)p.a[i]->d[0]:r;
	 }
        return r;
      }
     else if (type==Ct)
      {
        if (rank==0) return 1;
        else if (rank==1) return (int)a->d[0];
	else return (int)a->d[1];
      }
   }
  return 0;
}

const char *AplusLabel::formatOutput(MSString& str_, int row_)
{
  if (_model == 0) return 0;

  A a = ((AplusModel *)_model)->a();
  int type = ((AplusModel *)_model)->a_type();
  int rank = ((AplusModel *)_model)->rank();
  int n = ((AplusModel *)_model)->numElmts();
  I *data =  ((AplusModel *)_model)->data();

  if (a!=0&&row_<numRows())
   {
     P p; p.i=data;
     int len=numColumns(row_);
     switch (type)
      {
	case Ct:
	  if (rank<=1) str_ = MSString ((char *) p.c,len);
          else if (rank==2) str_ = MSString (((char *) p.c + (row_*len)), len);
	  break;
	case Et:
	  str_ = MSString((char *) p.a[row_]->p, len);
	  break;
	default:
	  break;
      }
   }
  return str_;
}


void AplusLabel::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusLabel"  << endl;
     MSLabel::updateData();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusLabel"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}

static MSBoolean checkNull(A a_) { return (QA(a_)&&a_->t==Et&&a_->n==0)?MSTrue:MSFalse; }

MSBoolean AplusLabel::verifyData(V,A a_)
{
  if (a_!=0&&QA(a_))
   {
     if (a_->t==Et)
      {
	P p; p.i=a_->p;
	for (int i=0;i<a_->n;i++)
	 {
           if (checkNull(p.a[i])==MSFalse)
	    {
  	      if (!QA(p.a[i])||p.a[i]->r>1||p.a[i]->t!=Ct) return MSFalse;
	    }
	 }
        return MSTrue;
      }
     else if (a_->t==Ct&&a_->r<=2) return MSTrue;
   }
  return MSFalse;
}

void AplusLabel::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}


// drawRow() is overriden in order to support functional foreground
//
void AplusLabel::drawRow(int row_)
{
  MSLabel::drawRow(row_);
}


void AplusLabel::drawRow(int row_,int column_,const char *pString_,int len_)
{
  XSetForeground(display(),textGC(),rowForeground(row_));
  MSLabel::drawRow(row_,column_,pString_,len_);
}


unsigned long AplusLabel::rowForeground(int row_) const
{
  unsigned long fg=foreground();
  AplusModel *m=(AplusModel *)model();
  if (m==0)
    {
      return fg;
    }

  V v=m->aplusVar();
  if (v==0)
    {
      return fg;
    }

  AColorFunction *colorFunc=AplusModel::getFgFunc(v);
  if (colorFunc!=0 && row_<numRows())
    {
      A a=(A)v->a;
      P p; p.i=a->p;

      switch(a->t)
	{
	case Ct:
	  {
	    int len = numColumns(row_);
	    int start = (a->r<2) ? 0 : row_*len;
	    fg=(unsigned long)colorFunc->invoke(v,(char *)MSString(p.c+start,len).string(),row_,0);   
	    break;
	  }
	case Et:
	  {
	    A d=gs(Et);
	    *d->p=ic(p.a[row_]);
	    fg=(unsigned long)colorFunc->invoke(v,d,row_,0);   
	    dc(d);
	    break;
	  }

	default:
	  break;
	}
    }

  return fg;
}


const MSSymbol& AplusLabel::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusLabel::symbol(void)
{
  static MSSymbol sym("AplusLabel");
  return sym;
}
