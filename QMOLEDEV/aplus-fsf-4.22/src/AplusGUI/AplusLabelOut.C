///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusLabelOut.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/AplusGraph.H>

AplusLabelOut::AplusLabelOut(void) : MSLabelOut(), _outFunc(0), _v(0), 
    _precision(-1), _format(AplusFormatter::BadFormat), _a(0),
    _tick(0), _grid(0), _value(0)
{}

AplusLabelOut::~AplusLabelOut(void)
{ if (a()!=0) dc(a()); }


const char *AplusLabelOut::formatOutput(MSString& buf_, double data_)
{
  if (_outFunc != 0)
    {
      A outStr=aplus_nl;
      if (_outFunc->func()!=0)
	{
	  if ((outStr=(A)_outFunc->invoke(v(),data_,0,0))!=0)
	    {
	      if (isNull(outStr)==MSFalse && outStr->t==Ct)
		{
		  buf_ = (char *) outStr->p;
		}
	      else
		{
		  buf_ = data_;
		}
	    }
	}
      else
	{
	  buf_ = data_;
	}

      return buf_;
    }
  else if (format() != AplusFormatter::BadFormat)
    {
      buf_ = AplusGraph::outFmt()->formatOutput(format(), data_, precision(), MSFalse);
      return buf_;
    }
  else
    {
      return MSLabelOut::formatOutput(buf_,data_);
    }
}

unsigned AplusLabelOut::tickSizeLength(void) const
{ return (_tick!=0)?_tick->n:0; }
unsigned AplusLabelOut::gridWidthLength(void) const
{ return (_grid!=0)?_grid->n:0; }
unsigned AplusLabelOut::tickPositionLength(void) const
{ return (_value!=0)?_value->n:0; }
  
double AplusLabelOut::tickSize(unsigned index_) const
{
  P p; int i;
  if (_tick!=0)
   {
     i=(int)(index_<_tick->n?index_:index_%_tick->n); p.i=_tick->p;
   }
  return _tick!=0?((double)p.f[i]):1.;
}

unsigned AplusLabelOut::gridWidth(unsigned index_) const
{
  P p; int i;
  if (_grid!=0) 
   {
     i=(int)(index_<_grid->n?index_:index_%_grid->n); p.i=_grid->p;
   }
  return _grid!=0?p.i[i]:0;
}

double AplusLabelOut::tickPosition(unsigned index_) const
{
  P p;
  if (_value!=0&&index_<=_value->n) p.i=_value->p;
  return _value!=0?(_value->t==Ft?(double)p.f[index_]:(double)p.i[index_]):0;
}

///////////////////////////////////////////////////////////////////////////////


AplusFuncLabelOut::AplusFuncLabelOut(AOutFunction *outFunc_, V v_, 
				     AplusLabelOut *alo_) : AplusLabelOut()
{
  outFunc(outFunc_);
  v(v_);
  if (alo_!=0 && alo_->a()!=0)
   {
     a((A)ic(alo_->a()));
     tick(alo_->tick());
     grid(alo_->grid());
     value(alo_->value());
   }
}

AplusFuncLabelOut::~AplusFuncLabelOut(void)
{}

///////////////////////////////////////////////////////////////////////////////

AplusFormatLabelOut::AplusFormatLabelOut(AplusFormatter::OutputFormat format_,
					 int precision_, AplusLabelOut *alo_)
  : AplusLabelOut()
{
  format(format_);
  precision(precision_);
  if (alo_!=0 && alo_->a()!=0)
   {
     a((A)ic(alo_->a()));
     tick(alo_->tick());
     grid(alo_->grid());
     value(alo_->value());
   }
}

AplusFormatLabelOut::~AplusFormatLabelOut(void)
{}

///////////////////////////////////////////////////////////////////////////////

AplusFuncLabel::AplusFuncLabel(A a_, AplusLabelOut *alo_) : AplusLabelOut()
{
  if (alo_!=0 && alo_->outFunc()!=0)
   {
     outFunc(alo_->outFunc());
     v(alo_->v());
   }

  if (alo_!=0 && alo_->format()!=AplusFormatter::BadFormat)
   {
     format(alo_->format());
     precision(alo_->precision());
   }

  if (verify(a_)==MSTrue)
   {
     a((A) ic(a_));
   }
  else
   {
     MSStringVector emptyStringVector;
     a((A)0);
     tick((A)0);
     grid((A)0);
     value((A)0);
     labels(emptyStringVector);
   }
}

AplusFuncLabel::~AplusFuncLabel(void)
{}

MSBoolean AplusFuncLabel::verify(A a_)
{
  MSBoolean ticks, r=MSFalse;
  int 	  i,j;
  
  if (a_!=0&&a_->n!=0&&QA(a_)&&(a_->t==Ft||a_->t==It||a_->t==Et))
  {
    P p; p.i=a_->p;
    if (a_->t==Et&&QA(p.a[0])&&(p.a[0]->t==Ft||p.a[0]->t==It))
    {
      r=MSTrue;
      value(p.a[0]);
      for (i=1; i<a_->n; i++)
      {
	if (isNull(p.a[i])==MSFalse)
	  {
	    switch(p.a[i]->t)
	      {
	      case Ct:
	      case Et:
		label((A)p.a[i]);
		break;

	      case Ft:
		ticks=MSFalse;
		P pt; pt.i=p.a[i]->p;
		for (j=0; j<p.a[i]->n; j++)
		  {
		    if (pt.f[j]<1) ticks=MSTrue;
		  }
		if (ticks==MSTrue) tick((A)p.a[i]);
		else grid((A)p.a[i]);
		break;

	      case It:
		grid((A)p.a[i]);
		break;

	      default:
		break;
	      }
	  }
      }
    }
    else if (a_->t==Ft||a_->t==It) 
    {
      value(a_);
      r=MSTrue;
    }
  }
  return r;
}

void AplusFuncLabel::label(A a_) {labels(AplusConvert::asMSStringVector(a_));}
