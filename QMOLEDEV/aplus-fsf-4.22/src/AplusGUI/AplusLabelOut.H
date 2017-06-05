#ifndef __AplusLabelOut_H
#define __AplusLabelOut_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLabelOut.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusFormatter.H>

///////////////////////////////////////////////////////////////////////////////
// LabelOut Architecture                                                     //
//                                                                           //
//                           +------------+                                  //
//                           | MSLabelOut |                                  //
//                           +------------+                                  //
//                                  |                                        //
//                                  |                                        //
//                           +-------------+                                 //
//                           |AplusLabelOut|                                 //
//                           +-------------+                                 //
//                     /            |          \                             //
//                   /              |             \                          //
//                 /                |                \                       //
//               /                  |                   \                    //
// +-------------------+  +-------------------+  +-------------------+       //
// | AplusFuncLabelOut |  |AplusFormatLabelOut|  |  AplusFuncLabel   |       //
// +-------------------+  +-------------------+  +-------------------+       //
//                                                                           //
//                                                                           //
//  AplusFuncLabelOut is used when:                                          //
//     `g has (`xlabelout; func)                                             //
//                                                                           //
//  AplusFormatLabelOut is used when:                                        //
//     `g has (`xlabelout; (`float; 5))                                      //
//                                                                           //
//  AplusFuncLabel is used when:                                             //
//     `g has (`xlabel; (1 2 3; ("A"; "B"; "C"); .5 ; .2))                   //
//                                                                           //
//  The data all reside in AplusLabelOut.  The subclass constructors has to  //
//  set the appropriate data members in AplusLabelOut for it to work         //
//  correctly.                                                               //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

class AplusLabelOut : public MSLabelOut
{
private:
  AOutFunction                *_outFunc;
  int                          _precision;
  AplusFormatter::OutputFormat _format;
  A			       _a;
  P			       _p;
  A			       _tick;
  A			       _grid;
  A			       _value;
  V                            _v;

public:
  AplusLabelOut(void);
  ~AplusLabelOut(void);
  
  // Accessors
  AOutFunction                *outFunc(void) { return _outFunc; }
  int                          precision(void) { return _precision; }
  AplusFormatter::OutputFormat format(void) { return _format; }
  A			       a(void) { return _a; }
  P			       p(void) { return _p; }
  A			       tick(void) { return _tick; }
  A			       grid(void) { return _grid; }
  A			       value(void) { return _value; }
  V                            v(void) { return _v; }

  // Mutators
  void outFunc(AOutFunction *outFunc_)              { _outFunc = outFunc_; }
  void precision(int precision_)                    { _precision = precision_;}
  void format(AplusFormatter::OutputFormat format_) { _format = format_; }
  void a(A a_)                                      { _a = a_; }
  void p(P p_)                                      { _p = p_; }
  void tick(A tick_)                                { _tick = tick_; }
  void grid(A grid_)                                { _grid = grid_; }
  void value(A value_)                              { _value = value_; }
  void v(V v_)                                      { _v = v_; }

  virtual const char *label(MSString &buf_, unsigned i_) const { return MSLabelOut::label(buf_,i_);}
  virtual unsigned tickPositionLength(void) const;
  virtual unsigned gridWidthLength(void) const;
  virtual unsigned tickSizeLength(void) const;
  virtual double tickPosition(unsigned) const;
  virtual double tickSize(unsigned) const;
  virtual unsigned gridWidth(unsigned) const;
  virtual const char *formatOutput(MSString&, double);
};

class AplusFuncLabelOut : public AplusLabelOut
{
public:
  AplusFuncLabelOut(AOutFunction *, V, AplusLabelOut *old_ = 0);
  ~AplusFuncLabelOut(void);
};

class AplusFormatLabelOut : public AplusLabelOut
{
public:
  AplusFormatLabelOut(AplusFormatter::OutputFormat,
		      int p_=-1,AplusLabelOut *old_=0);
  ~AplusFormatLabelOut(void);
};

class AplusFuncLabel : public AplusLabelOut
{
public:
  AplusFuncLabel(A, AplusLabelOut *old_=0);
  ~AplusFuncLabel(void);

  void label(A);
  virtual const char *label(MSString& buf_, unsigned i_)const{return AplusLabelOut::label(buf_,i_);}
  MSBoolean verify(A);
};

#endif
