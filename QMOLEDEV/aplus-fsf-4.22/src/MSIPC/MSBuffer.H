#ifndef MSBufferHEADER
#define MSBufferHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <string.h>
#include <MSTypes/MSDefines.H>

class MSIPCExport MSBuffer
{
protected:
  char *_buffer;
  char *_minofbuffer;
  char *_get;
  char *_put;
  char *_maxofbuffer;

  void reserve(int);
  
public:
  inline MSBuffer(void);
  MSBuffer(int);
  inline ~MSBuffer(void);
  
  inline char *buffer(void);
  inline char *minofbuffer(void);
  inline char *get(void);
  inline char *put(void);
  inline char *maxofbuffer(void);

  inline void minofbuffer(char *c_);
  inline void get(char *c_);
  inline void put(char *c_);
  inline void maxofbuffer(char *c_);

  inline void clear(void);
  inline void reset(void);

  inline void put(char c_);
  inline void put(double d_);
  inline void put(int d_);
  inline void put(short d_);
  inline void put(long d_);
  inline void put(const char *s_);

  inline MSBuffer& operator>>(int& d_);
  inline MSBuffer& operator>>(char& d_);

  inline MSBuffer& operator<<(char d_);
  inline MSBuffer& operator<<(double d_);
  inline MSBuffer& operator<<(int d_);
  inline MSBuffer& operator<<(short d_);
  inline MSBuffer& operator<<(long d_);
  inline MSBuffer& operator<<(const char *d_);
  void stuff(const char *,int);
  static int buffToBuff(MSBuffer *,MSBuffer *,int);
  void zero(int);

  int read(int,int);
  int write(int,int);
};

inline MSBuffer::MSBuffer(void)
{ _buffer=_minofbuffer=_get=_put=_maxofbuffer=0; }
inline MSBuffer::~MSBuffer(void)
{ if (_minofbuffer!=0) delete [] _minofbuffer; }
inline char *MSBuffer::buffer(void)
{ return _buffer; }
inline char *MSBuffer::minofbuffer(void)
{ return _minofbuffer; }
inline char *MSBuffer::get(void)
{ return _get; }
inline char *MSBuffer::put(void)
{ return _put; }
inline char *MSBuffer::maxofbuffer(void)
{ return _maxofbuffer; }
inline void MSBuffer::minofbuffer(char *c_)
{ _minofbuffer=c_; }
inline void MSBuffer::get(char *c_)
{ _get=c_; }
inline void MSBuffer::put(char *c_)
{ _put=c_; }
inline void MSBuffer::maxofbuffer(char *c_)
{ _maxofbuffer=c_; }
inline void MSBuffer::clear(void)
{ if (_minofbuffer!=0) delete [] _minofbuffer; _minofbuffer=_get=_put=_maxofbuffer=0; }
inline void MSBuffer::reset(void)
{ _get=_put=_minofbuffer; }

inline void MSBuffer::put(char c_)        { if (_maxofbuffer-_put<1) reserve(1); *(_put++)=c_; }
inline void MSBuffer::put(double d_)      { stuff((char *)(&d_),sizeof(d_)); }
inline void MSBuffer::put(int d_)         { stuff((char *)(&d_),sizeof(d_)); }
inline void MSBuffer::put(short d_)       { stuff((char *)(&d_),sizeof(d_)); }
inline void MSBuffer::put(long d_)        { stuff((char *)(&d_),sizeof(d_)); }
inline void MSBuffer::put(const char *s_) { if (s_!=0) stuff(s_,strlen(s_)); }

inline MSBuffer& MSBuffer::operator>>(int& d_)  { d_=(_get!=_put)?(0xff & *_get++):-1; return *this; }
inline MSBuffer& MSBuffer::operator>>(char& d_) { d_=(_get!=_put)?(0xff & *_get++):-1; return *this; }
inline MSBuffer& MSBuffer::operator<<(char d_)        { put(d_); return *this; }
inline MSBuffer& MSBuffer::operator<<(double d_)      { put(d_); return *this; }
inline MSBuffer& MSBuffer::operator<<(int d_)         { put(d_); return *this; }
inline MSBuffer& MSBuffer::operator<<(short d_)       { put(d_); return *this; }
inline MSBuffer& MSBuffer::operator<<(long d_)        { put(d_); return *this; }
inline MSBuffer& MSBuffer::operator<<(const char *d_) { put(d_); return *this; }

#endif






