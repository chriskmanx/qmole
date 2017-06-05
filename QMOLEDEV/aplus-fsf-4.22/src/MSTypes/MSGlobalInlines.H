#ifndef MSGlobalInlinesHEADER
#define MSGlobalInlinesHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


/*************************************************************************************/
// Global template inlines. Used inside MStk. 
/*************************************************************************************/

#include <MSTypes/MSString.H>
#include <MSTypes/MSStringTest.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

/*************************************************************************************/
// msClassName - default implementation calls class member function: className()
/*************************************************************************************/

template<class Type>
inline MSString msClassName(const Type& aType) 
{ return aType.className();          }

inline MSString msClassName(double ) 
{ return MSString("double");         }
inline MSString msClassName(int ) 
{ return MSString("int");            } 
inline MSString msClassName(long ) 
{ return MSString("long");           }
inline MSString msClassName(short ) 
{ return MSString("short");          }
inline MSString msClassName(char ) 
{ return MSString("char");           }
inline MSString msClassName(unsigned ) 
{ return MSString("unsigned");       }
inline MSString msClassName(unsigned long ) 
{ return MSString("unsigned long");  }
inline MSString msClassName(unsigned short ) 
{ return MSString("unsigned short"); }
inline MSString msClassName(unsigned char ) 
{ return MSString("unsigned char");  }


/*************************************************************************************/
// msAsString - default implementation calls class member function: asString()
/*************************************************************************************/
template <class Type>
inline MSString msAsString (const Type & value_)
{ return value_.asString();                 }

inline MSString msAsString (const char & value_)
{ return MSString ((char)value_);           }
inline MSString msAsString (const double & value_)
{ return MSString ((double)value_);         }
inline MSString msAsString (const float & value_)
{ return MSString ((double)value_);         }
inline MSString msAsString (const int & value_)
{ return MSString ((int)value_);            }
inline MSString msAsString (const long & value_)
{ return MSString ((long)value_);           }
inline MSString msAsString (const short & value_)
{ return MSString ((short)value_);          }
inline MSString msAsString (const unsigned char & value_)
{ return MSString ((unsigned char)value_);  }
inline MSString msAsString (const unsigned long & value_)
{ return MSString ((unsigned long)value_);  }
inline MSString msAsString (const unsigned short & value_)
{ return MSString ((unsigned short)value_); }
inline MSString msAsString (const unsigned int & value_)
{ return MSString ((unsigned int)value_);   }

/*************************************************************************************/
// msAsMSF- default implementation class class member function
/*************************************************************************************/
template <class Type>
inline MSString msAsMSF (const Type & value_)
{ return value_.asMSF();                 }
inline MSString msAsMSF (const char & value_)
{ return MSString ((char)value_);           }
inline MSString msAsMSF (const double & value_)
{ return MSString ((double)value_);         }
inline MSString msAsMSF (const float & value_)
{ return MSString ((double)value_);         }
inline MSString msAsMSF (const int & value_)
{ return MSString ((int)value_);            }
inline MSString msAsMSF (const long & value_)
{ return MSString ((long)value_);           }
inline MSString msAsMSF (const short & value_)
{ return MSString ((short)value_);          }
inline MSString msAsMSF (const unsigned char & value_)
{ return MSString ((unsigned char)value_);  }
inline MSString msAsMSF (const unsigned long & value_)
{ return MSString ((unsigned long)value_);  }
inline MSString msAsMSF (const unsigned short & value_)
{ return MSString ((unsigned short)value_); }
inline MSString msAsMSF (const unsigned int & value_)
{ return MSString ((unsigned int)value_);   }

/*************************************************************************************/
// msSetFromString- default implementation class class member function
/*************************************************************************************/
template <class Type>
inline MSError::ErrorStatus msSetFromString (Type & value_, const char * pString_)
{
  value_.set (pString_);
  return MSError::MSSuccess;
}
// builtin types
inline MSError::ErrorStatus msSetFromString (char & value_, const char *pString_)
{
  value_ = pString_[0];
  return MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (double & value_, const char *pString_)
{
  char *cp = 0;
  value_ = strtod (pString_, &cp);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (int & value_, const char *pString_)
{
  char *cp = 0;
  value_ = (int) strtol (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (long & value_, const char *pString_)
{
  char *cp = 0;
  value_ = strtol (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (short & value_, const char *pString_)
{
  char *cp = 0;
  value_ = (short) strtol (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (unsigned char & value_, const char *pString_)
{
  value_ = pString_[0];
  return MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (unsigned long & value_, const char *pString_)
{
  char *cp = 0;
  value_ = strtoul (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (unsigned short & value_, const char *pString_)
{
  char *cp = 0;
  value_ = (unsigned short) strtoul (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}
inline MSError::ErrorStatus msSetFromString (unsigned int & value_, const char *pString_)
{
  char *cp = 0;
  value_ = (unsigned int) strtoul (pString_, &cp, 10);  // Base 10
  return (cp == pString_) ? MSError::MSFailure : MSError::MSSuccess;
}

/*************************************************************************************/
// msSetFromMSF- default implementation call class member function
/*************************************************************************************/
template <class Type>
inline MSError::ErrorStatus msSetFromMSF (Type & value_, const char * pString_)
{
  value_.setFromMSF (pString_);
  return MSError::MSSuccess;
}
// builtin types
inline MSError::ErrorStatus msSetFromMSF (char&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (short&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (int&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (double&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (long&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (unsigned char&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (unsigned short&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (unsigned int&, const char*)
{ return MSError::MSFailure; }
inline MSError::ErrorStatus msSetFromMSF (unsigned long&, const char*)
{ return MSError::MSFailure; }


/*************************************************************************************/
// msBadData - returns Type as default, specialized for builtin and MStk types
/*************************************************************************************/

template <class Type>
inline Type msBadData(const Type&)
{ return Type(); }
// builtin types
inline char msBadData(const char&)
{ return 0;  }
inline double msBadData(const double&)
{ return 0.0;}
inline int msBadData(const int&)
{ return 0;  }
inline long msBadData(const long&)
{ return 0;  }
inline short msBadData(const short&)
{ return 0;  }
inline unsigned int msBadData(const unsigned int&)
{ return 0;  }
inline unsigned long msBadData(const unsigned long&)
{ return 0;  }
inline unsigned short msBadData(const unsigned short&)
{ return 0;  }
inline unsigned char msBadData(const unsigned char&)
{ return 0;  }

/////////
// Global functions and specializations for vectors.
////////////

template <class T>
inline unsigned int msNumElements(T*, const MSString& str_, const char )
{
  return str_.numWords();
}

inline unsigned int msNumElements(MSString*,
                                  const MSString & str_, const char delimiter_)
{
  if (str_.length()==0) return 0;
  unsigned int num = str_.occurrencesOf (delimiter_);
  return (str_.last() == delimiter_) ? num : num+1;
}  


template <class T>
inline void msVectorSetFromString(T& t_, const MSString& str_, unsigned int& startPos_,
                                  const char delimiter_)
{
 if (startPos_<str_.length())
    {
      if (isspace(str_(startPos_)))
	{
	  startPos_ = str_.indexOfAnyBut(MSStringTest(APLUS_ISPACE),startPos_+1);
	  if (startPos_>=str_.length())
	    {
	      return;
	    }
	}

      unsigned int endPos = str_.indexOfAnyOf(MSStringTest(APLUS_ISPACE),startPos_);
      ::msSetFromString(t_,(const char *)str_.subString(startPos_,endPos-startPos_));
      startPos_ = str_.indexOfAnyBut(MSStringTest(APLUS_ISPACE),endPos);
    }
}


inline void msVectorSetFromString(MSString& dest_, 
                                  const MSString& str_, unsigned int& startPos_,
                                  const char delimiter_)
{
  unsigned int endPos=0;

  if (startPos_<str_.length())
    {
      endPos = str_.indexOf(delimiter_,startPos_);
      ::msSetFromString (dest_,(const char *)str_.subString(startPos_, endPos-startPos_));
      startPos_ = endPos+1;
    }
}




template <class T>
inline void msOutputItem(const T& data_, ostream & stream_)
{
  stream_ << data_<< " ";
}


inline void msOutputItem(const char& data_, ostream & stream_) 
{
  stream_ << (char)data_;
}


  
template <class T>
inline MSString msVectorAsString(T*,MSVectorImpl *pImpl_)
{
  return pImpl_->asString();
}

inline MSString msVectorAsString(char *, MSVectorImpl *pImpl_)
{
  return pImpl_->asString(0);
}

#endif
