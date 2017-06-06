// This may look like C code, but it is really -*- C++ -*-
//
// Copyright Bob Friesenhahn, 1999
//
// Definition of Exception and derived classes MagickWarning*
// and MagickError*.  Derived from C++ STD 'exception' class for
// convenience.
//

#if !defined(Exception_header)
#define Exception_header

#include <string>
#include <exception>
#include <iostream>

namespace Magick
{
  class Exception : public std::exception
  {
    friend std::ostream& operator<<(std::ostream& stream_, const Exception& exception_);
  public:
    Exception( const std::string& what_ );
    virtual const char* what () const throw();
  private:
    std::string _what;
  };

  //
  // Warnings
  //

  class Warning : public Exception
  {
  public:
    Warning ( const std::string& what_ );
  };
  
  class WarningUndefined : public Warning
  {
  public:
    WarningUndefined ( const std::string& what_ );
  };
  
  class WarningResourceLimit : public Warning
  {
  public:
    WarningResourceLimit ( const std::string& what_ );
  };
  
  class WarningXServer : public Warning
  {
  public:
    WarningXServer ( const std::string& what_ );
  };
  
  class WarningOption : public Warning
  {
  public:
    WarningOption ( const std::string& what_ );
  };
  
  class WarningDelegate : public Warning
  {
  public:
    WarningDelegate ( const std::string& what_ );
  };
  
  class WarningMissingDelegate : public Warning
  {
  public:
    WarningMissingDelegate ( const std::string& what_ );
  };
  
  class WarningCorruptImage: public Warning
  {
  public:
    WarningCorruptImage ( const std::string& what_ );
  };
  
  class WarningFileOpen: public Warning
  {
  public:
    WarningFileOpen ( const std::string& what_ );
  };

  //
  // Error exceptions
  //

  class Error : public Exception
  {
  public:
    Error ( const std::string& what_ );
  };

  class ErrorUndefined : public Error
  {
  public:
    ErrorUndefined ( const std::string& what_ );
  };
  
  class ErrorResourceLimit : public Error
  {
  public:
    ErrorResourceLimit ( const std::string& what_ );
  };
  
  class ErrorXServer : public Error
  {
  public:
    ErrorXServer ( const std::string& what_ );
  };
  
  class ErrorOption : public Error
  {
  public:
    ErrorOption ( const std::string& what_ );
  };
  
  class ErrorDelegate : public Error
  {
  public:
    ErrorDelegate ( const std::string& what_ );
  };
  
  class ErrorMissingDelegate : public Error
  {
  public:
    ErrorMissingDelegate ( const std::string& what_ );
  };
  
  class ErrorCorruptImage: public Error
  {
  public:
    ErrorCorruptImage ( const std::string& what_ );
  };
  
  class ErrorFileOpen: public Error
  {
  public:
    ErrorFileOpen ( const std::string& what_ );
  };

} // namespace Magick
#endif
