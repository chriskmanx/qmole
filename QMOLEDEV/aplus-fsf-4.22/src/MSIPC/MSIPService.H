#ifndef MSIPServiceHEADER 
#define MSIPServiceHEADER 

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSIPC/MSHostPort.H>

class MSIPCExport MSIPService
{
 public:
  enum CN_TYPE{CN_TYPE_ERR=0x1,CN_TYPE_HOSTPORT=0x2,CN_TYPE_REMPROGS=0x3};
  
   class MSIPCExport Remprog 
   {
        friend class MSIPService;
        char    *_rp_host;
        long    _rp_prognum;
        char    *_rp_protocol;
   };

 private:
   static int _countRemprogEntries;
   static int _countService;
   static int _currentService;
   MSHostPort _hostport;
   MSString _serviceName;
   MSString _protocol;
   char *_remprogBuffer;
   Remprog *_remprogEntry,*_remprogEntryPointer,*_remprogEntries;
   MSBoolean _isValid;
   MSBoolean _isReady;

 protected:
   int getIPService(void);
   int getRemprog(void);
   int nextIPService(void);
   MSBoolean getServByName(void);
   int getRemprogByName(void);
   int remprogParseValue(char *);
   MSBoolean lookupRemprogService(void);
   static void compress(register char *fromptr);
   CN_TYPE inp_parse(const char *serviceName_);


 public:
   MSIPService(const MSString &serviceName);
   MSIPService(void);
   ~MSIPService(void);

   void establish(const MSString &serviceName_);
   void establish(void);
   const MSString &protocol(void);
   void protocol(const MSString &new_);
   const MSString &serviceName(void);
   void serviceName(const MSString &new_);
   const MSString &host(void);
   int port(void);
   const MSHostPort &hostPort(void);
   void setHostPort(const MSString &host_,long port_);
   MSBoolean isValid(void);
   MSBoolean isReady(void);
   void isReady(MSBoolean set_);
};

inline const MSString &MSIPService::protocol(void) { return _protocol; }
inline void MSIPService::protocol(const MSString &new_) { _protocol=new_; }
inline const MSString &MSIPService::serviceName(void) { return _serviceName; }
inline void MSIPService::serviceName(const MSString &new_) { _serviceName=new_; }
inline const MSString &MSIPService::host(void) { return _hostport.host(); }
inline int MSIPService::port(void) { return _hostport.port(); }
inline const MSHostPort &MSIPService::hostPort(void) { return _hostport; }
inline void MSIPService::setHostPort(const MSString &host_,long port_){ _hostport.set(host_,port_); }
inline MSBoolean MSIPService::isValid(void) { return _isValid; }
inline MSBoolean MSIPService::isReady(void) { return _isReady; }
inline void MSIPService::isReady(MSBoolean set_) { _isReady=set_; }

#endif
