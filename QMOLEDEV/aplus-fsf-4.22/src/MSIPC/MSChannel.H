#ifndef MSChannelHEADER
#define MSChannelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <sys/types.h>
#if !defined(INT_MAX)
#include <MSIPC/MSLimits.H>
#endif

#include <sys/uio.h>
#include <sys/time.h>
#include <MSTypes/MSCallback.H>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSString.H>

class MSNodeItem;
class MSFds;

#if !defined(MS_FD_SET_NOT_STRUCT)
struct fd_set;
#endif

class MSIPCExport MSChannel
{
public:
  enum Priority {Default=0,High=INT_MAX,Low=0}; 
  enum Type {Read=0,Write=1,Execute=2,Exception=2};
private:
  static MSFds      *_pFds;
  static MSNodeItem *_pChannelList;
  MSString           _name;
  int                _priority;
  int                _fd;
  Type               _type;
  MSNodeItem        *_pNode;
  MSCallback        *_pCallback;
  fd_set            *_efds;  // enabled fd set
  fd_set            *_afds;  // available fd set

  // The following class members are for CTL support
  class MSIPCExport CtlObject 
  {
  public:
    CtlObject(int fd_);
    ~CtlObject(void);
    
    void readChannel(MSChannel *read_);
    void writeChannel(MSChannel *write_);
    void exceptionChannel(MSChannel *except_);
    void mask(int mask_);
    void node(MSNodeItem *node_);
    
    MSChannel *readChannel(void);
    MSChannel *writeChannel(void);
    MSChannel *exceptionChannel(void);
    int mask(void);
    int fd(void);
    MSNodeItem *node(void);
  private:
    MSChannel   *_readChannel;
    MSChannel   *_writeChannel;
    MSChannel   *_exceptionChannel;
    int          _fd;
    int          _mask;
    MSNodeItem  *_node;
  };
  static MSNodeItem *_pCtlChannelList;
  CtlObject         *_pCtlObject;
  
  static void ctlHandler(int,int,void*);

  static int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);

public:
  MSChannel(const char *name_,int fd_,int pri_,Type type_,MSCallback *pCallback_=0);
  virtual ~MSChannel(void);

  static MSFds *fds(void);

  fd_set *afds(void);
  fd_set *efds(void);
  MSCallback *callback(void);

  int priority(void) const;
  int fd(void) const;
  const char *name(void) const;
  Type type(void) const;

  MSBoolean enabled(void);
  MSBoolean disabled(void);

  void disable(void);
  void enable(void); 
  void priority(int);
  void callback(MSCallback *);

  static void init(void);
  static void fdsfresh(int fd_);
  static void fdscopy(void);
  static void fdszero(void);
  static MSBoolean processChannels(void);
  static MSBoolean removeBadFds(void);
  static int select(struct timeval *pTimeout_);
  static int select(int fd_,Type,struct timeval *pTimeout_);  
  virtual void process(void);
};

#endif 
