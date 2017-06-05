///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

extern "C"
{
#include <arpa/inet.h>
}

#include <MSIPC/MSHostPort.H>
#include <MSTypes/MSMessageLog.H>

MSHostPort::MSHostPort(void)
{
  _host="";
  _port=0;
}

MSHostPort::MSHostPort(const char *host_,int port_)
{
  _host=host_;
  _port=port_;
}

// translate internet style address (struct sockaddr_in)
// into hostname and port style address
MSHostPort::MSHostPort(struct sockaddr_in *name_)
{
  if (name_!=(struct sockaddr_in *)(0))
   {
     if (name_->sin_addr.s_addr==INADDR_ANY) _host="";
     else
      {
	struct in_addr *a=&(name_->sin_addr);
	struct hostent *hp;
	
	hp=gethostbyaddr((char *)a,sizeof(*a),AF_INET);
	if (hp==(struct hostent *)(0)) _host=inet_ntoa(name_->sin_addr);
	else _host=hp->h_name;
      }
     _port=ntohs(name_->sin_port);
   }
  else
   {
     _host="";
     _port=0;
   }
}

void MSHostPort::set(const char *host_,int port_)
{
  _host=host_;
  _port=port_;
}

// translate hostname and port style address
// into an internet style address: struct sockaddr_in
struct sockaddr_in *MSHostPort::sockaddr_in(int& len_)
{
  struct hostent *hostp;
  struct sockaddr_in *name=(struct sockaddr_in *)new char[sizeof(struct sockaddr_in)];
  name->sin_family=AF_INET;
  if (host().length()>0)
   {
     if ((hostp=gethostbyname(host().string()))==(struct hostent *)(0))
      {
        MSMessageLog::errorMessage("MSHostPort:sockaddr: error: %s not found\n",host().string());
        delete [] name;
	return (struct sockaddr_in *)0;
      }
     if (hostp->h_addrtype!=AF_INET)
      {
        MSMessageLog::errorMessage("MSHostPort::sockaddr: error: %s not in AF_INET domain\n",host().string());
        delete [] name;
	return (struct sockaddr_in *)0;
      }
     if (hostp->h_length!=sizeof(name->sin_addr.s_addr))
      {
        MSMessageLog::errorMessage("MSHostPort::sockaddr: error: %s address length mismatch\n",host().string());
        delete [] name;
	return (struct sockaddr_in *)0;
      }
     memcpy((char *)(&(name->sin_addr.s_addr)),hostp->h_addr,sizeof(name->sin_addr.s_addr));
   }
  else name->sin_addr.s_addr=INADDR_ANY;

  name->sin_port=htons(port());
  memset(name->sin_zero,0,8);
  len_=sizeof(*name);
  return name;
}
