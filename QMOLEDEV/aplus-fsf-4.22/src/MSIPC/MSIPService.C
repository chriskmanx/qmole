///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSIPService.H>
#include <MSTypes/MSMessageLog.H>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <rpcsvc/ypclnt.h>

static char *domain;
static char *map = "remprogs";
static char *file = "/etc/remprogs";
 
int MSIPService::_countService, MSIPService::_currentService, MSIPService::_countRemprogEntries=0;

#define	MAX_TRIES	3

MSIPService::MSIPService(const MSString &serviceName_) : _hostport("",-1), _serviceName(serviceName_)
{
  _isValid=MSFalse;
  _isReady=MSFalse;
  _remprogEntries=(Remprog *)0;
  _remprogBuffer=(char *)0;
}

MSIPService::MSIPService(void) : _hostport("",-1), _serviceName("")
{
  _isValid=MSFalse;
  _isReady=MSFalse;
  _remprogEntries=(Remprog *)0;
  _remprogBuffer=(char *)0;
}

MSIPService::~MSIPService(void)
{
  if (_remprogEntries!=0) delete [] _remprogEntries;
  if (_remprogBuffer!=0) free(_remprogBuffer);
}

void MSIPService::establish(const MSString &serviceName_)
{
  _serviceName=serviceName_;
  establish();
}

void MSIPService::establish(void)
{
  switch(inp_parse(serviceName()))
   {
   case MSIPService::CN_TYPE_ERR:
     /* Shouldn't get here */
     MSMessageLog::warningMessage("MSIPService: Invalid service type: (%s)\n", serviceName().string());
     break;

   case MSIPService::CN_TYPE_REMPROGS:
     if (lookupRemprogService()==0) 
      {
        MSMessageLog::warningMessage("MSIPService: Unknown service: (%s)\n",serviceName().string());
      }
     else 
      {
        _isValid=MSTrue;
        _isReady=MSTrue;
      }
     break;

   case MSIPService::CN_TYPE_HOSTPORT:
     if (getServByName()==MSFalse) // attempt to use getservbyname
      {
        MSMessageLog::warningMessage("Unknown service: (%s)\n",serviceName().string());
      }
     else 
      {
        _isValid=MSTrue;
        _isReady=MSTrue;
      }
     break;
   default:
     MSMessageLog::warningMessage("Error: Invalid service type: (%s)\n", serviceName().string());
     break;
   }
}

MSIPService::CN_TYPE MSIPService::inp_parse(const char *serviceName_)
{
  if (serviceName_ == NULL || *serviceName_ == '\0') return MSIPService::CN_TYPE_ERR;

  if (strchr(serviceName_, '@') != NULL) return MSIPService::CN_TYPE_HOSTPORT;

  if (strchr(serviceName_, ':') != NULL) return MSIPService::CN_TYPE_HOSTPORT;

  /*
   * Restriction: remprogs can't start w. digit, in order to tell
   * port.
   */

  if (serviceName_[0] >= '0' && serviceName_[0] <= '9') return MSIPService::CN_TYPE_ERR;

  return MSIPService::CN_TYPE_REMPROGS;
}

MSBoolean MSIPService::getServByName(void)
{
  struct servent          *sp;

  int v;
  unsigned cp;
  if (isValid()) return (-1==port())?MSFalse:MSTrue;
  if ((cp=serviceName().indexOf("@")) != serviceName().length())
   {
     if ((sp = getservbyname((char *)serviceName().subString(0,cp).string(), "tcp")) == NULL)
      {
        if ((v = (serviceName().subString(0,cp)).asInt()) == 0) return MSFalse;
        setHostPort(serviceName().subString(cp+1),v);
      }
     else 
      {
        setHostPort(serviceName().subString(cp+1),ntohs(sp->s_port));
      }
     return MSTrue;
   }
  else 
   {
     if ((cp=serviceName().indexOf(":")) != serviceName().length())
      {
        if ((sp = getservbyname((char *)serviceName().subString(cp+1).string(), "tcp")) == NULL)
         {
           if ((v = (serviceName().subString(cp+1)).asInt()) == 0) return MSFalse;
           setHostPort(serviceName().subString(0,cp),v);
         }
        else
         {
           setHostPort(serviceName().subString(0,cp),ntohs(sp->s_port));
         }
        return MSTrue;
      }
   }
  return MSFalse;
}

MSBoolean MSIPService::lookupRemprogService(void)
{
  if (isValid()==MSTrue) return (-1==port())?MSFalse:MSTrue;
  if (0 >= getIPService()) {return MSFalse;}
  if (0 > nextIPService()) {return MSFalse;}
  return MSTrue;
}

int MSIPService::getIPService(void)
{
  if (_countService > 0) _countService = _currentService = 0;

  return(_countService = getRemprog());
}


int MSIPService::getRemprog(void)
{
  int tries = 0;

  for (;;)
   {
     switch (getRemprogByName()) {

       /* Success */
     case 0:
       _remprogEntryPointer = _remprogEntry;
       return(_countRemprogEntries);

       /* Not necessarily fatal errors - worth retrying */
     case YPERR_RPC:
     case YPERR_DOMAIN:
     case YPERR_MAP:
     case YPERR_KEY:
     case YPERR_YPERR:
     case YPERR_PMAP:
     case YPERR_YPBIND:
     case YPERR_YPSERV:
     case YPERR_RESRC:
     case YPERR_NOMORE:
     case YPERR_NODOM:
     case YPERR_BADDB:
     case YPERR_VERS:
#ifdef YPERR_BUSY
     case YPERR_BUSY:
#endif
#ifdef YPERR_ACCESS
     case YPERR_ACCESS:
#endif
       if (++tries > MAX_TRIES)
        {
          MSMessageLog::warningMessage("MSIPService: Cannot find service (%s)\n", serviceName().string());
          return(-1);
        }
       break;

       /* Fatal errors */
     case YPERR_BADARGS:	/* Args to function are bad */
     default:
       MSMessageLog::warningMessage("MSIPService: Cannot find service (%s)\n", serviceName().string());
       return(-1);
     }
   }
  /*NOTREACHED*/
}

int MSIPService::getRemprogByName(void)
{
  FILE *fp;
  int len, ret;
  register int i;

  if (domain == (char *)(0))
(void)yp_get_default_domain(&domain);

  if (_remprogBuffer != (char *)0) free(_remprogBuffer);
  _remprogBuffer = (char *)(0);
  if (_remprogEntries != (Remprog *)0) delete [] _remprogEntries;
  _remprogEntries=(Remprog *)0;

  ret = yp_match(domain, map, (char *)serviceName().string(), strlen((char *)serviceName().string()), &_remprogBuffer, &len);
  switch(ret)
   {
   case YPERR_BADARGS:
   case YPERR_MAP:
   case YPERR_KEY:
   case YPERR_NOMORE:
   case YPERR_ACCESS:
     return(ret);
   case 0:
   {
     int retval=remprogParseValue(_remprogBuffer);
     return retval;
   }
   }

  /* Try opening the file directly */
  if ((fp = fopen(file, "r")) == (FILE *)(0)) return(ret);/* return YP error */
  char buf[128]; /* default buffer size */
  while (fgets(buf, sizeof(buf), fp) == buf)
   {
     for (i = 0; buf[i] != 0; i++)
      {
        if(isspace(buf[i]))  break;
      }
     
     buf[i] = 0;
     if (strcmp((char *)buf, (char *)serviceName().string())==0)
      {
        (void)fclose(fp);
        return(remprogParseValue(&buf[i+1]));
      }
   }
  (void)fclose(fp);
  return(ret);			/* return YP error */
}

int MSIPService::remprogParseValue(char *value_)
{
  register char *cp = value_;
  register char *p;
  Remprog *rp;
  int i, nrp = 1;

  compress(value_);
  for (cp = value_; *cp; cp++)
   {
     if (*cp == ' ') nrp++;
   }
  
  _remprogEntries = new Remprog[nrp+1];
  if (_remprogEntries == (Remprog *)(0)) return(YPERR_RESRC);
  rp = &_remprogEntries[nrp];
  rp->_rp_host = rp->_rp_protocol = (char *)0;
  rp->_rp_prognum = 0;

  compress(value_);
  cp = value_;
  p = value_;
  for (i = 0; i < nrp; i++)
   {
     if (cp = (char *)strchr((char *)cp, ' '))
      {
        *cp = 0;
        cp++;
      }
     _remprogEntries[i]._rp_host = p;
     p = (char *)strchr((char *)p, ':');
     *p = 0;
     p++;
     _remprogEntries[i]._rp_prognum = atoi(p);
     p = (char *)strchr((char *)p, ':');
     p++;	
     _remprogEntries[i]._rp_protocol = p;
     p = cp;
   }
  _remprogEntry = _remprogEntries;
  _countRemprogEntries = nrp;
  return(0);
}

void MSIPService::compress(register char *fromptr_)
{
  register char   chr, *toptr = fromptr_;

  chr = ' ';                    /* init to skip leading spaces  */
  while ((*toptr = *fromptr_++) != '\0')
   {                             /* convert newlines and tabs to */
     if (isspace (*toptr))
      {                         /* only save first space      */
        if (chr != ' ')
         {
           *toptr++ = chr  = ' ';
         }
      }
     else
   chr = *toptr++;
   }

  if (chr == ' ')
   { /* remove trailing space if any */
     *--toptr = '\0';
   }
  
  return;
}

int MSIPService::nextIPService(void)
{
  /* eh? */
  if (_countService == 0)
   {
     MSMessageLog::infoMessage("Next Service before get?\n");
     return(-1);
   }
  /* fill in the blanks */

  setHostPort(_remprogEntryPointer->_rp_host,_remprogEntryPointer->_rp_prognum);
  protocol(_remprogEntryPointer->_rp_protocol);

  _currentService++, _remprogEntryPointer++;
  if (_currentService == _countService)
   {
     _currentService = 0, _remprogEntryPointer = _remprogEntry;
   }
  
  return(_currentService);
}
