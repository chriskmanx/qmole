/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             N   N  EEEEE  TTTTT  W   W   OOO   RRRR   K   K                 %
%             NN  N  E        T    W   W  O   O  R   R  K  K                  %
%             N N N  EEE      T    W W W  O   O  RRRR   KKK                   %
%             N  NN  E        T    WW WW  O   O  R R    K  K                  %
%             N   N  EEEEE    T    W   W   OOO   R  R   K   K                 %
%                                                                             %
%                                                                             %
%                          Network Routines.                                  %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                             October 1992                                    %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/

#include "xtp.h"
#include "regular.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t H o s t I n f o                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function GetHostInfo accepts a host name or address, verifies it is valid,
%  and returns both the host name and address from the network host entry.
%
%  The format of the GetHostInfo routine is:
%
%    info=GetHostInfo(name)
%
%  A description of each parameter follows:
%
%    o info:  Function GetHostInfo returns a pointer to the host name and
%      IP address.  A null pointer is returned if there the host cannot be
%      located.
%
%    o name:  Specifies a pointer to a character array that contains either
%      a name of a host or an IP address.
%
%
*/
char *GetHostInfo(char *name)
{
  char
    *p;

  static char
    info[2048];

  struct in_addr
    in;

  struct hostent
    *hp;

  /*
    Get host name and address.
  */
  if (isascii((int) *name) && isdigit((int) *name))
    in.s_addr=inet_addr(name);
  else
    {
      in.s_addr=(unsigned long) -1;
      hp=gethostbyname(name);
      if (hp != (struct hostent *) NULL)
        in.s_addr=(*(int *) hp->h_addr);
    }
  hp=gethostbyaddr((char *) &in,sizeof(struct in_addr),AF_INET);
  if (hp == (struct hostent *) NULL)
    {
      hp=gethostbyname(name);
      if (hp == (struct hostent *) NULL)
        return((char *) NULL);
    }
  /*
    Convert hostname to lower-case characters.
  */
  p=(char *) hp->h_name;
  while (*p)
  {
    if (isupper((int) *p))
      *p=tolower(*p);
    p++;
  }
  (void) sprintf(info,"%s [%s]: ",hp->h_name,inet_ntoa(in));
  return(info);
}
