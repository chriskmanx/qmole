/* w32_account.c - Account related W32 functions.
   Copyright (C) 2007-2009 g10 Code GmbH 
   Copyright (C) 1999-2005 Nullsoft, Inc.

   This software is provided 'as-is', without any express or implied
   warranty. In no event will the authors be held liable for any
   damages arising from the use of this software.
   
   Permission is granted to anyone to use this software for any
   purpose, including commercial applications, and to alter it and
   redistribute it freely, subject to the following restrictions:
   
   1. The origin of this software must not be misrepresented; you must
      not claim that you wrote the original software. If you use this
      software in a product, an acknowledgment in the product
      documentation would be appreciated but is not required.
   
   2. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.
   
   3. This notice may not be removed or altered from any source
      distribution.

 =======[ wk 2007-05-21 ]====
   The code for get_group_name has been taken from NSIS 2.05, module
   UserInfo.c.  NSIS bears the above license and along with the
   notice:
     This license applies to everything in the NSIS package, except where
     otherwise noted.
   Thus we make this module available under the same license - note,
   that this lincese is fully compatibe with the GNU GPL 2.0.
*/ 
  
  


#include <stdlib.h>
#include <string.h>

#include "w32lib.h"

#ifndef DIM
#define DIM(v)  (sizeof(v)/sizeof((v)[0]))
#endif


/* Return a malloced name of our user group.  */
static char *
get_group_name (void)
{
  HANDLE        hThread;
  TOKEN_GROUPS  *ptg = NULL;
  DWORD         cbTokenGroups;
  DWORD         i, j;
  SID_IDENTIFIER_AUTHORITY SystemSidAuthority = { SECURITY_NT_AUTHORITY };
  char *group;
  struct
  {
    DWORD auth_id;
    char *name;
  } groups[] = 
    {
      /* Every user belongs to the users group, hence
         users comes before guests */
      {DOMAIN_ALIAS_RID_USERS, "User"},
      {DOMAIN_ALIAS_RID_GUESTS, "Guest"},
      {DOMAIN_ALIAS_RID_POWER_USERS, "Power"},
      {DOMAIN_ALIAS_RID_ADMINS, "Admin"}
    };


  group = NULL;
  if (GetVersion() & 0x80000000) 
    {
      /* This is not NT; thus we are always Admin. */
      group = "Admin";
    }
  else if (OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, FALSE, &hThread) 
           || OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hThread))
    {
      /* With the token for the current thread or process in hand we
         query the size of the associated group information.  Note
         that we expect an error because buffer has been passed as
         NULL. cbTokenGroups will then tell use the required size.  */
      if (!GetTokenInformation (hThread, TokenGroups, NULL, 0, &cbTokenGroups)
          && GetLastError () == ERROR_INSUFFICIENT_BUFFER)
        {
          ptg = GlobalAlloc (GPTR, cbTokenGroups);
          if (ptg)
            {
              if (GetTokenInformation ( hThread, TokenGroups, ptg,
                                        cbTokenGroups, &cbTokenGroups))
                {

                  /* Now iterate through the list of groups for this
                     access token looking for a match against the SID
                     we created above. */
                  for (i = 0; i < DIM (groups); i++)
                    {
                      PSID psid = 0;
                      
                      AllocateAndInitializeSid (&SystemSidAuthority,
                                                2,
                                                SECURITY_BUILTIN_DOMAIN_RID,
                                                groups[i].auth_id,
                                                0, 0, 0, 0, 0, 0,
                                                &psid);
                      if (!psid) 
                        continue;
                      for (j = 0; j < ptg->GroupCount; j++)
                        if (EqualSid(ptg->Groups[j].Sid, psid))
                          group = groups[i].name;
                      FreeSid(psid);
                    }
                }
              
              GlobalFree(ptg);
            }
        }
      
      CloseHandle(hThread);
    }

  return group? strdup (group):NULL;
}


/* Return true if we are an administrator.  The chekc is done only
   once so if the current user has been hadded to the Administrator
   group the process needs to be rerstarted. */
int
w32_is_administrator (void)
{
  static int got_it;
  static int is_admin;

  if (!got_it)
    {
      char *name = get_group_name ();

      if (name && !strcmp (name, "Admin"))
        is_admin = 1;
      got_it = 1;
      free (name);
    }

  return is_admin;
}

