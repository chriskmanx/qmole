/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#include "wv.h"

char *xml_slash = "";

static const TokenTable s_Tokens[] = {
    {"TIME", FC_TIME},
    {"\\@", FC_DateTimePicture},
    {"HYPERLINK", FC_HYPERLINK},
    {"TOC", FC_TOC},
    {"\\o", FC_TOC_FROM_RANGE},
    {"PAGEREF", FC_PAGEREF},
    {"EMBED", FC_EMBED},
    {"EDITTIME", FC_EDITTIME},
    {"SPEICHERDAT", FC_SPEICHERDAT},
    {"DATEINAME"  , FC_DATEINAME  },
    {"*", FC_OTHER}
};

static unsigned int
s_mapNameToToken (const char *name)
{
    unsigned int k;
    for (k = 0; k < FieldCodeTableSize; k++)
      {
	  if (s_Tokens[k].m_name[0] == '*')
	      return k;
	  else if (!(strcasecmp (s_Tokens[k].m_name, name)))
	      return k;
      }
    return 0;
}


int
lookahead (char *token, char test1, char test2)
{
    int ret = 0;
    while ((*token == test1) || (*token == test2))
      {
	  token++;
	  ret++;
      }
    return (ret);
}

#define TIMESTR_SIZE 4096

int
wvHandleDateTimePicture (char *retstring, size_t max, char *token,
			 time_t * mytime)
{
    int no;
    int consumed = 0;
    struct tm *current;
    char timestr[TIMESTR_SIZE];
    char temp[64];
    timestr[0] = '\0';

    if (!token)
	return (0);
    current = localtime (mytime);

    /* the '11' is the max width of an integer (10 digits for '4 billion') + nul */
    while (*token && (consumed < (TIMESTR_SIZE - 11)))
      {
	  switch (*token)
	    {
	    case 'A':
		if ((strlen (token) > 5) && (0 == strncmp (token, "AM/PM", 5)))
		  {
		      strcat (timestr, "%p");
		      token += 5;
		      consumed += 2;
		  }
		break;
	    case 'a':
		if ((strlen (token) > 5) && (0 == strncmp (token, "AM/PM", 5)))
		  {
		      strcat (timestr, "%P");
		      token += 5;
		      consumed += 2;
		  }
		break;
	    case 'M':
		no = lookahead (token, 'M', 'M');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		      sprintf (temp, "%d", current->tm_mon+1);
		      strcat (timestr, temp);
		      consumed += strlen (temp);
		      break;
		  case 2:
		      strcat (timestr, "%m");
		      consumed += 2;
		      break;
		  case 3:
		      strcat (timestr, "%b");
		      consumed += 2;
		      break;
		  default:
		      strcat (timestr, "%B");
		      consumed += 2;
		      break;
		  }
		break;
	    case 's':
	    case 'S':
		no = lookahead (token, 's', 'S');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		  default:
		      strcat (timestr, "%S");
		      break;
		  }
		consumed += 2;
		break;
	    case 'd':
	    case 'D':
	    case 't':
	    case 'T':

	        if(*token == 't' || *token == 'T')
		    no = lookahead (token, 't', 't');
	        else
		    no = lookahead (token, 'd', 'D');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		      consumed += sprintf (temp, "%d", current->tm_wday);
		      strcat (timestr, temp);
		      consumed += strlen (temp);
		      break;
		  case 2:
		      strcat (timestr, "%d");
		      consumed += 2;
		      break;
		  case 3:
		      strcat (timestr, "%a");
		      consumed += 2;
		      break;
		  default:
		      strcat (timestr, "%A");
		      consumed += 2;
		      break;
		  }
		break;
	    case 'y':
	    case 'Y':
	    case 'j':
	    case 'J':
	        if(*token == 'j' || *token == 'J')
		    no = lookahead (token, 'j', 'J');
		else
		    no = lookahead (token, 'y', 'Y');
		token += (no - 1);
		switch (no)
		  {
		  case 2:
		      strcat (timestr, "%y");
		      break;
		  default:
		      strcat (timestr, "%Y");
		      break;
		  }
		consumed += 2;
		break;
	    case 'h':
		no = lookahead (token, 'h', 'h');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		      sprintf (temp, "%d", current->tm_hour % 12);
		      strcat (timestr, temp);
		      consumed += strlen (temp);
		      break;
		  default:
		      strcat (timestr, "%I");
		      consumed += 2;
		      break;
		  }
		break;
	    case 'H':
		no = lookahead (token, 'H', 'H');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		      consumed += sprintf (temp, "%d", current->tm_hour);
		      strcat (timestr, temp);
		      consumed += strlen (temp);
		      break;
		  default:
		      strcat (timestr, "%H");
		      consumed += 2;
		      break;
		  }
		break;
	    case 'm':
		no = lookahead (token, 'm', 'm');
		token += (no - 1);
		switch (no)
		  {
		  case 1:
		      consumed += sprintf (temp, "%d", current->tm_min);
		      strcat (timestr, temp);
		      consumed += strlen (temp);
		      break;
		  default:
		      strcat (timestr, "%M");
		      consumed += 2;
		      break;
		  }
		break;
	    case '\"':
		break;
	    case '`':
		break;
	    default:
		temp[0] = *token;
		temp[1] = '\0';
		strcat (timestr, temp);
		consumed += 1;
		break;
	    }
	  token++;
      }
    return (strftime (retstring, max, timestr, current));
}


int
wvHandleTotalField (char *command)
{
    int ret = 0;
    unsigned int tokenIndex;
    char *token;

    if (*command != 0x13)
      {
	  wvError (("field did not begin with 0x13\n"));
	  return (1);
      }
    strtok (command, "\t, ");
    while ((token = strtok (NULL, "\t, ")))
      {
	  tokenIndex = s_mapNameToToken (token);
	  switch (s_Tokens[tokenIndex].m_type)
	    {
	    case FC_HYPERLINK:
		token = strtok (NULL, "\"\" ");
		printf ("</a>");
		break;
	    default:
		break;
	    }
      }
    return (ret);
}

static time_t s_file_mtime(char *name)
{
    struct stat buf;

    if(stat(name, &buf) == -1) 
    {
	wvError(("stat %s failed.", name));
	return (time_t)-1;
    }
    return buf.st_mtime;
}

int
wvHandleCommandField (wvParseStruct *ps, char *command)
{
    int ret = 0;
    unsigned int tokenIndex;
    char *token;
    char datestr[4096];
    time_t mytime = (time_t)-1;

    if (*command != 0x13)
      {
	  wvError (("field did not begin with 0x13\n"));
	  return (1);
      }
    strtok (command, "\t, ");
    while ((token = strtok (NULL, "\t, ")))
      {
	  tokenIndex = s_mapNameToToken (token);
	  switch (s_Tokens[tokenIndex].m_type)
	    {
	    case FC_HYPERLINK:
		token = strtok (NULL, "\"\" ");
		printf ("<a href=\"%s\">", token);
		break;
	    case FC_EMBED:
		wvError (("embed\n"));
		token = strtok (NULL, "\t, ");
	/*	printf ("<!--%s-->", token);
	 */	break;
	    case FC_PAGEREF:
		token = strtok (NULL, "\"\" ");
		printf ("<a href=\"#%s\" %s>", token, xml_slash);
		break;
	    case FC_EDITTIME:
		token = strtok (NULL, "\"\" ");
		break;
	    case FC_TOC:
		wvTrace (("toc\n"));
		break;
	    case FC_TOC_FROM_RANGE:
		token = strtok (NULL, "\"\" ");
#ifdef DEBUG
		if (token)
		    wvTrace (("toc range is %s\n", token));
#endif
		break;
	    case FC_TIME:
		wvError (("time token\n"));
		ret = 1;
		time (&mytime);
		break;
	    case FC_DateTimePicture:
		wvTrace (("DateTimePicture\n"));
		token = strtok (NULL, "\"\"");
		if(mytime == (time_t)-1)
		    time (&mytime);
		if (wvHandleDateTimePicture (datestr, 4096, token, &mytime)) {
		  /* printf ("%s", datestr); */ /* prefer to print out the text that follows the spec char */
		}
		else
		    wvError (
			     ("date and time field function returned nothing\n"));
		ret = 0; /* print the text which follows after the spec char*/
		break;

	    case FC_DATEINAME:
	      if (ps->filename) {
		printf("%s", ps->filename);
	      }
		ret = 1;
		break;

	    case FC_SPEICHERDAT:
	      if (ps->filename) {
		mytime = s_file_mtime(ps->filename);
	      }
		ret = 1;
		break;
		
	    default:
		break;
	    }
      }
    return (ret);
}

int
fieldCharProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid)
{
    static U16 command[40000];
    static U16 argumen[40000];
    static U16 *which;
    static int i, depth;
    char *a;
    static char *c = NULL;
    static int ret;

    if (eachchar == 0x13)
      {
	  a = NULL;
	  ret = 1;
	  if (depth == 0)
	    {
		which = command;
		command[0] = 0;
		argumen[0] = 0;
		i = 0;
	    }
	  depth++;
      }
    else if (eachchar == 0x14)
      {
	  if (depth == 1)
	    {
		command[i] = 0;
		c = wvWideStrToMB (command);
		if (wvHandleCommandField (ps, c))
		    ret = 1;
		else
		    ret = 0;

		wvError (
			 ("command %s, ret is %d\n", wvWideStrToMB (command),
			  ret));
		wvFree (c);
		which = argumen;
		i = 0;
	    }
      }
    if (i >= 40000)
      {
	  wvError (("WHAT!\n"));
	  return 0;
      }

    which[i] = eachchar;
    if (chartype)
	which[i] = wvHandleCodePage (which[i], lid);
    i++;

    if (eachchar == 0x15)
      {
	  depth--;
	  if (depth == 0)
	    {
		which[i] = 0;
		a = wvWideStrToMB (argumen);
		c = wvWideStrToMB (command);
		wvHandleTotalField (c);
		wvFree (a);
		wvFree (c);
	    }
      }
    return (ret);
}
