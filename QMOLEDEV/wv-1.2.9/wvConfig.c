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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "wv.h"
#include "wvinternal.h"
#ifdef HAVE_LIBXML2
#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#define XML_Char xmlChar
#else
#ifdef HAVE_EXPAT
#include <expat.h>
#else
#include "xmlparse.h"
#endif
#endif

extern int (*wvConvertUnicodeToEntity) (U16 char16);

#define HANDLE_B_PARA_ELE(a,b,c,d) \
if ( (((PAP*)(mydata->props))->b == d) && (c == 0) ) \
	{ \
	text = (char *)wvMalloc(strlen(mydata->sd->elements[a].str[0])+1); \
	strcpy(text,mydata->sd->elements[a].str[0]); \
	str = mydata->retstring; \
	wvExpand(mydata,text,strlen(text)); \
	wvAppendStr(&str,mydata->retstring); \
	wvFree(mydata->retstring); \
	mydata->retstring = str; \
	wvFree(text); \
	mydata->currentlen = strlen(mydata->retstring); \
	c=d; \
	}

#define HANDLE_B_CHAR_ELE(a,b,c,d) \
if ( (((CHP*)(mydata->props))->b == d) && (c == 0) ) \
	{ \
	text = (char *)wvMalloc(strlen(mydata->sd->elements[a].str[0])+1); \
	strcpy(text,mydata->sd->elements[a].str[0]); \
	str = mydata->retstring; \
	wvExpand(mydata,text,strlen(text)); \
	wvAppendStr(&str,mydata->retstring); \
	wvFree(mydata->retstring); \
	mydata->retstring = str; \
	wvFree(text); \
	mydata->currentlen = strlen(mydata->retstring); \
	c=d; \
	}

#define HANDLE_E_CHAR_ELE(a,b,c,d) \
/* \
if ( (!((CHP*)(mydata->props))->b) && (c != 0) ) \
*/ \
if (c == d) \
	{ \
	text = (char *)wvMalloc(strlen(mydata->sd->elements[a].str[1])+1); \
	strcpy(text,mydata->sd->elements[a].str[1]); \
	str = mydata->retstring; \
	wvExpand(mydata,text,strlen(text)); \
	wvAppendStr(&str,mydata->retstring); \
	wvFree(mydata->retstring); \
	mydata->retstring = str; \
	wvFree(text); \
	mydata->currentlen = strlen(mydata->retstring); \
	c=0; \
	}

#define HANDLE_E_PARA_ELE(a,b,c,d) \
/* \
if ( (!((PAP*)(mydata->props))->b) && (c != 0) ) \
*/ \
if (c == d) \
	{ \
	text = (char *)wvMalloc(strlen(mydata->sd->elements[a].str[1])+1); \
	strcpy(text,mydata->sd->elements[a].str[1]); \
	str = mydata->retstring; \
	wvExpand(mydata,text,strlen(text)); \
	wvAppendStr(&str,mydata->retstring); \
	wvFree(mydata->retstring); \
	mydata->retstring = str; \
	wvFree(text); \
	mydata->currentlen = strlen(mydata->retstring); \
	c=0; \
	}

void
wvInitStateData (state_data * data)
{
    int i;
    data->fp = NULL;
    data->path = NULL;
    data->currentele = NULL;
    data->current = NULL;
    data->currentlen = 0;
    for (i = 0; i < TokenTableSize; i++)
      {
	  data->elements[i].nostr = 0;
	  data->elements[i].str = NULL;
      }
}

void
wvListStateData (state_data * data)
{
    int i, j;
    for (i = 0; i < TokenTableSize; i++)
      {
	  for (j = 0; j < data->elements[i].nostr; j++)
	    {
		if (data->elements[i].str[j])
		    wvError (
			     ("listing->element %s\n",
			      data->elements[i].str[j]));
	    }
      }

}

void
wvReleaseStateData (state_data * data)
{
    int i, k;
    if (data->fp)
	fclose (data->fp);
    for (i = 0; i < TokenTableSize; i++)
      {
	  for (k = 0; k < data->elements[i].nostr; k++)
	      wvFree (data->elements[i].str[k]);
	  wvFree (data->elements[i].str);
      }
}


static void
exstartElement (void *userData, const char *name, const char **atts)
{
    unsigned int token_type;
    expand_data *mydata = (expand_data *) userData;
    char *text, *str;
    static int bold, italic, strike, outline, smallcaps, caps, vanish,
	shadow, lowercase, emboss, imprint, dstrike, iss, kul, color, fontstr,
	proprmark, animation, deleted, added, FldRMark, ilfo, ilvl =
	-1, ulist, olist, fintable, fttp = 1, table, txt, lastcell;
    char buffer[64];
    static LVL lvl;
    static U32 lastid = 0;
    static LFO *retlfo;
    U32 k;
    int i, j;

    /*
    PATCH
    */
    PAP *pap;

 /*   printf("exstart: %s \n",name);*/

/*  tokenIndex = s_mapNameToToken ((const char *) name);
    token_type = s_Tokens[tokenIndex].m_type;
 */ token_type = wvMapNameToTokenType ((const char *) name);
    switch (token_type)
      {
      case TT_TITLE:
	  if (mydata->retstring)
	    {
		printf ("%s", mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = NULL;
		mydata->currentlen = 0;
	    }
	  wvPrintTitle (mydata->ps, mydata->anSttbfAssoc);
	  break;
      case TT_CHARSET:
	  wvTrace (("the charset is %d\n", mydata->charset));
	  wvAppendStr (&mydata->retstring, mydata->charset);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_ROWSPAN:
	  wvTrace (("This Para is here cell %d %d\n",
		    mydata->whichrow, mydata->whichcell));
	  if (*mydata->vmerges)
	    {
		wvTrace (
			 ("%d\n",
			  (*mydata->vmerges)[mydata->whichrow][mydata->
							       whichcell]));
		sprintf (buffer, "%d",
			 (*mydata->vmerges)[mydata->whichrow][mydata->
							      whichcell]);
	    }
	  else
	      sprintf (buffer, "1");
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_no_rows:
	  sprintf (buffer, "%d", *mydata->norows);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
	  break;
      case TT_no_cols:
	  sprintf (buffer, "%d", *(mydata->nocellbounds) - 1);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_TABLERELWIDTH:
	  {
	      S16 width =
		  ((PAP *) (mydata->props))->ptap.
		  rgdxaCenter[((PAP *) (mydata->props))->ptap.itcMac] -
		  ((PAP *) (mydata->props))->ptap.rgdxaCenter[0];
	      sprintf (buffer, "%.2f", wvRelativeWidth (width, mydata->asep));
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_CELLBGCOLOR:
	  wvTrace (
		   ("%d %d %d %d\n", mydata->whichrow, mydata->whichcell,
		    ((PAP *) (mydata->props))->ptap.itcMac, *(mydata->norows)));
	  k =
	      wvCellBgColor (mydata->whichrow, mydata->whichcell,
			     ((PAP *) (mydata->props))->ptap.itcMac,
			     *(mydata->norows),
			     &(((PAP *) (mydata->props))->ptap.tlp));
	  wvTrace (("k is %d\n", k));
	  if (k == 0)
	      k = 8;
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].
			 str[0]) + 1);
	  strcpy (text, mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_PARABGCOLOR:
	  k = ((PAP *) (mydata->props))->shd.icoBack;
	  if (k == 0)
	      k = 8;
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].
			 str[0]) + 1);
	  strcpy (text, mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_PARAFGCOLOR:
	  k = ((PAP *) (mydata->props))->shd.icoFore;
	  if (k == 0)
	      k = 1;
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].
			 str[0]) + 1);
	  strcpy (text, mydata->sd->elements[TT_BLACK + ((k - 1) * 3)].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_CELLRELPAGEWIDTH:
	  {
	      S16 width =
		  (((PAP *) (mydata->props))->ptap.
		   rgdxaCenter[mydata->whichcell + 1] -
		   ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
							       whichcell]);
	      sprintf (buffer, "%.2f", wvRelativeWidth (width, mydata->asep));
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_CELLRELWIDTH:
	  {
	      float pc;
	      long over;
	      long under;

	      over =
		  (((PAP *) (mydata->props))->ptap.
		   rgdxaCenter[mydata->whichcell + 1] -
		   ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
							       whichcell]);
	      over *= 100;
	      under =
		  (((PAP *) (mydata->props))->ptap.
		   rgdxaCenter[((PAP *) (mydata->props))->ptap.itcMac] -
		   ((PAP *) (mydata->props))->ptap.rgdxaCenter[0]);

	      pc = (float) over / under;

	      sprintf (buffer, "%.2f", pc);

	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_COLSPAN:
	  if (((PAP *) (mydata->props))->fInTable)
	    {
		wvTrace (
			 ("this cell is %d, pos %d ends %d\n",
			  mydata->whichcell,
			  ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
								      whichcell],
			  ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
								      whichcell
								      + 1]));
		i = 0;
		while (
		       (i < *mydata->nocellbounds) &&
		       (0 ==
			cellCompEQ (&((*mydata->cellbounds)[i]),
				    &(((PAP *) (mydata->props))->ptap.
				      rgdxaCenter[mydata->whichcell]))))
		    i++;

		j = i;
		while (
		       (j < *mydata->nocellbounds) &&
		       (0 ==
			cellCompEQ (&((*mydata->cellbounds)[j]),
				    &(((PAP *) (mydata->props))->ptap.
				      rgdxaCenter[mydata->whichcell + 1]))))
		  {
		      wvTrace (
			       ("j is %d %d %d\n", j,
				(*mydata->cellbounds)[j],
				((PAP *) (mydata->props))->ptap.
				rgdxaCenter[mydata->whichcell + 1]));
		      j++;
		  }

		sprintf (buffer, "%d", j - i);
		wvAppendStr (&mydata->retstring, buffer);
		mydata->currentlen = strlen (mydata->retstring);
	    }
	  break;
      case TT_FILENAME:
	if (mydata->filename) {
	  wvAppendStr (&mydata->retstring, mydata->filename);
	  mydata->currentlen = strlen (mydata->retstring);
	}
	  break;
      case TT_VERSION:
	  wvTrace (("the version is %s\n", wv_version));
	  wvAppendStr (&mydata->retstring, wv_version);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_COLORB:
	  wvTrace (("str is %s\n", mydata->sd->elements[TT_COLOR].str[0]));
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_COLOR].
			 str[((CHP *) (mydata->props))->ico]) + 1);
	  wvTrace (("the just is %d\n", ((CHP *) (mydata->props))->ico));
	  strcpy (text,
		  mydata->sd->elements[TT_COLOR].
		  str[((CHP *) (mydata->props))->ico]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_PARAMARGIN:


	  if (fintable)
	    {
		/*
		   if there are any table overrides check to make sure all are filled in,
		   and if all are set to 0
		 */
		if (
		    (mydata->sd->elements[TT_TABLEOVERRIDES].str) &&
		    (mydata->sd->elements[TT_TABLEOVERRIDES].str[0]) &&
		    (mydata->sd->elements[TT_TABLEOVERRIDES].str[1]) &&
		    (mydata->sd->elements[TT_TABLEOVERRIDES].str[2]) &&
		    (mydata->sd->elements[TT_TABLEOVERRIDES].str[3]))
		  {
		      if (
			  (atoi
			   (mydata->sd->elements[TT_TABLEOVERRIDES].str[0]) ==
			   0)
			  &&
			  (atoi
			   (mydata->sd->elements[TT_TABLEOVERRIDES].str[1]) ==
			   0)
			  &&
			  (atoi
			   (mydata->sd->elements[TT_TABLEOVERRIDES].str[2]) ==
			   0)
			  &&
			  (atoi
			   (mydata->sd->elements[TT_TABLEOVERRIDES].str[3]) ==
			   0))
			{
			    wvTrace (
				     ("The table has overrides such that paramargin is to be ignored\n"));
			    break;
			}
		  }
	    }

	  if (
	      ((PAP *) (mydata->props))->dyaBefore ||
	      ((PAP *) (mydata->props))->dyaAfter ||
	      ((PAP *) (mydata->props))->dxaLeft ||
	      ((PAP *) (mydata->props))->dxaRight)
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_PMARGIN].str[0])
			      + 1);
		strcpy (text, mydata->sd->elements[TT_PMARGIN].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
	    }
	  break;
      case TT_PARABORDER:
	  if (
	      ((PAP *) (mydata->props))->brcBetween.brcType ||
	      ((PAP *) (mydata->props))->brcRight.brcType ||
	      ((PAP *) (mydata->props))->brcBottom.brcType ||
	      ((PAP *) (mydata->props))->brcLeft.brcType ||
	      ((PAP *) (mydata->props))->brcTop.brcType)
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_PBORDER].str[0])
			      + 1);
		strcpy (text, mydata->sd->elements[TT_PBORDER].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
	    }
	  break;
      case TT_JUST:
	  wvTrace (("just is %d\n", ((PAP *) (mydata->props))->jc));
	  wvTrace (
		   ("str is %s\n",
		    mydata->sd->elements[TT_JUSTIFICATION].str[0]));
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_JUSTIFICATION].str[
								    ((PAP
								      *)
								     (mydata->
								      props))->jc])
			+ 1);
	  wvTrace (("the just is %d\n", ((PAP *) (mydata->props))->jc));
	  strcpy (text,
		  mydata->sd->elements[TT_JUSTIFICATION].
		  str[((PAP *) (mydata->props))->jc]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERTopSTYLE:
	  if (isPAPConform (&mydata->lastpap, (PAP *) (mydata->props)))
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BORDER].str[
								   ((PAP
								     *)
								    (mydata->
								     props))->brcBetween.
								   brcType]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BORDER].
			str[((PAP *) (mydata->props))->brcBetween.brcType]);
	    }
	  else
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BORDER].str[
								   ((PAP
								     *)
								    (mydata->
								     props))->brcTop.
								   brcType]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BORDER].
			str[((PAP *) (mydata->props))->brcTop.brcType]);
	    }
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERLeftSTYLE:
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BORDER].
			 str[((PAP *) (mydata->props))->brcLeft.brcType]) + 1);
	  strcpy (text,
		  mydata->sd->elements[TT_BORDER].
		  str[((PAP *) (mydata->props))->brcLeft.brcType]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERRightSTYLE:
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BORDER].
			 str[((PAP *) (mydata->props))->brcRight.brcType]) + 1);
	  strcpy (text,
		  mydata->sd->elements[TT_BORDER].
		  str[((PAP *) (mydata->props))->brcRight.brcType]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERBottomSTYLE:
	  if (isPAPConform (mydata->nextpap, (PAP *) (mydata->props)))
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BORDER].str[
								   ((PAP
								     *)
								    (mydata->
								     props))->brcBetween.
								   brcType]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BORDER].
			str[((PAP *) (mydata->props))->brcBetween.brcType]);
	    }
	  else
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BORDER].str[
								   ((PAP
								     *)
								    (mydata->
								     props))->brcBottom.
								   brcType]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BORDER].
			str[((PAP *) (mydata->props))->brcBottom.brcType]);
	    }
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERTopCOLOR:
	  if (isPAPConform (&mydata->lastpap, (PAP *) (mydata->props)))
	    {
		if (((PAP *) (mydata->props))->brcBetween.ico == 0)
		  {
		      /* temp fix this in a while */
		      ((PAP *) (mydata->props))->brcBetween.ico++;
		  }
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BLACK +
						    ((((PAP
							*) (mydata->
							    props))->brcBetween.
						      ico - 1) * 3)].str[0]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BLACK +
					     ((((PAP
						 *) (mydata->props))->
					       brcBetween.ico -
					       1) * 3)].str[0]);
	    }
	  else
	    {
		if (((PAP *) (mydata->props))->brcTop.ico == 0)
		  {
		      /* temp fix this in a while */
		      ((PAP *) (mydata->props))->brcTop.ico++;
		  }
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BLACK +
						    ((((PAP
							*) (mydata->
							    props))->brcTop.
						      ico - 1) * 3)].str[0]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BLACK +
					     ((((PAP
						 *) (mydata->props))->brcTop.
					       ico - 1) * 3)].str[0]);
	    }

	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERLeftCOLOR:
	  if (((PAP *) (mydata->props))->brcLeft.ico == 0)
	    {
		/* temp fix this in a while */
		((PAP *) (mydata->props))->brcLeft.ico++;
	    }
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BLACK +
					      ((((PAP
						  *) (mydata->props))->
						brcLeft.ico - 1) * 3)].str[0]) +
			1);
	  strcpy (text,
		  mydata->sd->elements[TT_BLACK +
				       ((((PAP
					   *) (mydata->props))->brcLeft.ico -
					 1) * 3)].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERRightCOLOR:
	  if (((PAP *) (mydata->props))->brcRight.ico == 0)
	    {
		/* temp fix this in a while */
		((PAP *) (mydata->props))->brcRight.ico++;
	    }
	  text =
	      (char *)
	      wvMalloc (strlen
			(mydata->sd->elements[TT_BLACK +
					      ((((PAP
						  *) (mydata->props))->
						brcRight.ico -
						1) * 3)].str[0]) + 1);
	  strcpy (text,
		  mydata->sd->elements[TT_BLACK +
				       ((((PAP
					   *) (mydata->props))->brcRight.ico -
					 1) * 3)].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_BORDERBottomCOLOR:
	  if (isPAPConform (mydata->nextpap, (PAP *) (mydata->props)))
	    {
		if (((PAP *) (mydata->props))->brcBetween.ico == 0)
		  {
		      /* temp fix this in a while */
		      ((PAP *) (mydata->props))->brcBetween.ico++;
		  }
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BLACK +
						    ((((PAP
							*) (mydata->
							    props))->brcBetween.
						      ico - 1) * 3)].str[0]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BLACK +
					     ((((PAP
						 *) (mydata->props))->
					       brcBetween.ico -
					       1) * 3)].str[0]);
	    }
	  else
	    {
		if (((PAP *) (mydata->props))->brcBottom.ico == 0)
		  {
		      /* temp fix this in a while */
		      ((PAP *) (mydata->props))->brcBottom.ico++;
		  }
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_BLACK +
						    ((((PAP
							*) (mydata->
							    props))->brcBottom.
						      ico - 1) * 3)].str[0]) +
			      1);
		strcpy (text,
			mydata->sd->elements[TT_BLACK +
					     ((((PAP
						 *) (mydata->props))->
					       brcBottom.ico - 1) * 3)].str[0]);
	    }
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_START:
	  wvTrace (
		   ("a: HANDLING INDEX %d, %d %d\n",
		    (((PAP *) (mydata->props))->ilfo - 1) * 9 +
		    ((PAP *) (mydata->props))->ilvl,
		    ((PAP *) (mydata->props))->ilfo,
		    ((PAP *) (mydata->props))->ilvl));
	  wvTrace (
		   ("b: HANDLING INDEX %d, %d %d no is %d\n",
		    (ilfo - 1) * 9 + ilvl, ilfo, ilvl,
		    (*mydata->liststartnos)[(ilfo - 1) * 9 + ilvl]));

	  sprintf (buffer, "%d",
		   (*mydata->liststartnos)[(ilfo - 1) * 9 + ilvl]);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_nfc:
	  wvTrace (("nfc is %d\n", lvl.lvlf.nfc));
	  wvTrace (("nfc is %d\n", (*mydata->listnfcs)[(ilfo - 1) * 9 + ilvl]));
	  if ((*mydata->listnfcs)[(ilfo - 1) * 9 + ilvl] < 5)
	    {
		wvTrace (
			 ("str is %s\n",
			  mydata->sd->elements[TT_numbering].
			  str[(*mydata->listnfcs)[(ilfo - 1) * 9 + ilvl]]));
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_numbering].
			       str[(*mydata->listnfcs)[(ilfo - 1) * 9 + ilvl]])
			      + 1);
		strcpy (text,
			mydata->sd->elements[TT_numbering].
			str[(*mydata->listnfcs)[(ilfo - 1) * 9 + ilvl]]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
	    }
#if 0
	  if (lvl.lvlf.nfc < 5)
	    {
		wvTrace (
			 ("str is %s\n",
			  mydata->sd->elements[TT_numbering].str[lvl.lvlf.
								 nfc]));
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_numbering].
			       str[lvl.lvlf.nfc]) + 1);
		strcpy (text,
			mydata->sd->elements[TT_numbering].str[lvl.lvlf.nfc]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
	    }
#endif
	  break;
      case TT_ULISTE:
	  if ((ilfo != 0) && (ilfo != ((PAP *) (mydata->props))->ilfo))
	    {
		if (ulist)
		  {
		      while (ilvl >= 0)
			{
			    wvTrace (
				     ("str is %s\n",
				      mydata->sd->elements[TT_ULIST].str[1]));
			    text =
				(char *)
				wvMalloc (strlen
					  (mydata->sd->elements[TT_ULIST].
					   str[1]) + 1);
			    strcpy (text,
				    mydata->sd->elements[TT_ULIST].str[1]);
			    str = mydata->retstring;
			    wvExpand (mydata, text, strlen (text));
			    wvAppendStr (&str, mydata->retstring);
			    wvFree (mydata->retstring);
			    mydata->retstring = str;
			    wvFree (text);
			    mydata->currentlen = strlen (mydata->retstring);
			    ilvl--;
			}
		      ilfo = 0;
		      ilvl = -1;
		      wvReleaseLVL (&lvl);
		      wvInitLVL (&lvl);
		      ulist = 0;
		  }
	    }
	  break;
      case TT_ULISTB:
	  wvTrace (("ilfo is %d\n", ((PAP *) (mydata->props))->ilfo));
	  if (wvIsListEntry
	      ((PAP *) (mydata->props), wvQuerySupported (mydata->fib, NULL)))
	    {
		wvReleaseLVL (&lvl);
		wvInitLVL (&lvl);
		wvTrace (
			 ("getting ulist list entry %d %d\n",
			  ((PAP *) (mydata->props))->ilfo,
			  ((PAP *) (mydata->props))->ilvl));
		if (wvGetListEntryInfo
		    (wvQuerySupported (mydata->fib, NULL), mydata->finallvl,
		     mydata->liststartnos, mydata->listnfcs, &lvl, &retlfo,
		     (PAP *) (mydata->props), mydata->lfo, mydata->lfolvl,
		     mydata->lvl, mydata->nolfo, mydata->lst, mydata->noofLST))
		  {
		      wvError (
			       ("aborted list entry, more work needed obviously\n"));
		      return;
		  }
		else
		  {
		      U32 i;
		      wvTrace (
			       ("start number is %d, type is %d\n",
				lvl.lvlf.iStartAt, lvl.lvlf.nfc));
		      if ((*mydata->liststartnos)
			  [(((PAP *) (mydata->props))->ilfo - 1) * 9 +
			   ((PAP *) (mydata->props))->ilvl] == 0xffffffffL)
			{

			    (*mydata->liststartnos)[
						    (((PAP
						       *) (mydata->
							   props))->ilfo -
						     1) * 9 +
						    ((PAP
						      *) (mydata->
							  props))->ilvl] =
				lvl.lvlf.iStartAt;
			    (*mydata->
			     listnfcs)[(((PAP *) (mydata->props))->ilfo -
					1) * 9 +
				       ((PAP *) (mydata->props))->ilvl] =
				lvl.lvlf.nfc;
			    wvTrace (
				     ("start number set to %d\n",
				      (*mydata->
				       liststartnos)[(
						      ((PAP
							*) (mydata->props))->
						      ilfo - 1) * 9 +
((PAP *) (mydata->props))->ilvl]));
			    wvCopyLVL (&
				       ((*mydata->
					 finallvl)[(((PAP *) (mydata->props))->
						    ilfo - 1) * 9 +
						   ((PAP *) (mydata->props))->
						   ilvl]), &lvl);
			}
		      for (i = 0; i < *(mydata->nolfo) * 9; i++)
			{
			    if ((i % 9 > ((PAP *) (mydata->props))->ilvl)
				&& ((*mydata->finallvl)[i].lvlf.fNoRestart ==
				    0))
				(*mydata->liststartnos)[i] =
				    (*mydata->finallvl)[i].lvlf.iStartAt;
			}
		  }

		ilfo = ((PAP *) (mydata->props))->ilfo;

		if (((PAP *) (mydata->props))->ilvl > ilvl)
		    while (ilvl < ((PAP *) (mydata->props))->ilvl)
		      {
			  wvTrace (
				   ("str is %s\n",
				    mydata->sd->elements[TT_ULIST].str[0]));
			  text =
			      (char *)
			      wvMalloc (strlen
					(mydata->sd->elements[TT_ULIST].
					 str[0]) + 1);
			  strcpy (text, mydata->sd->elements[TT_ULIST].str[0]);
			  str = mydata->retstring;
			  wvExpand (mydata, text, strlen (text));
			  wvAppendStr (&str, mydata->retstring);
			  wvFree (mydata->retstring);
			  mydata->retstring = str;
			  wvFree (text);
			  mydata->currentlen = strlen (mydata->retstring);
			  ilvl++;
		      }
		else if (((PAP *) (mydata->props))->ilvl < ilvl)
		    while (ilvl > ((PAP *) (mydata->props))->ilvl)
		      {
			  wvTrace (
				   ("str is %s\n",
				    mydata->sd->elements[TT_ULIST].str[1]));
			  text =
			      (char *)
			      wvMalloc (strlen
					(mydata->sd->elements[TT_ULIST].
					 str[1]) + 1);
			  strcpy (text, mydata->sd->elements[TT_ULIST].str[1]);
			  str = mydata->retstring;
			  wvExpand (mydata, text, strlen (text));
			  wvAppendStr (&str, mydata->retstring);
			  wvFree (mydata->retstring);
			  mydata->retstring = str;
			  wvFree (text);
			  mydata->currentlen = strlen (mydata->retstring);
			  ilvl--;
		      }
		ilvl = ((PAP *) (mydata->props))->ilvl;
		lastid = retlfo->lsid;
		ulist = 1;
		wvTrace (
			 ("start number still set to %d\n",
			  (*mydata->liststartnos)[(ilfo - 1) * 9 + ilvl]));
	    }
	  break;
      case TT_OLISTE:
	  wvTrace (("ilfo 1 is %d\n", ((PAP *) (mydata->props))->ilfo));
	  wvTrace (("ilfo 2 is %d\n", ilfo));

	  if (wvIsListEntry
	      ((PAP *) (mydata->props), wvQuerySupported (mydata->fib, NULL)))
	    {
		wvReleaseLVL (&lvl);
		wvInitLVL (&lvl);
		wvGetListEntryInfo (wvQuerySupported (mydata->fib, NULL),
				    mydata->finallvl, mydata->liststartnos,
				    mydata->listnfcs, &lvl, &retlfo,
				    (PAP *) (mydata->props), mydata->lfo,
				    mydata->lfolvl, mydata->lvl,
				    mydata->nolfo, mydata->lst,
				    mydata->noofLST);
	    }

	  if ((ilfo != 0) && (ilfo != ((PAP *) (mydata->props))->ilfo))
	    {
		if (olist)
		  {
		      while (ilvl >= 0)
			{
			    wvTrace (
				     ("str is %s\n",
				      mydata->sd->elements[TT_OLIST].str[1]));
			    text =
				(char *)
				wvMalloc (strlen
					  (mydata->sd->elements[TT_OLIST].
					   str[1]) + 1);
			    strcpy (text,
				    mydata->sd->elements[TT_OLIST].str[1]);
			    str = mydata->retstring;
			    wvExpand (mydata, text, strlen (text));
			    wvAppendStr (&str, mydata->retstring);
			    wvFree (mydata->retstring);
			    mydata->retstring = str;
			    wvFree (text);
			    mydata->currentlen = strlen (mydata->retstring);
			    ilvl--;
			}
		      ilfo = 0;
		      ilvl = -1;
		      olist = 0;
		      wvReleaseLVL (&lvl);
		      wvInitLVL (&lvl);
		  }
	    }
	  break;
      case TT_TEXTB:
	  text =
	      (char *) wvMalloc (strlen (mydata->sd->elements[TT_TEXT].str[0])
				 + 1);
	  strcpy (text, mydata->sd->elements[TT_TEXT].str[0]);
	  str = mydata->retstring;
	  wvExpand (mydata, text, strlen (text));
	  wvAppendStr (&str, mydata->retstring);
	  wvFree (mydata->retstring);
	  mydata->retstring = str;
	  wvFree (text);
	  mydata->currentlen = strlen (mydata->retstring);
	  txt = 1;
	  break;
      case TT_TEXTE:
	  if (txt)
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_TEXT].str[1]) +
			      1);
		strcpy (text, mydata->sd->elements[TT_TEXT].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		txt = 0;
	    }
	  break;
      case TT_OLISTB:
	  if (wvIsListEntry
	      ((PAP *) (mydata->props), wvQuerySupported (mydata->fib, NULL)))
	    {
		wvReleaseLVL (&lvl);
		wvInitLVL (&lvl);
		wvTrace (
			 ("getting olist list entry %d %d\n",
			  ((PAP *) (mydata->props))->ilfo,
			  ((PAP *) (mydata->props))->ilvl));
		if (wvGetListEntryInfo
		    (wvQuerySupported (mydata->fib, NULL), mydata->finallvl,
		     mydata->liststartnos, mydata->listnfcs, &lvl, &retlfo,
		     (PAP *) (mydata->props), mydata->lfo, mydata->lfolvl,
		     mydata->lvl, mydata->nolfo, mydata->lst, mydata->noofLST))
		  {
		      wvError (
			       ("aborted list entry, more work needed obviously\n"));
		      return;
		  }
		else
		  {
		      U8 i2;
		      U8 tilvl = ((PAP *) (mydata->props))->ilvl;
		      wvTrace (
			       ("start number is %d, type is %d\n",
				lvl.lvlf.iStartAt, lvl.lvlf.nfc));
		      wvTrace (
			       ("lfo is %d, ilvi is %d\n",
				((PAP *) (mydata->props))->ilfo,
				((PAP *) (mydata->props))->ilvl));
		      wvTrace (
			       ("Start No is %d\n",
				(*mydata->liststartnos)[
							(((PAP
							   *) (mydata->props))->
							 ilfo - 1) * 9 +
							((PAP
							  *) (mydata->props))->
							ilvl]));

		      for (i2 = 0; i2 < tilvl + 1; i2++)
			{
			    LVL lvl2;
			    LFO *retlfo2;
			    ((PAP *) (mydata->props))->ilvl = i2;
			    wvTrace (
				     ("Level %d united (%d %d %d)\n", i2,
				      ((PAP *) (mydata->props))->ilfo, i2,
				      (((PAP *) (mydata->props))->ilfo -
				       1) * 9 + i2));
			    wvInitLVL (&lvl2);
			    wvGetListEntryInfo (wvQuerySupported
						(mydata->fib, NULL),
						mydata->finallvl,
						mydata->liststartnos,
						mydata->listnfcs, &lvl2,
						&retlfo2,
						(PAP *) (mydata->props),
						mydata->lfo, mydata->lfolvl,
						mydata->lvl, mydata->nolfo,
						mydata->lst, mydata->noofLST);

			    /* begin temp special case */
			    if ((i2 < tilvl)
				&&
				((*mydata->liststartnos)
				 [(((PAP *) (mydata->props))->ilfo - 1) * 9 +
				  i2] == 0xffffffffL))
			      {
				  wvTrace (("Force this Level to be inced\n"));
				  (*mydata->liststartnos)[
							  (((PAP
							     *)
							    (mydata->props))->
							   ilfo - 1) * 9 + i2] =
				      lvl2.lvlf.iStartAt + 1;
			      }
			    /* end temp special case */

			    (*mydata->listnfcs)[
						(((PAP *) (mydata->props))->ilfo
						 - 1) * 9 + i2] = lvl2.lvlf.nfc;
			    wvTrace (
				     ("Level %d united,nfc set to %d\n", i2,
				      lvl2.lvlf.nfc));
			    wvCopyLVL (&
				       ((*mydata->
					 finallvl)[(((PAP *) (mydata->props))->
						    ilfo - 1) * 9 + i2]),
&lvl2);
			    wvTrace (("here\n"));
			    wvReleaseLVL (&lvl2);
			}

		      ((PAP *) (mydata->props))->ilvl =
			  ((PAP *) (mydata->props))->ilvl;


		      if ((*mydata->liststartnos)
			  [(((PAP *) (mydata->props))->ilfo - 1) * 9 +
			   ((PAP *) (mydata->props))->ilvl] == 0xffffffffL)
			{

			    (*mydata->liststartnos)[
						    (((PAP
						       *) (mydata->
							   props))->ilfo -
						     1) * 9 +
						    ((PAP
						      *) (mydata->
							  props))->ilvl] =
				lvl.lvlf.iStartAt;
			    wvCopyLVL (&
				       ((*mydata->
					 finallvl)[(((PAP *) (mydata->props))->
						    ilfo - 1) * 9 +
						   ((PAP *) (mydata->props))->
						   ilvl]), &lvl);
			}

		      for (i = 0; i < (int) *(mydata->nolfo) * 9; i++)
			{
			    if ((i % 9 > ((PAP *) (mydata->props))->ilvl)
				&& ((*mydata->finallvl)[i].lvlf.fNoRestart ==
				    0))
				(*mydata->liststartnos)[i] =
				    (*mydata->finallvl)[i].lvlf.iStartAt;
			}

		      if ((lvl.numbertext == NULL) || (lvl.numbertext[0] <= 1))
			{
			    return;
			}

		  }

		ilfo = ((PAP *) (mydata->props))->ilfo;

		if ((((PAP *) (mydata->props))->ilvl == ilvl)
		    && (retlfo->lsid != lastid))
		  {
		      if (ulist)
			{
			    wvTrace (
				     ("str is %s\n",
				      mydata->sd->elements[TT_ULIST].str[1]));
			    text =
				(char *)
				wvMalloc (strlen
					  (mydata->sd->elements[TT_ULIST].
					   str[1]) + 1);
			    strcpy (text,
				    mydata->sd->elements[TT_ULIST].str[1]);
			    str = mydata->retstring;
			    wvExpand (mydata, text, strlen (text));
			    wvAppendStr (&str, mydata->retstring);
			    wvFree (mydata->retstring);
			    mydata->retstring = str;
			    wvFree (text);
			    mydata->currentlen = strlen (mydata->retstring);
			}
		      else if (olist)
			{
			    wvTrace (
				     ("str is %s\n",
				      mydata->sd->elements[TT_OLIST].str[1]));
			    text =
				(char *)
				wvMalloc (strlen
					  (mydata->sd->elements[TT_OLIST].
					   str[1]) + 1);
			    strcpy (text,
				    mydata->sd->elements[TT_OLIST].str[1]);
			    str = mydata->retstring;
			    wvExpand (mydata, text, strlen (text));
			    wvAppendStr (&str, mydata->retstring);
			    wvFree (mydata->retstring);
			    mydata->retstring = str;
			    wvFree (text);
			    mydata->currentlen = strlen (mydata->retstring);
			}
		      ilvl--;
		  }


		if (((PAP *) (mydata->props))->ilvl > ilvl)
		    while (ilvl < ((PAP *) (mydata->props))->ilvl)
		      {
			  ilvl++;
			  wvTrace (
				   ("2: HANDLING INDEX %d\n",
				    (((PAP *) (mydata->props))->ilfo -
				     1) * 9 + ((PAP *) (mydata->props))->ilvl));
			  wvTrace (
				   ("3: HANDLING INDEX %d nfc is %d\n",
				    (ilfo - 1) * 9 + ilvl,
				    (*mydata->listnfcs)[(ilfo - 1) * 9 +
							ilvl]));

			  wvTrace (
				   ("str is %s\n",
				    mydata->sd->elements[TT_OLIST].str[0]));
			  text =
			      (char *)
			      wvMalloc (strlen
					(mydata->sd->elements[TT_OLIST].
					 str[0]) + 1);
			  strcpy (text, mydata->sd->elements[TT_OLIST].str[0]);
			  str = mydata->retstring;
			  wvExpand (mydata, text, strlen (text));
			  wvAppendStr (&str, mydata->retstring);
			  wvFree (mydata->retstring);
			  mydata->retstring = str;
			  wvFree (text);
			  mydata->currentlen = strlen (mydata->retstring);
		      }
		else if (((PAP *) (mydata->props))->ilvl < ilvl)
		    while (ilvl > ((PAP *) (mydata->props))->ilvl)
		      {
			  ilvl--;
			  wvTrace (
				   ("str is %s\n",
				    mydata->sd->elements[TT_OLIST].str[1]));
			  text =
			      (char *)
			      wvMalloc (strlen
					(mydata->sd->elements[TT_OLIST].
					 str[1]) + 1);
			  strcpy (text, mydata->sd->elements[TT_OLIST].str[1]);
			  str = mydata->retstring;
			  wvExpand (mydata, text, strlen (text));
			  wvAppendStr (&str, mydata->retstring);
			  wvFree (mydata->retstring);
			  mydata->retstring = str;
			  wvFree (text);
			  mydata->currentlen = strlen (mydata->retstring);
		      }
		ilvl = ((PAP *) (mydata->props))->ilvl;
		lastid = retlfo->lsid;
		olist = 1;
		wvTrace (
			 ("start number still set to %d\n",
			  (*mydata->liststartnos)[(ilfo - 1) * 9 + ilvl]));
	    }
	  break;
      case TT_ENTRYB:
	  fflush (stdout);
	  wvTrace (("ilvl is %d %d\n", ((PAP *) (mydata->props))->ilvl, ilvl));
	  if (ilfo)
	    {
		wvTrace (
			 ("str is %s\n",
			  mydata->sd->elements[TT_ENTRY].str[0]));
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_ENTRY].str[0]) +
			      1);
		strcpy (text, mydata->sd->elements[TT_ENTRY].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
	    }
	  break;
      case TT_ENTRYE:
	  wvTrace (("ilvl is %d %d\n", ((PAP *) (mydata->props))->ilvl, ilvl));
	  if (ilfo)
	    {
		wvTrace (
			 ("str is %s\n",
			  mydata->sd->elements[TT_ENTRY].str[1]));
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_ENTRY].str[1]) +
			      1);
		strcpy (text, mydata->sd->elements[TT_ENTRY].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		wvTrace (("no in list is %d\n", 9 * (*(mydata->nolfo))));
		(*mydata->liststartnos)[(ilfo - 1) * 9 + ilvl]++;
	    }
	  break;
      case TT_BOLDB:
	  HANDLE_B_CHAR_ELE (TT_BOLD, fBold, bold, 1) break;
      case TT_DispFldRMarkB:
	  HANDLE_B_CHAR_ELE (TT_DispFldRMark, fDispFldRMark, FldRMark, 1) break;
      case TT_RMarkDelB:
	  HANDLE_B_CHAR_ELE (TT_RMarkDel, fRMarkDel, deleted, 1) break;
      case TT_OUTLINEB:
	  HANDLE_B_CHAR_ELE (TT_OUTLINE, fOutline, outline, 1) break;
      case TT_STRIKEB:
	  HANDLE_B_CHAR_ELE (TT_STRIKE, fStrike, strike, 1) break;
      case TT_ITALICB:
	  HANDLE_B_CHAR_ELE (TT_ITALIC, fItalic, italic, 1) break;
      case TT_RMarkDelE:
	  HANDLE_E_CHAR_ELE (TT_RMarkDel, fRMarkDel, deleted, 1) break;
      case TT_DispFldRMarkE:
	  HANDLE_E_CHAR_ELE (TT_DispFldRMark, fDispFldRMark, FldRMark, 1) break;
      case TT_OUTLINEE:
	  HANDLE_E_CHAR_ELE (TT_OUTLINE, fOutline, outline, 1) break;
      case TT_STRIKEE:
	  HANDLE_E_CHAR_ELE (TT_STRIKE, fStrike, strike, 1) break;
      case TT_ITALICE:
	  HANDLE_E_CHAR_ELE (TT_ITALIC, fItalic, italic, 1) break;
      case TT_BOLDE:
	  HANDLE_E_CHAR_ELE (TT_BOLD, fBold, bold, 1) break;
      case TT_SMALLCAPSB:
	  HANDLE_B_CHAR_ELE (TT_SMALLCAPS, fSmallCaps, smallcaps, 1) break;
      case TT_SMALLCAPSE:
	  HANDLE_E_CHAR_ELE (TT_SMALLCAPS, fSmallCaps, smallcaps, 1) break;
      case TT_CAPSB:
	  HANDLE_B_CHAR_ELE (TT_CAPS, fCaps, caps, 1) break;
      case TT_CAPSE:
	  HANDLE_E_CHAR_ELE (TT_CAPS, fCaps, caps, 1) break;
      case TT_VANISHB:
	  HANDLE_B_CHAR_ELE (TT_VANISH, fVanish, vanish, 1) break;
      case TT_VANISHE:
	  HANDLE_E_CHAR_ELE (TT_VANISH, fVanish, vanish, 1) break;
      case TT_RMarkB:
	  HANDLE_B_CHAR_ELE (TT_RMark, fRMark, added, 1) break;
      case TT_RMarkE:
	  HANDLE_E_CHAR_ELE (TT_RMark, fRMark, added, 1) break;
      case TT_SHADOWB:
	  HANDLE_B_CHAR_ELE (TT_SHADOW, fShadow, shadow, 1) break;
      case TT_SHADOWE:
	  HANDLE_E_CHAR_ELE (TT_SHADOW, fShadow, shadow, 1) break;
      case TT_LOWERCASEB:
	  HANDLE_B_CHAR_ELE (TT_LOWERCASE, fLowerCase, lowercase, 1) break;
      case TT_LOWERCASEE:
	  HANDLE_E_CHAR_ELE (TT_LOWERCASE, fLowerCase, lowercase, 1) break;
      case TT_EMBOSSB:
	  HANDLE_B_CHAR_ELE (TT_EMBOSS, fEmboss, emboss, 1) break;
      case TT_EMBOSSE:
	  HANDLE_E_CHAR_ELE (TT_EMBOSS, fEmboss, emboss, 1) break;
      case TT_IMPRINTB:
	  HANDLE_B_CHAR_ELE (TT_IMPRINT, fImprint, imprint, 1) break;
      case TT_IMPRINTE:
	  HANDLE_E_CHAR_ELE (TT_IMPRINT, fImprint, imprint, 1) break;
      case TT_DSTRIKEB:
	  HANDLE_B_CHAR_ELE (TT_DSTRIKE, fDStrike, dstrike, 1) break;
      case TT_DSTRIKEE:
	  HANDLE_E_CHAR_ELE (TT_DSTRIKE, fDStrike, dstrike, 1) break;
      case TT_SUPERB:
      case TT_SUBB:
	  HANDLE_B_CHAR_ELE (token_type - 1, iss, iss,
			     (U32) (token_type - TT_SUPERB) / 3 +
			     1) break;
      case TT_SUPERE:
      case TT_SUBE:
	  HANDLE_E_CHAR_ELE (token_type - 2, iss, iss,
			     (token_type - TT_SUPERE) / 3 +
			     1) break;

      case TT_SINGLEUB:
      case TT_WORDUB:
      case TT_DOUBLEUB:
      case TT_DOTTEDUB:
      case TT_HIDDENUB:
      case TT_THICKUB:
      case TT_DASHUB:
      case TT_DOTUB:
      case TT_DOTDASHUB:
      case TT_DOTDOTDASHUB:
      case TT_WAVEUB:
	  HANDLE_B_CHAR_ELE (token_type - 1, kul, kul,
			     (U32) (token_type - TT_SINGLEUB) / 3 +
			     1) break;

      case TT_BLACKB:
      case TT_BLUEB:
      case TT_CYANB:
      case TT_GREENB:
      case TT_MAGENTAB:
      case TT_REDB:
      case TT_YELLOWB:
      case TT_WHITEB:
      case TT_DKBLUEB:
      case TT_DKCYANB:
      case TT_DKGREENB:
      case TT_DKMAGENTAB:
      case TT_DKREDB:
      case TT_DKYELLOWB:
      case TT_DKGRAYB:
      case TT_LTGRAYB:
	  HANDLE_B_CHAR_ELE (token_type - 1, ico, color,
			     (U32) (token_type - TT_BLACKB) / 3 +
			     1) break;


      case TT_SINGLEUE:
      case TT_WORDUE:
      case TT_DOUBLEUE:
      case TT_DOTTEDUE:
      case TT_HIDDENUE:
      case TT_THICKUE:
      case TT_DASHUE:
      case TT_DOTUE:
      case TT_DOTDASHUE:
      case TT_DOTDOTDASHUE:
      case TT_WAVEUE:
	  HANDLE_E_CHAR_ELE (token_type - 2, kul, kul,
			     (token_type - TT_SINGLEUE) / 3 +
			     1) break;

      case TT_BLACKE:
      case TT_BLUEE:
      case TT_CYANE:
      case TT_GREENE:
      case TT_MAGENTAE:
      case TT_REDE:
      case TT_YELLOWE:
      case TT_WHITEE:
      case TT_DKBLUEE:
      case TT_DKCYANE:
      case TT_DKGREENE:
      case TT_DKMAGENTAE:
      case TT_DKREDE:
      case TT_DKYELLOWE:
      case TT_DKGRAYE:
      case TT_LTGRAYE:
	  HANDLE_E_CHAR_ELE (token_type - 2, ico, color,
			     (token_type - TT_BLACKE) / 3 +
			     1) break;

      case TT_LasVegasB:
      case TT_BackgroundBlinkB:
      case TT_SparkleTextB:
      case TT_MarchingAntsB:
      case TT_MarchingRedAntsB:
      case TT_ShimmerB:
	  HANDLE_B_CHAR_ELE (token_type - 1, sfxtText,
			     animation,
			     (token_type -
			      TT_LasVegasB) / 3 + 1) break;

      case TT_LasVegasE:
      case TT_BackgroundBlinkE:
      case TT_SparkleTextE:
      case TT_MarchingAntsE:
      case TT_MarchingRedAntsE:
      case TT_ShimmerE:
	  HANDLE_E_CHAR_ELE (token_type - 2, sfxtText,
			     animation,
			     (token_type -
			      TT_LasVegasE) / 3 + 1) break;

      case TT_FONTSTRB:
	  wvTrace (("flag is %d\n", ((CHP *) (mydata->props))->ico));
	  wvTrace (("str is %s\n", mydata->sd->elements[TT_FONTSTR].str[0]));
	  if ((((CHP *) (mydata->props))->ico) && (fontstr == 0))
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_FONTSTR].str[0])
			      + 1);
		strcpy (text, mydata->sd->elements[TT_FONTSTR].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		fontstr = 1;
	    }

	  break;
      case TT_FONTSTRE:
	  wvTrace (("str is %s\n", mydata->sd->elements[TT_FONTSTR].str[0]));
	  if (fontstr)
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_FONTSTR].str[1])
			      + 1);
		strcpy (text, mydata->sd->elements[TT_FONTSTR].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		fontstr = 0;
	    }
	  break;

      case TT_ANIMATIONB:
	  wvTrace (("flag is %d\n", ((CHP *) (mydata->props))->sfxtText));
	  wvTrace (("str is %s\n", mydata->sd->elements[TT_ANIMATION].str[0]));
	  if ((((CHP *) (mydata->props))->sfxtText) && (animation == 0))
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_ANIMATION].str[0]) + 1);
		strcpy (text, mydata->sd->elements[TT_ANIMATION].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		animation = 1;
	    }

	  break;
      case TT_ANIMATIONE:
	  wvTrace (("str is %s\n", mydata->sd->elements[TT_ANIMATION].str[0]));
	  if (animation)
	    {
		text =
		    (char *)
		    wvMalloc (strlen
			      (mydata->sd->elements[TT_ANIMATION].str[1]) + 1);
		strcpy (text, mydata->sd->elements[TT_ANIMATION].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		animation = 0;
	    }
	  break;

      case TT_IBSTANNO:
	  sprintf (buffer, "%d", ((ATRD *) (mydata->props))->ibst);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;

      case TT_xstUsrInitl:
	  str = wvWideStrToMB (((ATRD *) (mydata->props))->xstUsrInitl + 1);
	  sprintf (buffer, "%s", str);
	  wvFree (str);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmParaBefore:
	  if (fintable)
	    {
		if ((mydata->sd->elements[TT_TABLEOVERRIDES].str)
		    && (mydata->sd->elements[TT_TABLEOVERRIDES].str[0]))
		  {
		      text =
			  (char *)
			  wvMalloc (strlen
				    (mydata->sd->elements[TT_TABLEOVERRIDES].
				     str[0]) + 1);
		      strcpy (text,
			      mydata->sd->elements[TT_TABLEOVERRIDES].str[0]);
		      str = mydata->retstring;

		      wvExpand (mydata, text, strlen (text));
		      sprintf (buffer, "%.2fmm",
			       (double) wvTwipsToMM ( (S16) atoi (mydata->retstring)));
		      wvFree (mydata->retstring);

		      mydata->retstring = str;
		      wvFree (text);
		      wvAppendStr (&mydata->retstring, buffer);
		      mydata->currentlen = strlen (mydata->retstring);
		      break;
		  }
	    }
	  sprintf (buffer, "%.2fmm",
		   (double) wvTwipsToMM ( (S16) ((PAP *) (mydata->props))->dyaBefore));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmParaAfter:
	  if (fintable)
	    {
		if ((mydata->sd->elements[TT_TABLEOVERRIDES].str)
		    && (mydata->sd->elements[TT_TABLEOVERRIDES].str[2]))
		  {
		      text =
			  (char *)
			  wvMalloc (strlen
				    (mydata->sd->elements[TT_TABLEOVERRIDES].
				     str[2]) + 1);
		      strcpy (text,
			      mydata->sd->elements[TT_TABLEOVERRIDES].str[2]);
		      str = mydata->retstring;

		      wvExpand (mydata, text, strlen (text));
		      sprintf (buffer, "%.2fmm",
			       (double) wvTwipsToMM ( (S16) atoi (mydata->retstring)));
		      wvFree (mydata->retstring);

		      mydata->retstring = str;
		      wvFree (text);
		      wvAppendStr (&mydata->retstring, buffer);
		      mydata->currentlen = strlen (mydata->retstring);
		      break;
		  }
	    }
	  sprintf (buffer, "%.2fmm",
		   (double) wvTwipsToMM ( (S16) ((PAP *) (mydata->props))->dyaAfter));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmParaLeft:
	  if (fintable)
	    {
		if ((mydata->sd->elements[TT_TABLEOVERRIDES].str)
		    && (mydata->sd->elements[TT_TABLEOVERRIDES].str[3]))
		  {
		      text =
			  (char *)
			  wvMalloc (strlen
				    (mydata->sd->elements[TT_TABLEOVERRIDES].
				     str[3]) + 1);
		      strcpy (text,
			      mydata->sd->elements[TT_TABLEOVERRIDES].str[3]);
		      str = mydata->retstring;

		      wvExpand (mydata, text, strlen (text));
		      sprintf (buffer, "%.2fmm",
			       (double) wvTwipsToMM ( (S16) atoi (mydata->retstring)));
		      wvFree (mydata->retstring);

		      mydata->retstring = str;
		      wvFree (text);
		      wvAppendStr (&mydata->retstring, buffer);
		      mydata->currentlen = strlen (mydata->retstring);
		      break;
		  }
	    }
	  sprintf (buffer, "%.2fmm",
		   (double) wvTwipsToMM ( (S16) ((PAP *) (mydata->props))->dxaLeft));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
	  /* 	>> PATCH -----------------------  */
	case  TT_stylename:
	    pap=(PAP *)(mydata->props);
#if 0
	    wvAppendStr (&mydata->retstring, pap->stylename);
#else
	    wvAppendStr(&mydata->retstring,
			wvConvertStylename(pap->stylename, mydata->charset));
#endif
	    mydata->currentlen = strlen (mydata->retstring);
	  break;

	  /*  	-------------------------<<   */
      case TT_mmParaRight:

	/*  printf("name: %s \n",mydata->stsh->std[0].xstzName);*/
/*pap=(PAP *)(mydata->props);
	 printf("name: %s \n",(char *)(pap->stylename));*/


	  if (fintable)
	    {
		if ((mydata->sd->elements[TT_TABLEOVERRIDES].str)
		    && (mydata->sd->elements[TT_TABLEOVERRIDES].str[1]))
		  {
		      text =
			  (char *)
			  wvMalloc (strlen
				    (mydata->sd->elements[TT_TABLEOVERRIDES].
				     str[1]) + 1);
		      strcpy (text,
			      mydata->sd->elements[TT_TABLEOVERRIDES].str[1]);
		      str = mydata->retstring;

		      wvExpand (mydata, text, strlen (text));
		      sprintf (buffer, "%.2fmm",
			       (double) wvTwipsToMM ( (S16) atoi (mydata->retstring)));
		      wvFree (mydata->retstring);

		      mydata->retstring = str;
		      wvFree (text);
		      wvAppendStr (&mydata->retstring, buffer);
		      mydata->currentlen = strlen (mydata->retstring);
		      break;
		  }
	    }
	  sprintf (buffer, "%.2fmm",
		   (double) wvTwipsToMM ( (S16) ((PAP *) (mydata->props))->dxaRight));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmParaLeft1:
	  if (fintable)
	    {
		if ((mydata->sd->elements[TT_TABLEOVERRIDES].str)
		    && (mydata->sd->elements[TT_TABLEOVERRIDES].str[4]))
		  {
		      text =
			  (char *)
			  wvMalloc (strlen
				    (mydata->sd->elements[TT_TABLEOVERRIDES].
				     str[4]) + 1);
		      strcpy (text,
			      mydata->sd->elements[TT_TABLEOVERRIDES].str[4]);
		      str = mydata->retstring;

		      wvExpand (mydata, text, strlen (text));
		      sprintf (buffer, "%.2fmm",
			       (double) wvTwipsToMM ( (S16) atoi (mydata->retstring)));
		      wvFree (mydata->retstring);

		      mydata->retstring = str;
		      wvFree (text);
		      wvAppendStr (&mydata->retstring, buffer);
		      mydata->currentlen = strlen (mydata->retstring);
		      break;
		  }
	    }
	  sprintf (buffer, "%.2fmm",
		   (double) wvTwipsToMM ( (S16) ((PAP *) (mydata->props))->dxaLeft1));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmPadTop:
	  if (isPAPConform (&mydata->lastpap, (PAP *) (mydata->props)))
	      sprintf (buffer, "%.2fmm",
		       (double)
		       wvPointsToMM ( (S16) ((PAP *) (mydata->props))->
				     brcBetween.dptSpace));
	  else
	      sprintf (buffer, "%.2fmm",
		       (double) wvPointsToMM ( (S16) ((PAP *) (mydata->props))->
					      brcTop.dptSpace));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmPadRight:


	  sprintf (buffer, "%.2fmm",
		   (double) wvPointsToMM ( (S16) ((PAP *) (mydata->props))->brcRight.
					  dptSpace));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmPadBottom:
	  if (isPAPConform (mydata->nextpap, (PAP *) (mydata->props)))
	      sprintf (buffer, "%.2fmm",
		       (double)
		       wvPointsToMM ( (S16) ((PAP *) (mydata->props))->
				     brcBetween.dptSpace));
	  else
	      sprintf (buffer, "%.2fmm",
		       (double) wvPointsToMM ( (S16) ((PAP *) (mydata->props))->
					      brcBottom.dptSpace));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmPadLeft:
	  sprintf (buffer, "%.2fmm",
		   (double) wvPointsToMM ( (S16) ((PAP *) (mydata->props))->brcLeft.
					  dptSpace));
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_mmLineHeight:
	  if (wvTwipsToMM (((PAP *) (mydata->props))->lspd.dyaLine))
	      sprintf (buffer, "%fmm",
		       fabs (wvTwipsToMM
			     (((PAP *) (mydata->props))->lspd.dyaLine)));
	  else
	      sprintf (buffer, "normal");
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;

      case TT_pixPicWidth:
	  {
	      FSPA *fspa = (FSPA *) (mydata->props);
	      sprintf (buffer, "%d",
		       (int) (wvTwipsToHPixels ( (S16) ( fspa->xaRight - fspa->xaLeft ) )
			      + (float) 0.5));
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_pixPicHeight:
	  {
	      FSPA *fspa = (FSPA *) (mydata->props);
	      sprintf (buffer, "%d",
		       (int) (wvTwipsToVPixels ( (S16) ( fspa->yaBottom - fspa->yaTop ) )
			      + (float) 0.5));
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_htmlNextLineGuess:
	  {
	      FSPA *fspa = (FSPA *) (mydata->props);
	      if (fspa->wr == 1)
		  sprintf (buffer, "<br>");
	      else
		  sprintf (buffer, "\n");
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_htmlAlignGuess:
	  {
	      FSPA *fspa = (FSPA *) (mydata->props);
	      switch (fspa->wr)
		{
		default:
		case 1:
		case 3:
		    buffer[0] = '\0';
		    break;
		case 0:
		case 5:
		    sprintf (buffer, "align=\"left\"");
		    break;
		case 2:
		case 4:
		    if (fspa->wrk == 1)
			sprintf (buffer, "align=\"right\"");
		    else
			sprintf (buffer, "align=\"left\"");
		    break;
		}
	      wvAppendStr (&mydata->retstring, buffer);
	      mydata->currentlen = strlen (mydata->retstring);
	  }
	  break;
      case TT_ibstRMark:
	  sprintf (buffer, "%d", ((CHP *) (mydata->props))->ibstRMark);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_ibstRMarkDel:
	  sprintf (buffer, "%d", ((CHP *) (mydata->props))->ibstRMarkDel);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_ibstDispFldRMark:
	  sprintf (buffer, "%d", ((CHP *) (mydata->props))->ibstDispFldRMark);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_dttmRMark:
	  wvAppendStr (&mydata->retstring,
		       wvDTTMtoUnix (&(((CHP *) (mydata->props))->dttmRMark)));
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_dttmRMarkDel:
	  wvAppendStr (&mydata->retstring,
		       wvDTTMtoUnix (&
				     (((CHP
					*) (mydata->props))->dttmRMarkDel)));
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_dttmDispFldRMark:
	  wvAppendStr (&mydata->retstring,
		       wvDTTMtoUnix (&
				     (((CHP
					*) (mydata->props))->
				      dttmDispFldRMark)));
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_xstDispFldRMark:
	  text = wvWideStrToMB (((CHP *) (mydata->props))->xstDispFldRMark);
	  wvAppendStr (&mydata->retstring, text);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_PropRMarkB:
	  HANDLE_B_CHAR_ELE (TT_PropRMark, fPropRMark, proprmark, 1) break;
      case TT_PropRMarkE:
	  HANDLE_E_CHAR_ELE (TT_PropRMark, fPropRMark, proprmark, 1) break;
      case TT_ibstPropRMark:
	  wvTrace (("flag is %d\n", ((CHP *) (mydata->props))->ibstPropRMark));
	  sprintf (buffer, "%d", ((CHP *) (mydata->props))->ibstPropRMark);
	  wvAppendStr (&mydata->retstring, buffer);
	  mydata->currentlen = strlen (mydata->retstring);
	  break;
      case TT_dttmPropRMark:
	  wvAppendStr (&mydata->retstring,
		       wvDTTMtoUnix (&
				     (((CHP
					*) (mydata->props))->dttmPropRMark)));
	  mydata->currentlen = strlen (mydata->retstring);
	  break;

      case TT_TABLEB:
	  if ((((PAP *) (mydata->props))->fInTable) && (table == 0))
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_TABLE].str[0]) +
			      1);
		strcpy (text, mydata->sd->elements[TT_TABLE].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		table = 1;
	    }
	  break;
      case TT_ROWB:
	  if ((((PAP *) (mydata->props))->fInTable)
	      && (((PAP *) (mydata->props))->fTtp == 0) && (fttp == 1))
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_ROW].str[0]) + 1);
		strcpy (text, mydata->sd->elements[TT_ROW].str[0]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		fttp = 0;
	    }
	  break;
      case TT_CELLB:
	  if ((fintable != 1) && (((PAP *) (mydata->props))->fInTable == 1))
	    {
		wvTrace (("the current cell is %d\n", mydata->whichcell));
		wvTrace (
			 ("the end boundary is %d\n",
			  ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
								      whichcell
								      + 1]));
		wvTrace (
			 ("the table look for this cell is %d\n",
			  ((PAP *) (mydata->props))->ptap.tlp.itl));
		if (((PAP *) (mydata->props))->ptap.tlp.itl)
		  {
		      wvTrace (
			       ("table look is %d\n",
				((PAP *) (mydata->props))->ptap.tlp.itl));
		  }
	    HANDLE_B_PARA_ELE (TT_CELL, fInTable, fintable, 1)}
	  break;
      case TT_CELLE:
	  if (*(mydata->endcell))
	    {
		if (fintable == 1)
		  {
		      mydata->whichcell++;
		      wvTrace (("inc whichcell to %d\n", mydata->whichcell));
		  }
		HANDLE_E_PARA_ELE (TT_CELL, fInTable, fintable, 1)
		    * (mydata->endcell) = 0;
	    }
	  break;
      case TT_LASTCELLB:
	  if ((fintable != 1) && (((PAP *) (mydata->props))->fInTable == 1)
	      && (mydata->whichcell ==
		  ((PAP *) (mydata->props))->ptap.itcMac - 1))
	    {
		wvTrace (("the current cell is %d\n", mydata->whichcell));
		wvTrace (
			 ("the end boundary is %d\n",
			  ((PAP *) (mydata->props))->ptap.rgdxaCenter[mydata->
								      whichcell
								      + 1]));
		wvTrace (
			 ("the table look for this cell is %d\n",
			  ((PAP *) (mydata->props))->ptap.tlp.itl));
		if (((PAP *) (mydata->props))->ptap.tlp.itl)
		  {
		      wvTrace (
			       ("table look is %d\n",
				((PAP *) (mydata->props))->ptap.tlp.itl));
		  }
		HANDLE_B_PARA_ELE (TT_LASTCELL, fInTable, fintable, 1)
		    lastcell = 1;
	    }
	  break;
      case TT_LASTCELLE:
	  if ((*(mydata->endcell)) && (lastcell))
	    {
		if (fintable == 1)
		  {
		      mydata->whichcell++;
		      wvTrace (("inc whichcell to %d\n", mydata->whichcell));
		  }
		HANDLE_E_PARA_ELE (TT_LASTCELL, fInTable, fintable, 1)
		    * (mydata->endcell) = 0;
		lastcell = 0;
	    }
	  break;
      case TT_ROWE:
	  if ((((PAP *) (mydata->props))->fTtp == 1) && (fttp == 0))
	    {
		mydata->whichrow++;
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_ROW].str[1]) + 1);
		strcpy (text, mydata->sd->elements[TT_ROW].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		mydata->whichcell = 0;
		fttp = 1;
	    }
	  break;

      case TT_TABLEE:
	  if ((((PAP *) (mydata->props))->fInTable == 0) && (table == 1))
	    {
		text =
		    (char *)
		    wvMalloc (strlen (mydata->sd->elements[TT_TABLE].str[1]) +
			      1);
		strcpy (text, mydata->sd->elements[TT_TABLE].str[1]);
		str = mydata->retstring;
		wvExpand (mydata, text, strlen (text));
		wvAppendStr (&str, mydata->retstring);
		wvFree (mydata->retstring);
		mydata->retstring = str;
		wvFree (text);
		mydata->currentlen = strlen (mydata->retstring);
		table = 0;
		mydata->whichrow = 0;
		*(mydata->intable) = 0;
	    }
	  break;

      }

}

static void
wvstartElement (void *userData, const XML_Char *name, const XML_Char **atts)
{
    unsigned int nAtts = 0;
    const XML_Char **p;
    unsigned int token_type, i;
    state_data *mydata = (state_data *) userData;
    if (atts)
      {
	p = atts;
	while (*p)
	  ++p;
	nAtts = (p - atts) >> 1;
      }
/*  tokenIndex = s_mapNameToToken ((const char *) name);
    token_type = s_Tokens[tokenIndex].m_type;
 */ token_type = wvMapNameToTokenType ((const char *) name);
/*    printf("start: %s \n",name);*/

    wvTrace (("element %s started\n", name));
    switch (token_type)
      {
      case TT_DOCUMENT:
      case TT_PARA:
      case TT_COMMENT:
      case TT_SECTION:
      case TT_BOLD:
      case TT_ITALIC:
      case TT_STRIKE:
      case TT_RMarkDel:
      case TT_OUTLINE:
      case TT_SMALLCAPS:
      case TT_CAPS:
      case TT_VANISH:
      case TT_RMark:
      case TT_SHADOW:
      case TT_LOWERCASE:
      case TT_EMBOSS:
      case TT_IMPRINT:
      case TT_DSTRIKE:
      case TT_SUPER:
      case TT_SUB:
      case TT_SINGLEU:
      case TT_WORDU:
      case TT_DOUBLEU:
      case TT_DOTTEDU:
      case TT_HIDDENU:
      case TT_THICKU:
      case TT_DASHU:
      case TT_DOTU:
      case TT_DOTDASHU:
      case TT_DOTDOTDASHU:
      case TT_WAVEU:
      case TT_BLACK:
      case TT_BLUE:
      case TT_CYAN:
      case TT_GREEN:
      case TT_MAGENTA:
      case TT_RED:
      case TT_YELLOW:
      case TT_WHITE:
      case TT_DKBLUE:
      case TT_DKCYAN:
      case TT_DKGREEN:
      case TT_DKMAGENTA:
      case TT_DKRED:
      case TT_DKYELLOW:
      case TT_DKGRAY:
      case TT_LTGRAY:
      case TT_FONTSTR:
      case TT_ANIMATION:
      case TT_PropRMark:
      case TT_LasVegas:
      case TT_BackgroundBlink:
      case TT_SparkleText:
      case TT_MarchingAnts:
      case TT_MarchingRedAnts:
      case TT_Shimmer:
      case TT_DispFldRMark:
      case TT_OLIST:
      case TT_TEXT:
      case TT_ULIST:
      case TT_ENTRY:
      case TT_TABLE:
      case TT_ROW:
      case TT_CELL:
      case TT_LASTCELL:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 2);
	  mydata->elements[token_type].nostr = 2;
	  for (i = 0; i < 2; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
      case TT_PICTURE:
      case TT_CHARENTITY:
      case TT_PMARGIN:
      case TT_PBORDER:


	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 1);
	  mydata->elements[token_type].nostr = 1;
	  mydata->elements[token_type].str[0] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
      case TT_CHAR:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 2);
	  mydata->elements[token_type].nostr = 2;
	  for (i = 0; i < 2; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
	  break;
      case TT_JUSTIFICATION:
      case TT_numbering:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 5);
	  mydata->elements[token_type].nostr = 5;
	  for (i = 0; i < 5; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
      case TT_TABLEOVERRIDES:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 6);
	  mydata->elements[token_type].nostr = 6;
	  for (i = 0; i < 6; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
      case TT_COLOR:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 16);
	  mydata->elements[token_type].nostr = 16;
	  for (i = 0; i < 16; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;
      case TT_BORDER:
	  mydata->elements[token_type].str =
	      (char **) wvMalloc (sizeof (char *) * 27);
	  mydata->elements[token_type].nostr = 27;
	  for (i = 0; i < 27; i++)
	      mydata->elements[token_type].str[i] = NULL;
	  mydata->currentele = &(mydata->elements[token_type]);
	  break;

      case TT_STYLE:
	  wvTrace (("style element, no atts is %d\n", nAtts));

	  for (i = 0; i < nAtts; i++) {
	      wvTrace (("%s is %s\n", atts[i * 2], atts[(i * 2) + 1]));
	}

	  break;

      case TT_BEGIN:
      case TT_LEFT:
      case TT_Arabic:
      case TT_NONED:
      case TT_ParaBefore:
	  mydata->current = &(mydata->currentele->str[0]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_END:
      case TT_CENTER:
      case TT_UpperRoman:
      case TT_SINGLED:
      case TT_ParaRight:
	  mydata->current = &(mydata->currentele->str[1]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_RIGHT:
      case TT_LowerRoman:
      case TT_THICKD:
      case TT_ParaAfter:
	  mydata->current = &(mydata->currentele->str[2]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BLOCK:
      case TT_UpperCaseN:
      case TT_DOUBLED:
      case TT_ParaLeft:
	  mydata->current = &(mydata->currentele->str[3]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ASIAN:
      case TT_LowerCaseN:
      case TT_NUMBER4D:
      case TT_ParaLeft1:
	  mydata->current = &(mydata->currentele->str[4]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_VertMergedCells:
	  mydata->current = &(mydata->currentele->str[5]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_HAIRLINED:
      case TT_DOTD:
      case TT_DASHLARGEGAPD:
      case TT_DOTDASHD:
      case TT_DOTDOTDASHD:
      case TT_TRIPLED:
      case TT_thin_thicksmallgapD:
      case TT_thick_thinsmallgapD:
      case TT_thin_thick_thinsmallgapD:
      case TT_thin_thickmediumgapD:
      case TT_thick_thinmediumgapD:
      case TT_thin_thick_thinmediumgapD:
      case TT_thin_thicklargegapD:
      case TT_thick_thinlargegapD:
      case TT_thin_thick_thinlargegapD:
      case TT_WAVED:
      case TT_DOUBLEWAVED:
      case TT_DASHSMALLGAPD:
      case TT_DASHDOTSTROKEDD:
      case TT_EMBOSS3DD:
      case TT_ENGRAVE3DD:
      case TT_DEFAULTD:
	  mydata->current =
	      &(mydata->currentele->str[5 +
					(token_type -
					 TT_HAIRLINED)]);
	  wvAppendStr (mydata->current, "<begin>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_TITLE:
	  wvAppendStr (mydata->current, "<title/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CHARSET:
	  wvAppendStr (mydata->current, "<charset/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_COLSPAN:
	  wvAppendStr (mydata->current, "<colspan/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CELLRELWIDTH:
	  wvAppendStr (mydata->current, "<cellrelwidth/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CELLRELPAGEWIDTH:
	  wvAppendStr (mydata->current, "<cellrelpagewidth/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_TABLERELWIDTH:
	  wvAppendStr (mydata->current, "<tablerelwidth/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_no_rows:
	  wvAppendStr (mydata->current, "<no_rows/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_no_cols:
	  wvAppendStr (mydata->current, "<no_cols/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CELLBGCOLOR:
	  wvAppendStr (mydata->current, "<cellbgcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PARABGCOLOR:
	  wvAppendStr (mydata->current, "<parabgcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PARAFGCOLOR:
	  wvAppendStr (mydata->current, "<parafgcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ROWSPAN:
	  wvAppendStr (mydata->current, "<rowspan/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_FILENAME:
	  wvAppendStr (mydata->current, "<filename/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_VERSION:
	  wvAppendStr (mydata->current, "<version/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_STRIKEB:
	  wvAppendStr (mydata->current, "<strike.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_OUTLINEB:
	  wvAppendStr (mydata->current, "<outline.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_RMarkDelB:
	  wvAppendStr (mydata->current, "<rmarkdel.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DispFldRMarkB:
	  wvAppendStr (mydata->current, "<DispFldRMark.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ITALICB:
	  wvAppendStr (mydata->current, "<italic.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ITALICE:
	  wvAppendStr (mydata->current, "<italic.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_OUTLINEE:
	  wvAppendStr (mydata->current, "<outline.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_STRIKEE:
	  wvAppendStr (mydata->current, "<strike.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_RMarkDelE:
	  wvAppendStr (mydata->current, "<rmarkdel.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DispFldRMarkE:
	  wvAppendStr (mydata->current, "<DispFldRMark.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BOLDB:
	  wvAppendStr (mydata->current, "<bold.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BOLDE:
	  wvAppendStr (mydata->current, "<bold.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SMALLCAPSB:
	  wvAppendStr (mydata->current, "<smallcaps.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SMALLCAPSE:
	  wvAppendStr (mydata->current, "<smallcaps.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CAPSB:
	  wvAppendStr (mydata->current, "<caps.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CAPSE:
	  wvAppendStr (mydata->current, "<caps.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_VANISHB:
	  wvAppendStr (mydata->current, "<vanish.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_VANISHE:
	  wvAppendStr (mydata->current, "<vanish.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_RMarkB:
	  wvAppendStr (mydata->current, "<rmark.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_RMarkE:
	  wvAppendStr (mydata->current, "<rmark.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SHADOWB:
	  wvAppendStr (mydata->current, "<shadow.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SHADOWE:
	  wvAppendStr (mydata->current, "<shadow.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_LOWERCASEB:
	  wvAppendStr (mydata->current, "<lowercase.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_LOWERCASEE:
	  wvAppendStr (mydata->current, "<lowercase.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_EMBOSSB:
	  wvAppendStr (mydata->current, "<emboss.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_EMBOSSE:
	  wvAppendStr (mydata->current, "<emboss.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_IMPRINTB:
	  wvAppendStr (mydata->current, "<imprint.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_IMPRINTE:
	  wvAppendStr (mydata->current, "<imprint.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DSTRIKEB:
	  wvAppendStr (mydata->current, "<dstrike.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DSTRIKEE:
	  wvAppendStr (mydata->current, "<dstrike.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SUPERB:
	  wvAppendStr (mydata->current, "<super.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SUPERE:
	  wvAppendStr (mydata->current, "<super.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SUBB:
	  wvAppendStr (mydata->current, "<sub.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SUBE:
	  wvAppendStr (mydata->current, "<sub.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_JUST:
	  wvAppendStr (mydata->current, "<just/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PARAMARGIN:
	  wvAppendStr (mydata->current, "<paramargin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PARABORDER:
	  wvAppendStr (mydata->current, "<paraborder/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERTopSTYLE:
	  wvAppendStr (mydata->current, "<bordertopstyle/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERTopCOLOR:
	  wvAppendStr (mydata->current, "<bordertopcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERLeftSTYLE:
	  wvAppendStr (mydata->current, "<borderleftstyle/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERLeftCOLOR:
	  wvAppendStr (mydata->current, "<borderleftcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERRightSTYLE:
	  wvAppendStr (mydata->current, "<borderrightstyle/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERRightCOLOR:
	  wvAppendStr (mydata->current, "<borderrightcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERBottomSTYLE:
	  wvAppendStr (mydata->current, "<borderbottomstyle/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BORDERBottomCOLOR:
	  wvAppendStr (mydata->current, "<borderbottomcolor/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_nfc:
	  wvAppendStr (mydata->current, "<nfc/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_START:
	  wvAppendStr (mydata->current, "<start/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_COLORB:
	  wvAppendStr (mydata->current, "<color.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_SINGLEUB:
	  wvAppendStr (mydata->current, "<singleu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WORDUB:
	  wvAppendStr (mydata->current, "<wordu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOUBLEUB:
	  wvAppendStr (mydata->current, "<doubleu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTTEDUB:
	  wvAppendStr (mydata->current, "<dottedu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_HIDDENUB:
	  wvAppendStr (mydata->current, "<hiddenu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_THICKUB:
	  wvAppendStr (mydata->current, "<thicku.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DASHUB:
	  wvAppendStr (mydata->current, "<dashu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTUB:
	  wvAppendStr (mydata->current, "<dotu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTDASHUB:
	  wvAppendStr (mydata->current, "<dotdashu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTDOTDASHUB:
	  wvAppendStr (mydata->current, "<dotdotdashu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WAVEUB:
	  wvAppendStr (mydata->current, "<waveu.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SINGLEUE:
	  wvAppendStr (mydata->current, "<singleu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WORDUE:
	  wvAppendStr (mydata->current, "<wordu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOUBLEUE:
	  wvAppendStr (mydata->current, "<doubleu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTTEDUE:
	  wvAppendStr (mydata->current, "<dottedu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_HIDDENUE:
	  wvAppendStr (mydata->current, "<hiddenu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_THICKUE:
	  wvAppendStr (mydata->current, "<thicku.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DASHUE:
	  wvAppendStr (mydata->current, "<dashu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTUE:
	  wvAppendStr (mydata->current, "<dotu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTDASHUE:
	  wvAppendStr (mydata->current, "<dotdashu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DOTDOTDASHUE:
	  wvAppendStr (mydata->current, "<dotdotdashu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WAVEUE:
	  wvAppendStr (mydata->current, "<waveu.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_BLACKB:
	  wvAppendStr (mydata->current, "<black.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BLUEB:
	  wvAppendStr (mydata->current, "<blue.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CYANB:
	  wvAppendStr (mydata->current, "<cyan.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_GREENB:
	  wvAppendStr (mydata->current, "<green.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MAGENTAB:
	  wvAppendStr (mydata->current, "<magenta.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_REDB:
	  wvAppendStr (mydata->current, "<red.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_YELLOWB:
	  wvAppendStr (mydata->current, "<yellow.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WHITEB:
	  wvAppendStr (mydata->current, "<white.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKBLUEB:
	  wvAppendStr (mydata->current, "<dkblue.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKCYANB:
	  wvAppendStr (mydata->current, "<dkcyan.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKGREENB:
	  wvAppendStr (mydata->current, "<dkgreen.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKMAGENTAB:
	  wvAppendStr (mydata->current, "<dkmagenta.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKREDB:
	  wvAppendStr (mydata->current, "<dkred.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKYELLOWB:
	  wvAppendStr (mydata->current, "<dkyellow.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKGRAYB:
	  wvAppendStr (mydata->current, "<dkgray.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_LTGRAYB:
	  wvAppendStr (mydata->current, "<ltgray.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_BLACKE:
	  wvAppendStr (mydata->current, "<black.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BLUEE:
	  wvAppendStr (mydata->current, "<blue.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CYANE:
	  wvAppendStr (mydata->current, "<cyan.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_GREENE:
	  wvAppendStr (mydata->current, "<green.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MAGENTAE:
	  wvAppendStr (mydata->current, "<magenta.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_REDE:
	  wvAppendStr (mydata->current, "<red.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_YELLOWE:
	  wvAppendStr (mydata->current, "<yellow.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_WHITEE:
	  wvAppendStr (mydata->current, "<white.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKBLUEE:
	  wvAppendStr (mydata->current, "<dkblue.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKCYANE:
	  wvAppendStr (mydata->current, "<dkcyan.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKGREENE:
	  wvAppendStr (mydata->current, "<dkgreen.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKMAGENTAE:
	  wvAppendStr (mydata->current, "<dkmagenta.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKREDE:
	  wvAppendStr (mydata->current, "<dkred.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKYELLOWE:
	  wvAppendStr (mydata->current, "<dkyellow.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_DKGRAYE:
	  wvAppendStr (mydata->current, "<dkgray.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_LTGRAYE:
	  wvAppendStr (mydata->current, "<ltgray.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_FONTSTRB:
	  wvAppendStr (mydata->current, "<fontstr.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_FONTSTRE:
	  wvAppendStr (mydata->current, "<fontstr.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_ANIMATIONB:
	  wvAppendStr (mydata->current, "<animation.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ANIMATIONE:
	  wvAppendStr (mydata->current, "<animation.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_IBSTANNO:
	  wvAppendStr (mydata->current, "<ibstanno/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_xstUsrInitl:
	  wvAppendStr (mydata->current, "<xstUsrInitl/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmParaBefore:
	  wvAppendStr (mydata->current, "<mmParaBefore/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmParaAfter:
	  wvAppendStr (mydata->current, "<mmParaAfter/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmParaLeft:
	  wvAppendStr (mydata->current, "<mmParaLeft/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmParaRight:
	  wvAppendStr (mydata->current, "<mmParaRight/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  /* >>----------- PATCH */
   	  case  TT_stylename:
	  wvAppendStr (mydata->current, "<stylename/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  /* --------------<< */

      case TT_mmParaLeft1:
	  wvAppendStr (mydata->current, "<mmParaLeft1/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmPadTop:
	  wvAppendStr (mydata->current, "<mmPadTop/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmPadRight:
	  wvAppendStr (mydata->current, "<mmPadRight/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmPadBottom:
	  wvAppendStr (mydata->current, "<mmPadBottom/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmPadLeft:
	  wvAppendStr (mydata->current, "<mmPadLeft/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_mmLineHeight:
	  wvAppendStr (mydata->current, "<mmLineHeight/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_pixPicWidth:
	  wvAppendStr (mydata->current, "<pixPicWidth/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_pixPicHeight:
	  wvAppendStr (mydata->current, "<pixPicHeight/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_htmlAlignGuess:
	  wvAppendStr (mydata->current, "<htmlAlignGuess/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_htmlNextLineGuess:
	  wvAppendStr (mydata->current, "<htmlNextLineGuess/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ibstRMark:
	  wvAppendStr (mydata->current, "<ibstrmark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ibstRMarkDel:
	  wvAppendStr (mydata->current, "<ibstrmarkdel/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ibstDispFldRMark:
	  wvAppendStr (mydata->current, "<ibstdispfldrmark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_dttmRMark:
	  wvAppendStr (mydata->current, "<dttmrmark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_dttmRMarkDel:
	  wvAppendStr (mydata->current, "<dttmrmarkdel/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_dttmDispFldRMark:
	  wvAppendStr (mydata->current, "<dttmdispfldrmark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_xstDispFldRMark:
	  wvAppendStr (mydata->current, "<xstDispFldRMark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PropRMarkB:
	  wvAppendStr (mydata->current, "<proprmark.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_PropRMarkE:
	  wvAppendStr (mydata->current, "<proprmark.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ibstPropRMark:
	  wvAppendStr (mydata->current, "<ibstPropRMark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_dttmPropRMark:
	  wvAppendStr (mydata->current, "<dttmPropRMark/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_LasVegasB:
	  wvAppendStr (mydata->current, "<lasvegas.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BackgroundBlinkB:
	  wvAppendStr (mydata->current, "<backgroundblink.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SparkleTextB:
	  wvAppendStr (mydata->current, "<sparkletext.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MarchingAntsB:
	  wvAppendStr (mydata->current, "<marchingants.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MarchingRedAntsB:
	  wvAppendStr (mydata->current, "<marchingredants.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ShimmerB:
	  wvAppendStr (mydata->current, "<shimmer.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_LasVegasE:
	  wvAppendStr (mydata->current, "<lasvegas.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_BackgroundBlinkE:
	  wvAppendStr (mydata->current, "<backgroundblink.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_SparkleTextE:
	  wvAppendStr (mydata->current, "<sparkletext.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MarchingAntsE:
	  wvAppendStr (mydata->current, "<marchingants.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_MarchingRedAntsE:
	  wvAppendStr (mydata->current, "<marchingredants.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ShimmerE:
	  wvAppendStr (mydata->current, "<shimmer.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_TEXTB:
	  wvAppendStr (mydata->current, "<text.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_TEXTE:
	  wvAppendStr (mydata->current, "<text.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      case TT_OLISTB:
	  wvAppendStr (mydata->current, "<olist.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_OLISTE:
	  wvAppendStr (mydata->current, "<olist.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ULISTB:
	  wvAppendStr (mydata->current, "<ulist.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_ULISTE:
	  wvAppendStr (mydata->current, "<ulist.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ENTRYB:
	  wvAppendStr (mydata->current, "<entry.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_ENTRYE:
	  wvAppendStr (mydata->current, "<entry.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_TABLEB:
	  wvAppendStr (mydata->current, "<table.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_TABLEE:
	  wvAppendStr (mydata->current, "<table.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_ROWB:
	  wvAppendStr (mydata->current, "<row.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_ROWE:
	  wvAppendStr (mydata->current, "<row.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_CELLB:
	  wvAppendStr (mydata->current, "<cell.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_CELLE:
	  wvAppendStr (mydata->current, "<cell.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
      case TT_LASTCELLB:
	  wvAppendStr (mydata->current, "<lastcell.begin/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;
	  break;
      case TT_LASTCELLE:
	  wvAppendStr (mydata->current, "<lastcell.end/>");
	  mydata->currentlen = strlen (*(mydata->current));
	  break;

      }
}

static void
wvendElement (void *userData, const XML_Char *name)
{
    state_data *mydata = (state_data *) userData;
    unsigned int token_type;

/*  tokenIndex = s_mapNameToToken ((const char *) name);
    token_type = s_Tokens[tokenIndex].m_type;
 */ token_type = wvMapNameToTokenType ((const char *) name);
    switch (token_type)
      {
      case TT_BEGIN:
      case TT_END:
      case TT_ParaBefore:
      case TT_ParaRight:
      case TT_ParaAfter:
      case TT_ParaLeft:
      case TT_ParaLeft1:
      case TT_VertMergedCells:
      case TT_LEFT:
      case TT_RIGHT:
      case TT_CENTER:
      case TT_BLOCK:
      case TT_ASIAN:
      case TT_Arabic:
      case TT_UpperRoman:
      case TT_LowerRoman:
      case TT_UpperCaseN:
      case TT_LowerCaseN:
      case TT_NONED:
      case TT_SINGLED:
      case TT_THICKD:
      case TT_DOUBLED:
      case TT_NUMBER4D:
      case TT_HAIRLINED:
      case TT_DOTD:
      case TT_DASHLARGEGAPD:
      case TT_DOTDASHD:
      case TT_DOTDOTDASHD:
      case TT_TRIPLED:
      case TT_thin_thicksmallgapD:
      case TT_thick_thinsmallgapD:
      case TT_thin_thick_thinsmallgapD:
      case TT_thin_thickmediumgapD:
      case TT_thick_thinmediumgapD:
      case TT_thin_thick_thinmediumgapD:
      case TT_thin_thicklargegapD:
      case TT_thick_thinlargegapD:
      case TT_thin_thick_thinlargegapD:
      case TT_WAVED:
      case TT_DOUBLEWAVED:
      case TT_DASHSMALLGAPD:
      case TT_DASHDOTSTROKEDD:
      case TT_EMBOSS3DD:
      case TT_ENGRAVE3DD:
      case TT_DEFAULTD:
	  wvAppendStr (mydata->current, "</begin>");
	  wvTrace (("When we finish the str is %s\n", *(mydata->current)));
	  mydata->currentlen = 0;
	  mydata->current = NULL;
	  break;
      case TT_TITLE:
      case TT_CHARSET:
      case TT_COLSPAN:
      case TT_CELLRELWIDTH:
      case TT_CELLRELPAGEWIDTH:
      case TT_ROWSPAN:
      case TT_CELLBGCOLOR:
      case TT_PARABGCOLOR:
      case TT_PARAFGCOLOR:
      case TT_TABLERELWIDTH:
      case TT_no_cols:
      case TT_no_rows:
      case TT_VERSION:
      case TT_FILENAME:
      case TT_JUST:
      case TT_PARAMARGIN:
      case TT_PARABORDER:
      case TT_BORDERTopSTYLE:
      case TT_BORDERTopCOLOR:
      case TT_BORDERLeftSTYLE:
      case TT_BORDERLeftCOLOR:
      case TT_BORDERRightSTYLE:
      case TT_BORDERRightCOLOR:
      case TT_BORDERBottomSTYLE:
      case TT_BORDERBottomCOLOR:
      case TT_nfc:
      case TT_START:
      case TT_BOLDB:
      case TT_BOLDE:
      case TT_ITALICB:
      case TT_ITALICE:
      case TT_STRIKEB:
      case TT_STRIKEE:
      case TT_RMarkDelB:
      case TT_RMarkDelE:
      case TT_OUTLINEB:
      case TT_OUTLINEE:
      case TT_SMALLCAPSB:
      case TT_SMALLCAPSE:
      case TT_CAPSB:
      case TT_CAPSE:
      case TT_VANISHB:
      case TT_VANISHE:
      case TT_RMarkB:
      case TT_RMarkE:
      case TT_SHADOWB:
      case TT_SHADOWE:
      case TT_LOWERCASEB:
      case TT_LOWERCASEE:
      case TT_EMBOSSB:
      case TT_EMBOSSE:
      case TT_IMPRINTB:
      case TT_IMPRINTE:
      case TT_DSTRIKEB:
      case TT_DSTRIKEE:
      case TT_SUPERB:
      case TT_SUPERE:
      case TT_SUBB:
      case TT_SUBE:

      case TT_SINGLEUB:
      case TT_WORDUB:
      case TT_DOUBLEUB:
      case TT_DOTTEDUB:
      case TT_HIDDENUB:
      case TT_THICKUB:
      case TT_DASHUB:
      case TT_DOTUB:
      case TT_DOTDASHUB:
      case TT_DOTDOTDASHUB:
      case TT_WAVEUB:
      case TT_SINGLEUE:
      case TT_WORDUE:
      case TT_DOUBLEUE:
      case TT_DOTTEDUE:
      case TT_HIDDENUE:
      case TT_THICKUE:
      case TT_DASHUE:
      case TT_DOTUE:
      case TT_DOTDASHUE:
      case TT_DOTDOTDASHUE:
      case TT_WAVEUE:

      case TT_BLACKB:
      case TT_BLUEB:
      case TT_CYANB:
      case TT_GREENB:
      case TT_MAGENTAB:
      case TT_REDB:
      case TT_YELLOWB:
      case TT_WHITEB:
      case TT_DKBLUEB:
      case TT_DKCYANB:
      case TT_DKGREENB:
      case TT_DKMAGENTAB:
      case TT_DKREDB:
      case TT_DKYELLOWB:
      case TT_DKGRAYB:
      case TT_LTGRAYB:
      case TT_BLACKE:
      case TT_BLUEE:
      case TT_CYANE:
      case TT_GREENE:
      case TT_MAGENTAE:
      case TT_REDE:
      case TT_YELLOWE:
      case TT_WHITEE:
      case TT_DKBLUEE:
      case TT_DKCYANE:
      case TT_DKGREENE:
      case TT_DKMAGENTAE:
      case TT_DKREDE:
      case TT_DKYELLOWE:
      case TT_DKGRAYE:
      case TT_LTGRAYE:
      case TT_FONTSTRB:
      case TT_FONTSTRE:
      case TT_COLORB:
      case TT_COLORE:
      case TT_IBSTANNO:
      case TT_xstUsrInitl:
      case TT_mmParaBefore:
      case TT_mmParaAfter:
      case TT_mmParaLeft:
      case TT_mmParaRight:

      /* >>----------- PATCH */
      case TT_stylename:
/* ------------------ << */
      case TT_mmParaLeft1:
      case TT_mmPadTop:
      case TT_mmPadRight:
      case TT_mmPadBottom:
      case TT_mmPadLeft:
      case TT_mmLineHeight:
      case TT_pixPicWidth:
      case TT_pixPicHeight:
      case TT_htmlAlignGuess:
      case TT_htmlNextLineGuess:
      case TT_ibstRMark:
      case TT_ibstRMarkDel:
      case TT_ibstDispFldRMark:
      case TT_dttmRMark:
      case TT_dttmRMarkDel:
      case TT_dttmDispFldRMark:
      case TT_xstDispFldRMark:
      case TT_PropRMarkB:
      case TT_PropRMarkE:
      case TT_ibstPropRMark:
      case TT_dttmPropRMark:
      case TT_LasVegasB:
      case TT_BackgroundBlinkB:
      case TT_SparkleTextB:
      case TT_MarchingAntsB:
      case TT_MarchingRedAntsB:
      case TT_ShimmerB:
      case TT_LasVegasE:
      case TT_BackgroundBlinkE:
      case TT_SparkleTextE:
      case TT_MarchingAntsE:
      case TT_MarchingRedAntsE:
      case TT_ShimmerE:
      case TT_ANIMATIONB:
      case TT_ANIMATIONE:
      case TT_DispFldRMarkB:
      case TT_DispFldRMarkE:
      case TT_TEXTB:
      case TT_TEXTE:
      case TT_OLISTB:
      case TT_OLISTE:
      case TT_ULISTB:
      case TT_ULISTE:
      case TT_ENTRYB:
      case TT_ENTRYE:
      case TT_TABLEB:
      case TT_TABLEE:
      case TT_ROWB:
      case TT_ROWE:
      case TT_CELLB:
      case TT_CELLE:
      case TT_LASTCELLB:
      case TT_LASTCELLE:

      case TT_STYLE:
	  break;
      default:
	  mydata->currentlen = 0;
	  mydata->current = NULL;
	  break;
      }
    wvTrace (("ele ended\n"));
}

static void
exendElement (void *userData, const char *name)
{
    /*
       expand_data *mydata = (expand_data *)userData;
     */
    unsigned int token_type;

/*  tokenIndex = s_mapNameToToken ((const char *) name);
    token_type = s_Tokens[tokenIndex].m_type;
 */ token_type = wvMapNameToTokenType ((const char *) name);
    switch (token_type)
      {
      case TT_TITLE:
      case TT_CHARSET:
      case TT_COLSPAN:
      case TT_ROWSPAN:
      case TT_CELLBGCOLOR:
      case TT_PARABGCOLOR:
      case TT_PARAFGCOLOR:
      case TT_TABLERELWIDTH:
      case TT_no_rows:
      case TT_no_cols:
      case TT_CELLRELWIDTH:
      case TT_CELLRELPAGEWIDTH:
      case TT_VERSION:
      case TT_FILENAME:
	  break;
      default:
	  break;
      }
    wvTrace (("ele ended\n"));
}

static void
charData (void *userData, const XML_Char * s, int len)
{
  int i;

  state_data *mydata = (state_data *) userData;
  if ((len > 0) && (mydata->current != NULL))
    *(mydata->current) =
	    (char *) realloc (*(mydata->current), len + mydata->currentlen + 1);
  else
    return;

  (*(mydata->current))[mydata->currentlen] = 0;

  for (i = 0; i < len; i++)
  {
    switch (s[i])
    {
    case '&':
      mydata->currentlen += strlen ("&amp;") - 1;
      *(mydata->current) =
        (char *) realloc (*(mydata->current), len + mydata->currentlen + 1);
      wvStrcat (*mydata->current, "&amp;");
      break;
    case '<':
      mydata->currentlen += strlen ("&lt;") - 1;
      *(mydata->current) =
        (char *) realloc (*(mydata->current), len + mydata->currentlen + 1);
      wvStrcat (*mydata->current, "&lt;");
      break;
    case '>':
      mydata->currentlen += strlen ("&gt;") - 1;
      *(mydata->current) =
        (char *) realloc (*(mydata->current), len + mydata->currentlen + 1);
      wvStrcat (*mydata->current, "&gt;");
      break;
    case '"':
      mydata->currentlen += strlen ("&quot;") - 1;
      *(mydata->current) =
        (char *) realloc (*(mydata->current), len + mydata->currentlen + 1);
      wvStrcat (*mydata->current, "&quot;");
      break;
    default:
      (*(mydata->current))[i + mydata->currentlen] = s[i];
      (*(mydata->current))[i + mydata->currentlen + 1] = 0;      
      break;
    }
  }
  if (mydata->current != NULL)
  {
	  (*(mydata->current))[len + mydata->currentlen] = '\0';
	  mydata->currentlen += len;
  }
}

static void
excharData (void *userData, const XML_Char * s, int len)
{
    int i;

    expand_data *mydata = (expand_data *) userData;
    if (len > 0)
	mydata->retstring =
	    (char *) realloc (mydata->retstring, len + mydata->currentlen + 1);
    else
	return;

    for (i = 0; i < len; i++)
      {
	  if (mydata->retstring != NULL)
	      mydata->retstring[i + mydata->currentlen] = s[i];
      }
    if (mydata->retstring != NULL)
      {
	  mydata->retstring[i + mydata->currentlen] = '\0';
	  mydata->currentlen += len;
      }
}

#ifdef HAVE_LIBXML2

static void
free_libxml2_parser (xmlParserCtxtPtr ctxt)
{
  xmlDocPtr xmlDoc;

  ctxt->sax = NULL;

  xmlDoc = ctxt->myDoc;
  xmlFreeParserCtxt (ctxt);

  if (xmlDoc)
    xmlFreeDoc(xmlDoc);
}

static xmlEntityPtr
_getEntity (void * user_data, const xmlChar * name)
{
	return xmlGetPredefinedEntity (name);
}

int
wvParseConfig (state_data * myhandle)
{
	int ret = 0;

	xmlSAXHandler hdl; /* flagrant copying from AbiWord */
	xmlParserCtxtPtr ctxt;

	memset(&hdl, 0, sizeof(hdl));

	hdl.getEntity = _getEntity;
	hdl.startElement = wvstartElement;
	hdl.endElement = wvendElement;
	hdl.characters = charData;

	if (myhandle->fp)
	{
		fclose (myhandle->fp);
		myhandle->fp = NULL;
	}
	if (myhandle->path == NULL)
	{
		wvError (("No path has been set? Since I'm using libxml2 at the moment, I need a path.\n"));
		exit (-1);
	}

	ctxt = xmlCreateFileParserCtxt (myhandle->path);
	if (ctxt == NULL)
	{
		/* by this point we haven't allocated anything so we can just return right here */
		return 1;
	}
	ctxt->sax = &hdl;
	ctxt->userData = (void *) myhandle;

	xmlParseDocument (ctxt);

        if (!ctxt->wellFormed) ret = 1;

	free_libxml2_parser (ctxt);

	return ret;
}
#else
int
wvParseConfig (state_data * myhandle)
{
    char buf[BUFSIZ];

    XML_Parser parser = XML_ParserCreate (NULL);
    int done;
    size_t len;

    XML_SetUserData (parser, myhandle);
    XML_SetElementHandler (parser, wvstartElement, wvendElement);
    XML_SetCharacterDataHandler (parser, charData);

    if (myhandle->fp == NULL)
      {
	  wvError (
		   ("how can this happen, i bet you added TT_ lines to wv.h and didn't recompile wvHtml.o etc\n"));
	  exit (-1);
      }

    do
      {
	  wvTrace (("loop in\n"));
	  len = fread (buf, 1, sizeof (buf), myhandle->fp);
	  wvTrace (("loop out\n"));
	  done = len < sizeof (buf);
	  if (!XML_Parse (parser, buf, len, done))
	    {
		wvError (("%s at line %d\n",
			  XML_ErrorString (XML_GetErrorCode (parser)),
			  XML_GetCurrentLineNumber (parser)));
		return (1);
	    }
      }
    while (!done);
    XML_ParserFree (parser);
#ifdef DEBUG
    wvListStateData (myhandle);
#endif

    return 0;
}
#endif



void
wvInitExpandData (expand_data * data)
{
    data->retstring = NULL;
    data->currentlen = 0;
}

#ifdef HAVE_LIBXML2
int
wvExpand (expand_data * myhandle, char *buf, int len)
{
	int ret = 0;

	xmlSAXHandler hdl;
	xmlParserCtxtPtr ctxt;

	memset(&hdl, 0, sizeof(hdl));

	hdl.getEntity = _getEntity;
	hdl.startElement = exstartElement;
	hdl.endElement = exendElement;
	hdl.characters = excharData;

	ctxt = xmlCreateMemoryParserCtxt ((const char *) buf, len);
	if (ctxt == NULL)
	{
		/* by this point we haven't allocated anything so we can just return right here */
		return 1;
	}
	ctxt->sax = &hdl;
	ctxt->userData = (void *) myhandle;

	wvInitExpandData (myhandle);

	xmlParseDocument (ctxt);

        if (!ctxt->wellFormed) ret = 1;

	free_libxml2_parser (ctxt);

	return ret;
}
#else
int
wvExpand (expand_data * myhandle, char *buf, int len)
{
    XML_Parser parser = XML_ParserCreate (NULL);

    wvTrace (("expanding string %s\n", buf));

    XML_SetUserData (parser, myhandle);
    XML_SetElementHandler (parser, exstartElement, exendElement);
    XML_SetCharacterDataHandler (parser, excharData);
    wvInitExpandData (myhandle);

    if (!XML_Parse (parser, buf, len, 1))
      {
	  wvError (("%s at line %d\n",
		    XML_ErrorString (XML_GetErrorCode (parser)),
		    XML_GetCurrentLineNumber (parser)));
	  return 1;
      }

    XML_ParserFree (parser);

    return 0;
}
#endif

void
wvSetEntityConverter (expand_data * data)
{
    if ((data->sd) && (data->sd->elements[TT_CHARENTITY].str)
	&& (data->sd->elements[TT_CHARENTITY].str[0]))
      {
	  wvExpand (data, data->sd->elements[TT_CHARENTITY].str[0],
		    strlen (data->sd->elements[TT_CHARENTITY].str[0]));
	  if (data->retstring)
	    {
		if (!(strcasecmp (data->retstring, "HTML")))
		    wvConvertUnicodeToEntity = wvConvertUnicodeToHtml;
		else if (!(strcasecmp (data->retstring, "LaTeX")))
		    wvConvertUnicodeToEntity = wvConvertUnicodeToLaTeX;
		else if (!(strcasecmp (data->retstring, "XML")))
		    wvConvertUnicodeToEntity = wvConvertUnicodeToXml;
		wvTrace (
			 ("Using %s entity conversion in conjunction with ordinary charset conversion\n",
			  data->retstring));
		wvFree (data->retstring);
	    }
      }
}

void
wvBeginDocument (expand_data * data)
{
    if ((data->sd) && (data->sd->elements[TT_DOCUMENT].str)
	&& (data->sd->elements[TT_DOCUMENT].str[0] != NULL))
      {
	  wvTrace (("doc begin is %s", data->sd->elements[TT_DOCUMENT].str[0]));
	  wvExpand (data, data->sd->elements[TT_DOCUMENT].str[0],
		    strlen (data->sd->elements[TT_DOCUMENT].str[0]));
	  if (data->retstring)
	    {
		wvTrace (("doc begin is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvEndDocument (expand_data * data)
{
    PAP apap;
    /*
       just for html mode, as this is designed for, I always have an empty
       para end just to close off any open lists
     */
    wvInitPAP (&apap);
    data->props = (void *) &apap;
    wvEndPara (data);

    if ((data->sd) && (data->sd->elements[TT_DOCUMENT].str)
	&& (data->sd->elements[TT_DOCUMENT].str[1] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_DOCUMENT].str[1],
		    strlen (data->sd->elements[TT_DOCUMENT].str[1]));
	  if (data->retstring)
	    {
		wvTrace (("doc end is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvBeginSection (expand_data * data)
{
    if (data != NULL)
	data->asep = (SEP *) data->props;

    if ((data != NULL) && (data->sd != NULL)
	&& (data->sd->elements[TT_SECTION].str != NULL)
	&& (data->sd->elements[TT_SECTION].str[0] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_SECTION].str[0],
		    strlen (data->sd->elements[TT_SECTION].str[0]));
	  if (data->retstring)
	    {
		wvTrace (("para begin is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvEndSection (expand_data * data)
{
    if ((data != NULL) && (data->sd != NULL)
	&& (data->sd->elements[TT_SECTION].str != NULL)
	&& (data->sd->elements[TT_SECTION].str[1] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_SECTION].str[1],
		    strlen (data->sd->elements[TT_SECTION].str[1]));
	  if (data->retstring)
	    {
		wvTrace (("para end is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvBeginComment (expand_data * data)
{
    if (data != NULL)
      {
	  wvTrace (("comment beginning\n"));
	  if ((data->sd != NULL) && (data->sd->elements[TT_COMMENT].str)
	      && (data->sd->elements[TT_COMMENT].str[0] != NULL))
	    {
		wvExpand (data, data->sd->elements[TT_COMMENT].str[0],
			  strlen (data->sd->elements[TT_COMMENT].str[0]));
		if (data->retstring)
		  {
		      printf ("%s", data->retstring);
		      wvFree (data->retstring);
		  }
	    }
      }
}

void
wvEndComment (expand_data * data)
{
    if ((data->sd != NULL) && (data->sd->elements[TT_COMMENT].str)
	&& (data->sd->elements[TT_COMMENT].str[1] != NULL))
      {
	  wvTrace (("comment ending\n"));
	  wvExpand (data, data->sd->elements[TT_COMMENT].str[1],
		    strlen (data->sd->elements[TT_COMMENT].str[1]));
	  if (data->retstring)
	    {
		wvTrace (("comment end is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvBeginPara (expand_data * data)
{
    if (wvIsEmptyPara ((PAP *) data->props, data, 1))
	return;

    if (data != NULL)
      {
	  wvTrace (
		   ("para of style %d beginning\n",
		    ((PAP *) (data->props))->istd));
	  if ((data->sd != NULL) && (data->sd->elements[TT_PARA].str)
	      && (data->sd->elements[TT_PARA].str[0] != NULL))
	    {
		wvExpand (data, data->sd->elements[TT_PARA].str[0],
			  strlen (data->sd->elements[TT_PARA].str[0]));
		if (data->retstring)
		  {
		      printf ("%s", data->retstring);
		      wvFree (data->retstring);
		  }
	    }
      }
    wvTrace (
	     ("This Para is out cell %d %d \n", data->whichrow,
	      data->whichcell));
}

void
wvEndPara (expand_data * data)
{
    if ((data->sd != NULL) && (data->sd->elements[TT_PARA].str)
	&& (data->sd->elements[TT_PARA].str[1] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_PARA].str[1],
		    strlen (data->sd->elements[TT_PARA].str[1]));
	  if (data->retstring)
	    {
		wvTrace (("para end is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

int
wvIsEmptyPara (PAP * apap, expand_data * data, int inc)
{
    /* 
       if we are a end of table para then i consist of nothing that is of
       any use for beginning of a para
     */
    if (apap == NULL)
	return (0);

    if (apap->fTtp == 1)
	return (1);

    /* 
       if i consist of a vertically merged cell that is not the top one, then
       also i am of no use
     */
    if (apap->fInTable == 1)
      {
#if 0
	  wvTrace (
		   ("This Para is in cell %d %d\n", data->whichrow,
		    data->whichcell));
	  if (*data->vmerges)
	    {
		/* only ignore a vertically merged cell if the setting in the config file have been set that way */
		if (data
		    && data->sd
		    && data->sd->elements[TT_TABLEOVERRIDES].str
		    && data->sd->elements[TT_TABLEOVERRIDES].str[5])
		  {
		      if ((*data->vmerges)[data->whichrow][data->whichcell] ==
			  0)

			{
			    wvTrace (("Skipping the next paragraph\n"));
			    if (inc)
				data->whichcell++;
			    return (1);
			}
		  }
	    }
#else
	  return 0;
#endif
      }
    return (0);
}

void
wvBeginCharProp (expand_data * data, PAP * apap)
{
    CHP *achp;

    if (wvIsEmptyPara (apap, data, 0))
	return;

    achp = (CHP *) data->props;
    wvTrace (("beginning character run\n"));
    if (achp->ico)
      {
	  wvTrace (("color is %d\n", achp->ico));
      }

    if ((data != NULL) && (data->sd != NULL)
	&& (data->sd->elements[TT_CHAR].str)
	&& (data->sd->elements[TT_CHAR].str[0] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_CHAR].str[0],
		    strlen (data->sd->elements[TT_CHAR].str[0]));
	  if (data->retstring)
	    {
		wvTrace (("char begin is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}

void
wvEndCharProp (expand_data * data)
{
    wvTrace (("ending character run\n"));
    if ((data->sd != NULL) && (data->sd->elements[TT_CHAR].str)
	&& (data->sd->elements[TT_CHAR].str[1] != NULL))
      {
	  wvExpand (data, data->sd->elements[TT_CHAR].str[1],
		    strlen (data->sd->elements[TT_CHAR].str[1]));
	  if (data->retstring)
	    {
		wvTrace (("char end is now %s", data->retstring));
		printf ("%s", data->retstring);
		wvFree (data->retstring);
	    }
      }
}
