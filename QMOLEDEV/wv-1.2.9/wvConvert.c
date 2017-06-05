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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "wv.h"
#include "getopt.h"

/*
Released under GPL, written by Caolan.McNamara@ul.ie.

Copyright (C) 1998,1999 
	Caolan McNamara

Real Life: Caolan McNamara           *  Doing: MSc in HCI
Work: Caolan.McNamara@ul.ie          *  Phone: +353-86-8790257
URL: http://skynet.csn.ul.ie/~caolan *  Sig: an oblique strategy
How would you have done it?
*/

/*
returns 1 for not an ole doc
2 ole but not supported word doc
-1 for an error of some unknown kind
0 on success
*/

int myelehandler (wvParseStruct * ps, wvTag tag, void *props, int dirty);
int mydochandler (wvParseStruct * ps, wvTag tag);
int wvOpenConfig (state_data *myhandle,char *config);

void
usage (void)
{
    printf
	("Usage: wvConvert [--config config.xml] [--password password] filename.doc\n");
    exit (-1);
}

int
main (int argc, char **argv)
{
    char *config = NULL;
    char *password = NULL;

    int ret;
    state_data myhandle;
    expand_data expandhandle;
    wvParseStruct ps;
    int c, index = 0;
    static struct option long_options[] = {
	{"config", 1, 0, 'x'},
	{"password", 1, 0, 'p'},
	{0, 0, 0, 0}
    };

    if (argc < 2)
	usage ();

    while (1)
      {
	  c = getopt_long (argc, argv, "x:p:", long_options, &index);
	  if (c == -1)
	      break;
	  switch (c)
	    {
	    case 'x':
		if (optarg)
		    config = optarg;
		else
		    wvError (("No config file given to config option"));
		break;
	    case 'p':
		if (optarg)
		    password = optarg;
		else
		    wvError (("No password given to password option"));
		break;
	    default:
		usage ();
		break;
	    }
      }

    wvInit ();
    ret = wvInitParser (&ps, argv[optind]);
    ps.filename = argv[optind];

    if (ret == 4)
      {
	  ret = 0;
	  if (password == NULL)
	    {
		wvError (
			 ("Password required, this is an encrypted document\n"));
		return (-1);
	    }
	  else
	    {
		wvSetPassword (password, &ps);
		if (wvDecrypt97 (&ps))
		  {
		      wvError (("Incorrect Password\n"));
		      return (-1);
		  }
	    }
      }
    else if (ret == 7)
      {
	  ret = 0;
	  if (password == NULL)
	    {
		wvError (
			 ("Password required, this is an encrypted document\n"));
		return (-1);
	    }
	  else
	    {
		wvSetPassword (password, &ps);
		if (wvDecrypt95 (&ps))
		  {
		      wvError (("Incorrect Password\n"));
		      return (-1);
		  }
	    }
      }

    if (ret)
      {
	  wvError (("startup error with file %s\n", argv[1]));
	  wvOLEFree (&ps);
	  return (2);
      }

    wvSetElementHandler (&ps, myelehandler);
    wvSetDocumentHandler (&ps, mydochandler);

    wvInitStateData (&myhandle);

    if (wvOpenConfig (&myhandle,config) == 0)
	wvError (("config file not found\n"));
    else
	ret = wvParseConfig (&myhandle);

    if (!ret)
      {
	  expandhandle.sd = &myhandle;
	  ps.userData = &expandhandle;
	  ret = wvText (&ps);
      }

    wvReleaseStateData (&myhandle);
    if (ret == 2)
	return (2);
    else if (ret != 0)
	ret = -1;
    wvOLEFree (&ps);
    return (ret);
}

int
myelehandler (wvParseStruct * ps, wvTag tag, void *props, int dirty)
{
    expand_data *data = (expand_data *) ps->userData;
    data->anSttbfAssoc = &ps->anSttbfAssoc;
    data->lfo = &ps->lfo;
    data->lfolvl = ps->lfolvl;
    data->lvl = ps->lvl;
    data->nolfo = &ps->nolfo;
    data->nooflvl = &ps->nooflvl;
    data->stsh = &ps->stsh;
    data->lst = &ps->lst;
    data->noofLST = &ps->noofLST;
    data->liststartnos = &ps->liststartnos;
    data->listnfcs = &ps->listnfcs;
    data->finallvl = &ps->finallvl;
    data->fib = &ps->fib;
    data->dop = &ps->dop;
    data->intable = &ps->intable;
    data->cellbounds = &ps->cellbounds;
    data->nocellbounds = &ps->nocellbounds;
    data->endcell = &ps->endcell;
    data->vmerges = &ps->vmerges;
    data->norows = &ps->norows;
    data->nextpap = &ps->nextpap;
    data->charset = wvAutoCharset (ps);
    data->props = props;

    switch (tag)
      {
      case PARABEGIN:
	  wvBeginPara (data);
	  break;
      case PARAEND:
	  wvEndPara (data);
	  wvCopyPAP (&data->lastpap, (PAP *) (data->props));
	  break;
      case SECTIONBEGIN:
	  wvBeginSection (data);
	  break;
      case SECTIONEND:
	  wvEndSection (data);
	  break;
      case CHARPROPBEGIN:
	  wvBeginCharProp (data, NULL);
	  break;
      case CHARPROPEND:
	  wvEndCharProp (data);
	  break;
      default:
	  break;
      }
    return (0);
}

int
mydochandler (wvParseStruct * ps, wvTag tag)
{
    expand_data *data = (expand_data *) ps->userData;
    data->anSttbfAssoc = &ps->anSttbfAssoc;
    data->lfo = &ps->lfo;
    data->lfolvl = ps->lfolvl;
    data->lvl = ps->lvl;
    data->nolfo = &ps->nolfo;
    data->nooflvl = &ps->nooflvl;
    data->stsh = &ps->stsh;
    data->lst = &ps->lst;
    data->noofLST = &ps->noofLST;
    data->liststartnos = &ps->liststartnos;
    data->listnfcs = &ps->listnfcs;
    data->finallvl = &ps->finallvl;
    data->fib = &ps->fib;
    data->dop = &ps->dop;
    data->intable = &ps->intable;
    data->cellbounds = &ps->cellbounds;
    data->nocellbounds = &ps->nocellbounds;
    data->endcell = &ps->endcell;
    data->vmerges = &ps->vmerges;
    data->norows = &ps->norows;

    data->charset = wvAutoCharset (ps);

    switch (tag)
      {
      case DOCBEGIN:
	  wvBeginDocument (data);
	  break;
      case DOCEND:
	  wvEndDocument (data);
	  break;
      default:
	  break;
      }
    return (0);
}

int
wvOpenConfig (state_data *myhandle,char *config)
{
    FILE *tmp;
    int i = 0;
    if (config == NULL)
	config = "wvConfig.xml";
    else
	i = 1;
    tmp = fopen (config, "rb");
    if (tmp == NULL)
      {
	  if (i)
	      wvError (
		       ("Attempt to open %s failed, using %s\n", config,
			HTMLCONFIG));
	  config = XMLCONFIG;
	  tmp = fopen (config, "rb");
      }
    myhandle->path = config;
    myhandle->fp = tmp;
    return (tmp == NULL ? 0 : 1);
}
