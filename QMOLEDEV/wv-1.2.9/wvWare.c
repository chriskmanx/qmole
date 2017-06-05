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
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include "wv.h"
#include "getopt.h"

/* strdup isn't declared in <string.h> for `gcc -ansi'; declare it here */
extern char *strdup (const char *);

extern char *str_copy(char *d, size_t n, char *s);
extern char *str_append(char *d, size_t n, char *s);

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
2 ole but not word doc
-1 for an error of some unknown kind
0 on success
*/

char *config = "wvHtml.xml";

/* flags for -X / --xml option */
int   xml_output = 0;
extern char *xml_slash;

/* flag for disabling graphics */
int   no_graphics = 0;

int myelehandler (wvParseStruct * ps, wvTag tag, void *props, int dirty);
int mydochandler (wvParseStruct * ps, wvTag tag);
int myCharProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid);
int mySpecCharProc (wvParseStruct * ps, U16 eachchar, CHP * achp);

int wvOpenConfig (state_data *myhandle,char *config);

char * wv_arg_basename = 0;
char * figure_name (wvParseStruct * ps);
char * name_to_url (char * name);

char wv_cwd[4097];

int HandleBitmap (wvParseStruct * ps, char *name, BitmapBlip * bitmap);
int HandleMetafile (wvParseStruct * ps, char *name, MetaFileBlip * bitmap);

/* should really be a config.h decl for having strdup, but... */
#ifdef __MWERKS__
char *
strdup (const char *text)
{
    char *buf;
    size_t len;

    len = strlen (text) + 1;
    buf = (char *) wvMalloc (len);
    memcpy (buf, text, len);

    return buf;
}


#endif

char *
wvHtmlGraphic (wvParseStruct * ps, Blip * blip)
{
    char *name;
    wvStream * fd;
    char test[3];

    name = figure_name (ps);
    if (name == 0) return (0);

    /* 
       temp hack to test older included bmps in word 6 and 7,
       should be wrapped in a modern escher strucure before getting
       to here, and then handled as normal
     */
    wvTrace (("type is %d\n", blip->type));
    switch (blip->type)
      {
      case msoblipJPEG:
      case msoblipDIB:
      case msoblipPNG:
	  fd =  (blip->blip.bitmap.m_pvBits);
	  test[2] = '\0';
	  test[0] = read_8ubit (fd);

	  test[1] = read_8ubit (fd);
	  wvStream_rewind (fd);
	  if (!(strcmp (test, "BM")))
	    {
		wvAppendStr (&name, ".bmp");
		if (0 != HandleBitmap (ps, name, &blip->blip.bitmap))
		    return (NULL);
		return (name);
	    }
      default:
	  break;
      }

    switch (blip->type)
      {
      case msoblipWMF:
	  wvAppendStr (&name, ".wmf");
	  if (0 != HandleMetafile (ps, name, &blip->blip.metafile))
	      return (NULL);
	  break;
      case msoblipEMF:
	  wvAppendStr (&name, ".emf");
	  if (0 != HandleMetafile (ps, name, &blip->blip.metafile))
	      return (NULL);
	  break;
      case msoblipPICT:
	  wvAppendStr (&name, ".pict");
	  if (0 != HandleMetafile (ps, name, &blip->blip.metafile))
	      return (NULL);
	  break;
      case msoblipJPEG:
	  wvAppendStr (&name, ".jpg");
	  if (0 != HandleBitmap (ps, name, &blip->blip.bitmap))
	      return (NULL);
	  break;
      case msoblipDIB:
	  wvAppendStr (&name, ".dib");
	  if (0 != HandleBitmap (ps, name, &blip->blip.bitmap))
	      return (NULL);
	  break;
      case msoblipPNG:
	  wvAppendStr (&name, ".png");
	  if (0 != HandleBitmap (ps, name, &blip->blip.bitmap))
	      return (NULL);
	  break;
      }
    return (name);
}


int
HandleBitmap (wvParseStruct * ps, char *name, BitmapBlip * bitmap)
{
    wvStream * pwv = bitmap->m_pvBits;
    FILE *fd = NULL;
    size_t size = 0, i;

    if (ps->dir) chdir (ps->dir);
    fd = fopen (name, "wb");
    if (ps->dir) chdir (wv_cwd);
    if (fd == NULL)
      {
	fprintf (stderr,"\nCannot open %s for writing\n",name);
	exit (1);
      }
    size = wvStream_size (pwv);
    wvStream_rewind(pwv);

    for (i = 0; i < size; i++)
      fputc (read_8ubit(pwv), fd);
    fclose (fd);
    wvTrace (("Name is %s\n", name));
    return (0);
}


int
HandleMetafile (wvParseStruct * ps, char *name, MetaFileBlip * bitmap)
{
    wvStream * pwv = bitmap->m_pvBits;
    FILE *fd = NULL;
    size_t size = 0, i;
    U8 decompressf = 0;

    if (ps->dir) chdir (ps->dir);
    fd = fopen (name, "wb");
    if (ps->dir) chdir (wv_cwd);
    if (fd == NULL)
      {
	fprintf (stderr,"\nCannot open %s for writing\n",name);
	exit (1);
      }
    size = wvStream_size (pwv);
    wvStream_rewind(pwv);

    if (bitmap->m_fCompression == msocompressionDeflate)
	decompressf = setdecom ();

    if ( !decompressf)
      {
	for (i = 0; i < size; i++)
	  fputc (read_8ubit(pwv), fd);
      }
    else /* decompress here */
      {
	  FILE *tmp = tmpfile ();
	  FILE *out = tmpfile ();

	  for (i = 0; i < size; i++)
	    fputc (read_8ubit(pwv), tmp);

	  rewind (tmp);
	  decompress (tmp, out, bitmap->m_cbSave, bitmap->m_cb);
	  fclose (tmp);

	  rewind(out);

	  for (i = 0; i < bitmap->m_cb; i++)
	    fputc ( fgetc(out), fd);

	  fclose(out);

      }

    fclose (fd);
    wvTrace (("Name is %s\n", name));
    return (0);
}

static void
do_version (void)
{
    /* todo: initialize this in a configure script */
    printf ("wvWare %s\n", VERSION);
}

static void
do_help (void)
{
    do_version ();
    printf ("Usage: wvWare [OPTION...] filename.doc\n");
    printf ("\nCommon Options:\n");
    printf ("  -x --config=config.xml\tSpecify an output filter to use\n");
    printf ("  -c --charset=charset\t\tSpecify an iconv charset encoding\n");
    printf ("  -p --password=password\tSpecify password for encrypted\n\t\t\t\tWord Documents\n");
    printf ("  -d --dir=dir\t\t\tDIR\n");
    printf ("  -b --basename=name\t\tUse name as base name of image files\n");
    printf ("  -a --auto-eps=fmt\t\tQuery support for conversion of fmt to eps\n");
    printf ("  -s --suppress=fmt\t\tDon't convert fmt to eps\n");
    printf ("  -X --xml\t\t\tXML output\n");
    printf ("  -1 --nographics\t\tno 0x01 graphics output\n");
    printf ("  -v --version\t\t\tPrint wvWare's version number\n");
    printf ("  -? --help\t\t\tPrint this help message\n");
    printf
	("\nwvWare is a suite of applications that converts Microsoft Word Documents\n");
    printf
	("(versions 2,5,6,7,8,9) into more \"useful\" formats such as HTML, LaTeX,\n");
    printf
	("ABW, WML, Text, etc... wvWare is also a library which can be used by\n");
    printf
	("other applications to import (and soon export) Word documents.\n\n");
    printf ("Authors:\nDom Lachowicz (dominicl@seas.upenn.edu)\n");
    printf ("Caolan McNamara (original author)\nVisit http://www.wvware.com\n");
}

static void wv_query_eps (const char* format);
static void wv_suppress (const char* format);

char *charset = NULL;

int
main (int argc, char **argv)
{
    FILE *input;
    char *password = NULL;
    char *dir = NULL;
    int ret;
    state_data myhandle;
    expand_data expandhandle;
    wvParseStruct ps;
    int c, index = 0;
    static struct option long_options[] = {
	{"charset", 1, 0, 'c'},
	{"config", 1, 0, 'x'},
	{"password", 1, 0, 'p'},
	{"dir", 1, 0, 'd'},
	{"basename", 1, 0, 'b'},
	{"auto-eps", 1, 0, 'a'},
	{"suppress", 1, 0, 's'},
	{"version", 0, 0, 'v'},
	{"help", 0, 0, '?'},
	{"xml", 0, 0, 'X'},
	{"nographics", 0, 0, '1'},
	{0, 0, 0, 0}
    };

    if (argc < 2)
      {
	  do_help ();
	  exit (-1);
      }

    while (1)
      {
	  c = getopt_long (argc, argv, "?vc:x:p:d:b:a:s:X1", long_options, &index);
	  if (c == -1)
	      break;
	  switch (c)
	    {
	    case '?':
		do_help ();
		return 0;
	    case 'v':
		do_version ();
		return 0;
	    case 'c':
		if (optarg)
		    charset = optarg;
		else
		    wvError (("No argument given to charset"));
		break;
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
	    case 'd':
		if (optarg)
		    dir = optarg;
		else
		    wvError (("No directory given to dir option"));
		break;
	    case 'b':
		if (optarg)
		    wv_arg_basename = optarg;
		else
		    wvError (("No name given to basename option"));
		break;
	    case 'a':
		wv_query_eps (optarg);
		return 0;
	    case 's':
		wv_suppress (optarg);
		break;

	    case 'X':
		config     = "wvXml.xml";
		charset    = "utf-8";
		xml_output = 1;
		xml_slash  = " /";
		break;
		
	    case '1':
		no_graphics = 1;
		break;
		
	    default:
		do_help ();
		return -1;
	    }
      }

    if (optind >= argc)
      {
	  fprintf (stderr, "No file name given to open\n");
	  return (-1);
      }

#if 0
    input = fopen (argv[optind], "rb");
    if (!input)
      {
	fprintf (stderr, "Failed to open %s\n", argv[optind]);
	  return (-1);
      }
    fclose (input);
#endif

    getcwd (wv_cwd,4096);
    wv_cwd[4096] = 0;

    wvInit ();
    ret = wvInitParser (&ps, argv[optind]);
    ps.filename = argv[optind];
    ps.dir = dir;

    if (ret & 0x8000)		/* Password protected? */
      {
	  if ((ret & 0x7fff) == WORD8)
	    {
		ret = 0;
		if (password == NULL)
		  {
		      fprintf (stderr,
			       "Password required, this is an encrypted document\n");
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
	  else if (((ret & 0x7fff) == WORD7) || ((ret & 0x7fff) == WORD6))
	    {
		ret = 0;
		if (password == NULL)
		  {
		      fprintf (stderr,
			       "Password required, this is an encrypted document\n");
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
      }

    if (ret)
      {
	  wvError (("startup error #%d\n", ret));
	  wvOLEFree (&ps);
	  return (-1);
      }

    wvSetElementHandler (&ps, myelehandler);
    wvSetDocumentHandler (&ps, mydochandler);
    wvSetCharHandler (&ps, myCharProc);
    wvSetSpecialCharHandler (&ps, mySpecCharProc);

    wvInitStateData (&myhandle);

    if (wvOpenConfig (&myhandle,config) == 0)
      {
	  wvError (("config file not found\n"));
	  return (-1);
      }
    else
      {
	  wvTrace (("x for FILE is %x\n", myhandle.fp));
	  ret = wvParseConfig (&myhandle);
      }

    if (!ret)
      {
	  expandhandle.sd = &myhandle;
	  ps.userData = &expandhandle;
	  ret = wvHtml (&ps);
      }
    wvReleaseStateData (&myhandle);

    if (ret == 2)
	return (2);
    else if (ret != 0)
	ret = -1;
    wvOLEFree (&ps);
    wvShutdown ();

    return (ret);
}

int
myelehandler (wvParseStruct * ps, wvTag tag, void *props, int dirty)
{
    static PAP *ppap;
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
    if (charset == NULL)
      {
	  data->charset = wvAutoCharset (ps);
	  charset = data->charset;
      }
    else
	data->charset = charset;
    data->props = props;

    switch (tag)
      {
      case PARABEGIN:
	  {
	      S16 tilfo = 0;
	      /* test begin */
	      if (*(data->endcell))
		{
		    tilfo = ((PAP *) (data->props))->ilfo;
		    ((PAP *) (data->props))->ilfo = 0;
		}
	      /* test end */
	      ppap = (PAP *) data->props;
	      wvTrace (
		       ("fore back is %d %d\n",
			((PAP *) (data->props))->shd.icoFore,
			((PAP *) (data->props))->shd.icoBack));
	      wvBeginPara (data);
	      if (tilfo)
		  ((PAP *) (data->props))->ilfo = tilfo;
	  }
	  break;
      case PARAEND:
	  {
	      S16 tilfo = 0;
	      /* test begin */
	      if (*(data->endcell))
		{
		    tilfo = ((PAP *) (data->props))->ilfo;
		    ((PAP *) (data->props))->ilfo = 0;
		}
	      /* test end */
	      wvEndCharProp (data);	/* danger will break in the future */
	      wvEndPara (data);
	      if (tilfo)
		  ((PAP *) (data->props))->ilfo = tilfo;
	      wvCopyPAP (&data->lastpap, (PAP *) (data->props));
	  }
	  break;
      case CHARPROPBEGIN:
	  wvBeginCharProp (data, ppap);
	  break;
      case CHARPROPEND:
	  wvEndCharProp (data);
	  break;
      case SECTIONBEGIN:
	  wvBeginSection (data);
	  break;
      case SECTIONEND:
	  wvEndSection (data);
	  break;
      case COMMENTBEGIN:
	  wvBeginComment (data);
	  break;
      case COMMENTEND:
	  wvEndComment (data);
	  break;
      default:
	  break;
      }
    return (0);
}

int
mydochandler (wvParseStruct * ps, wvTag tag)
{
    static int i;
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
    if (i == 0)
      {
	  wvSetEntityConverter (data);
	  data->filename = ps->filename;
	  data->whichcell = 0;
	  data->whichrow = 0;
	  data->asep = NULL;
	  i++;
	  wvInitPAP (&data->lastpap);
	  data->nextpap = NULL;
	  data->ps = ps;
      }

    if (charset == NULL)
      {
	  data->charset = wvAutoCharset (ps);
	  charset = data->charset;
      }
    else
	data->charset = charset;

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

void
wvStrangeNoGraphicData (char *config, int graphicstype)
{
    wvError (("Strange No Graphic Data in the 0x01/0x08 graphic\n"));

    if ((strstr (config, "wvLaTeX.xml") != NULL)
	|| (strstr (config, "wvCleanLaTeX.xml") != NULL))
	printf
	    ("\n\\resizebox*{\\baselineskip}{!}{\\includegraphics{placeholder.eps}}\
 		  \n-- %#.2x graphic: StrangeNoGraphicData --",
	     graphicstype);
    else
	printf ("<img alt=\"%#.2x graphic\" src=\"%s\"%s><br%s>", graphicstype,
		"StrangeNoGraphicData", xml_slash, xml_slash);
    return;
}

/* routines for conversion from WMF to EPS or PNG using libwmf(2) library.
 */
int wv_wmfRead (void *);
int wv_wmfSeek (void *, long);
long wv_wmfTell (void *);

void wvConvert_WMF_to_EPS (int, int, char **);
void wvConvert_WMF_to_PNG (int, int, char **);
void wvConvert_PNG_to_EPS (int, int, char **);
void wvConvert_JPG_to_EPS (int, int, char **);

int
wv_wmfRead (void *context)
{
    return (fgetc ((FILE *) context));
}

int
wv_wmfSeek (void *context, long position)
{
    return (fseek ((FILE *) context, position, SEEK_SET));
}

long
wv_wmfTell (void *context)
{
    return (ftell ((FILE *) context));
}

#ifdef HAVE_LIBWMF

#include <libwmf/api.h>
#include <libwmf/eps.h>
#ifdef HAVE_LIBWMF_FOREIGN_H
#include <libwmf/foreign.h>
#endif

#endif /* HAVE_LIBWMF */

void
wvConvert_WMF_to_EPS (int width, int height, char **source)
{
#ifdef HAVE_LIBWMF
    FILE *in = 0;
    FILE *out = 0;

    char *sink = 0;

    unsigned long flags;

    wmf_error_t err;

    wmf_eps_t *ddata = 0;

    wmfAPI *API = 0;

    wmfAPI_Options api_options;

    wmfD_Rect bbox;

    in = fopen (*source, "rb");

    if (in == 0)
	return;

    sink = strdup (*source);

    remove_suffix (sink, ".wmf");
    wvAppendStr (&sink, ".eps");

    out = fopen (sink, "wb");

    if (out == 0)
      {
	  wvFree (sink);
	  fclose (in);
	  return;
      }

    flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
    api_options.function = wmf_eps_function;

    err = wmf_api_create (&API, flags, &api_options);
    if (err != wmf_E_None)
	goto _wmf_error;

    ddata = WMF_EPS_GetData (API);

    err = wmf_bbuf_input (API, wv_wmfRead, wv_wmfSeek, wv_wmfTell, (void *) in);
    if (err != wmf_E_None)
	goto _wmf_error;

    err = wmf_scan (API, 0, &bbox);
    if (err != wmf_E_None)
	goto _wmf_error;

    ddata->out = wmf_stream_create (API,out);
    if (out == 0)
	goto _wmf_error;

    ddata->bbox = bbox;

    ddata->eps_width = width;
    ddata->eps_height = height;

    err = wmf_play (API, 0, &bbox);
    if (err != wmf_E_None)
	goto _wmf_error;

    wmf_api_destroy (API);

    fclose (in);
    fclose (out);

    *source = sink;

    return;

  _wmf_error:
    if (API)
	wmf_api_destroy (API);

    fclose (in);
    fclose (out);

    wvFree (sink);
#endif /* HAVE_LIBWMF */
}

#ifdef HAVE_LIBWMF

#include <libwmf/api.h>
#include <libwmf/gd.h>

#endif /* HAVE_LIBWMF */

void
wvConvert_WMF_to_PNG (int width, int height, char **source)
{
#ifdef HAVE_LIBWMF
    FILE *in = 0;
    FILE *out = 0;

    char *sink = 0;

    unsigned long flags;

    wmf_error_t err;

    wmf_gd_t *ddata = 0;

    wmfAPI *API = 0;

    wmfAPI_Options api_options;

    wmfD_Rect bbox;

    in = fopen (*source, "rb");

    if (in == 0)
	return;

    sink = strdup (*source);

    remove_suffix (sink, ".wmf");
    wvAppendStr (&sink, ".png");

    out = fopen (sink, "wb");

    if (out == 0)
      {
	  wvFree (sink);
	  fclose (in);
	  return;
      }

    flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
    api_options.function = wmf_gd_function;

    err = wmf_api_create (&API, flags, &api_options);
    if (err != wmf_E_None)
	goto _wmf_error;

    ddata = WMF_GD_GetData (API);
    if ((ddata->flags & WMF_GD_SUPPORTS_PNG) == 0)
	goto _wmf_error;

    err = wmf_bbuf_input (API, wv_wmfRead, wv_wmfSeek, wv_wmfTell, (void *) in);
    if (err != wmf_E_None)
	goto _wmf_error;

    err = wmf_scan (API, 0, &bbox);
    if (err != wmf_E_None)
	goto _wmf_error;

    ddata->type = wmf_gd_png;

    ddata->flags |= WMF_GD_OUTPUT_FILE;
    ddata->file = out;

    ddata->bbox = bbox;

    ddata->width = width;
    ddata->height = height;

    err = wmf_play (API, 0, &bbox);
    if (err != wmf_E_None)
	goto _wmf_error;

    wmf_api_destroy (API);

    fclose (in);
    fclose (out);

    *source = sink;

    return;

  _wmf_error:
    if (API)
	wmf_api_destroy (API);

    fclose (in);
    fclose (out);

    wvFree (sink);
#endif /* HAVE_LIBWMF */
}

void
wvConvert_PNG_to_EPS (int width, int height, char **source)
{
#ifdef HAVE_LIBWMF_FOREIGN_H
    FILE *in = 0;
    FILE *out = 0;

    char *sink = 0;

    unsigned long flags;

    wmf_error_t err;

    wmf_foreign_t *ddata = 0;

    wmfAPI *API = 0;

    wmfAPI_Options api_options;

    wmfImage image;

    flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
    api_options.function = wmf_foreign_function;

    err = wmf_api_create (&API, flags, &api_options);
    if (err != wmf_E_None)
	return;

    ddata = WMF_FOREIGN_GetData (API);

    if ((ddata->flags & WMF_FOREIGN_SUPPORTS_PNG) == 0)
      {
      	wmf_api_destroy (API);
      	return;
      }

    in = fopen (*source, "rb");

    if (in == 0)
      {
      	wmf_api_destroy (API);
      	return;
      }

    if (wmf_image_load_png (API,in,&image) == (-1))
      {
        fclose (in);
      	wmf_api_destroy (API);
      	return;
      }

    fclose (in);

    sink = strdup (*source);

    remove_suffix (sink, ".png");
    wvAppendStr (&sink, ".eps");

    out = fopen (sink, "wb");

    if (out == 0)
      {
        wvFree (sink);
        wmf_image_free (API,&image);
        wmf_api_destroy (API);
        return;
      }

    wmf_image_save_eps (API,out,&image);

    fclose (out);

    wmf_image_free (API,&image);
    wmf_api_destroy (API);

    *source = sink;

    return;
#endif /* HAVE_LIBWMF_FOREIGN_H */
}

void
wvConvert_JPG_to_EPS (int width, int height, char **source)
{
#ifdef HAVE_LIBWMF_FOREIGN_H
    FILE *in = 0;
    FILE *out = 0;

    char *sink = 0;

    unsigned long flags;

    wmf_error_t err;

    wmf_foreign_t *ddata = 0;

    wmfAPI *API = 0;

    wmfAPI_Options api_options;

    wmfImage image;

    flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
    api_options.function = wmf_foreign_function;

    err = wmf_api_create (&API, flags, &api_options);
    if (err != wmf_E_None)
	return;

    ddata = WMF_FOREIGN_GetData (API);

    if ((ddata->flags & WMF_FOREIGN_SUPPORTS_JPEG) == 0)
      {
      	wmf_api_destroy (API);
      	return;
      }

    in = fopen (*source, "rb");

    if (in == 0)
      {
      	wmf_api_destroy (API);
      	return;
      }

    if (wmf_image_load_jpg (API,in,&image) == (-1))
      {
        fclose (in);
      	wmf_api_destroy (API);
      	return;
      }

    fclose (in);

    sink = strdup (*source);

    remove_suffix (sink, ".jpg");
    wvAppendStr (&sink, ".eps");

    out = fopen (sink, "wb");

    if (out == 0)
      {
        wvFree (sink);
        wmf_image_free (API,&image);
        wmf_api_destroy (API);
        return;
      }

    wmf_image_save_eps (API,out,&image);

    fclose (out);

    wmf_image_free (API,&image);
    wmf_api_destroy (API);

    *source = sink;

    return;
#endif /* HAVE_LIBWMF_FOREIGN_H */
}

static void wv_query_eps (const char* format)
{
#ifdef HAVE_LIBWMF
  unsigned long flags;

  wmf_error_t err;
#ifdef HAVE_LIBWMF_FOREIGN_H
  wmf_foreign_t *ddata = 0;
#endif /* HAVE_LIBWMF_FOREIGN_H */
  wmfAPI* API = 0;
  wmfAPI_Options api_options;
#endif /* HAVE_LIBWMF */

  if (format == 0)
    {
      printf ("no\n");
      return;
    }

#ifdef HAVE_LIBWMF
  if (strcmp (format,"wmf") == 0)
    {
      printf ("yes\n");
      return;
    }
#ifdef HAVE_LIBWMF_FOREIGN_H
  if (strcmp (format,"png") == 0)
    {
      flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
      api_options.function = wmf_foreign_function;

      err = wmf_api_create (&API, flags, &api_options);
      if (err != wmf_E_None)
        {
          printf ("no\n");
          return;
        }

      ddata = WMF_FOREIGN_GetData (API);

      if (ddata->flags & WMF_FOREIGN_SUPPORTS_PNG)
        {
          printf ("yes\n");
        }
      else
        {
          printf ("no\n");
        }

      wmf_api_destroy (API);
      return;
    }
  if (strcmp (format,"jpg") == 0)
    {
      flags = WMF_OPT_IGNORE_NONFATAL | WMF_OPT_FUNCTION;
      api_options.function = wmf_foreign_function;

      err = wmf_api_create (&API, flags, &api_options);
      if (err != wmf_E_None)
        {
          printf ("no\n");
          return;
        }

      ddata = WMF_FOREIGN_GetData (API);

      if (ddata->flags & WMF_FOREIGN_SUPPORTS_JPEG)
        {
          printf ("yes\n");
        }
      else
        {
          printf ("no\n");
        }

      wmf_api_destroy (API);
      return;
    }
#endif /* HAVE_LIBWMF_FOREIGN_H */
#endif /* HAVE_LIBWMF */

  printf ("no\n");
  return;
}

static int Convert_WMF  = 1;
static int Convert_EMF  = 1;
static int Convert_PNG  = 1;
static int Convert_JPG  = 1;
static int Convert_PICT = 1;

static void wv_suppress (const char* format)
{
  const char* ptr = format;

  if (format == 0)
    {
      Convert_WMF  = 1;
      Convert_EMF  = 1;
      Convert_PNG  = 1;
      Convert_JPG  = 1;
      Convert_PICT = 1;

      return;
    }

  while (*ptr)
    {
      if (strncmp (ptr,"wmf,",4) == 0)
        {
          Convert_WMF = 0;
          ptr += 4;
          continue;
        }
      if (strncmp (ptr,"emf,",4) == 0)
        {
          Convert_EMF = 0;
          ptr += 4;
          continue;
        }
      if (strncmp (ptr,"png,",4) == 0)
        {
          Convert_PNG = 0;
          ptr += 4;
          continue;
        }
      if (strncmp (ptr,"jpg,",4) == 0)
        {
          Convert_JPG = 0;
          ptr += 4;
          continue;
        }
      if (strncmp (ptr,"pict,",5) == 0)
        {
          Convert_PICT = 0;
          ptr += 5;
          continue;
        }

      if (strcmp (ptr,"wmf") == 0)
        {
          Convert_WMF = 0;
          break;
        }
      if (strcmp (ptr,"emf") == 0)
        {
          Convert_EMF = 0;
          break;
        }
      if (strcmp (ptr,"png") == 0)
        {
          Convert_PNG = 0;
          break;
        }
      if (strcmp (ptr,"jpg") == 0)
        {
          Convert_JPG = 0;
          break;
        }
      if (strcmp (ptr,"pict") == 0)
        {
          Convert_PICT = 0;
          break;
        }

      fprintf (stderr,"format(s) `%s' not recognized!\n",ptr);
      break;
    }
}

void
wvPrintGraphics (char *config, int graphicstype, int width, int height,
		 char *source)
{
    if ((strstr (config, "wvLaTeX.xml") != NULL)
	|| (strstr (config, "wvCleanLaTeX.xml") != NULL))
      {
	  if (strlen (source) >= 4)
	    {
	      if (Convert_WMF && strcmp (source + strlen (source) - 4, ".wmf") == 0)
		  wvConvert_WMF_to_EPS (width, height, &source);
	      else if (Convert_PNG && strcmp (source + strlen (source) - 4, ".png") == 0)
		  wvConvert_PNG_to_EPS (width, height, &source);
	      else if (Convert_JPG && strcmp (source + strlen (source) - 4, ".jpg") == 0)
		  wvConvert_JPG_to_EPS (width, height, &source);
	    }
	  remove_suffix (source, ".eps");
	  remove_suffix (source, ".wmf");
	  remove_suffix (source, ".pict");
	  remove_suffix (source, ".png");
	  remove_suffix (source, ".jpg");
	  /* 
	     Output to real file name. Conversion to .eps must be done manually for now 
	   */
	  printf ("\n\\resizebox{%dpt}{%dpt}\
		  {\\includegraphics{%s.eps}}\
		  \n% -- %#.2x graphic -- \n", width, height, source, graphicstype);
      }
    else
      {
	  if (strlen (source) >= 4)
	      if (strcmp (source + strlen (source) - 4, ".wmf") == 0)
		  wvConvert_WMF_to_PNG (width, height, &source);
	  if ((strstr (config, "wvHtml.xml") != NULL)
	   || (strstr (config, "wvWml.xml")  != NULL))
	    {
	      printf ("<img width=\"%d\" height=\"%d\" alt=\"%#.2x graphic\" src=\"%s\"%s><br%s>",
	              width, height, graphicstype, name_to_url (source),
		      xml_slash, xml_slash);
	    }
	  else
	    {
	      printf ("<img width=\"%d\" height=\"%d\" alt=\"%#.2x graphic\" src=\"%s\"%s><br%s>",
	              width, height, graphicstype, source,
		      xml_slash, xml_slash);
	    }
      }
    return;
}

int
mySpecCharProc (wvParseStruct * ps, U16 eachchar, CHP * achp)
{
    static int message;
    PICF picf;
    FSPA *fspa;
    expand_data *data = (expand_data *) ps->userData;

    switch (eachchar)
      {
      case 19:
	  wvError (("field began\n"));
	  ps->fieldstate++;
	  ps->fieldmiddle = 0;
	  fieldCharProc (ps, eachchar, 0, 0x400);	/* temp */
	  return (0);
	  break;
      case 20:
	  wvTrace (("field middle\n"));
	  if (achp->fOle2)
	    {
		wvError (
			 ("this field has an associated embedded object of id %x\n",
			  achp->fcPic_fcObj_lTagObj));
		/*test = wvFindObject(achp->fcPic_fcObj_lTagObj);
		   if (test)
		   wvError(("data can be found in object entry named %s\n",test->name));
		 */ }
	  fieldCharProc (ps, eachchar, 0, 0x400);	/* temp */
	  ps->fieldmiddle = 1;
	  return (0);
	  break;
      case 21:
	  wvTrace (("field end\n"));
	  ps->fieldstate--;
	  ps->fieldmiddle = 0;
	  fieldCharProc (ps, eachchar, 0, 0x400);	/* temp */
	  return (0);
	  break;
      }

    if (ps->fieldstate)
      {
	  if (fieldCharProc (ps, eachchar, 0, 0x400))
	      return (0);
      }

    switch (eachchar)
      {
      case 0x05:
	  /* this should be handled by the COMMENTBEGIN and COMMENTEND events */
	  return (0);
	  break;
      case 0x01:
	  {
	      wvStream *f;
	      Blip blip;
	      char *name;
	      long p = wvStream_tell (ps->data);
	      wvError (
		       ("picture 0x01 here, at offset %x in Data Stream, obj is %d, ole is %d\n",
			achp->fcPic_fcObj_lTagObj, achp->fObj, achp->fOle2));

	      if (achp->fOle2)
		return (0);
	      if(!no_graphics) 
	      {
	      wvStream_goto (ps->data, achp->fcPic_fcObj_lTagObj);
	      wvGetPICF (wvQuerySupported (&ps->fib, NULL), &picf, ps->data);
	      f = picf.rgb;
	      if (wv0x01 (&blip, f, picf.lcb - picf.cbHeader))
		{
		    wvTrace (("Here\n"));
		    name = wvHtmlGraphic (ps, &blip);
		    if (ps->dir) chdir (ps->dir);
		    wvPrintGraphics (config, 0x01,
				     (int) wvTwipsToHPixels (picf.dxaGoal),
				     (int) wvTwipsToVPixels (picf.dyaGoal),
				     name);
		    if (ps->dir) chdir (wv_cwd);
		    wvFree (name);
		}
	      else
		  wvStrangeNoGraphicData (config, 0x01);
	      }

	      wvStream_goto (ps->data, p);
	      return (0);
	  }
      case 0x08:
	  {
	      Blip blip;
	      char *name;
	      if (wvQuerySupported (&ps->fib, NULL) == WORD8)
		{
		    if(!no_graphics) 
		    {
		    if (ps->nooffspa > 0)
		      {
			  fspa =
			      wvGetFSPAFromCP (ps->currentcp, ps->fspa,
					       ps->fspapos, ps->nooffspa);

			  if (!fspa)
			    {
				wvError (("No fspa! Insanity abounds!\n"));
				return 0;
			    }

			  data->props = fspa;
			  if (wv0x08 (&blip, fspa->spid, ps))
			    {
				wvTrace (("Here\n"));
				name = wvHtmlGraphic (ps, &blip);
				if (ps->dir) chdir (ps->dir);
				wvPrintGraphics (config, 0x08,
						 (int)
						 wvTwipsToHPixels (fspa->xaRight
								   -
								   fspa->
								   xaLeft),
						 (int) wvTwipsToVPixels (fspa->
									 yaBottom
									 -
									 fspa->
									 yaTop),
						 name);
				if (ps->dir) chdir (wv_cwd);
				wvFree (name);
			    }
			  else
			      wvStrangeNoGraphicData (config, 0x08);
		      }
		    else
		      {
			  wvError (("nooffspa was <=0!  Ignoring.\n"));
		      }
		    }
		}
	      else
		{
		    FDOA *fdoa;
		    wvError (
			     ("pre word8 0x08 graphic, unsupported at the moment\n"));
		    fdoa =
			wvGetFDOAFromCP (ps->currentcp, ps->fdoa, ps->fdoapos,
					 ps->nooffdoa);
		    data->props = fdoa;
		}





#if 0
	      if ((fspa) && (data->sd != NULL)
		  && (data->sd->elements[TT_PICTURE].str)
		  && (data->sd->elements[TT_PICTURE].str[0] != NULL))
		{
		    wvExpand (data, data->sd->elements[TT_PICTURE].str[0],
			      strlen (data->sd->elements[TT_PICTURE].str[0]));
		    if (data->retstring)
		      {
			  wvTrace (
				   ("picture string is now %s",
				    data->retstring));
			  printf ("%s", data->retstring);
			  wvFree (data->retstring);
		      }
		}
#endif
	      return (0);
	  }
      case 0x28:
	  {
	      U16 symbol[6] = { 'S', 'y', 'm', 'b', 'o', 'l' };
	      U16 wingdings[9] =
		  { 'W', 'i', 'n', 'g', 'd', 'i', 'n', 'g', 's' };
	      U16 mtextra[8] = 
		  { 'M', 'T', ' ', 'E', 'x', 't', 'r', 'a' };

	      wvTrace (
		       ("no of strings %d %d\n", ps->fonts.nostrings,
			achp->ftcSym));
	      if (0 == memcmp (symbol, ps->fonts.ffn[achp->ftcSym].xszFfn, 12))
		{
		    if ((!message) && (strcasecmp ("UTF-8", charset)))
		      {
			  wvWarning
			      ("Symbol font detected (too late sorry!), rerun wvHtml with option --charset utf-8\n\
option to support correct symbol font conversion to a viewable format.\n");
			  message++;
		      }
		    wvTrace (
			     ("symbol char %d %x %c, using font %d %s\n",
			      achp->xchSym, achp->xchSym, achp->xchSym,
			      achp->ftcSym,
			      wvWideStrToMB (ps->fonts.ffn[achp->ftcSym].
					     xszFfn)));
		    wvTrace (
			     ("symbol char ends up as a unicode %x\n",
			      wvConvertSymbolToUnicode (achp->xchSym - 61440)));
		    wvOutputFromUnicode (wvConvertSymbolToUnicode
					 (achp->xchSym - 61440), charset);
		    return (0);
		}
	      else if (0 == 
		       memcmp (mtextra, ps->fonts.ffn[achp->ftcSym].xszFfn, 
				16))
		{
		    if ((!message) && (strcasecmp ("UTF-8", charset)))
		      {
			  wvWarning
			      ("MT Extra font detected (too late sorry!), rerun wvHtml with option --charset utf-8\n\
option to support correct symbol font conversion to a viewable format.\n");
			  message++;
		      }
		    wvTrace (
			     ("Symbol char %d %x %c, using font %d %s\n",
			      achp->xchSym, achp->xchSym, achp->xchSym,
			      achp->ftcSym,
			      wvWideStrToMB (ps->fonts.ffn[achp->ftcSym].
					     xszFfn)));
		    wvTrace (
			     ("symbol char ends up as a unicode %x\n",
			      wvConvertMTExtraToUnicode (achp->xchSym - 61440)));
		    wvOutputFromUnicode (wvConvertMTExtraToUnicode
					 (achp->xchSym - 61440), charset);
		    return (0);
		}	
	      else if (0 ==
		       memcmp (wingdings, ps->fonts.ffn[achp->ftcSym].xszFfn,
			       18))
		{
		    if (!message)
		      {
			  wvError (
				   ("I have yet to do a wingdings to unicode mapping table, if you know of one tell me\n"));
			  message++;
		      }
		}
	      else
		{
		    if (!message)
		      {
			  char *fontname =
			      wvWideStrToMB (ps->fonts.ffn[achp->ftcSym].
					     xszFfn);
			  wvError (
				   ("Special font %s, i need a mapping table to unicode for this\n",
				    fontname));
			  wvFree (fontname);
			  printf ("*");
		      }
		    return (0);
		}
	  }
      default:
	  return (0);
      }



    return (0);
}


int
myCharProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid)
{
    switch (eachchar)
      {
      case 19:
	  wvTrace (("field began\n"));
	  ps->fieldstate++;
	  ps->fieldmiddle = 0;
	  fieldCharProc (ps, eachchar, chartype, lid);	/* temp */
	  return (0);
	  break;
      case 20:
	  wvTrace (("field middle\n"));
	  fieldCharProc (ps, eachchar, chartype, lid);
	  ps->fieldmiddle = 1;
	  return (0);
	  break;
      case 21:
	  wvTrace (("field began\n"));
	  ps->fieldmiddle = 0;
	  ps->fieldstate--;
	  fieldCharProc (ps, eachchar, chartype, lid);	/* temp */
	  return (0);
	  break;
      case 0x08:
	  wvError (
		   ("hmm did we loose the fSpec flag ?, this is possibly a bug\n"));
	  break;
      }

    if (ps->fieldstate)
      {
	  if (fieldCharProc (ps, eachchar, chartype, lid))
	      return (0);
      }

    wvTrace (
	     ("charset is %s, lid is %x, type is %d, char is %x\n", charset,
	      lid, chartype, eachchar));

    if ((chartype) && (wvQuerySupported (&ps->fib, NULL) == WORD8))
	wvTrace (("lid is %x\n", lid));

    if (charset != NULL)
	wvOutputHtmlChar (eachchar, chartype, charset, lid);
    else
	wvOutputHtmlChar (eachchar, chartype, wvAutoCharset (ps), lid);
    return (0);
}

int
wvOpenConfig (state_data *myhandle,char *config)
{
    static char buf[BUFSIZ] = "";
    FILE *tmp;
    int i = 0;
    if (config == NULL)
	config = "wvHtml.xml";
    else
	i = 1;
    tmp = fopen (config, "rb");

    if(tmp == NULL)
    {
	str_copy  (buf, sizeof(buf), WVDATADIR);
	str_append(buf, sizeof(buf), "/");
	str_append(buf, sizeof(buf), config);
	config = buf;
	tmp = fopen(config, "rb");
    }

    if (tmp == NULL)
      {
	  if (i)
	      wvError (
		       ("Attempt to open %s failed, using %s\n", config,
			HTMLCONFIG));
	  config = HTMLCONFIG;
	  tmp = fopen (config, "rb");
      }
    myhandle->path = config;
    myhandle->fp = tmp;
    return (tmp == NULL ? 0 : 1);
}

char * figure_name (wvParseStruct * ps)
{
  static int number;
  static char * b_name = 0;
  char * f_name = 0;
  char buffer[10];

  if (b_name == 0)
    {
      if (wv_arg_basename)
        {
          b_name = strdup (wv_arg_basename);
#ifdef WV_REMOVE_SUFFIX
          if (b_name) /* remove any suffix */
            {
              char * dot = 0;
              char * ptr = b_name;
              while (*ptr)
                {
                  if (*ptr == '.') dot = ptr;
                  ptr++;
                }
              if (dot) *dot = 0;
            }
#endif /* WV_REMOVE_SUFFIX */
        }
      else
        {
          b_name = strdup (base_name (ps->filename));
          if (b_name) /* remove '.doc' suffix; case insensitive */
            {
              if (strlen (b_name) >= 4)
                {
                  char * dot = b_name + strlen (b_name) - 4;
                  if (strcasecmp (dot,".doc") == 0) *dot = 0;
                }
            }
        }
    }

  if (b_name == 0)
    {
      fprintf (stderr,"error: unable to create basename!");
      exit (1);
    }

  f_name = strdup (b_name);
  if (f_name)
    {
      sprintf (buffer, "%d", number++);
      wvAppendStr (&f_name, buffer);
    }
  else
    {
      fprintf (stderr,"error: unable to create filename!");
      exit (1);
    }

  return (f_name);
}

char * name_to_url (char * name)
{
  static char * url = 0;
  static long max = 0;
  char * ptr = 0;
  long count = 0;

  ptr = name;
  while (*ptr)
    {
      switch (*ptr)
        {
        case ' ':
          count += 3;
        break;
        default:
          count++;
        break;
        }
      ptr++;
    }
  count++;

  if (count > max)
    {
      char * more = 0;
      if (url == 0)
        {
          more = malloc (count);
        }
      else
        {
          more = realloc (url,count);
        }
      if (more)
        {
          url = more;
          max = count;
        }
    }

  if (url)
    {
      count = 0;
      ptr = name;
      while (*ptr && (count < max))
        {
          switch (*ptr)
            {
            case ' ':
              url[count++] = '%';
              if (count < max) url[count++] = '2';
              if (count < max) url[count++] = '0';
            break;
            default:
              url[count++] = *ptr;
            break;
            }
          ptr++;
        }
      url[max-1] = 0;
    }
  else
    {
      wvError (("failed to convert name to URL\n"));
      return (name);
    }

  return (url);
}
