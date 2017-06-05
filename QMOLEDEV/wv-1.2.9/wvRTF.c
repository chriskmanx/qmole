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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <time.h>

#include "getopt.h"
#include "wv.h"

/* By Dom Lachowicz (cinamod@hotmail.com) */

/* i don't like appearing to use printf */
#define rtf_output printf
#define rtf_output_char(c)  do {rtf_output("%c", (c));} while(0)
#define ENSURE_BUF()    fflush(stdout)

typedef struct _rtfUserData {
    /* formatting variables */

    /* cached integer values */
    int cFont;
    int cFontSize;
    int cCol;

    /* boolean formats */
    int bIsBold:1;
    int bIsItalic:1;
    int bIsStrike:1;
    int bIsUl:1;
    int bIsSup:1;
    int bIsSub:1;

    /* paragraph related */
    int bInPara:1;
    int bInSec:1;
} rtfUserData;

/* this gets printed at the top of each document in the {\fonttbl section */

struct _fontMapping {
    char *word;
    char *rtf;
};

/* TODO: build me up appropriately */
static struct _fontMapping fontMap[] = {
    {"Arial", "Arial"},
    {"Bitstream Charter", "Bitstream Charter"},
    {"Bookman", "Bookman"},
    {"Courier", "Courier"},
    {"Courier New", "Courier New"},
    {"Century Schoolbook", "Century Schoolbook"},
    {"Dingbats", "Dingbats"},
    {"Goth", "Goth"},
    {"Nimbus Sans", "Nimbus Sans"},
    {"Palladio", "Palladio"},
    {"Standard Symbol", "Standard Symbol"},
    {"Symbol", "Symbol"},
    {"Times", "Times New Roman"},
    {"Times New Roman", "Times New Roman"},
};

#define FontTblSize (sizeof(fontMap)/sizeof(fontMap[0]))
#define DFL_FONT_INDEX 13	/* I map this to whatever "Times New Roman" is */
static void
output_fonttable (void)
{
    int i;

    rtf_output ("{\\fonttbl\n");

    for (i = 0; i < FontTblSize; i++)
	rtf_output ("{\\f%d\\fnil\\fcharset0\\fprq0\\fttruetype %s;}\n", i,
		    fontMap[i].rtf);

    rtf_output ("}\n");
}

/* map the MSWord name to the corresponding RTF name index */
static int
mapFont (const char *name)
{
    int k;

    for (k = 0; k < FontTblSize; k++)
	if (!strcasecmp (fontMap[k].word, name))
	    return k;

    return DFL_FONT_INDEX;
}

#undef DFL_FONT_INDEX
#undef FontTblSize

/* this gets printed at the top of each document in the {\colortbl section */
static int colorTable[][3] = {
    {0x00, 0x00, 0x00},		/* black */
    {0x00, 0x00, 0xff},		/* blue */
    {0x00, 0xff, 0xff},		/* cyan */
    {0x00, 0xff, 0x00},		/* green */
    {0xff, 0x00, 0xff},		/* magenta */
    {0xff, 0x00, 0x00},		/* red */
    {0xff, 0xff, 0x00},		/* yellow */
    {0xff, 0xff, 0xff},		/* white */
    {0x00, 0x00, 0x80},		/* dark blue */
    {0x00, 0x80, 0x80},		/* dark cyan */
    {0x00, 0x80, 0x00},		/* dark green */
    {0x80, 0x00, 0x80},		/* dark magenta */
    {0x80, 0x00, 0x00},		/* dark red */
    {0x80, 0x80, 0x00},		/* dark yellow */
    {0x80, 0x80, 0x80},		/* dark gray */
    {0xc0, 0xc0, 0xc0},		/* light gray */
};

/* rtf names for the color_table as above */
static char *rtfColors[] = {
    "\\cf0",			/* black */
    "\\cf1",			/* blue */
    "\\cf2",			/* cyan */
    "\\cf3",			/* green */
    "\\cf4",			/* magenta */
    "\\cf5",			/* red */
    "\\cf6",			/* yellow */
    "\\cf7",			/* white */
    "\\cf8",			/* dark blue */
    "\\cf9",			/* dark cyan */
    "\\cf10",			/* dark green */
    "\\cf11",			/* dark magenta */
    "\\cf12",			/* dark red */
    "\\cf13",			/* dark yellow */
    "\\cf14",			/* dark gray */
    "\\cf15",			/* light gray */
};

#define RED(i)   colorTable[(i)][0]
#define GREEN(i) colorTable[(i)][1]
#define BLUE(i)  colorTable[(i)][2]
#define ClrTblSize (sizeof(colorTable)/sizeof(colorTable[0]))
static void
output_colortable (void)
{
    int i;

    rtf_output ("{\\colortbl\n");

    for (i = 0; i < ClrTblSize; i++)
      {
	  rtf_output ("\\red%d\\green%d\\blue%d;\n", RED (i), GREEN (i),
		      BLUE (i));
      }

    rtf_output ("}\n");
}

#undef RED
#undef GREEN
#undef BLUE
#undef ClrTblSize

static void
output_rtfUserData (rtfUserData * ud)
{
    /* add the initial bracket */
    rtf_output_char ('{');

    /* font color */
    rtf_output (rtfColors[ud->cCol]);

    /* font face */
    rtf_output ("\\f%d", ud->cFont);

    /* font size */
    rtf_output ("\\fs%d", ud->cFontSize);

    /* italic text */
    if (ud->bIsItalic)
	rtf_output ("\\i");

    /* bold text */
    if (ud->bIsBold)
	rtf_output ("\\b");

    /* underline and strike-through */
    if (ud->bIsUl)
	rtf_output ("\\ul");
    if (ud->bIsStrike)
	rtf_output ("\\strike");

    /* sub/superscript */
    if (ud->bIsSup)
      {
	  rtf_output ("\\super");
      }
    else if (ud->bIsSub)
      {
	  rtf_output ("\\sub");
      }
    /* add the final space */
    rtf_output_char (' ');
}

static void
fill_rtfUserData (rtfUserData * ud, CHP * chp, wvParseStruct * ps)
{
    char *fname = NULL;

    if (!ps->fib.fFarEast)
      {
	  fname = wvGetFontnameFromCode (&ps->fonts, chp->ftcAscii);
      }
    else
      {
	  fname = wvGetFontnameFromCode (&ps->fonts, chp->ftcFE);
      }

    ud->cCol = 0;
    if (chp->ico)
      ud->cCol = chp->ico - 1;    

    ud->cFont = mapFont (fname);
    ud->cFontSize = chp->hps;
    ud->bIsBold = (chp->fBold);
    ud->bIsItalic = (chp->fItalic);
    ud->bIsUl = (chp->kul);
    ud->bIsStrike = (chp->fStrike);
    ud->bIsSup = (chp->iss == 1);
    ud->bIsSub = (chp->iss == 2);
    free(fname);
}

static void
handleImage (Blip * b, long width, long height)
{

    /* TODO: image support */
    wvStream * pwv = NULL;
    size_t size = 0;
    int data = 0;
    int cnt = 0;
    int tag = time (NULL);

    /* short-circuit this method if we don't support
       the incoming format */
    switch (b->type)
      {
      case msoblipPNG:
	  /* conveniently I know how to export to PNG */
	  rtf_output
	      ("{\\*\\shppict{\\pict\\pngblip\\picw%d\\pich%d\\picwgoal\\pichgoal\n",
	       width, height);
	  break;
      case msoblipDIB:
      case msoblipWMF:
      case msoblipEMF:
      case msoblipPICT:
      case msoblipJPEG:
      default:
	  /* TODO: support other image types */
	  return;
      }

    rtf_output ("\bliptag%d{\\*\\blipuid%032x}", tag, tag);

    pwv = b->blip.bitmap.m_pvBits;
    size = wvStream_size (pwv);
    wvStream_rewind(pwv);
    while (cnt < size)
      {
	  if (cnt++ % 64 == 0)
	      rtf_output_char ('\n');
	  rtf_output ("%02x", read_8ubit(pwv));
      }

    rtf_output_char ('}');
}

static int
charProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid)
{

    /* convert incoming character to unicode */
    if (chartype)
	eachchar = wvHandleCodePage (eachchar, lid);

    /* take care of any oddities in Microsoft's character "encoding" */
    /* TODO: does the above code page handler take care of these? */
    if (chartype == 1 && eachchar == 146)
	eachchar = 39;		/* apostrophe */

    switch (eachchar)
      {
      case 13:			/* paragraph end */
	  return 0;

      case 11:			/* hard line break */
	  break;

      case 12:			/* page breaks, section marks */
	  break;

      case 14:			/* column break */
	  break;

      case 19:			/* field begin */
	  /* flush current text buffer */
	  ps->fieldstate++;
	  ps->fieldmiddle = 0;
	  return 0;
      case 20:			/* field separator */
	  ps->fieldmiddle = 1;
	  return 0;
      case 21:			/* field end */
	  ps->fieldstate--;
	  ps->fieldmiddle = 0;
	  return 0;

      default:
	  break;
      }

    /* todo: properly handle fields */
    if (eachchar == 0x13 || eachchar == 0x14)
	return 0;

    /* properly escape this */
    if (eachchar == '{' || eachchar == '}')
	rtf_output_char ('\\');

    rtf_output_char (eachchar);
    return 0;
}

static int
specCharProc (wvParseStruct * ps, U16 eachchar, CHP * achp)
{
    Blip blip;
    wvStream *fil;
    long pos;
    FSPA *fspa;
    PICF picf;
    FDOA *fdoa;

    switch (eachchar)
      {
      case 19:			/* field begin */
	  ps->fieldstate++;
	  ps->fieldmiddle = 0;
	  return 0;
      case 20:			/* field separator */
	  if (achp->fOle2)
	    {
		wvTrace (("Field has an embedded OLE2 object\n"));
	    }
	  ps->fieldmiddle = 1;
	  return 0;
      case 21:			/* field end */
	  ps->fieldstate--;
	  ps->fieldmiddle = 0;
	  return 0;
      default:
	  break;
      }

    /* TODO: properly handle fields */
    if (ps->fieldstate)
      {
	  if (eachchar == 0x13 || eachchar == 0x14)
	      return 0;
      }

    /* image handling */
    switch (eachchar)
      {
      case 0x01:

	  if (achp->fOle2)
	    {
		wvTrace (("embedded OLE2 component. currently unsupported"));
		return 0;
	    }

	  pos = wvStream_tell (ps->data);

	  wvStream_goto (ps->data, achp->fcPic_fcObj_lTagObj);

	  wvGetPICF (wvQuerySupported (&ps->fib, NULL), &picf, ps->data);

	  fil = picf.rgb;

	  if (wv0x01 (&blip, fil, picf.lcb - picf.cbHeader))
	    {
		handleImage (&blip, picf.dxaGoal, picf.dyaGoal);
	    }
	  else
	    {
		wvTrace (("Dom: strange no graphic data 1\n"));
	    }

	  wvStream_goto (ps->data, pos);

	  return 0;
	  break;

      case 0x08:

	  if (wvQuerySupported (&ps->fib, NULL) == WORD8)
	    {
		if (ps->nooffspa > 0)
		  {

		      fspa = wvGetFSPAFromCP (ps->currentcp, ps->fspa,
					      ps->fspapos, ps->nooffspa);

		      if (!fspa)
			{
			    wvError (
				     ("No fspa! Panic and Insanity Abounds!\n"));
			    return 0;
			}

		      if (wv0x08 (&blip, fspa->spid, ps))
			{
			    handleImage (&blip, fspa->xaRight - fspa->xaLeft,
					 fspa->yaBottom - fspa->yaTop);
			}
		      else
			{
			    wvTrace (("Dom: strange no graphic data 2\n"));
			    return 0;
			}
		  }
		else
		  {
		      wvTrace (("nooffspa was <=0 -- ignoring"));
		  }
	    }
	  else
	    {
		wvError (
			 ("pre Word8 0x08 graphic -- unsupported at the moment"));
		fdoa =
		    wvGetFDOAFromCP (ps->currentcp, NULL, ps->fdoapos,
				     ps->nooffdoa);
	    }

      }

    return 0;
}

static int
eleProc (wvParseStruct * ps, wvTag tag, void *props, int dirty)
{
    /* some word structures */
    PAP *apap;
    CHP *achp;
    SEP *asep;
    int iRes;

    rtfUserData *ud = (rtfUserData *) ps->userData;

    switch (tag)
      {
      case SECTIONBEGIN:

	  /* TODO: get smarter */
	  asep = (SEP *) props;
	  rtf_output ("\\sectd\\sbknone\\colsx360\n");

	  ud->bInSec = 1;
	  break;

      case SECTIONEND:
	  ud->bInSec = 0;
	  break;

      case PARABEGIN:
	  apap = (PAP *) props;

	  ud->bInPara = 1;

	  rtf_output ("\\pard");
	  switch (apap->jc)
	    {
	    case 0:		/* left */
		break;
	    case 1:		/* center */
		rtf_output ("\\qc");
		break;
	    case 2:		/* right */
		rtf_output ("\\qr");
		break;
	    default:
		break;
	    }

	  break;

      case PARAEND:		/* pretty much nothing */
	  rtf_output_char ('\n');
	  ud->bInPara = 0;
	  break;

      case CHARPROPBEGIN:
	  achp = (CHP *) props;
	  fill_rtfUserData (ud, achp, ps);
	  output_rtfUserData (ud);
	  break;

      case CHARPROPEND:
	  achp = (CHP *) props;
	  fill_rtfUserData (ud, achp, ps);
	  if (ud->bInPara)
	    {
		rtf_output_char ('}');
	    }
	  break;

      default:
	  break;
      }

    return 0;
}

static int
docProc (wvParseStruct * ps, wvTag tag)
{
    switch (tag)
      {
      case DOCBEGIN:
	  /* print out my rtf preamble */
	  rtf_output ("{\\rtf1\\ansi\\ansicpg1252\\deff0\n");

	  /* now print out a font table */
	  /* and a color table */
	  output_fonttable ();
	  output_colortable ();
	  rtf_output
	      ("\\kerning0\\cf0\\viewkind1\\paperw12240\\paperh15840\\margl1440\\margr1440\\widowctl\n");
	  break;

      case DOCEND:
	  rtf_output ("}\n");
	  ENSURE_BUF ();
	  break;

      default:
	  break;
      }

    return 0;
}

static void
do_version (void)
{
    printf ("wvRTF version %s\n",VERSION);
}

static void
do_help (void)
{
    do_version ();
    printf ("(c) Dom Lachowicz 2000\n");
    printf ("Usage:\n");
    printf ("\t-c --charset=set\n");
    printf ("\t-p --password=pass\n");
    printf ("\t-v --version\n");
    printf ("\t-? --help\n");
    printf ("\nConverts MSWord documents to RTF\n");
}

static char *charset = NULL;

int
main (int argc, char *argv[])
{
    FILE *input;
    char *fname, *password;
    int ret;

    wvParseStruct ps;
    char *dir = NULL;

    rtfUserData ud;

    static struct option long_options[] = {
	{"charset", 1, 0, 'c'},
	{"password", 1, 0, 'p'},
	{"dir", 1, 0, 'd'},
	{"version", 0, 0, 'v'},
	{"help", 0, 0, '?'},
	{0, 0, 0, 0}
    };

    int c, index = 0;

    if (argc < 2)
      {
	  do_help ();
	  return 1;
      }

    while (1)
      {
	  c = getopt_long (argc, argv, "?vc:p:d:", long_options, &index);
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
	    default:
		do_help ();
		return -1;
	    }
      }

    if (optind >= argc)
      {
	  fprintf (stderr, "No file name given to open\n");
	  return -1;
      }

    fname = argv[optind];

    input = fopen (fname, "rb");
    if (!input)
      {
	fprintf (stderr, "Failed to open %s\n", fname);
	  return -1;
      }
    fclose (input);

    wvInit ();
    ret = wvInitParser (&ps, fname);
    ps.filename = fname;
    ps.dir = dir;

    /* set to 0 */
    memset (&ud, 1, sizeof (rtfUserData));
    ps.userData = &ud;

    if (ret & 0x8000)		/* Password protected? */
      {
	  if ((ret & 0x7fff) == WORD8)
	    {
		ret = 0;
		if (password == NULL)
		  {
		      fprintf (stderr,
			       "Password required, this is an encrypted document\n");
		      return -1;
		  }
		else
		  {
		      wvSetPassword (password, &ps);
		      if (wvDecrypt97 (&ps))
			{
			    wvError (("Incorrect Password\n"));
			    return -1;
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
		      return -1;
		  }
		else
		  {
		      wvSetPassword (password, &ps);
		      if (wvDecrypt95 (&ps))
			{
			    wvError (("Incorrect Password\n"));
			    return -1;
			}
		  }
	    }
      }

    if (ret)
      {
	  wvError (("startup error\n"));
	  wvOLEFree (&ps);
	  return -1;
      }

    wvSetElementHandler (&ps, eleProc);
    wvSetDocumentHandler (&ps, docProc);
    wvSetCharHandler (&ps, charProc);
    wvSetSpecialCharHandler (&ps, specCharProc);

    wvText (&ps);

    /* free associated memory */
    wvOLEFree (&ps);

    return 0;
}
