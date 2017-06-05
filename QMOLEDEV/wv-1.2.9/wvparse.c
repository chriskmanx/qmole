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
#include <ctype.h>
#include "wv.h"
#include "utf.h"

#ifdef HAVE_LIBXML2
#include <libxml/parser.h>
#endif

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-utils.h>

int
wvInit (void)
{
  gsf_init ();

#ifdef HAVE_LIBXML2
  xmlInitParser ();
#endif

  return 1;
}

void
wvShutdown (void)
{
  gsf_shutdown ();

#ifdef HAVE_LIBXML2
  xmlCleanupParser ();
#endif
}

static int
wvOpenPreOLE (GsfInput *path, wvStream ** mainfd, wvStream ** tablefd0,
	      wvStream ** tablefd1, wvStream ** data, wvStream ** summary)
{
    int ret = -1;
    U16 magic;

    if (path == NULL)
      {
	  wvError (("Cannot open file $s\n", path));
	  return (-1);
      }

    wvStream_gsf_create (mainfd, path);

    /* what's the lifecycle on these look like? */
    *tablefd0 = *mainfd;
    *tablefd1 = *mainfd;
    *data     = *mainfd;
    *summary  = *mainfd;

    magic = read_16ubit (*mainfd);
    if (0xa5db == magic)
      {
	  wvError (
		   ("Theres a good chance that this is a word 2 doc of nFib %d\n",
		    read_16ubit (*mainfd)));
	  wvStream_rewind (*mainfd);
	  /* return(-1); */
	  return (0);
      }
    else if (0x37fe == magic)
      {
	  wvError (
		   ("Theres a good chance that this is a word 5 doc of nFib %d\n",
		    read_16ubit (*mainfd)));
	  wvStream_rewind (*mainfd);
	  return (0);
      }

    return (ret);
}

static void tokenTreeInit (void);

wvParseStruct * wvCreateParser (void)
{
  return (wvParseStruct *)calloc (1, sizeof (wvParseStruct));
}

void wvDeleteParser (wvParseStruct * ps)
{
  if (ps)
    free (ps);
}

int wvInitParser_gsf (wvParseStruct * ps, GsfInput *path)
{
    int ret = 0, reason = 0;

    memset ( ps, 0, sizeof ( wvParseStruct ) ) ;

    ps->userData = NULL;
    ps->lst = NULL;
    ps->intable = 0;
    ps->endcell = 0;
    ps->vmerges = NULL;
    ps->norows = 0;
    ps->cellbounds = NULL;
    ps->nocellbounds = 0;
    ps->fieldstate = 0;
    ps->fieldmiddle = 0;

    ps->charhandler = 0;
    ps->scharhandler = 0;
    ps->elehandler = 0;
    ps->dochandler = 0;

    ps->password[0] = 0;
    /* set up the token table tree for faster lookups */
    tokenTreeInit ();

    ret = wvOLEDecode_gsf (ps, path, &ps->mainfd, &ps->tablefd0, &ps->tablefd1,
			   &ps->data, &ps->summary);

    switch (ret)
      {
      case 0:
	  break;
      case 2:
	  ret = wvOpenPreOLE (path, &ps->mainfd, &ps->tablefd0, &ps->tablefd1,
			      &ps->data, &ps->summary);
	  if (ret)
	      return (ret);
	  break;
      case 3:
      case 5:
	  wvError (("Bad Ole\n"));
	  return (3);
      default:
	  return (-1);
      }

    if (ps->mainfd == NULL)
      {
	  ret = 4;
	  wvOLEFree (ps);
	  wvError (("Not a word document\n"));
	  return (-1);
      }

    wvGetFIB (&ps->fib, ps->mainfd);

    ps->tablefd = wvWhichTableStream (&ps->fib, ps);

    /* Check the validity of the table stream. */
    if (ps->tablefd == NULL)
      {
	wvOLEFree(ps);
	wvError(("Data Stream Corrupt or Not Readable\n"));
	return (-1);
      }
    
    /* When the data stream is null, it is highly probable
       that the document is corrupt */
    if (ps->data == NULL)
      {
	/* checking for the validity of the Clx data
	   from the table stream for not encrypted files */
	if (!ps->fib.fEncrypted && wvStream_goto(ps->tablefd, ps->fib.fcClx)==-1)
	  {
	    wvOLEFree(ps);
	    wvError(("Data Stream Corrupt or Not Readable\n"));
	    return (-1);
	  }

	/* Reset the stream to the begining */
	wvStream_rewind(ps->tablefd);
      }

    ret = wvQuerySupported (&ps->fib, &reason);

    if ((ret & 0x7fff) != WORD8)
	ps->data = ps->mainfd;

    if ((ret != WORD8) && (ret != WORD7) && (ret != WORD6) && (ret != WORD2))
	/* WORD2 test */
      {
	  /* return the errors and the encrypted files */
	  if (!(ret & 0x8000))
	      wvError (("%s\n", wvReason (reason)));
	  return (ret);
      }
    ret = 0;
    return ret;
}

int
wvInitParser (wvParseStruct * ps, char *path)
{
  GsfInput *input;
  int rval;

  input = gsf_input_stdio_new (path, NULL);
  rval = wvInitParser_gsf (ps, input);

  if (rval == 0)
    ps->filename = path;
  ps->input = input;

  return rval;
}

void
wvSetPassword (const char *pass, wvParseStruct * ps)
{
    int i = 0, len;
    
    char * password = (char *)pass; /* causes no harm */

    /* at this stage we are passing in an utf-8 password and
       later converting it to unicode, we should use the generic
       available mb to wide char stuff, but that isnt very prevalent
       yet, and this is the only time i think i go from utf to 
       unicode */

    while (*password)
      {
	  len = our_mbtowc (&(ps->password[i]), password, 5);
	  i++;
	  password += len;
	  if (i == 16)
	      break;
      }
    ps->password[i] = 0;
}

static Tokenptr tokenTreeRoot = NULL;

static const TokenTable s_Tokens[] = {
    {"*", TT_OTHER},		/* must be FIRST */
    {"begin", TT_BEGIN},
    {"end", TT_END},
    {"title", TT_TITLE},
    {"charset", TT_CHARSET},
    {"version", TT_VERSION},
    {"filename", TT_FILENAME},
    {"htmlgraphic", TT_htmlgraphic},
    {"colspan", TT_COLSPAN},
    {"cellrelwidth", TT_CELLRELWIDTH},
    {"cellrelpagewidth", TT_CELLRELPAGEWIDTH},
    {"rowspan", TT_ROWSPAN},
    {"cellbgcolor", TT_CELLBGCOLOR},
    {"parabgcolor", TT_PARABGCOLOR},
    {"parafgcolor", TT_PARAFGCOLOR},
    {"tablerelwidth", TT_TABLERELWIDTH},
    {"no_rows", TT_no_rows},
    {"no_cols", TT_no_cols},
    {"style", TT_STYLE},
    {"comment", TT_COMMENT},
    {"ibstanno", TT_IBSTANNO},
    {"xstUsrInitl", TT_xstUsrInitl},
    {"mmParaBefore", TT_mmParaBefore},
    {"mmParaAfter", TT_mmParaAfter},
    {"mmParaLeft", TT_mmParaLeft},
    {"mmParaRight", TT_mmParaRight},
    {"mmParaLeft1", TT_mmParaLeft1},
    /* >>---------- PATCH */
    {"stylename", TT_stylename},
    /* << ---------------- */
    {"bordertopstyle", TT_BORDERTopSTYLE},
    {"bordertopcolor", TT_BORDERTopCOLOR},
    {"borderleftstyle", TT_BORDERLeftSTYLE},
    {"borderleftcolor", TT_BORDERLeftCOLOR},
    {"borderrightstyle", TT_BORDERRightSTYLE},
    {"borderrightcolor", TT_BORDERRightCOLOR},
    {"borderbottomstyle", TT_BORDERBottomSTYLE},
    {"borderbottomcolor", TT_BORDERBottomCOLOR},
    {"mmPadTop", TT_mmPadTop},
    {"mmPadRight", TT_mmPadRight},
    {"mmPadBottom", TT_mmPadBottom},
    {"mmPadLeft", TT_mmPadLeft},
    {"mmLineHeight", TT_mmLineHeight},
    {"document", TT_DOCUMENT},
    {"picture", TT_PICTURE},
    {"charentity", TT_CHARENTITY},
    {"pixPicWidth", TT_pixPicWidth},
    {"pixPicHeight", TT_pixPicHeight},
    {"htmlAlignGuess", TT_htmlAlignGuess},
    {"htmlNextLineGuess", TT_htmlNextLineGuess},
    {"section", TT_SECTION},
    {"paragraph", TT_PARA},
    {"table", TT_TABLE},
    {"table.begin", TT_TABLEB},
    {"table.end", TT_TABLEE},
    {"row", TT_ROW},
    {"row.begin", TT_ROWB},
    {"row.end", TT_ROWE},
    {"cell", TT_CELL},
    {"cell.begin", TT_CELLB},
    {"cell.end", TT_CELLE},
    {"lastcell", TT_LASTCELL},
    {"lastcell.begin", TT_LASTCELLB},
    {"lastcell.end", TT_LASTCELLE},
    {"tableoverrides", TT_TABLEOVERRIDES},
    {"ParaBefore", TT_ParaBefore},
    {"ParaAfter", TT_ParaAfter},
    {"ParaLeft", TT_ParaLeft},
    {"ParaRight", TT_ParaRight},
    {"ParaLeft1", TT_ParaLeft1},
    {"VertMergedCells", TT_VertMergedCells},
    {"block", TT_BLOCK},
    {"justification", TT_JUSTIFICATION},
    {"just", TT_JUST},
    {"left", TT_LEFT},
    {"right", TT_RIGHT},
    {"center", TT_CENTER},
    {"asian", TT_ASIAN},
    {"pmargin", TT_PMARGIN},
    {"pborder", TT_PBORDER},
    {"paramargin", TT_PARAMARGIN},
    {"paraborder", TT_PARABORDER},
    {"nfc", TT_nfc},
    {"start", TT_START},
    {"numbering", TT_numbering},
    {"arabic", TT_Arabic},
    {"upperroman", TT_UpperRoman},
    {"lowerroman", TT_LowerRoman},
    {"uppercasen", TT_UpperCaseN},
    {"lowercasen", TT_LowerCaseN},
    {"text", TT_TEXT,},
    {"text.begin", TT_TEXTB,},
    {"text.end", TT_TEXTE,},
    {"olist", TT_OLIST,},
    {"olist.begin", TT_OLISTB,},
    {"olist.end", TT_OLISTE,},
    {"ulist", TT_ULIST,},
    {"ulist.begin", TT_ULISTB,},
    {"ulist.end", TT_ULISTE,},
    {"entry", TT_ENTRY,},
    {"entry.begin", TT_ENTRYB,},
    {"entry.end", TT_ENTRYE,},
    {"character", TT_CHAR},
    {"bold", TT_BOLD},
    {"bold.begin", TT_BOLDB},
    {"bold.end", TT_BOLDE},
    {"italic", TT_ITALIC},
    {"italic.begin", TT_ITALICB},
    {"italic.end", TT_ITALICE,},
    {"strike", TT_STRIKE},
    {"strike.begin", TT_STRIKEB},
    {"strike.end", TT_STRIKEE,},
    {"rmarkdel", TT_RMarkDel,},
    {"rmarkdel.begin", TT_RMarkDelB,},
    {"rmarkdel.end", TT_RMarkDelE,},
    {"outline", TT_OUTLINE,},
    {"outline.begin", TT_OUTLINEB,},
    {"outline.end", TT_OUTLINEE,},
    {"smallcaps", TT_SMALLCAPS,},
    {"smallcaps.begin", TT_SMALLCAPSB,},
    {"smallcaps.end", TT_SMALLCAPSE,},
    {"caps", TT_CAPS,},
    {"caps.begin", TT_CAPSB,},
    {"caps.end", TT_CAPSE,},
    {"vanish", TT_VANISH,},
    {"vanish.begin", TT_VANISHB,},
    {"vanish.end", TT_VANISHE,},
    {"rmark", TT_RMark,},
    {"rmark.begin", TT_RMarkB,},
    {"rmark.end", TT_RMarkE,},
    {"shadow", TT_SHADOW,},
    {"shadow.begin", TT_SHADOWB,},
    {"shadow.end", TT_SHADOWE,},
    {"lowercase", TT_LOWERCASE,},
    {"lowercase.begin", TT_LOWERCASEB,},
    {"lowercase.end", TT_LOWERCASEE,},
    {"emboss", TT_EMBOSS,},
    {"emboss.begin", TT_EMBOSSB,},
    {"emboss.end", TT_EMBOSSE,},
    {"imprint", TT_IMPRINT,},
    {"imprint.begin", TT_IMPRINTB,},
    {"imprint.end", TT_IMPRINTE,},
    {"dstrike", TT_DSTRIKE,},
    {"dstrike.begin", TT_DSTRIKEB,},
    {"dstrike.end", TT_DSTRIKEE,},
    {"super", TT_SUPER,},
    {"super.begin", TT_SUPERB,},
    {"super.end", TT_SUPERE,},
    {"sub", TT_SUB,},
    {"sub.begin", TT_SUBB,},
    {"sub.end", TT_SUBE,},
    {"singleu", TT_SINGLEU,},
    {"singleu.begin", TT_SINGLEUB,},
    {"singleu.end", TT_SINGLEUE,},
    {"wordu", TT_WORDU,},
    {"wordu.begin", TT_WORDUB,},
    {"wordu.end", TT_WORDUE,},
    {"doubleu", TT_DOUBLEU,},
    {"doubleu.begin", TT_DOUBLEUB,},
    {"doubleu.end", TT_DOUBLEUE,},
    {"dottedu", TT_DOTTEDU,},
    {"dottedu.begin", TT_DOTTEDUB,},
    {"dottedu.end", TT_DOTTEDUE,},
    {"hiddenu", TT_HIDDENU,},
    {"hiddenu.begin", TT_HIDDENUB,},
    {"hiddenu.end", TT_HIDDENUE,},
    {"thicku", TT_THICKU,},
    {"thicku.begin", TT_THICKUB,},
    {"thicku.end", TT_THICKUE,},
    {"dashu", TT_DASHU,},
    {"dashu.begin", TT_DASHUB,},
    {"dashu.end", TT_DASHUE,},
    {"dotu", TT_DOTU,},
    {"dotu.begin", TT_DOTUB,},
    {"dotu.end", TT_DOTUE,},
    {"dotdashu", TT_DOTDASHU,},
    {"dotdashu.begin", TT_DOTDASHUB,},
    {"dotdashu.end", TT_DOTDASHUE,},
    {"dotdotdashu", TT_DOTDOTDASHU,},
    {"dotdotdashu.begin", TT_DOTDOTDASHUB,},
    {"dotdotdashu.end", TT_DOTDOTDASHUE,},
    {"waveu", TT_WAVEU,},
    {"waveu.begin", TT_WAVEUB,},
    {"waveu.end", TT_WAVEUE,},
    {"black", TT_BLACK,},
    {"black.begin", TT_BLACKB,},
    {"black.end", TT_BLACKE,},
    {"blue", TT_BLUE,},
    {"blue.begin", TT_BLUEB,},
    {"blue.end", TT_BLUEE,},
    {"cyan", TT_CYAN,},
    {"cyan.begin", TT_CYANB,},
    {"cyan.end", TT_CYANE,},
    {"green", TT_GREEN,},
    {"green.begin", TT_GREENB,},
    {"green.end", TT_GREENE,},
    {"magenta", TT_MAGENTA,},
    {"magenta.begin", TT_MAGENTAB,},
    {"magenta.end", TT_MAGENTAE,},
    {"red", TT_RED,},
    {"red.begin", TT_REDB,},
    {"red.end", TT_REDE,},
    {"yellow", TT_YELLOW,},
    {"yellow.begin", TT_YELLOWB,},
    {"yellow.end", TT_YELLOWE,},
    {"white", TT_WHITE,},
    {"white.begin", TT_WHITEB,},
    {"white.end", TT_WHITEE,},
    {"dkblue", TT_DKBLUE,},
    {"dkblue.begin", TT_DKBLUEB,},
    {"dkblue.end", TT_DKBLUEE,},
    {"dkcyan", TT_DKCYAN,},
    {"dkcyan.begin", TT_DKCYANB,},
    {"dkcyan.end", TT_DKCYANE,},
    {"dkgreen", TT_DKGREEN,},
    {"dkgreen.begin", TT_DKGREENB,},
    {"dkgreen.end", TT_DKGREENE,},
    {"dkmagenta", TT_DKMAGENTA,},
    {"dkmagenta.begin", TT_DKMAGENTAB,},
    {"dkmagenta.end", TT_DKMAGENTAE,},
    {"dkred", TT_DKRED,},
    {"dkred.begin", TT_DKREDB,},
    {"dkred.end", TT_DKREDE,},
    {"dkyellow", TT_DKYELLOW,},
    {"dkyellow.begin", TT_DKYELLOWB,},
    {"dkyellow.end", TT_DKYELLOWE,},
    {"dkgray", TT_DKGRAY,},
    {"dkgray.begin", TT_DKGRAYB,},
    {"dkgray.end", TT_DKGRAYE,},
    {"ltgray", TT_LTGRAY,},
    {"ltgray.begin", TT_LTGRAYB,},
    {"ltgray.end", TT_LTGRAYE,},
    {"fontstr", TT_FONTSTR,},
    {"fontstr.begin", TT_FONTSTRB,},
    {"fontstr.end", TT_FONTSTRE,},
    {"color", TT_COLOR,},
    {"color.begin", TT_COLORB,},
    {"color.end", TT_COLORE,},
    {"ibstrmark", TT_ibstRMark,},
    {"ibstrmarkdel", TT_ibstRMarkDel,},
    {"dttmRMark", TT_dttmRMark,},
    {"dttmRMarkDel", TT_dttmRMarkDel,},
    {"proprmark", TT_PropRMark,},
    {"proprmark.begin", TT_PropRMarkB,},
    {"proprmark.end", TT_PropRMarkE,},
    {"ibstPropRMark", TT_ibstPropRMark,},
    {"dttmPropRMark", TT_dttmPropRMark,},
    {"LasVegas", TT_LasVegas,},
    {"LasVegas.begin", TT_LasVegasB,},
    {"LasVegas.end", TT_LasVegasE,},
    {"BackgroundBlink", TT_BackgroundBlink,},
    {"BackgroundBlink.begin", TT_BackgroundBlinkB,},
    {"BackgroundBlink.end", TT_BackgroundBlinkE,},
    {"SparkleText", TT_SparkleText,},
    {"SparkleText.begin", TT_SparkleTextB,},
    {"SparkleText.end", TT_SparkleTextE,},
    {"MarchingAnts", TT_MarchingAnts,},
    {"MarchingAnts.begin", TT_MarchingAntsB,},
    {"MarchingAnts.end", TT_MarchingAntsE,},
    {"MarchingRedAnts", TT_MarchingRedAnts,},
    {"MarchingRedAnts.begin", TT_MarchingRedAntsB,},
    {"MarchingRedAnts.end", TT_MarchingRedAntsE,},
    {"Shimmer", TT_Shimmer,},
    {"Shimmer.begin", TT_ShimmerB,},
    {"Shimmer.end", TT_ShimmerE,},
    {"ANIMATION", TT_ANIMATION,},
    {"ANIMATION.begin", TT_ANIMATIONB,},
    {"ANIMATION.end", TT_ANIMATIONE,},
    {"DispFldRMark", TT_DispFldRMark,},
    {"DispFldRMark.begin", TT_DispFldRMarkB,},
    {"DispFldRMark.end", TT_DispFldRMarkE,},
    {"ibstDispFldRMark", TT_ibstDispFldRMark,},
    {"dttmDispFldRMark", TT_dttmDispFldRMark,},
    {"xstDispFldRMark", TT_xstDispFldRMark,},
    {"border", TT_BORDER,},
    {"noned", TT_NONED,},
    {"singled", TT_SINGLED,},
    {"thickd", TT_THICKD,},
    {"doubled", TT_DOUBLED,},
    {"number4d", TT_NUMBER4D,},
    {"hairlined", TT_HAIRLINED,},
    {"dotd", TT_DOTD,},
    {"dashlargegapd", TT_DASHLARGEGAPD,},
    {"dotdashd", TT_DOTDASHD,},
    {"dotdotdashd", TT_DOTDOTDASHD,},
    {"tripled", TT_TRIPLED,},
    {"thin-thicksmallgapd", TT_thin_thicksmallgapD,},
    {"thick-thinsmallgapd", TT_thick_thinsmallgapD,},
    {"thin-thick-thinsmallgapd", TT_thin_thick_thinsmallgapD,},
    {"thin-thickmediumgapd", TT_thin_thickmediumgapD,},
    {"thick-thinmediumgapd", TT_thick_thinmediumgapD,},
    {"thin-thick-thinmediumgapd", TT_thin_thick_thinmediumgapD,},
    {"thin-thicklargegapd", TT_thin_thicklargegapD,},
    {"thick-thinlargegapd", TT_thick_thinlargegapD,},
    {"thin-thick-thinlargegapd", TT_thin_thick_thinlargegapD,},
    {"waved", TT_WAVED,},
    {"doublewaved", TT_DOUBLEWAVED,},
    {"dashsmallgapd", TT_DASHSMALLGAPD,},
    {"dashdotstrokedd", TT_DASHDOTSTROKEDD,},
    {"emboss3Dd", TT_EMBOSS3DD,},
    {"engrave3Dd", TT_ENGRAVE3DD,},
    {"direction", TT_DIRECTION,},
    {"dir", TT_DIR,},
    {"defaultd", TT_DEFAULTD,}
};

#define TOKEN_BUFSIZE 1000
static Tokenptr tokenbuf;
static int tokenbufn = 0, tokenfreen = 0;
static void *tokenfreearr[10000];

static void
tokenTreeInsert (int token)
{
    int pos;
    int d;
    const char *s;
    char ch;
    Tokenptr pp, *p;
    /* start at one - TT_OTHER is the zero element. */
    p = &tokenTreeRoot;
    s = s_Tokens[token].m_name;
    pos = 0;
    for (;;)
      {
	  ch = toupper (s[pos]);
	  pp = *p;
	  while (pp != NULL)
	    {
		d = ch - pp->splitchar;
		if (d == 0)
		  {
		      if (s[pos] == 0)
			  break;
		      pos++;
		      ch = toupper (s[pos]);
		      p = &(pp->eqkid);
		  }
		else if (d < 0)
		    p = &(pp->lokid);
		else
		    p = &(pp->hikid);
		pp = *p;
	    }
	  if (tokenbufn == 0)
	    {
		tokenbuf = (Tokenptr) wvMalloc (TOKEN_BUFSIZE *
						sizeof (Tokennode));
		tokenfreearr[tokenfreen++] = (void *) tokenbuf;
		tokenbufn = TOKEN_BUFSIZE;
	    }
	  tokenbufn--;
	  *p = &(tokenbuf[tokenbufn]);
	  pp = *p;
	  pp->splitchar = ch;
	  pp->lokid = pp->eqkid = pp->hikid = 0;
	  pp->token = 0;
	  if (s[pos] == 0)
	    {
		pp->token = token;
		break;
	    }
	  pos++;
	  p = &(pp->eqkid);
      }
}

/* this routine will insert the tokens in a balanced way
as long as the token table is sorted. */
static void
tokenTreeRecursiveInsert (int min, int max)
{
    int token;
    if (min > max)
	return;
    token = (min + max) / 2;
    tokenTreeInsert (token);
    tokenTreeRecursiveInsert (token + 1, max);
    tokenTreeRecursiveInsert (min, token - 1);
}

static void
tokenTreeInit (void)
{
    tokenTreeRecursiveInsert (1, TokenTableSize - 1);
}

void
tokenTreeFreeAll (void)
{
    int i;
    for (i = 0; i < tokenfreen; i++)
	wvFree (tokenfreearr[i]);
    tokenfreen = 0;
    tokenbufn = 0;
    tokenbuf = NULL;
    tokenTreeRoot = NULL;
}

 /* this loop is called *a lot* so I've made it a binary search */
static unsigned int
s_mapNameToToken (const char *name)
{
    Tokenptr p;
    int i = 0;
    char ch;

    p = tokenTreeRoot;

    ch = toupper (name[i]);
    while (p)
      {
	  if (ch < p->splitchar)
	      p = p->lokid;
	  else if (ch == p->splitchar)
	    {
		if (name[i] == 0)
		    return p->token;
		p = p->eqkid;
		i++;
		ch = toupper (name[i]);
	    }
	  else
	      p = p->hikid;
      }
    /* this is one of several lines of code that rely
       on TT_OTHER being first in the token table. */
    return 0;
}

unsigned int
wvMapNameToTokenType (const char * name)
{
    unsigned int tokenIndex = s_mapNameToToken (name);
    return s_Tokens[tokenIndex].m_type;
}
