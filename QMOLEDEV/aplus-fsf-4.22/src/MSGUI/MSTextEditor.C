///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif

#include <MSGUI/MSTextEditor.H>
#include <MSGUI/MSKeyClassCallback.H>

///////////////////////////////////////////////////////////////////////////////

const unsigned long MSTextEditorDefaultBlinkRate=300;

///////////////////////////////////////////////////////////////////////////////
//                         ccfilter functions declarations
///////////////////////////////////////////////////////////////////////////////

#ifndef ccfilter_h
#define ccfilter_h
/*
** ccfilter.h
**
**	This header defines the functions related to the Character
**	Content Filter. Some these are defined in ccfilter.c and
**	some must be defined by the application using the ccfilter.
**
**	Character Content Filter interprets the character stream
**	according to various standards (ISO 2022, CCITT T.51)
**
** Copyright 1992, 1993 by Markku Savela and 
**	Technical Research Centre of Finland
*/


enum ccf_Gs
    {
	ccf_G0 = 0,
	ccf_G1 = 1,
	ccf_G2 = 2,
	ccf_G3 = 3,
	ccf_CC = 4
    };

enum ccf_Cs
    {
	ccf_C0 = 0,
	ccf_C1 = 1
    };

/*
** ccf_Gn
**	ccf_Gn funtion is called to produce the graphic characters
**	extracted from the stream. The function is supplied with the
**	number of *characters* and a pointer to string of bytes.
**	Each character is represented by one or more bytes in the
**	string, depending on what kind of character set is designated
**	into the Gn set. (Application must remember this from
**	ccf_Designate_G function call). ccf_CC functions like ccf_Gn
**	functions, but is called when filter is working in passthrough
**	mode (Complete Code, coding system different from ISO 2022).
**
**	n is always > 0, when these are called.
*/
typedef void (*ccf_Gn)(
		       void *,	/* client data */
		       ccf_Gs,	/* Identify G0, G1, G2, G3 or CC */
		       char *,	/* String of data bytes */
		       int	/* Number of *characters* in the string */
		       );

/*
** ccf_Cn
**	ccf_Cn is called for codes belonging to the control sets C0 and C1.
**	(control codes that are used in code extensions are handled
**	internally within the ccfilter).
**
**	The argument c is always within range 0..31.
*/
typedef void (*ccf_Cn)(
		       void *,	/* client data */
		       ccf_Cs,	/* Identify C0 or C1 */
		       int	/* Control code: always in range 0..31 */
		       );
/*
** ccf_ESC
**	ccf_ESC is called for unrecognized escape sequences.
**	m is number of intermediate bytes, and I points to
**	the first intermediate (if m > 0).
*/
typedef void (*ccf_ESC)(
			void *,	/* client data */
			char *,	/* Intermediate bytes pointer */
			int,	/* Number of intermediate bytes */
			int	/* Final Character (F) */
);

/*
** ccf_CSI
**	ccf_CSI is called for all CSI sequences.
**
**	n = number of parameters ( >= 0)
**	p = pointer to integers representing the parameter values,
**	    (value 0 indicates a request for default value).
**	    (parameter characters are '0'..'?')
**	I = Intermediate character(s) (' '..'/')
**	F = Final character ('@'..'~').
**	P = Private indicator ('<', '=', '>' or '?')
*/
typedef void (*ccf_CSI)(
			void *,	/* client data */
			int,	/* Private Indicator, if non-zero */
			int *,	/* Parameter values array (p) */
			int,	/* Number of parameters >= 0 (n) */
			char *,	/* Intermediate bytes pointer */
			int,	/* Number of intermediate bytes */
			int	/* Final Character (F) */
			);
/*
** ccf_DG
**	Designate and invoke a character set into Gn. The function
**	must return the number of bytes per character in this set.
**	Return value 0 is an error and the designation of the Gn
**	doesn't change.
**
**	Gx = ccf_CC, for complete set. After this ccf_Filter will
**		be in a "pass through" state until ccf_Reset is called.
**		All characters are produced through ccf_CC function.
**
**	S  = zero for 94 character set, and non-zero for 96 character set.
**	M  = MSFalse(=0), if single byte set, MSTrue(=1), if multibyte set
**	F  = Final character, identifying the registered set.
*/
typedef int (*ccf_DG)(
		      void *,	/* client data */
		      ccf_Gs,	/* Gx */
		      int,	/* Zero = 94-set, and non-Zero = 96-set */
		      int,	/* M Single/Multibyte indicator */
		      int	/* F Final Character */
		      );
/*
** ccf_DC
**	Designate and invoke control characters. Return Non-Zero,
**	if error.
**
**	Cx = 0, for C0
**	   = 1, for C1
**	F  = Final character, identifying the registered set.
*/
typedef int (*ccf_DC)(
		      void *,	/* client data */
		      ccf_Cs,	/* Cx */
		      int	/* F Final Byte */
		      );
/*
** ccf_Feed
**	Feed in a portion of a byte stream. Note, that escape
**	sequences and multibyte characters can extend over portions
**	(e.g. there is no requirement that an escape sequence must
**	be contained within single portion as a whole). Returns on
**	success. (No error returns defined at the moment.)
*/
int ccf_Feed(
	       ccf_Context,	/* Context to feed (from ccf_Reset) */
	       const char *,		/* Pointer to bytes */
	       int		/* Number of bytes to feed */
);

/*
** ccf_Open
**	Open the filter stream. Returns a pointer to a filter context block
**	that must be given as parameter to ccf_Filter.
*/
ccf_Context ccf_Open(
		      void *,	/* client data (for callbacks) */
		      ccf_Gn,	/* Craphic characters callback */
		      ccf_Cn,	/* Control characters callback */
		      ccf_ESC,	/* ESC Sequence callback */
		      ccf_CSI,	/* CSI Sequence callback */
		      ccf_DG,	/* Designate Graphic callback */
		      ccf_DC	/* Designate Control callback */
		      );
/*
** ccf_Close
**	Close ccf_Context (release data associated with the context)
*/
void ccf_Close(
	       ccf_Context
);

#endif

///////////////////////////////////////////////////////////////////////////////
//                            ccfilter functions definition
///////////////////////////////////////////////////////////////////////////////

/*  Character Content Filter interprets the character stream
**  according to various standards (ISO 2022, ISO 649, ISO6429,
**  ISO 6937).
**
** *NOTE*
**  In various places this module uses hex values for byte
**  codes. This is *intentional* and *required* so that the
**  generated code will be independent of the characterset
**  the C compiler is using.
**
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
**  Technical Research Centre of Finland
*/

#define MAX_MULTIPLE_BYTES (4)
#define NumberOf(array)      (sizeof(array) / sizeof(array[0]))
#include <stdlib.h>

typedef enum
{
  NUL,  SOH,  STX,  ETX,  EOT,  ENQ,  ACK,  BEL,
  BS ,  HT ,  LF ,  VT ,  FF ,  CR ,  SO ,  SI ,
  DLE,  DC1,  DC2,  DC3,  DC4,  NAK,  SYN,  ETB,
  CAN,  EM ,  SUB,  ESC,  FS ,  GS ,  RS ,  US
} C0_Code;

typedef enum
{
  X80,  X81,  BPH,  NBH,  IND,  NEL,  SSA,  ESA,
  HTS,  HTJ,  VTS,  PLD,  PLU,  RI ,  SS2,  SS3,
  DCS,  PU1,  PU2,  STS,  CCH,  MW ,  SPA,  EPA,
  SOS,  X99,  X9A,  CSI,  ST ,  OSC,  PM ,  APC
} C1_Code;

typedef void (*CodeHandler)(ccf_Context, int Code);

static void Handler_CC(ccf_Context, int);/* General Complete Code Handler */
static void Handler_C0(ccf_Context, int);/* General C0 Handler */
static void Handler_C1(ccf_Context, int);/* General C1 Handler */
static void Handler_GC(ccf_Context, int);/* General GL/GR Handler */
static void Handler_GC_Switch(ccf_Context, int); /* General GL/GR switch handler */
static void Handler_GLSS2(ccf_Context, int);/* Single Shift G2 Handler */
static void Handler_GLSS3(ccf_Context, int);/* Single Shift G3 Handler */
static void Handler_ESC(ccf_Context, int);/* General ESC handler */
static void Handler_CSI(ccf_Context, int);/* General CSI handler */

typedef struct GC_type
{
  CodeHandler handler;
  ccf_Gs Gn;
} GC_type;

/*
**  CSI Interpreter State
*/
typedef struct CSI_State
{
#  define CSI_STATE_INITIAL (0)
#  define CSI_STATE_PARAMETER (1)
#  define CSI_STATE_INTERMEDIATE (2)

  int state;
  int isprivate;    /* Private sequence indicator */
  int parameter[30];  /* Parameters of a CSI sequence */
  int current;    /* Current Parameter being interpreted */
  char intermediate[4];  /* Intermediate Codes */
  int count;    /* Number of Intermediate characters */
} CSI_State;

/*
** ESC Interpreter State
*/
typedef struct ESC_State
{
  char intermediate[9];  /* Intermediate Codes */
  int count;    /* Number of Intermediate characters */
} ESC_State;

typedef struct ccf_ContextRec
{
  void *client_data;
  ccf_Gn call_Gn;
  ccf_Cn call_Cn;
  ccf_ESC call_ESC;
  ccf_CSI call_CSI;
  ccf_DG call_DG;
  ccf_DC call_DC;
  int multiple_byte;  /* Bytes remaining from next character */
  int GC_multiple_byte;  /* Bytes in one character in current GC */
  int run_length;    /* Length of contiguous bytes in same set */
  int run_characters;  /* Number of completed contiguous characters */
  char *run_start;  /* First byte of the run */
  CodeHandler C0;    /* C0 Control Handler */
  CodeHandler C1;    /* C1 Control Handler */
  GC_type GL;    /* Current GL state */
  GC_type GR;    /* Current GR state */
  GC_type *current;  /* Current GC (GL or GR) */
  GC_type *other;    /* Other GC (GL or GR) */
  int saveS;    /* Temporary for interpreting Gn designation */
  ccf_Gs saveGn;    /* Temporary for interpreting Gn designation */
  ccf_Cs saveCn;    /* Temporary for interpreting Cn designation */
  int bytes[5];    /* Bytes in sets in G0, G1, G2, G3 and CC */
  char hold_over[MAX_MULTIPLE_BYTES];
  char *holding;
  union 
  {
    CSI_State csi;
    ESC_State esc;
  } i;
} ccf_ContextRec;


/*
** ExitControl
**  Must be called when the last byte of a control sequence
**  has been processed. This prepares the system for the next
**  run of normal graphic characters.
*/
static void ExitControl(ccf_Context cc)
{
  cc->run_start += cc->run_length; /* Skips over the control sequence */
  cc->run_length = 0;
  cc->current = &cc->GL;
  cc->other = &cc->GR;
  cc->GL.handler = Handler_GC;
  cc->GR.handler = Handler_GC_Switch;
  cc->multiple_byte = cc->GC_multiple_byte = cc->bytes[cc->GL.Gn];
}

/*
** EnterControl
**  Must be called when a control sequence starts (does not cause
**  any harm if called repeatedly within the control sequence).
**  Flush out a sequence of graphic characters preceding the
**  control sequence (if any). Any incomplete multi-byte character
**  will be scratched by this.
*/
static void EnterControl(ccf_Context cc)
{
  if (cc->run_characters > 0)
   {
     (*cc->call_Gn)(cc->client_data,  cc->current->Gn,
		    cc->run_start, cc->run_characters);
     cc->run_characters = 0;
   }
}

/*
** Handler_CSI
**  Stay in this handler until a Final Character is reached or
**  an error is detected (in which case the whole sequence up
**  to this point is discarded).
*/
static void Handler_CSI(ccf_Context cc, int Code)
{
  switch (Code)
   {
   case 0x30: /* 0 */
   case 0x31: /* 1 */
   case 0x32: /* 2 */
   case 0x33: /* 3 */
   case 0x34: /* 4 */
   case 0x35: /* 5 */
   case 0x36: /* 6 */
   case 0x37: /* 7 */
   case 0x38: /* 8 */
   case 0x39: /* 9 */
     if (cc->i.csi.state > CSI_STATE_PARAMETER)
     break;
     cc->i.csi.state = CSI_STATE_PARAMETER;
     cc->i.csi.parameter[cc->i.csi.current] =
     cc->i.csi.parameter[cc->i.csi.current]*10 + Code-0x30;
     return;  /* Continue CSI */

   case 0x3A: /* : */
   case 0x3B: /* ; */
     if (cc->i.csi.state > CSI_STATE_PARAMETER)
     break;
     cc->i.csi.state = CSI_STATE_PARAMETER;
     if (cc->i.csi.current < NumberOf(cc->i.csi.parameter)-1)
     ++cc->i.csi.current;
     cc->i.csi.parameter[cc->i.csi.current] = 0;
     return;  /* Continue CSI */
   case 0x3C: /* < */
   case 0x3D: /* = */
   case 0x3E: /* > */
   case 0x3F: /* ? */
     if (cc->i.csi.state != CSI_STATE_INITIAL)
     break; /* -- Error! */
     cc->i.csi.isprivate = Code;
     cc->i.csi.state = CSI_STATE_PARAMETER;
     return;  /* Continue CSI */
   case 0x20: /*   */
   case 0x21: /* ! */
   case 0x22: /* " */
   case 0x23: /* # */
 case 0x24: /* $ */
   case 0x25: /* % */
   case 0x26: /* & */
   case 0x27: /* ' */
   case 0x28: /* ( */
   case 0x29: /* ) */
   case 0x2A: /* * */
   case 0x2B: /* + */
   case 0x2C: /* , */
   case 0x2D: /* - */
   case 0x2E: /* . */
   case 0x2F: /* / */
     /*
     **  Intermediate Codes
     */
     cc->i.csi.state = CSI_STATE_INTERMEDIATE;
     if (cc->i.csi.count == NumberOf(cc->i.csi.intermediate))
     break; /* -- ERROR, abort sequence */
     cc->i.csi.intermediate[cc->i.csi.count++] = Code;
     return;
   default:
     /*
     ** Call different functions depending on what kind of
     ** sequence we had...
     */
     cc->i.csi.current += 1;
     (*cc->call_CSI)(cc->client_data,
		     cc->i.csi.isprivate,
		     cc->i.csi.parameter,
		     cc->i.csi.current,
		     cc->i.csi.intermediate,
		     cc->i.csi.count,
		     Code);
     break;
   }
  ExitControl(cc);
}

/*
** Handler_C1
**  Handle C1 control codes.
*/
static void Handler_C1(ccf_Context cc, int Code)
{
  EnterControl(cc);
  switch (Code)
   {
   case SS2:  /* (N) Single Shift Two (Introducer) */
     cc->multiple_byte = cc->bytes[ccf_G2];
     cc->run_start += cc->run_length;
     cc->run_length = 0;
     cc->GL.handler = Handler_GLSS2;
     cc->GR.handler = Handler_GLSS2;
     break;
   case SS3:  /* (O) Single Shift Three (Introducer) */
     cc->multiple_byte = cc->bytes[ccf_G3];
     cc->run_start += cc->run_length;
     cc->run_length = 0;
     cc->GL.handler = Handler_GLSS3;
     cc->GR.handler = Handler_GLSS3;
     break;
   case CSI:  /* ([) Control Sequence Introducer */
     cc->i.csi.state = CSI_STATE_INITIAL;
     cc->i.csi.current = 0;
     cc->i.csi.count = 0;
     cc->i.csi.isprivate = 0;
     cc->i.csi.parameter[0] = 0;
     cc->GL.handler = Handler_CSI;
     cc->GR.handler = Handler_CSI;
     break;
   default:
     (*cc->call_Cn)(cc->client_data, ccf_C1, Code);
     ExitControl(cc);
     break;
   }
}

/*
** Handler_LoadCx
**  Changing of Control Repertoire is not really implemented.
*/
static void Handler_LoadCx(ccf_Context cc, int)
{
  ExitControl(cc);
}

/*
** Handler_LoadGx
**  Handle designations of single-byte character sets
**
**  94  G0  ESC 2/8 F
**    G1  ESC 2/9 F
**    G2  ESC 2/10 F
**    G3  ESC 2/11 F
**
**  96  G1  ESC 2/13 F
**    G2  ESC 2/14 F
**    G3  ESC 2/15 F
**         ^
**        (this & 7) = SaveGx
**
**  Complete Code  ESC 2/5 F  (SaveGx = -1)
*/
static void Handler_LoadGx(ccf_Context cc, int Code)
{
  ccf_Gs save_Gn = cc->saveGn; /* Just in case  */
  int save_S = cc->saveS;

  Code = (*cc->call_DG)(cc->client_data, save_Gn, save_S, MSFalse, Code);
  if (Code == 0)
  ;
  else if (save_Gn == ccf_CC)
   {
     /*
     ** Escape from ISO 2022
     */
     cc->GL.handler = cc->C0 = cc->C1 = Handler_CC;
     cc->GL.Gn = ccf_CC;
     cc->GR = cc->GL;
   }
  else
  cc->bytes[save_Gn] =
  Code > MAX_MULTIPLE_BYTES ? MAX_MULTIPLE_BYTES : Code;
  ExitControl(cc);
}

/*
** Handler_LoadMGx
**  Handle designations of multiple-byte character sets
**
**  n x 94  G0  ESC 2/4 F
**    G1  ESC 2/4 2/9 F
**    G2  ESC 2/4 2/10 F
**    G3  ESC 2/4 2/11 F
**
**  n x 96  G1  ESC 2/4 2/13 F
**    G2  ESC 2/4 2/14 F
**    G3  ESC 2/4 2/15 F
**          ^
**        (this & 7) = SaveGx
** *NOTE*
**  this function *leaks*, it passes any number of
**  intermediate codes and remembers only the last.
*/
static void Handler_LoadMGx(ccf_Context cc, int Code)
{
  ccf_Gs save_Gn = cc->saveGn; /* (Paranoid programming ;) */
  int save_S = cc->saveS;

  if (Code == 0x29 || Code == 0x2A || Code == 0x2B ||
      Code == 0x2D || Code == 0x2E || Code == 0x2F)
   {
     cc->saveGn = (ccf_Gs)(Code & 3);
     cc->saveS = (Code & 4) != 0;
   }
  else if (Code > 0x3F)
   {
     Code=(*cc->call_DG)(cc->client_data,save_Gn,save_S,MSTrue,Code);
     if (Code)
     cc->bytes[cc->saveGn] =
     Code > MAX_MULTIPLE_BYTES ?
     MAX_MULTIPLE_BYTES : Code;
     ExitControl(cc);
   }
}

static void Handler_ESC_Intermediate(ccf_Context cc, int Code)
{
  if (Code < 0x30)
   {
     if (cc->i.esc.count < NumberOf(cc->i.esc.intermediate))
     cc->i.esc.intermediate[cc->i.esc.count++] = Code;
   }
  else
   {
     (cc->call_ESC)(cc->client_data,
		    cc->i.esc.intermediate,
		    cc->i.esc.count,
		    Code);
     ExitControl(cc);
   }
}

static void Handler_ESC(ccf_Context cc, int Code)
{
  if (Code < 0x30)
  /*
  ** Intermediate character
  */
  switch (Code)
   {
   case 0x21:
   case 0x22:
     cc->saveCn = (ccf_Cs)(Code & 1);
     cc->GL.handler = (CodeHandler)Handler_LoadCx;
     cc->GR.handler = (CodeHandler)Handler_LoadCx;
     break;
   case 0x24:
     cc->saveGn = ccf_G0; /* Default to n x 94, G0 */
     cc->saveS = 0;
     cc->GL.handler = (CodeHandler)Handler_LoadMGx;
     cc->GR.handler = (CodeHandler)Handler_LoadMGx;
     break;
   case 0x25: /* CC, 256 (Complete Code) */
     cc->saveGn = ccf_CC;
     cc->GL.handler = (CodeHandler)Handler_LoadGx;
     cc->GR.handler = (CodeHandler)Handler_LoadGx;
     break;
   case 0x28: /* G0, 94 */
   case 0x29: /* G1, 94 */
   case 0x2A: /* G2, 94 */
   case 0x2B: /* G3, 94 */

   case 0x2D: /* G1, 96 */
   case 0x2E: /* G2, 96 */
   case 0x2F: /* G3, 96 */
     cc->saveGn = (ccf_Gs)(Code & 3);
     cc->saveS = (Code & 4) != 0;
     cc->GL.handler = (CodeHandler)Handler_LoadGx;
     cc->GR.handler = (CodeHandler)Handler_LoadGx;
     break;
   default:
     cc->GL.handler = (CodeHandler)Handler_ESC_Intermediate;
     cc->GR.handler = (CodeHandler)Handler_ESC_Intermediate;
     cc->i.esc.count = 1;
     cc->i.esc.intermediate[0] = Code;
     break;
   }
  else if (Code < 0x60)
  /*
  ** 7 bit representation of C1 control
  */
  Handler_C1(cc, Code & 0x1F);
  else
   {
     if (Code == 0x7E) /* LS1R */
     cc->GR.Gn = ccf_G1;
     else if (Code == 0x6E) /* LS2 */
     cc->GL.Gn = ccf_G2;
     else if (Code == 0x7D) /* LS2R */
     cc->GR.Gn = ccf_G2;
     else if (Code == 0x6F) /* LS3 */
     cc->GL.Gn = ccf_G3;
     else if (Code == 0x7C) /* LS3R */
     cc->GR.Gn = ccf_G3;
     else
     (*cc->call_ESC)(cc->client_data,
		     (char *)0, 0, Code);
     ExitControl(cc);
   }
}

/*
** Handler_C0
*/
static void Handler_C0(ccf_Context cc, int Code)
{
  EnterControl(cc);
  switch (Code)
   {
   case ESC:
     cc->GL.handler = Handler_ESC;
     cc->GR.handler = Handler_ESC;
     return;
   case SI:  /* LS0, lock shift G0 */
     cc->GL.Gn = ccf_G0;
     break;
   case SO:  /* LS1, lock shift G1 */
     cc->GL.Gn = ccf_G1;
     break;
   case CAN:
     /* Will abort CSI/ESC sequence if in progress */
   default:
     (*cc->call_Cn)(cc->client_data, ccf_C0, Code);
     break;
   }
  ExitControl(cc);
}


/*
** Handler_CC
*/
static void Handler_CC(ccf_Context, int)
{
}

/*
** Handler_GC
**  Passes consecutive bytes belonging to the same set.
*/
static void Handler_GC(ccf_Context cc, int)
{
  if (--cc->multiple_byte == 0)
   {
     /*
     ** One complete multiple-byte or single-byte character
     ** completed.
     */
     cc->multiple_byte = cc->GC_multiple_byte;
     cc->run_characters += 1;
   }
}

/*
** Handler_GC_Switch
**  GC_Switch is called when after a sequence of characters
**  in GL or GR set, a byte belonging to another set arrives.
*/
static void Handler_GC_Switch(ccf_Context cc, int Code)
{
  GC_type *temp;
  /*
  ** If a multiple-byte sequence within GL or GR is interrupted by
  ** a byte belonging to the other set, just include this byte into
  ** the multiple byte character and continue using the same GL/GR set.
  ** (This is an error condition not specified in the standard and
  ** this way of handling it is the usual practise in the existing
  ** implementations --msa)
  */
  if (cc->multiple_byte == cc->GC_multiple_byte)
   {
     /*
     ** Starting a Run on the other set. Flush out any accumulated
     ** characters and initialize for new alternate run.
     */
     if (cc->run_characters > 0)
     (*cc->call_Gn)(cc->client_data,
		    cc->current->Gn,
		    cc->run_start,
		    cc->run_characters);
     cc->run_start += cc->run_length - 1;
     cc->run_length = 1;
     cc->run_characters = 0;
     temp = cc->current;
     cc->current = cc->other;
     cc->other = temp;
     cc->other->handler = Handler_GC_Switch;
     cc->current->handler = Handler_GC;
     cc->multiple_byte =
     cc->GC_multiple_byte = cc->bytes[cc->current->Gn];
   }
  Handler_GC(cc, Code);
}

static void Handler_GLSS2(ccf_Context cc, int)
{
  if (--cc->multiple_byte == 0)
   {
     (*cc->call_Gn)(cc->client_data, ccf_G2, cc->run_start, 1);
     ExitControl(cc);
   }
}

static void Handler_GLSS3(ccf_Context cc, int)
{
  if (--cc->multiple_byte == 0)
   {
     (*cc->call_Gn)(cc->client_data, ccf_G3, cc->run_start, 1);
     ExitControl(cc);
   }
}

/*
** ccf_Feed
**  Decode a portion of the stream.
*/
int ccf_Feed(ccf_Context cc, const char *s, int n)
{
  register int Code;
  register char *h;
  if (cc->multiple_byte < cc->GC_multiple_byte)
   {
     /*
     ** A multiple-byte character was divided across portions.
     ** Assuming that this will not really be a common occurrence,
     ** we do it by rather brute force method.
     */
     h = cc->holding;
     cc->run_start = cc->hold_over;
     cc->run_length = h - cc->hold_over;
     cc->run_characters = 0;
     do
      {
	if (n == 0)
	 {
	   /*
	   ** This portion didn't complete the character.
	   */
	   cc->holding = h;
	   return 0;
	 }
	Code = *(unsigned char *)s;
	/*
	** Occurrence of any control code will abort
	** the multibyte sequence.
	*/
	if ((0x60 & Code) == 0)
        break;
	--n;
	cc->run_length += 1;
	*h++ = Code;
	s++;
	if (Code & ~(0x7F))
        (*cc->GR.handler)(cc, Code & 0x7F);
	else
        (*cc->GL.handler)(cc, Code);
      } while (cc->multiple_byte < cc->GC_multiple_byte);
     if (cc->run_characters > 0)
     (*cc->call_Gn)(cc->client_data,
		    cc->current->Gn,
		    cc->hold_over, 1);
   }
  cc->run_start = (char *)s;
  cc->run_length = 0;
  cc->run_characters = 0;
  while (--n >= 0)
   {
     cc->run_length += 1;
     Code = *(unsigned char *)s++;
     if (Code & ~(0x7F))
     (*(Code&0x60 ? cc->GR.handler:cc->C1))(cc,Code & 0x7F);
     else  
     (*(Code&0x60 ? cc->GL.handler:cc->C0))(cc,Code);
   }
  /*
  ** Flush out all remaining full characters from this portion
  */
  if (cc->run_characters > 0)
  (*cc->call_Gn)(cc->client_data,
		 cc->current->Gn,
		 cc->run_start,
		 cc->run_characters);
  /*
  ** If multiple-byte character is being devided across portions,
  ** copy the first part into HoldOver.
  */
  if (cc->multiple_byte < cc->GC_multiple_byte)
   {
     s = cc->run_start + cc->run_characters * cc->GC_multiple_byte;
     n = cc->GC_multiple_byte - cc->multiple_byte;
     for (h = cc->hold_over; --n >= 0;)
     *h++ = *s++;
     cc->holding = h;
   }
  return 0;
}

ccf_Context ccf_Open(void *client_data, ccf_Gn Gn, ccf_Cn Cn, ccf_ESC ESC,
		     ccf_CSI CSI, ccf_DG DG, ccf_DC DC)
{
  ccf_Context cc = (ccf_Context)calloc(1, sizeof(ccf_ContextRec));
  int i;

  if (cc)
   {
     cc->client_data = client_data;
     cc->call_Gn = Gn;
     cc->call_Cn = Cn;
     cc->call_ESC = ESC;
     cc->call_CSI = CSI;
     cc->call_DG = DG;
     cc->call_DC = DC;
     cc->C0 = Handler_C0;
     cc->C1 = Handler_C1;
     cc->GL.handler = Handler_GC;
     cc->GL.Gn = ccf_G0;
     cc->GR.handler = Handler_GC_Switch;
     cc->GR.Gn = ccf_G2;
     for (i = 0; i < NumberOf(cc->bytes); ++i)
     cc->bytes[i] = 1;
     cc->multiple_byte=cc->GC_multiple_byte=cc->bytes[cc->GL.Gn];
     cc->current = &cc->GL;
     cc->other = &cc->GR;
   }
  return cc;
}

void ccf_Close(ccf_Context cc)
{
  if (cc) free((char *)cc);
}

typedef void (MSTextEditor::*PMFV)(void);

typedef struct 
{
  char      *_str;
  PMFV       _func;
} TextKeys;

/*
  class KT : public MSKeyCallback
{
  PMFV _func; 
public:
  KT(PMFV func_):
    _func(func_){}
  ~KT(){}
  MSBoolean process(MSWidget* widget_,const MSKeyPress&)
    {   
      (((MSTextEditor*)widget_)->*_func)(); 
      return MSTrue;
    } 
};
*/
typedef MSKeyClassCallback<MSTextEditor> KT;

static TextKeys EmacsKeyTable[]=
{ 
  {"<Key>Escape",    &MSTextEditor::escape},
  {"<Key>Return",    &MSTextEditor::insertNewLine},
  {"<Key>BackSpace", &MSTextEditor::deleteBackwardChar},
  {"<Key>Delete",    &MSTextEditor::deleteBackwardChar},
  {"<Key>Up",        &MSTextEditor::movePreviousLine},
  {"<Key>Down",      &MSTextEditor::moveNextLine},
  {"<Key>Right",     &MSTextEditor::moveForwardChar},
  {"<Key>Left",      &MSTextEditor::moveBackwardChar},
  {"Ctrl<Key>Right", &MSTextEditor::moveForwardWord},
  {"Ctrl<Key>Left",  &MSTextEditor::moveBackwardWord},
  {"Ctrl<Key>a",     &MSTextEditor::moveToLineStart},
  {"Ctrl<Key>e",     &MSTextEditor::moveToLineEnd},
  {"Ctrl<Key>k",     &MSTextEditor::killToEndOfLine},
  {"Ctrl<Key>d",     &MSTextEditor::deleteForwardChar},
  {"Ctrl<Key>f",     &MSTextEditor::moveForwardChar},
  {"Ctrl<Key>b",     &MSTextEditor::moveBackwardChar},
  {"Ctrl<Key>y",     &MSTextEditor::selectLine},
  {"Ctrl<Key>p",     &MSTextEditor::movePreviousLine},
  {"Ctrl<Key>n",     &MSTextEditor::moveNextLine },
  {"Ctrl<Key>l",     &MSTextEditor::redrawDisplay},
  {"Ctrl<Key>r",     &MSTextEditor::switchColor},
  {"<Key>Tab",       &MSTextEditor::tab},
  {"Shift<Key>Tab",  &MSTextEditor::shiftTab},
  {"!<Key>F27",      &MSTextEditor::moveBeginningOfFile},
  {"<Key>Home",      &MSTextEditor::moveBeginningOfFile},
  {"!<Key>R13",      &MSTextEditor::moveEndOfFile},
  {"<Key>End",       &MSTextEditor::moveEndOfFile},
  {"!<Key>F29",      &MSTextEditor::moveBackwardPage},
  {"<Key>Prior",     &MSTextEditor::moveBackwardPage},
  {"!<Key>F35",      &MSTextEditor::moveForwardPage},
  {"<Key>Next",      &MSTextEditor::moveForwardPage},
  {0,0}
};

///////////////////////////////////////////////////////////////////////////////
//                               Defines
///////////////////////////////////////////////////////////////////////////////

#define OffsetOf(ptype,field) ((unsigned long)&(((ptype *)0)->field))


  //
  // Snip	is a run of graphic characters having same attributes (font,
  //	size,etc), or it is a control element representing a word
  //	space, subscript/superscript, movements, inserted hyphen etc.
  //	Snip is also used to represent various control functions.
  //
  // All measures and computations are done using the coordinate system
  // of the current output device (that is Pixels on screen).
  //
#define FIELD_MASK(a,w)		(~0<<(a)^~0<<((a)+(w)))
#define FIELD_VALUE(a,v)	((v)<<a)
  //
  // SnipContent
  //	will indicate whether the snip represents
  //
  //	processable content
  //		is processed by editing, layout and imaging processes.
  //	formatted content
  //		is generated by the layout process for the imaging
  //		process. This content is ignored by layout and
  //		editing processes. In ODIF stream this is indicated
  //		by <SOS> ... <ST> sequence.
  //	original content
  //		is yet unstandardized method of marking processable
  //		content that should be *ignored* by the imaging
  //		process. This marking also generated by the layout
  //		process. Layout and editing processes should treat
  //		this the same way as processable content. This feature
  //		is required, for example to enable hyphenation in
  //		languages where the form of the word changes when it's
  //		hyphenated. The suggested indication in ODIF stream
  //		is by <SOOS> ... <ST> sequence.
  //	protected content
  //		once inserted, cannot be edited or changed (for future
  //		study--not yet sure what this would be).
  //
  //	*NOTE*	The values are tailored so that the processes can
  //		use bit test operation in deducing whether they want
  //		to deal with particular snip.
  //
  //		..for editing process ignore, if (SkipContent & 1)
  //		..for layout process ignore, if (SkipContent & 1)
  //		..for imaging process ignore, if (SkipContent & 2)
  //
#define Content_MASK		FIELD_MASK(0,2)
#define Content_PROCESSABLE	FIELD_VALUE(0,0)
#define Content_FORMATTED	FIELD_VALUE(0,1)
#define Content_ORIGINAL	FIELD_VALUE(0,2)
#define Content_PROTECTED	FIELD_VALUE(0,3)
#define IsEditableContent(x)	(!((x) & Content_FORMATTED))
#define IsImagedContent(x)	(!((x) & Content_ORIGINAL))
#define IsLayoutContent(x)	(((x) & Content_MASK) == Content_FORMATTED)
#define IsProtectedContent(x)	(((x) & Content_MASK) == Content_PROTECTED)

#define Underline_MASK		FIELD_MASK(2,2)
#define Underline_NONE		FIELD_VALUE(2,0)
#define Underline_SINGLE	FIELD_VALUE(2,1)
#define Underline_DOUBLE	FIELD_VALUE(2,2)

#define Framing_MASK		FIELD_MASK(4,2)
#define Framing_NONE		FIELD_VALUE(4,0)
#define Framing_FRAMED		FIELD_VALUE(4,1)
#define	Framing_ENCIRCLED	FIELD_VALUE(4,2)

#define Blinking_MASK		FIELD_MASK(6,2)
#define Blinking_STEADY		FIELD_VALUE(6,0)
#define Blinking_SLOWLY		FIELD_VALUE(6,1)
#define Blinking_RAPIDLY	FIELD_VALUE(6,2)
  //
  //	Subscript and superscript (6.3) (PLD/PLU only from there)
  //	(Oda allows only three states here: either PLD,
  //	PLU or none is active at any time).
  //
  //	*NOTE*	In code the values for UP and DOWN are assumed to be single
  //		bit masks.
  //
#define PartialLine_MASK	FIELD_MASK(8,2)
#define PartialLine_NONE	FIELD_VALUE(8,0)
#define PartialLine_UP		FIELD_VALUE(8,1)
#define PartialLine_DOWN	FIELD_VALUE(8,2)

  //
  // Reversed Direction, the content of Snip has logical order
  // opposite to the presentation order.
  //
#define Reversed_MASK		FIELD_MASK(10,1)

#define ImageInversion_MASK	FIELD_MASK(11,1)
#define Overlined_MASK		FIELD_MASK(12,1)
#define CrossedOut_MASK		FIELD_MASK(13,1)

#define Weight_MASK		FIELD_MASK(14,2)
#define Weight_NORMAL		FIELD_VALUE(14,0)
#define Weight_FAINT		FIELD_VALUE(14,1)
#define Weight_BOLD		FIELD_VALUE(14,2)
  //
  //	Justify field contains the current alignment mode for this
  //	portion of the text. The value is defined by Alignment, but
  //	only NONE, START, END and CENTER have meaning here. NONE
  //	(default) will use the aligment of the "alignment" resource.
  //
#define Justify_MASK		FIELD_MASK(16,3)
#define Justify_MODE(x)		(MSTextEditor::Alignment)(((x) >> 16) & 0x7)
#define Justify_VALUE(v)	(((int)(v) & 0x7) << 16)
  //
  //	Text can be rendered with specific foreground or background color.
  //	When not specified, the default background and foreground colors of
  //	the widget will be used. The color index defines the color as follows:
  //	0=black, 1=red, 2=green, 3=yellow, 4=blue, 5=magenta, 6=cyan and
  //	7=white. (ISO 6429: 1988). The color information is coded into 4 bit
  //	field such that 0 indicates the default and non-zero value is (color
  //	index)+1.
  //
#define Foreground_MASK		FIELD_MASK(19,4)
#define Background_MASK		FIELD_MASK(23,4)
#define Foreground_COLOR(x)	(((x) >> 19) & 0xf)
#define Background_COLOR(x)	(((x) >> 23) & 0xf)
#define Foreground_VALUE(v)	(((v) & 0xf) << 19)
#define Background_VALUE(v)	(((v) & 0xf) << 23)
  //
  //	Font number selects one of the 10 possible entries defined by
  //	the fonts resource.
  //
#define Font_MASK		FIELD_MASK(27,4)
#define Font_NUMBER(x)		(((x) >> 27) & 0xf)
#define Font_VALUE(v)		(((v) & 0xf) << 27)
  //
  //	To silence warnings about shift count, don't use FIELD_MASK here...
  //
#define Italicized_MASK		(1<<31) /* FIELD_MASK(31,1) */

  //
  // FontSelectInfo
  //	Must return key information for a font that corresponds the
  //	specified mode.
  //
  // *NOTE*
  //	The current definition below takes advantage of the positions
  //	of the Weight/Italicized/Font in the bits, and assumes that
  //	size_modification can safely be ORed without overlap!
  //
#define fontSelectInfo(m) \
	((m.bits & Weight_MASK) | (m.bits & Italicized_MASK) | \
	 (m.bits & Font_MASK) | m.size_modification)

#define IsSameMode(m1,m2) \
	(m1.bits == m2.bits && m1.tag == m2.tag && \
	 m1.size_modification == m2.size_modification)

#define NEXTTABSTOP (0xffff)

#define TextExportFormatted(f) ((f)&1)
#define TextExportOdif(f) ((f)&2)
#define LAYOUT_CONTENT_TAG (MSTextEditorTextTag)(-1)
#define FRAME_NO_PIXEL	~((unsigned long)0)


///////////////////////////////////////////////////////////////////////////////
//                       MSTextEditorTypes::Snip
///////////////////////////////////////////////////////////////////////////////

MSTextEditorTypes::Snip::~Snip()
{
  if (mode._callback) delete mode._callback; 		// delete my copy
}

MSTextEditorTypes::Snip::Snip()
{
  next = (Snip *)0;
  back = (Snip **)0;	
  tabref = 0;
  tab = 0;
  quad = 0;
  brk = 0;
  endseq = 0;
  space = 0;
  layout = 0;
  valid = 0;
  widget = 0;
  floating = 0;
  content.widget = (MSWidget *)0;
  data = (char *)0;
  length = 0;		
  x = y = xWidth = ascent = descent = offset = 0;
  mode._callback = (TextFlowCallback*)0;
}

MSTextEditorTypes::Snip* MSTextEditorTypes::Snip::previous()
{
  return (Snip *)((char *)(back) - OffsetOf(Snip,next)); 
}

MSBoolean MSTextEditorTypes::Snip::hasLayoutContents()   { return (MSBoolean)IsLayoutContent(mode.bits);   }
MSBoolean MSTextEditorTypes::Snip::hasEditableContents() { return (MSBoolean)IsEditableContent(mode.bits); }
MSBoolean MSTextEditorTypes::Snip::hasProtectedContents(){ return (MSBoolean)IsProtectedContent(mode.bits);}

void MSTextEditorTypes::Snip::callback(TextFlowCallback *cb_) 
{
  if (mode._callback) delete mode._callback;
  mode._callback = new TextFlowCallback(cb_); 	// create my own copy
}

MSBoolean MSTextEditorTypes::Snip::activateCallback()
{
  if (callback()) { 
    (*callback()->func())(callback()->owner(), callback()->name()); 
    return MSTrue; 
  }
  return MSFalse;
}

//
//  Insert a new empty Snip before the Snip pointed by 'h'.
//  Return a pointer to the newly created Snip.
//
MSTextEditorTypes::Snip *MSTextEditorTypes::Snip::Insert(MSTextEditorTypes::Snip **h)
{
  Snip *t = new Snip;
  if ((t->next = *h) != NULL) (*h)->back = &t->next;
  *h = t;
  t->back = h;
  t->content.head = NULL;
  t->data = NULL;
  t->length = 0;
  return t;
}

//
//    Subtract references to the data header. Returns NULL, if the
//    header has been released and the header pointer otherwise.
//
static MSTextEditorTypes::SnipData *derefDataHeader(MSTextEditorTypes::SnipData *head)
{
  if (head == NULL)
  return (MSTextEditorTypes::SnipData*)0; 
  else if (head->refs == 0); 
  else if (--head->refs == 0)
  free((void *)head);
  else
  return head;
  return (MSTextEditorTypes::SnipData*)0; 
}

//
//  Delete Snip pointed by 'h'
//
void MSTextEditorTypes::Snip::Delete(MSTextEditorTypes::Snip **h)
{
  MSTextEditorTypes::Snip *s = *h;

  if (s)
   {
     if (s->widget) 			// snip is a child widget ?
      {
        if (s->content.widget) 		// the child widget
	 {
	   s->content.widget->destroy();
	   s->content.widget = None;
	 }
      }
     else 
     s->content.head = derefDataHeader(s->content.head);

     if ((*h = s->next) != NULL) s->next->back = h;
     delete s;
   }

}

//
//  Split Snip t into two at delete_offset, and return the pointer to the
//  *NEW* snip, which represents the fisrt part of the split.
//
//  *** No checks are made. Function assumes the delete_offset <= length.
//
MSTextEditorTypes::Snip *MSTextEditorTypes::Snip::Split(InsertContext *cx, 
							      Snip *t, int delete_offset)
{
  register Snip *r;
  //
  // If the Snip has layout information, update expose area
  //
  if (t->layout)
   {
     cx->updateExposeArea(t->x, t->y - t->ascent,
			  t->xWidth, t->ascent + t->descent);
     t->layout = t->valid = MSFalse;
   }
  //
  // The new snip is inserted before t, and the leading
  // data characters are moved to this snip.
  //
  r = Snip::Insert(t->back);
  r->mode = t->mode;
  r->length = delete_offset;
  if ((r->data = t->data) != NULL)
   {
     if (cx->_first == t)
     cx->_first = r;
     t->data += (t->content.head->bytes * delete_offset);
     r->content.head = t->content.head;
     r->content.head->refs += 1;
   }
  t->length -= delete_offset;
  return r;
}

///////////////////////////////////////////////////////////////////////////////
//                       MSTextEditorTypes::SnipMode
///////////////////////////////////////////////////////////////////////////////

MSTextEditorTypes::SnipMode& MSTextEditorTypes::SnipMode::operator=(const MSTextEditorTypes::SnipMode&
                                                                    original_)
{
  if (this != &original_) {
    bits = original_.bits;
    tag = original_.tag;
    size_modification = original_.size_modification;
    if (_callback) delete _callback;
    if (original_._callback) 
    _callback = new TextFlowCallback(original_._callback);	// create copy
    else 
    _callback = (TextFlowCallback *)0;	   		// no callback
  }
  return *this;
}

MSTextEditorTypes::SnipMode::~SnipMode() 
{ //if (_callback) delete _callback;
}

///////////////////////////////////////////////////////////////////////////////
//                         MSTextEditorTypes::InsertContext 
///////////////////////////////////////////////////////////////////////////////

//
// insertLocation
//
//	Return the TextLocation corresponding the current insert
//	point. Additionally, if requested, return the current
//	accumulated expose region (will be reset in this case).
//
void MSTextEditorTypes::InsertContext::insertLocation(
							 TextLocation *dot, Region expose_)
{
  Snip *s = _last;

  dot->snip = s;
  if (s) dot->offset = s->virtualLength();
  else   dot->offset = 0;
  if (expose_ && _expose)
   {
     XUnionRegion(_expose, expose_, expose_);
     XDestroyRegion(_expose);
     _expose = 0;
   }
}

MSTextEditorTypes::InsertContext::InsertContext() :
_mode(0,0,100), _lock(0,0,100)
{
  _locked = 0;
  _list = 0;
  _last = 0;
  _pendingAccent = 0;
  _editor = 0;
  _first = 0;
  _size = 0;
  _used = 0;
  _fontinfo = 0;
  _head = 0;
  _ccf = 0;
  _expose = 0;
}

MSTextEditorTypes::InsertContext::~InsertContext()
{
  if (_ccf) free(_ccf);
}

int MSTextEditorTypes::InsertContext::feedContent(const char *s, long n) 
{ 
  ccf_Feed(_ccf, s, n); 
  return 1;
}


///////////////////////////////////////////////////////////////////////////////
//                        MSTextEditorTypes::ExposeContext 
///////////////////////////////////////////////////////////////////////////////
void MSTextEditorTypes::ExposeContext::flushPendingGraphics(MSTextEditorTypes::Snip *, int, int)
{
}

void MSTextEditorTypes::ExposeContext::markArea(MSTextEditorTypes::Snip *s, Drawable, MarkerId id, 
						   int x, int y, int h)
{
  register CornerMark *p = &markers[(int)id];

  if (p->h != 0)
   {
     // Found closing Marker, Draw Area
     int ascent, descent;
     int width = x - p->x;

     switch (id)
      {
      case MARK_Framed:
	MSTextEditor::findExtents(p->s, s, &ascent, &descent);
	p->y -= ascent;
	p->h = ascent + descent;
	if (my_r && XRectInRegion(my_r, p->x, p->y, width, p->h) == 
	    RectangleOut)
	break;
	/*
	::XDrawRectangle(editor->display(), d,
	editor->gc_Normal(), p->x - _origin->x, p->y - _origin->y, 
	width, p->h);
	*/
	/*
	MSTextEditor *w = editor;
	XeFrameBorderP *f = w->text.framed_rendition;
	DrawBorderRectangle
	((Widget)w,
	w->text.mygcBorder,
	&w->text.line,
	p->x, p->y, width, p->h,
	&f[FRAME_LEFT], &f[FRAME_RIGHT],
	&f[FRAME_TOP], &f[FRAME_BOTTOM],
	w->text.invert_shadow != 0);
	*/
	break;
      case MARK_Crossed:
	MSTextEditor::findExtents(p->s, s, &ascent, &descent);
	p->y -= ascent / 2;
	// FALL TRHOUGH 
      default:
	if (my_r && XRectInRegion(my_r, p->x, p->y, width, p->h) ==
	    RectangleOut)
	break;
	/*
	::XFillRectangle(editor->display(), d,
	editor->gc_Normal(), 
	p->x - _origin.x, p->y - _origin.y,
	width, p->h);
	*/
	break;
      }
   }
  p->s = s;
  p->x = x;
  p->y = y;
  p->h = h;
#if 0
#endif
}

void MSTextEditorTypes::ExposeContext::changeUnderline(MSTextEditorTypes::Snip *s, Drawable d, 
							  int x, int y)
{
  switch (p.bits & Underline_MASK)
   {
   case Underline_DOUBLE: 
     markArea(s, d, MARK_Double, x, y, 0);
   case Underline_SINGLE:
     markArea(s, d, MARK_Single, x, y, 0);
     break;
   default:
     break;
   }
  if (s == NULL)
  return;
  x = s->x;
  y = s->y;
  switch (s->mode.bits & Underline_MASK)
   {
   case Underline_DOUBLE:
     markArea(s, d, MARK_Double, x, y + 3, 1);
   case Underline_SINGLE:
     markArea(s, d, MARK_Single, x, y + 1, 1);
     break;
   default:
     break;
   }
}

void MSTextEditorTypes::ExposeContext::changeCrossedOut(MSTextEditorTypes::Snip *s, Drawable d, 
							   int x, int y)
{
  if (p.bits & CrossedOut_MASK)
  markArea(s, d, MARK_Crossed, x, y, 0);
  if (s == NULL)
  return;
  x = s->x;
  y = s->y;
  if (s->mode.bits & CrossedOut_MASK)
  markArea(s, d, MARK_Crossed, x, y, 1);
}

void MSTextEditorTypes::ExposeContext::changeFraming(MSTextEditorTypes::Snip *s, Drawable d, 
							int x, int y)
{
  // Only FRAMED implemented, no ENCIRCLED yet 

  if (p.bits & Framing_MASK)
  markArea(s, d, MARK_Framed, x, y, 0);
  if (s == NULL)
  return;
  x = s->x;
  y = s->y;
  if (s->mode.bits & Framing_MASK)
  markArea(s, d, MARK_Framed, x, y, 1);
}

void MSTextEditorTypes::ExposeContext::changeForeground(MSTextEditorTypes::Snip *s, Drawable)
{
  unsigned long pixel;
  MSTextEditor *e = editor;

  if (s && (s->mode.bits & Foreground_MASK) &&
      e->color(Foreground_COLOR(s->mode.bits)-1)!=FRAME_NO_PIXEL)
  pixel = e->color(Foreground_COLOR(s->mode.bits)-1);
  else
  pixel = e->foreground();
  if (s && (s->mode.bits & ImageInversion_MASK))
  e->setBackground(pixel);
  else
  e->setForeground(pixel);
}

void MSTextEditorTypes::ExposeContext::changeBackground(MSTextEditorTypes::Snip *s, Drawable)
{
  unsigned long pixel;
  MSTextEditor *e = editor;

  if (s && (s->mode.bits & Background_MASK) &&
      e->color(Background_COLOR(s->mode.bits)-1)!=FRAME_NO_PIXEL)
  pixel = e->color(Background_COLOR(s->mode.bits)-1);
  else
  pixel = e->background();
  if (s && (s->mode.bits & ImageInversion_MASK))
  e->setForeground(pixel);
  else
  e->setBackground(pixel);
}

///////////////////////////////////////////////////////////////////////////////
//                             MSTextEditor 
///////////////////////////////////////////////////////////////////////////////
unsigned long MSTextEditor::color(int n)
{
  return _colors ? _colors[n] : FRAME_NO_PIXEL;
}

void MSTextEditor::initKeyTable()
{
  if (MSKeyTranslationTable::keyTableData("MSTextEditor")==MSFalse)
   {
     keyTranslationTable()->addKeyTableData("MSTextEditor");
     unsigned i=0;
     KT *entry=0;
     while (EmacsKeyTable[i]._str!=0)
      {
        entry = new KT(EmacsKeyTable[i]._func );
        keyTranslationTable()->addCallback(EmacsKeyTable[i]._str,entry,"MSTextEditor");
        i++;
      }
   }
  else  keyTranslationTable()->addKeyTableData("MSTextEditor");
}

void MSTextEditor::keyPress(const XEvent *e_, KeySym keysym_, 
			       unsigned int state_, const char *b_)
{
  MSKeyPress keyPress(keysym_, state_);
  if (!keysym_) return;
  if(keyTranslate(keyPress) == MSFalse && !(state_&(ControlMask|Mod1Mask)) && 
     strlen(b_)==1 && b_[0])
    {
      insert((char*)(void*)b_, 1);
      setOrigin();
    }
}


// #pragma hdrstop

//
// FontCacheInfo
//  Pack Weight, Italic and size information into single keyword.
//
#define fontCacheInfo(m,size) \
(((m.bits & Weight_MASK) != 0) | \
 (((m.bits & Italicized_MASK) != 0) << 1) | \
 (size << 2))

class FontCacheEntry
{
public:
  unsigned long fontkey;  		// Posture, weight, size (FontCacheInfo)
  MSTextEditor::EditorFontList fontlist;  // Font List
  char *charset;    // Character Set Id
  XFontStruct *font;  // Associated font
};

static FontCacheEntry font_cache[211]; // Size should be a *PRIME* 
static int cache_hit = 0;  // just for temp testing 
static int cache_miss = 0;
static int cache_full = 0;  // This should stay ZERO!! 
static int cache_reset = 0;  // Non-zero, if cache should be cleared 
static Display *cache_display;  // Display for which cache is computed.
				// (If application alternates between two
				// displays, cache will be almost nullified)


void MSTextEditor::insetSnipExtents(LayoutContext *, Snip *t) 
{
  if (t->content.widget) {
    t->xWidth = t->content.widget->width();
    t->ascent = t->content.widget->height();
    t->descent = 0;
  }
}


//
//  Compute the width of the current Snip
//
void MSTextEditor::computeWidth(LayoutContext *cx, Snip *t)
{
  static int lastFontHeight; // Last Baseline font height //

  int tmp;

  if (lastFontHeight <= 0)
  lastFontHeight = cx->lineSpacing;
  switch (t->mode.bits & PartialLine_MASK)
   {
   case PartialLine_NONE:
     cx->yShift = 0;
     break;
   case PartialLine_UP:
     cx->yShift = -(lastFontHeight / 3);
     break;
   case PartialLine_DOWN:
     cx->yShift = lastFontHeight / 3;
     break;
   }
  t->y += cx->yShift;

  if (t->widget) insetSnipExtents(cx, t);
  else cx->editor->snipExtents(t);

  if (t->mode.bits & Framing_MASK && !t->floating)
   {
     t->ascent += cx->top_framing;
     t->descent += cx->bottom_framing;
     //
     // NOTE: while framed is MSTrue, the reservation for
     // frame ending (right_framing) is kept in wNow, and
     // subtracted when framing explicitly ends. This attempts
     // to get lines right with wrapping, which requires implicit
     // ending/start of framing at end of lines.
     //
     if (!cx->current.framed)
      {
	// Starting framed section, must leave space for the frame graphics //
	t->xWidth += cx->left_framing;
	t->offset += cx->left_framing;
	cx->current.wNow += cx->right_framing;
      }
     if (t->hasEndLine() || t->next == NULL || !(Framing_MASK&t->next->mode.bits))
     // Ending framed section, must leave space for the frame graphics 
     // (newline breaks framing temporarily 
      {
	t->xWidth += cx->right_framing;
	cx->current.wNow -= cx->right_framing;
	cx->current.framed = MSFalse;
      }
     else
     cx->current.framed = MSTrue;
   }
  cx->current.wNow += t->xWidth;
  tmp = t->ascent - cx->yShift;
  if (tmp > cx->current.maxAscent)
  cx->current.maxAscent = tmp;
  tmp = t->descent + cx->yShift;
  if (tmp > cx->current.maxDescent)
  cx->current.maxDescent = tmp;
}

//
// position_XXXX functions will do the appropriate changes into the
// Snip chain to achieve the positioning of the content.
//
// Each will return a pointer to the last Snip that actually is included
// into this sequence. Usually it will be the supplied parameter 'last',
// except for the TEXT.
//

//
//  Do final touches to the line box placement. Compute the amount of
//  vertical adjustment for the baseline (relative to the baseline of
//  the previous line) and update all snips within line.
//
//  Returns the vertical movement that was required to move from the
//  previous baseline to the baseline of this (closed) linebox.
//
int MSTextEditor::closeLineBox(LayoutContext *cx, Snip *end)
{
  int y_adj = cx->previousExtent;
  Snip *t;

  //
  // Proportional spacing is used only if the line has some content
  // in it. Otherwise fixed lineSpacing is assumed.
  //
  if (cx->editor->_proportional&&(cx->forwardExtent||cx->backwardExtent))
   {
     cx->previousExtent = cx->forwardExtent;
     y_adj += cx->backwardExtent;
   }
  else
   {
     cx->previousExtent = cx->lineSpacing / 3;
     //
     // If fixed line spacing is selected, then use the backward
     // extent of current line, if this is the first line.
     // (otherwise the widget might clip the content)
     //
     if (cx->firstLine && cx->backwardExtent > 0)
     y_adj += cx->backwardExtent;
     else
     y_adj += cx->lineSpacing - cx->previousExtent;
   }
  for (t = cx->beginLine;;)
   {
     t->y += y_adj;
     if (t == end) break;
     t = t->next;
   }
  cx->firstLine = MSFalse;
  return y_adj;
}

//
//  Do the finishing touches for the positioning functions:
//  search for endseq etc.
//
MSTextEditorTypes::Snip *MSTextEditor::wrapUpSequence(LayoutContext *cx, SequenceState *s, 
							    Snip *end, int x_adj)
{
  Snip *t;

  if (end->widget && end->content.widget)
   {
     //
     // Widget Snip has *always* endseq set. This means that
     // in a sequence, there can be only one Widget, and that
     // is the terminating Snip (end). Thus, it is sufficient
     // to check the end only...
     //

     end->offset = cx->editor->_set_width - cx->rightWidgetIndent
     - end->xWidth - end->x - x_adj;
     if (s->maxAscent > cx->backwardExtent)
     cx->backwardExtent = s->maxAscent;
     if (s->maxDescent > cx->forwardExtent)
     cx->forwardExtent = s->maxDescent;
     for (t = cx->firstSnip; ;t = t->next)
      {
	t->x += x_adj;
	if (t == end) break;
      }
   }
  return end;
}

//
//  Position a TEXT sequence.
//
//  last  the last Snip actually taking part in positioning.
//
//  end  the actual Snip ending the sequence (this differs
//  from last, if line end contains spaces).
//
MSTextEditorTypes::Snip *MSTextEditor::position_TEXT(LayoutContext *cx, SequenceState *text,
							   Snip *last, Snip *end)
{
  int offset = cx->lineOffset;
  int wExtra = cx->lineLength - text->wNow;
  Alignment alignment;
  //
  // Force START aligment, if no real lineLength or if the sequence
  // is actually ended with Horizontal TAB. Use the alignment given
  // by the resource, unless the Snip specifies it explicitly by
  // Justify_MODE.
  //
  if (cx->lineLength <= 0 || (end->tab && end->tabref == NEXTTABSTOP))
  alignment = START;
  else if (end->quad)
  alignment = (Alignment)end->quad;
  else if (end->mode.bits & Justify_MASK)
  alignment = Justify_MODE(end->mode.bits);
  else
  alignment = cx->editor->_alignment;

  alignment = JUSTIFIED;

  switch (alignment)
   {
   default:
   case START:
     break;
   case END:
     offset += wExtra;
     break;
   case CENTER:
     offset += wExtra / 2;
     break;
   case JUSTIFIED:
     //
     // For the justifying to be applicable, the line
     // must include adjustable spaces, there must be
     // positive amount of loose space. Additionally,
     // unless this is "forced quad", this must not be the
     // "hard line end" or last line of the block.
     //
     if (text->spaces && wExtra > 0 &&
	 (end->quad || 
	  (IsLayoutContent(end->mode.bits) && end->next != NULL)))
      {
	Snip *t;
	int wSpace = wExtra / text->spaces;
	int adjust = 0;

	text->wNow += wExtra;
	wExtra -= text->spaces * wSpace;
	for (t = cx->firstSnip; t != last; t = t->next)
	 {
	   if (t->space)
	    {
	      adjust += wSpace;
	      if (wExtra)
	       {
		 adjust += 1;
		 wExtra -= 1;
	       }
	    }
	   t->x += adjust;
	 }
	for (;; t = t->next)
	 {
	   t->x += adjust;
	   if (t == end)
	   break;
	 }
      }
     break;
   }
  return wrapUpSequence(cx, text, end, offset);
}

//
//  Position ITEM identifier
//
MSTextEditorTypes::Snip *MSTextEditor::position_ITEM(
							   LayoutContext *cx, SequenceState *item, Snip *last)
{
  int offset;
  //
  // Compute the offset of the item from the line home position,
  // taking alignment into account.
  //
  if (cx->editor->_itemization->identifier_alignment == START)
  offset = cx->editor->_itemization->identifier_start_offset;
  else
   {
     offset = cx->editor->_itemization->identifier_end_offset -
     item->wNow;
     //
     // Non-ODA implementation addition: if the item would be
     // positioned outside the available area, then start it
     // from the start edge of the available area.
     //
     if (offset + cx->editor->_indentation < 0)
     offset = -cx->editor->_indentation;
   }
  //
  // If a floating widget is on the left, compute as if line home
  // position was to the right of it.. (does not really work! --msa)
  //
  offset += cx->leftWidgetIndent;
  return wrapUpSequence(cx, item, last, offset);
}


//
// LookFor
//  Search for characters from a bounded string. Return
//  a pointer to the first character found or NULL, if none.
//  (comparable with strpbrk).
//
char *MSTextEditor::lookFor(char *d, char *s, int n)
{
  unsigned char c;
  register unsigned char *p;
  unsigned char *q = (unsigned char *)s + n;

  if (d == NULL || s == NULL || n <= 0)
  return NULL; // This really means erroneus call.. //
  for ( ; (c = *d) != 0; d++)
  for (p = (unsigned char *)s; p < q; ++p)
  if (*p == c)
  return (char *)p;
  return NULL;
}


//
//  Split the line from the specified Snip (s) and return a pointer
//  to a Snip that will belong to the next line.
//
MSTextEditorTypes::Snip *MSTextEditor::splitLine(LayoutContext *cx, 
						       SequenceState *text, Snip *last)
{
  Snip *t = last;
  Snip *s;
  int x, y;

  x = t->x;
  y = t->y;
  //
  // Eat up all extra spaces to the end of the line
  //
  for ( ; ; )
   {
     if (t->space)
      {
	t->xWidth = 0;
	t->x = x;
	t->y = y;
      }
     if (t->endseq)
     break;
     s = t->next;
     if (s == NULL || !s->space)
     break;
     t = s;
   }
  //
  // If the last element doesn't end the sequence, then generate
  // the line ending sequence here.
  //
  if (!t->endseq)
   {
     if (text->framed)
     // Reserve space for the framing end //

     t->xWidth += cx->right_framing;
     s = Snip::Insert(&t->next);
     s->endseq = EndLine;
     s->mode = t->mode;
     s->mode.bits &= ~Content_MASK;
     s->mode.bits |= Content_FORMATTED;
     s->mode.tag = LAYOUT_CONTENT_TAG;
     s->layout = MSTrue;
     s->valid = MSTrue;
     s->x = x;
     s->y = y;
     t = s;
   }
  return position_TEXT(cx, text, last, t);
}

//
//  Extend expose area from the Snip
//
void MSTextEditor::updateExposeArea(XRectangle *r, int x, int y, 
				       int width, int height)
{
  if (width == 0 || height == 0) return;
  if (r->width == 0)
   {
     r->x = x;
     r->y = y;
     r->width = width;
     r->height = height;
   }
  else
   {
     if (x < r->x)
      {
	r->width += r->x - x;
	r->x = x;
      }
     if (x + width > r->x + r->width)
     r->width = x + width - r->x;
     if (y < r->y)
      {
	r->height += r->y - y;
	r->y = y;
      }
     if (y + height > r->y + r->height)
     r->height = y + height - r->y;
   }
}

//
//  Compare Snips in the line box to the saved coordinates of the
//  previous Snips. Update expose region to cover all changes.
//
//  Return 'mode', which has the following interpretation
//
//  mode=-1  searching for valid=MSFalse position,
//  mode=1  have found valid=MSFalse, now searching for valid=MSTrue,
//  mode=0  have found valid=MSTrue, layout process can break now.
//  mode=2  do not search for anything. Layout will not terminate
//  until end of content.
//
#define LOOKFOR_MODE (-1)
#define LOOKFOR_NONE 2

int MSTextEditor::updateExpose(XRectangle *expose, ExpectEntry *start, 
				  ExpectEntry *end, Snip *last, int mode)
{
  int changes = 0;
  Snip *s;

  if (!expose)
   {
     for ( ;start < end && (s = start->s) != last; start++)
      {
	if (IsEditableContent(s->mode.bits))
	 {
	   if (!s->valid ||
	       start->x != s->x || start->y != s->y)
	   changes++;
	   if (s->valid)
	    {
	      if (mode == 1)
	      mode = 0;
	    }
	   else if (mode == -1)
	   mode = 1;
	 }
	s->layout = s->valid = MSTrue;
      }
   }
  else for ( ;start < end && (s = start->s) != last; start++)
   {
     updateExposeArea(expose, s->x, s->y - s->ascent, 
		      s->xWidth, s->ascent + s->descent);
     if (IsEditableContent(s->mode.bits))
      {
	if (!s->valid || start->x != s->x || start->y != s->y)
	 {
	   changes++;
	   if (s->layout)
	   updateExposeArea
	   (expose, start->x,
	    start->y - start->ascent,
	    start->xWidth,
	    start->ascent+start->descent);
	 }
	if (s->valid)
	 {
	   if (mode == 1)
	   mode = 0;
	 }
	else if (mode == -1)
	mode = 1;
      }
     s->layout = s->valid = MSTrue;
   }
  
  //
  // Restore the original (x,y) for the snips that were processed,
  // but didn't get included into the linebox.
  //
  while (start < end)
   {
     start->s->x = start->x;
     start->s->y = start->y;
     start->s->xWidth = start->xWidth;
     start->s->ascent = start->ascent;
     start->s->descent = start->descent;
     start->s->offset = start->offset;
     start++;
   }
  return mode ? mode : changes != 0;
}

//
//  checks if the terminating inset Snip has a widget that is
//  floated to the start/end of sequence and activates a
//  wraparound. In that case one has to use only the 'x' from
//  the widget and ignore the xWidth. (The real positioning of
//  the widget is made by offset field).
//
int MSTextEditor::adjustForWidgets(LayoutContext *, Snip *s)
{
  if (s->widget && s->content.widget)
   {
     if (s->floating)
      {
	// should handle alignment of floating widgets
	return s->x;
      }
   }
  return s->x + s->xWidth;
}


void MSTextEditor::snipExtents(MSTextEditorTypes::Snip *snip_)
{
  XCharStruct XE;
  int dir, fontAscent, fontDescent;
  SnipData *h = snip_->content.head;

  if (h == NULL)
   {
     snip_->xWidth = snip_->ascent = snip_->descent = snip_->offset = 0;
     return;
   }
//  if (h->font == NULL)
//  {
//    h->font = findFont(0, 0, Font_NUMBER(snip_->mode.bits), 0, 0, 0);
//  }
  if (h->font == NULL)		// chris, was 0 &&
   {
     //
     // Font struct still undefined, load it
     //
     int fn = Font_NUMBER(snip_->mode.bits);
     EditorFontList fId;
     int hit, i;
     int pSize;
     unsigned long fontkey;

     if (fn >= _num_fonts)
     fn = 0; // Try for default font 
     if (_fonts == NULL || _num_fonts == 0)
      {
        pSize = _line_spacing; // or lineSpacing? 
        fId = 0;
      }
     else
      {
        pSize = _fonts[fn].font_size;
        fId = _fonts[fn].font_list;
      }
     if (snip_->mode.size_modification)
     pSize = (snip_->mode.size_modification * pSize + 50) / 100;
     fontkey = (unsigned long)fontCacheInfo(snip_->mode, pSize);
     hit = ((fontkey ^ (unsigned long)h->character_set) ^
	    (unsigned long)fId) % NumberOf(font_cache);
     for (i = hit ;; )
      {
        if (font_cache[i].font == 0)
	 {
	   cache_miss++;
	   break;
	 }
        else 
	if (font_cache[i].fontkey == fontkey &&
	    font_cache[i].fontlist == fId &&
	    font_cache[i].charset == h->character_set)
	 {
	   cache_hit++;
	   break;
	 }
        i += 1;
        if (i == NumberOf(font_cache)) i = 0;
        if (i == hit)
	 {
	   //
	   // Nasty thing.. should really not happen.
	   // Just take over the first hash entry. Not
	   // really right solution, but we assume for
	   // now that we really don't get here...
	   //
	   font_cache[i].font = 0;
	   cache_full++;
	   cache_reset = MSTrue;
	 }
      }
     if (font_cache[i].font == 0)
      {
        font_cache[i].fontkey = fontkey;
        font_cache[i].fontlist = fId;
        font_cache[i].charset = h->character_set;

        font_cache[i].font = findFont(h->character_set, fId,
				      Font_NUMBER(snip_->mode.bits),
				      snip_->mode.bits & Weight_MASK,
				      snip_->mode.bits & Italicized_MASK, pSize);
	
      }
     h->font = font_cache[i].font;
   }
  if (h->bytes == 2) XTextExtents16((XFontStruct *)h->font,
				    (XChar2b *)snip_->data, snip_->length,
				    &dir, &fontAscent, &fontDescent, &XE);
  else
  XTextExtents((XFontStruct *)h->font, snip_->data, snip_->length,
	       &dir, &fontAscent, &fontDescent, &XE);
  snip_->xWidth = XE.width;
  //
  // Using Font ascent/descent gives better result for now..
  //
  snip_->ascent = fontAscent;
  snip_->descent = fontDescent;
  snip_->offset = 0;
}

XFontStruct *MSTextEditor::findFont(char *, EditorFontList,
				       int font_nr, int, int, int) 
{
  const XFontStruct *f;
  if (font_nr > 0 && font_nr < _n_font_names)
   {
     f=server()->fontStruct(server()->fontID(_font_names(font_nr)));
   }
  else
   {
     f= server()->fontStruct(font());
   }
  if(f!=0)
   {
     if(vsb()!=0) vsb()->inc(f->max_bounds.ascent+f->max_bounds.descent);
     if(hsb()!=0) hsb()->inc(f->max_bounds.width);
   }
  return (XFontStruct*)f;
}

const int LEFTWIDGETINDENT  = 0;
const int RIGHTWIDGETINDENT = 3;

#define LOOKFOR_MODE (-1)
#define LOOKFOR_NONE 2

//
//  Layout widget character content into widget *internal*
//  "Processable Formatted Form". This function can be called
//  any number of times.
//
//  from =   NULL, redo the layout fully, ignore all existing
//  old layout information.
//
//  from != NULL, start layout process from the first possible
//  linebox before this point (Snip).
//
//  If expose is non-NULL, the pointed XRectangle will be expanded
//  to contain the area of the layout (usefull only if from != NULL).
//  The rectangle must be properly initialize before call
//  (expose->width = 0 is sufficient to make area initially empty).
//
//  set_width is the new target width of the layout process 
//
void MSTextEditor::layout(int set_width, Snip *from, Region expose_region)
{
  typedef enum
   {
     Sequence_TEXT,
     Sequence_ITEM,
     Sequence_STAB
   } LayoutSequence;

  //
  // lay, addLayout
  //  if addLayout == non-zero, then we are processing application
  //  inserted layout content before the Snip pointed by 'lay'.
  //  (note! lay may also be NULL, if inserting occurs after
  //  the last editable Snip). When addLayout == 0, 'lay' is
  //  NULL. A special value -1 is used when the layout process
  //  should start in addLayout mode, but not call BEGIN_LINE
  //  callbacks.
  // expect_default, expect, expect_point, expect_end
  //  are used in line building to detect when layout hits a point
  //  that doesn't change. Fixed array expect_default is used and
  //  it should be sufficient for most cases. However, the code
  //  is prepared to allocate a larger array dynamically on demand.
  //
  Snip *s, *p, **h, *lay;
  MSTextEditorTextTag tag;
  int i, x, y, yHighMark, xHighMark;
  ExpectEntry expect_default[200];
  ExpectEntry *expect, *expect_point = &expect_default[0];
  ExpectEntry *expect_end = &expect_default[NumberOf(expect_default)];
  int layout = _format;
  int lookfor, tabref, firstSeq, addLayout, itemization;
  LayoutSequence sequence;
  XRectangle expose_rect, *expose;
  LayoutContext cx;

  static LayoutContext init_cx = {0, 0,0,0,0, 0,0,0,0,0,0,0, 0,0,0, 0,0, 0,0};

  cx = init_cx;
  //
  // Keep track of the bounding box of exposed area only if
  // expose_region is requested by the caller. (Init box to empty)
  //
  expose_rect.width = 0;
  expose = expose_region ? &expose_rect : NULL;
  //
  // Initialize the global state
  //
  if (cache_display != display() || cache_reset)
   {
     for (i = 0; i < NumberOf(font_cache); i++)
      {
//        if(font_cache[i].font!=0) XFreeFont(display(),font_cache[i].font);
        font_cache[i].font = 0;
      }
     
     cache_hit = 0;
     cache_miss = 0;
     cache_reset = 0;
     cache_display = display();
   }
  if (set_width != _set_width) from = NULL; 
  if (set_width <= 0) layout = MSFalse;
  _set_width = set_width;
  cx.editor = this;
  cx.lineSpacing = _line_spacing;
  cx.firstLine = MSTrue;

  firstSeq = MSTrue;
  if (1 || cx.lineSpacing == 0) {
    //
    // If lineSpacing == 0, then the default lineSpacing
    // will be the font height of the primary (default) font
    // using the default character set. Unfortunately, one
    // needs to do some complex stuff to get it...
    //

#if 1 
    //
    // This code is buggy. It causes a memory leak while freeing heads.
    //
    Snip *list = NULL;
    s = Snip::Insert(&list);
    s->content.head = (SnipData *)calloc(1, OffsetOf(SnipData, s[0]));
    s->data = &s->content.head->s[0];
    s->content.head->bytes = 1;
    s->content.head->refs = 1;
    s->content.head->font = NULL;
    s->content.head->character_set = charsetDefault();
    snipExtents(s);
    // ..assuming fontAscent and fontDescent are put into Snip //
    cx.lineSpacing = s->descent + s->ascent;
    Snip::Delete(&list);

#else
    cx.lineSpacing = _defaultFontSize;
#endif
  }
  _line_spacing = cx.lineSpacing;
  yHighMark = cx.previousExtent = 0;
  cx.leftWidgetIndent  = LEFTWIDGETINDENT;
  cx.rightWidgetIndent = RIGHTWIDGETINDENT;
  //
  // Initialize layout breaking parameters
  //
  y = 0;
  x = _indentation;
  h = &_first;
  tag = 0;
  lay = NULL;
  addLayout = MSFalse;
  if (from == NULL) {
    _x = x;
    _y = y;
    xHighMark = 0;
    lookfor = LOOKFOR_NONE;
  }
  else {
    int gotcha, editable, base_y, max_y, min_y;
    //
    // Skip over already laid out material, start layout from the
    // beginning of the previous line from the line has been
    // modified.
    //
    for (s = from; ;s = s->next) 
    if (s == NULL) return; // Nothing to layout
    else if (!s->valid) break;
    //
    // Search backwards for the line end marker from
    // which a new layout process can be started. It will
    // be the first line end having one of the following
    // conditions:
    // - line end is an editable hard line end, or
    // - line end is followed by valid layout which includes
    //   an editable space, or
    // - line end is followed by a full valid line which
    //   includes at least one valid editable component.
    //
    for (gotcha = MSFalse, editable = 0;;) {
      if (s->back == &_first) break;
      s = s->previous();
      if (!s->valid)
       {
	 gotcha = MSFalse;
	 editable = 0;
	 continue;
       }
      if (s->hasEditableContents())
       {
	 editable += 1;
	 if (s->space) gotcha = MSTrue;
	 else if (s->hasEndLine()) break;
       }
      else 
      if (s->hasEndLine())
       {
	 if (gotcha && editable) break;
	 else
	  {
	    gotcha = MSTrue;
	    editable = 0;
	  }
       }
    }

    //
    // Now 's' points to the Snip (HasEndLine) *AFTER* which
    // the new layout will start. If the proportional spacing has
    // has been requested, we have to "guess" the forwardExtent
    // of the preceding line is, and set previousExtent from it.
    // If the line is has only white space or no proportional
    // spacing, use formula "lineSpacing / 3".
    // Additionally, have to find out the initial value for the
    // previous 'tag'.
    //
    p = s;
    gotcha = MSFalse;
    base_y = max_y = min_y = p->y;
    do {
      if (!gotcha && p->hasEditableContents()) {
	tag = p->mode.tag;
	gotcha = MSTrue;
      }
      if (!p->floating) {
	if (p->y + p->descent > max_y)
	max_y = p->y + p->descent;
	if (p->y - p->ascent < min_y)
	min_y = p->y - p->ascent;
      }
      if (p->back == &_first) break;
      p = p->previous();
    } while (!p->hasEndLine());

    if (_proportional && (max_y - min_y) > 0) {
      cx.previousExtent = max_y - base_y;
    }
    else
    cx.previousExtent = cx.lineSpacing / 3;

    //
    // Do we have the 'tag'? If not, search more backwards...
    //
    if (!gotcha) {
      while (!p->hasEditableContents() && p->back != &_first)
      p = p->previous();
      tag = p->mode.tag;
    }
    if (s->back == &_first) {
      tag = 0;
      cx.previousExtent = 0;
    }
    else {
      cx.firstLine = MSFalse;
      yHighMark = y = s->y;
      firstSeq = s->hasEndParagraph();
      h = &s->next;
    }
    //
    // If the starting point is application generated layout
    // information, the actual layout process must start in
    // "add layout" mode until next editable content. Also,
    // assume the tag change has already been processed at
    // this point.
    //
    if (!s->hasEditableContents() && s->mode.tag != LAYOUT_CONTENT_TAG)
    for (p = s; (p = p->next) != NULL; )
    if (p->hasEditableContents()) {
      lay = p;
      addLayout = -1;
      tag = p->mode.tag;
      break;
    }
    xHighMark = pWidth();
    lookfor = LOOKFOR_MODE;
  } // from != NULL

  cx.lineOffset = 0;
  p = cx.beginLine = NULL;
  while (*h) {
    static SequenceState initial = {0,0,0,0};
    
    Snip *space = NULL;  // Set Non-NULL, if last space found //
    
    cx.current = initial;  // Initialize 'current' state //
    cx.current.tag = tag;
    cx.firstSnip = *h;

    if (cx.beginLine == NULL) {
      cx.beginLine = cx.firstSnip;
      cx.forwardExtent = 0;
      cx.backwardExtent = 0;
      itemization = MSFalse;
      expect = expect_point;
      if (firstSeq && _itemization &&
	  _itemization->identifier_alignment != NOALIGNMENT)
      sequence = Sequence_ITEM;
      else {
	sequence = Sequence_TEXT;
	if (firstSeq) cx.lineOffset += _first_line_offset;
      }
      firstSeq = MSFalse;
    }

    x = _x;

    cx.yShift = 0; 

    cx.lineLength = set_width - _indentation - cx.lineOffset - 
    cx.rightWidgetIndent;
    do {
      //
      // Locate the next Snip to process. Delete all
      // Snips generated by the previous layout process.
      // If the content ends without a proper endseq
      // Snip, then add one on behalf of the layout process.
      //
      while (1) {
	if ((s = *h) == NULL) {
	  s = Snip::Insert(h);
	  s->endseq = End;
	  s->mode.bits &= ~Content_MASK;
	  s->mode.bits |= Content_FORMATTED;
	  s->mode.tag = LAYOUT_CONTENT_TAG;
	  if (cx.firstSnip == NULL)
	  cx.firstSnip = s;
	  if (cx.beginLine == NULL)
	  cx.beginLine = s;
	  break;
	}
	else if (addLayout) 
	break;
	else if (s->hasLayoutContents()) {
	  if (s == cx.firstSnip) p = cx.firstSnip = s->next;
	  if (s == cx.beginLine) cx.beginLine = s->next;
	  if (expose && s->layout && s->xWidth)
	  updateExposeArea(expose, s->x, s->y - s->ascent, s->xWidth, 
			   s->ascent+s->descent);
	  Snip::Delete(h);
	}
	else 
	if (tag != s->mode.tag) { }
	else {
	  cx.current.tag = tag = s->mode.tag;
	  break;
	}
      }
      
      if (lay == s) {
	// Layout Insertion terminated //
	addLayout = MSFalse;
	lay = NULL;
	tag = cx.current.tag = s->mode.tag;
      }

      if (expect == expect_end) {
	ExpectEntry *e = expect_point;
	int old_expect = expect_end - expect_point;
	int new_expect = old_expect + NumberOf(expect_default);
	if (expect_point == expect_default) {
	  expect_point = (ExpectEntry *)calloc(new_expect, 
					       sizeof(ExpectEntry));
	  memcpy((void *)expect_point, (void *)e, 
		 sizeof(ExpectEntry) * old_expect);
	}
	else
	expect_point = (ExpectEntry *) realloc((char *)expect_point, 
					       sizeof(ExpectEntry) * new_expect);
	expect = &expect_point[old_expect];
	expect_end = &expect_point[new_expect];
      }
      expect->s = s;
      expect->x = s->x;
      expect->y = s->y;


      expect->xWidth = s->xWidth;
      expect->ascent = s->ascent;
      expect->descent = s->descent;
      expect->offset = s->offset;
      expect++;
      s->x = x;
      s->y = y;

      if (0 && s->widget)
      s->content.widget->moveTo(s->x - _origin.x, 
				s->y - _origin.y - (s->ascent + s->descent));

      //
      // All snips before the current one (s) will
      // fit into the line. If the current one
      // represents a valid breaking point, remember
      // this point by saving the line state into
      // 'lastSpace' *before* adding any effects
      // from this snip. (used by TEXT sequence only)
      //
      if (s->space) {
	cx.lastSpace = cx.current;
	space = s;
	cx.current.spaces += 1;
      }

      computeWidth(&cx, s);

      //
      // Do the line splitting only if new layout is
      // requested.
      //
      switch (sequence) {
      case Sequence_ITEM:
	if (s->endseq) {
	  s = position_ITEM(&cx, &cx.current, s);
	  itemization = MSTrue;
	  sequence = Sequence_TEXT;
	}
	break;

      case Sequence_STAB:
	if (s->endseq)
	s = (Snip*)0; //position_STAB(&cx, &cx.current, s, tabref);
	break;

      default:
	if (layout && cx.current.wNow > cx.lineLength && space) {
	  s = splitLine(&cx,&cx.lastSpace,space);
	  tag = cx.lastSpace.tag;
	}
	else if (s->endseq)
	s = position_TEXT(&cx,&cx.current,s,s);
	break;
      }


      y = s->y - cx.yShift;
      x = s->x + s->xWidth;
      h = &s->next;
      p = s;
    } while (!s->endseq);
    
    if (s->widget) {
      x = adjustForWidgets(&cx, s);
      _x = x;
    }
    else 
    _x = 0;
    
    if (x > xHighMark)
    xHighMark = x;
    if (s->hasEndLine()) {
      y += closeLineBox(&cx, s);
      lookfor = updateExpose(expose, expect_point, expect, s->next, lookfor);
      if (!lookfor) {
	if (xHighMark > pWidth())
	xHighMark=xHighMark;	// should set pWidth() to xHighMark
	goto clean_up;
      }
      cx.beginLine = NULL;
    }
    if (y > yHighMark)
    yHighMark = y;
    sequence = Sequence_TEXT;

    if (s->tab) {
      if ((tabref = s->tabref) == NEXTTABSTOP) {
	x = x+60; // HorizontalTab(&cx, x, &tabref);
	if (tabref != NEXTTABSTOP)
	sequence = Sequence_STAB;
      }
      else
      sequence = Sequence_STAB;
    }
    if (s->hasEndLine()) {
      //
      // The next line will start a new line box
      //
      cx.lineOffset = 0;
      firstSeq = s->hasEndParagraph();
    }
    else {
      //
      // This was just end of some sequence within the
      // same line box. Continue filling the box from
      // the current point. (Because lineOffset is relative
      // to the "home position", indentation must be
      // subtracted from the absolute 'x').
      //
      cx.lineOffset = x - _indentation;
      //
      // If the sequence just ended was an 'item' then
      // the next sequence should start from the home
      // position adjusted with the first_line_offset.
      //
      // Additional Non-ODA(?) implementation: if the
      // item would overlap the normal text, then do not
      // use the fixed start (instead, just continue
      // filling the line from where the item ended).
      //
      if (itemization && cx.lineOffset < _first_line_offset)
      cx.lineOffset = _first_line_offset;
    }
    itemization = MSFalse;
  }

  if (p && p->hasEndLine()) {
    //
    // The content terminated by End of Line. This means that
    // the content includes one *EMPTY* line at end. (This is
    // ODA character content interpretation).
    //
    y += cx.previousExtent + cx.lineSpacing;
  }
  else if (cx.beginLine) {
    y += closeLineBox(&cx, s);	
    (void)updateExpose(expose, expect_point, expect, p->next, 0);
    y += cx.previousExtent;
  }
  else {
    y += cx.lineSpacing; // Empty Content! //
  }

  if (y > yHighMark)
  yHighMark = y;
  
  yHighMark=yHighMark;	// should set pHeight() to xHighMark
  xHighMark=xHighMark;  // should set pWidth() to xHighMark
 clean_up:
  if (expose && expose->width > 0)
  XUnionRectWithRegion(expose, expose_region, expose_region);
  if (expect_point != expect_default) free((char *)expect_point);
  return;
}

static char iso8859_1[] = "iso8859-1";
char *MSTextEditor::charsetDefault() { return iso8859_1; }

int MSTextEditor::copy1_GL(InsertContext *cx, int n, unsigned char *s, 
			      unsigned char *, char *cset)
{
  Snip *t;
  SnipData *head;
  register unsigned char c, *p;

  if (n == 0)
  return 0;
  if (cx == NULL || (t = cx->beginAppend(n, cset, 1)) == NULL)
  return 1;
  head = t->content.head;
  p = (unsigned char *)t->data + (head->bytes * t->length);
  //
  // This special kludge/hook is required to map ISO 6937 floating
  // accents into plain ISO 8859-1 characters. Not a pretty sight,
  // there must be a neater way... --msa
  // If accent combination is not found, the accent is ignored.
  //
#if 0
  if (cset == iso8859_1 && // Should probably test for all iso8859's
      //..and in real word should expect floating
      //accents from one character set combined
      //with character from another.. ugh... --msa //
      cx->_pendingAccent != 0)
  {
    int *q, i;

    c = *s & 0x7f;
    i = COMBINE(cx->_pendingAccent, c);
    for (q = T51diatrical_ISO8859_1; *q; ++q)
    if (*q++ == i)
     {
       *p++ = *q;
       t->length += 1;
       n -= 1;
       s += 1;
       break;
     }
  }
  cx->_pendingAccent = 0;
#endif
  for ( ; n > 0; ++s, --n )
   {
     c = *s & 0x7f;
     if (c == 0x20)
      {
	if (!t->space && (t->endseq || t->length > 0))
	 {
	   t = cx->appendSnip();
	   t->data = (char *)p;
	   t->content.head = head;
	   head->refs += 1;
	 }
	t->valid = MSFalse;
	t->space = MSTrue;
      }
     else if (t->space)
      {
	t = cx->appendSnip();
	t->data = (char *)p;

	t->content.head = head;
	head->refs += 1;
      }
     *p++ = c;
     t->length += 1;
   }
  return 0;
}

// #pragma hdrstop

const int DefaultScrollBarSize = 15;

const char *MSTextEditor::GRSetFontDefault	= "\033[10m";
const char *MSTextEditor::GRSetFont1 	= "\033[11m";
const char *MSTextEditor::GRSetFont2 	= "\033[12m";
const char *MSTextEditor::GRSetFont3 	= "\033[13m";
const char *MSTextEditor::GRSetFont4 	= "\033[14m";
const char *MSTextEditor::GRSetFont5 	= "\033[15m";
const char *MSTextEditor::GRSetFont6 	= "\033[16m";
const char *MSTextEditor::GRSetFont7 	= "\033[17m";
const char *MSTextEditor::GRSetFont8 	= "\033[18m";
const char *MSTextEditor::GRSetFont9 	= "\033[19m";

const char *MSTextEditor::GRSetForegroundDefault	= "\033[39m";
const char *MSTextEditor::GRSetForegroundBlack 	= "\033[30m";
const char *MSTextEditor::GRSetForegroundRed  	= "\033[31m";
const char *MSTextEditor::GRSetForegroundGreen	= "\033[32m";
const char *MSTextEditor::GRSetForegroundYellow	= "\033[33m";
const char *MSTextEditor::GRSetForegroundBlue	= "\033[34m";
const char *MSTextEditor::GRSetForegroundMagenta	= "\033[35m";
const char *MSTextEditor::GRSetForegroundCyan	= "\033[36m";
const char *MSTextEditor::GRSetForegroundWhite	= "\033[37m";

const char *MSTextEditor::GRSetBackgroundDefault	= "\033[49m";
const char *MSTextEditor::GRSetBackgroundBlack 	= "\033[40m";
const char *MSTextEditor::GRSetBackgroundRed  	= "\033[41m";
const char *MSTextEditor::GRSetBackgroundGreen	= "\033[42m";
const char *MSTextEditor::GRSetBackgroundYellow	= "\033[43m";
const char *MSTextEditor::GRSetBackgroundBlue	= "\033[44m";
const char *MSTextEditor::GRSetBackgroundMagenta	= "\033[45m";
const char *MSTextEditor::GRSetBackgroundCyan	= "\033[46m";
const char *MSTextEditor::GRSetBackgroundWhite	= "\033[47m";


Bool double_buffering = MSTrue;

MSTextEditor::MSTextEditor(MSWidget *parent_,const char *title_) :
MSWidgetCommon(parent_, title_)
{
  init();
}

MSTextEditor::MSTextEditor(MSWidget *parent_,const MSStringVector&) :
MSWidgetCommon(parent_, "No Title")
{
  init();
}


MSTextEditor::~MSTextEditor(void)
{
  deleteWholeContents();
  if (_inserting) delete _inserting;
  
  delete _pixmap;
  if (_regions) delete [] _regions;
  freeColors();
//  for (int i = 0; i < NumberOf(font_cache); i++)
//  if (font_cache[i].font) XFreeFont(display(), font_cache[i].font);
  delete _cursor;
  XFreeGC( display(), gc_Normal() );
  XFreeGC( display(), gc_XOR() );
  if (blinkTimer())	delete _blinkTimer;
  if (vsb())		delete _vsb;
  if (hsb())		delete _hsb;
  if (panner())		delete _panner;
}

void MSTextEditor::readonly(MSBoolean mode_) { _readonly = mode_; }
MSBoolean MSTextEditor::readonly() { return _readonly; }

void MSTextEditor::init()
{
  _pixmap = new MSBackingStorePixmap(server(),"MSGeneral");
  _pixmap->resize(100,100);

  _readonly		= MSFalse;
  _alignment		= START;
  _column_width		= 0;
  _content_file		= NULL;
  _content_length	= 0;
  _content_offset	= 0;
  _content_stream	= NULL;
  _content_string	= NULL;
  _export_format	= ODIF;
  _first_line_offset	= 0;
  _fonts		= NULL;

  _num_fonts		= 0;

  _format		= MSTrue;
  _indentation		= 0;
  _initial_state	= NULL;
  _graphic_rendition	= NULL;
  _itemization		= NULL;
  _kerning_offset	= NULL;
  _line_layout_table	= NULL;
  //_line_spacing		= font;
  _line_spacing		= 0;
  _defaultFontSize	= 12;

  _proportional		= MSFalse; 
  //_proportional		= MSTrue;


  _mult 			= 1;
  _num_regions 			= 1;
  _max_regions 			= 1;
  _regions 			= (TextRegion *)new TextRegion[1];
  _regions[0].mode 		= REVERSE;
  _cursor 			= new InsertCursor(display(), pixmap());
  _refresh 			= 0;
  _insert_prefix 		= NULL;
  _insert_prefix_length 	= 0;
  _colors                     	= NULL;
  _color_names 			= 0;
  _font_names 			= 0;

  _origin.x = 0;
  _origin.y = 0;

  _panner=new Panner(this);

  XGCValues values;
  values.foreground=foreground();
  values.background=background();
  _mygc = XCreateGC(display(), panner()->window(), 
		    GCForeground|GCBackground, &values);

  values.foreground=foreground()^background();
  values.background=background();
  values.function=GXxor;
  _mygcXOR = XCreateGC(display(), panner()->window(), 
		       GCForeground|GCBackground|GCFunction, &values);

  XSetLineAttributes(display(), gc_Normal(), 1, LineSolid, CapButt, JoinMiter);

  _first = (Snip*)0;
  _inserting = (InsertContext*)0;
  _set_width = -1;  // Initialize to impossible value //
  _highlight = MSFalse;
  _enable_display = 0;

  _vsb = new Vsb(this);
  _hsb = new Hsb(this);
  _vsb->shadowThickness( 10 );
  
  setupContent(MSTrue, MSTrue);

  acceptFocus(MSTrue);
  selectInput(ExposureMask|ButtonPressMask|ButtonMotionMask|ButtonReleaseMask);
  addToFocusList();

  initKeyTable();

  _cmap = 0;

  setColors("black\nred\ngreen\nyellow\nblue\nmagenta\ncyan\nwhite");
  setFonts("\
-*-courier--r-normal--12-*\n\
-*-courier-bold-r-normal--12-*\n\
-*-courier--r-normal--24-*\n\
-*-courier-bold-r-normal--24-*\n\
fixed\n\
-*-helvetica--r-normal--12-*\n\
-*-helvetica-bold-r-normal--12-*\n\
-*-helvetica--r-normal--24-*\n\
-*-helvetica-bold-r-normal--24-*");

  _blinkTimer=new CursorTimer(this,MSTextEditorDefaultBlinkRate);
}

MSTextEditor::InsertCursor* MSTextEditor::cursor()
{ 
  return _cursor;
}

void MSTextEditor::setColors(const MSStringVector color_names_)
{
  _color_names = color_names_;
  _n_color_names = _color_names.length();
  initColors();
}

void MSTextEditor::setFonts(const MSStringVector font_names_)
{
  _font_names = font_names_;
  _n_font_names = _font_names.length();
}

void MSTextEditor::updateFont(Font /*oldFid_*/)
{
  cache_reset=1;
  Snip *s=_first;
  while(s)
   {
     if(s->content.head!=0 && s->content.head->font!=0) //&& oldFid_==s->content.head->font->fid)
   s->content.head->font=0;
     s=s->next;
   }
  layout(_column_width > 0 ? _column_width : pWidth(), 0, 0);
  redrawImmediately();
}

void MSTextEditor::updateForeground(unsigned long oldbg_)
{
  MSWidgetCommon::updateForeground(oldbg_);
  panner()->foreground(foreground());
  vsb()->foreground(foreground());
  hsb()->foreground(foreground());
  redraw();
}

void MSTextEditor::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_);
  panner()->background(background());
  vsb()->background(background());
  hsb()->background(background());
  redraw();
}

void MSTextEditor::configure()
{
  _pixmap->resize(width(),height());
  XRectangle clipRect[1];
  clipRect[0].x=0;
  clipRect[0].y=0;
  clipRect[0].width=pWidth();
  clipRect[0].height=pHeight();
  XSetClipRectangles(display(),gc_Normal(),0,0,&clipRect[0],1,Unsorted);

  panner()->resize(pWidth(), pHeight());
  if (panner()->mapped()==MSFalse) panner()->map();

  vsb()->moveTo(pWidth() + offset(), offset());
  vsb()->height(pHeight());
  hsb()->moveTo(offset(), pHeight() + offset());
  hsb()->width(pWidth());
  updateScrollbars();
}

void MSTextEditor::unfreeze()
{
  _enable_display += 1;

  if (_enable_display == 0) {
    layout(_column_width > 0 ? _column_width : pWidth(), 0, 0);
    redrawImmediately();
  }
}


void MSTextEditor::freeze()
{
  _enable_display -= 1;
}

void MSTextEditor::redraw()
{
  if (_enable_display >= 0) redrawImmediately();
}

typedef int (*F_DRAW16)(Display *,Drawable,GC,int,int,const XChar2b *,int);
typedef int (*F_DRAW)(Display *,Drawable,GC,int,int,const char *,int);


void MSTextEditor::redisplay(XExposeEvent *e, Region r, Drawable d)
{
  if (_enable_display < 0) return;
  _pixmap->lock();

  Snip *s;
  int x, y;
  XRectangle clip;
  ExposeContext cx;

  F_DRAW16 f_draw16;
  F_DRAW   f_draw;

  cx.my_r   = r;
  cx.editor = this;

  if (cx.my_r == NULL && e != NULL)
   {
     //
     // No Region, make one from exposed area. This should be
     // exceptional, widgets should have the compress exposures
     // mode set and always have a region!
     //
     clip.x = e->x;
     clip.y = e->y;
     clip.width = e->width;
     clip.height = e->height;
     cx.my_r = XCreateRegion();
     XUnionRectWithRegion(&clip, cx.my_r, cx.my_r); 
   }
  if (cx.my_r)
   {
     XClipBox(cx.my_r, &clip);
     clip.x -= _origin.x;
     clip.y -= _origin.y;
     Region clip_region = XCreateRegion();
     XUnionRectWithRegion(&clip, clip_region, clip_region); 
     //XSetRegion(display(), gc_Normal(), clip_region);
     //XSetRegion(display(), gc_XOR(), clip_region);
     XDestroyRegion(clip_region);
   }
  else
   {
     XSetClipMask(display(), gc_Normal(), None);
     XSetClipMask(display(), gc_XOR(), None);
   }
  XSetBackground(display(), gc_Normal(), background());
  if (e == NULL)
   {
     //
     // If the XEvent parameter is NULL, then assume this is
     // internally generated expose event and the covered
     // area is not necessarily initially cleared. Clear the
     // area covered by the expose region (my_r). [The following
     // code is the simplest I could invent for the task, if
     // anyone has better, let me know --msa].
     // When also the Region is not given, make this a special
     // case that refreshes the whole window.
     //
     if (cx.my_r)
      {
	XClipBox(cx.my_r, &clip);
	XFillRectangle(display(), d, backgroundShadowGC(), 
		       clip.x - _origin.x,clip.y - _origin.y,
		       clip.width, clip.height);
      }
     else
     XFillRectangle(display(), d, backgroundShadowGC(),
		    0, 0, pWidth(),  pHeight());
   }

  if (cx.my_r)	// repair the right area..
   {
     XClipBox(cx.my_r, &clip);
     XUnionRectWithRegion(&clip, cx.my_r, cx.my_r);
   }

  markSelection(d);
  
  f_draw16 = (F_DRAW16) ::XDrawString16;
  f_draw   = (F_DRAW)   ::XDrawString;
  y = _y;
  x = _x;

  for (s = _first; s; s = s->next)
   {
     if ((unsigned int)s->widget == MSTrue)
     s->content.widget->moveTo(s->x - _origin.x, 
			       s->y - _origin.y - (s->ascent + s->descent));

     SnipData *h = s->content.head;
     unsigned int m = cx.p.bits ^ s->mode.bits;

     if (m) 
      {

	//if (m & CrossedOut_MASK) 	cx.changeCrossedOut(s, d, x, y);
	//if (m & Underline_MASK) 	cx.changeUnderline(s, d, x, y);
	//if (m & Framing_MASK) 		cx.changeFraming(s, d, x, y);

	if (m & (Background_MASK|ImageInversion_MASK)) cx.changeBackground(s, d);
	if (m & (Foreground_MASK|ImageInversion_MASK)) cx.changeForeground(s, d);

	if (s->mode.bits&(ImageInversion_MASK|Background_MASK))
	 {
	   f_draw16 = (F_DRAW16) ::XDrawImageString16;
	   f_draw   = (F_DRAW)   ::XDrawImageString;
	 }
	else
	 {
	   f_draw16 = (F_DRAW16) ::XDrawString16;
	   f_draw   = (F_DRAW)   ::XDrawString;
	 }
      }
     cx.p = s->mode;
     x = s->x;
     y = s->y;

     if (!s->space && s->xWidth > 0 && s->data != NULL &&
	 (cx.my_r == NULL || XRectInRegion(cx.my_r, x, y - s->ascent, s->xWidth,
					   s->ascent + s->descent) != RectangleOut) 
       )
      {
	XSetFont(display(), gc_Normal(), h->font->fid);
	if (h->bytes == 2)
        (*f_draw16)(display(), d, gc_Normal(), 
		    (int)x + s->offset - _origin.x, (int)y - _origin.y,
		    (XChar2b *)s->data, s->length);
	else
        (*f_draw)(display(), d, gc_Normal(), 
		  (int)x + s->offset - _origin.x, (int)y - _origin.y,
		  s->data, s->length);
      }
     if (!s->floating) x += s->xWidth;
     if (s->hasEndLine() && 
	 (cx.p.bits&(Underline_MASK|CrossedOut_MASK|Framing_MASK)))
     cx.flushPendingGraphics(s->next, x, y);
   }

  cx.flushPendingGraphics(s, x, y); // In case endseq was omitted! //

  if (d == pixmap()) { // we are doing double buffering
    if (cx.my_r)
     {
       XClipBox(cx.my_r, &clip);
       XCopyArea(display(),pixmap(),panner()->window(), gc_Normal(),
		 clip.x - _origin.x, clip.y - _origin.y,
		 clip.width,clip.height,
		 clip.x - _origin.x, clip.y - _origin.y);
     }
    else {
      XCopyArea(display(),pixmap(),panner()->window(), gc_Normal(),
		0,0,pWidth(),pHeight(),0,0);
    }
  }

  if (cx.my_r != r && cx.my_r) {
    XDestroyRegion(cx.my_r);
    cx.my_r = 0;
  }

  XDrawRectangle(display(), d, gc_Normal(), 0, 0, pWidth()-1, pHeight()-1);

  drawCursor();
  _pixmap->unlock();
}

void MSTextEditor::drawCursor()
{
  _cursor->draw(gc_XOR(), panner()->window(), &_origin, _line_spacing,
		((_cursor->location.snip && _cursor->location.snip->data) ?
		 snipWidth(_cursor->location.snip, _cursor->location.snip->data, 1) : 9));
}

Region updateXRegion(Region region_, int x, int y, int w, int h)
{
  XRectangle rect;

  if (!region_) region_ = XCreateRegion();
  if (w == 0 || h == 0) return region_;
  rect.x = x;
  rect.y = y;
  rect.width = w;
  rect.height = h;
  XUnionRectWithRegion(&rect, region_, region_);
  return region_;
}

void MSTextEditor::redrawImmediately()
{
  initRefreshRegion();
  updateCursorPosition(MSFalse, MSTrue, MSFalse);
  updateTextRegions(MSFalse);
  _refresh = updateXRegion(_refresh, _origin.x, _origin.y, pWidth(), pHeight());
  doRefreshRegions();

  updateScrollbars();
}

char * MSTextEditor::getString(long *)
{
  return 0;
}


char * MSTextEditor::getSubstring(long *, long, long)
{
  return 0;
}


void MSTextEditor::extract(long, long, ExtractFeedFunction, void *)
{
}

MSString MSTextEditor::extractSelectedContent()
{
  TextLocation dot[2];
  XPoint xy[2];
  long range[2];

  MSString selection("");
  unsigned int len = 0;

  range[0] = _regions[0].range[0];// To prevent reordering //
  range[1] = _regions[0].range[1];//  in the widgets data. //

  locations(range, 2, dot, xy);
  if (range[0] < range[1])
  for (Snip *s = dot[0].snip; s; s = s->next) {
    if (s->content.head && s->length > 0 &&
	s->content.head->bytes * s->length > 0) {
      char *buf = (char*)malloc(s->length + 1);
      strncpy(buf, (char *)(&s->data[s->content.head->bytes * s->offset]),
							s->length);
      buf[s->length - 1] = '\0';
      selection << buf;
      len += s->length;
      selection = selection(0, len);
      if (s->endseq) { selection << "\n"; len ++; }
      free(buf);
    }
    if (s == dot[1].snip) break;
  }
  return selection;
}

void MSTextEditor::replace(long , long , char *, long )
{
}

void MSTextEditor::replaceTagged(long , long , char *, long , MSTextEditorTextTag )
{
}

void MSTextEditor::insertPrefix(char *, long )
{
}

long MSTextEditor::getInsertionPoint()
{
  return _cursor->position;
}

long MSTextEditor::search(int , char *, long , int )
{
  return 0;
}

MSTextEditorTextTag MSTextEditor::getInsertionTag()
{
  return 0;
}

void MSTextEditor::setInsertionTag(MSTextEditorTextTag)
{
}

void MSTextEditor::getRegionPosition(TextRegion, long *, long *)
{
}

int MSTextEditor::setRegion(long, long,MSTextEditor::TextHighlight)
{
  return 0;
}

int MSTextEditor::unsetRegion (MSTextEditor::TextRegion)
{
  return 0;
}

///// private methods

void MSTextEditor::initRefreshRegion() 
{ 
  if (!_refresh) {
    _refresh = XCreateRegion(); 
  }
}

//
//  Change the cursor into a new location and/or refresh cursor.
//  if goal, then reset horizontal goal position.
//  if expose, then request expose events
//  if bounds, then request that cursor is in visible window
//
void MSTextEditor::updateCursorPosition(MSBoolean goal, MSBoolean expose, MSBoolean) 
{
  XPoint xy;
  XRectangle rect;
  TextLocation *location = &_cursor->location;

  if (_enable_display < 0) return;
  if (expose)
   {
     _cursor->getBounds(&rect); // to clear out old cursor mark 
     initRefreshRegion();
     _refresh = updateXRegion(_refresh, rect.x, rect.y, rect.width, rect.height);
   }
  if (!_cursor->valid)
   {
     _cursor->position = offset(location);
     _cursor->valid = MSTrue;
   }
  coordinates(location->snip, location->offset, &xy);
  _cursor->x = xy.x;
  _cursor->y = xy.y + _cursor->h + 1;
  if (goal) _cursor->goal = _cursor->x;
  _cursor->getBounds(&rect);
  if (expose) 
  _refresh = updateXRegion(_refresh, rect.x, rect.y, rect.width, rect.height);
  if (location->snip)
   {
     rect.y -= location->snip->ascent;
     rect.height += location->snip->ascent+location->snip->descent;
   }
  if (rect.x < 0)
   {
     //
     // Because the caret extends to the left of the current
     // point, it would generate this notify on every beginning
     // line. Clip area to positive side of x.
     
     if ((int)rect.width + rect.x < 0)
     rect.width = 0;
     else
     rect.width += rect.x;
     rect.x = 0;
   }
  checkBounds(&rect);
}


void MSTextEditor::initColors()
{
  int i;
  XColor exact, color;

  if (!_cmap) _cmap = DefaultColormap(display(), DefaultScreen(display()));
  if (_colors) free(_colors);
  _colors = (unsigned long *)malloc(_n_color_names * sizeof(unsigned long));
  for (i = 0; i < _n_color_names; i++)
   {
     if (!XAllocNamedColor(display(), _cmap, _color_names(i), &exact, &color))
      {
	color.pixel = FRAME_NO_PIXEL; // failed...
      }
     _colors[i] = color.pixel;
   }
}

void MSTextEditor::freeColors()
{
  if (_colors == NULL)
  return;
/*
for (int i = 0; i < 8; i++)
if (_colors[i] != FRAME_NO_PIXEL)
XFreeColors(display(), _cmap, &_colors[i], 1, (unsigned long)0);
*/
  free((void *)_colors);
  _colors = NULL;
}

void MSTextEditor::switchColor()
{
  double_buffering = 1 - double_buffering;
}

void MSTextEditor::setForeground(unsigned long pixel_)
{
  XSetForeground(display(), gc_Normal(), pixel_);
}

void MSTextEditor::setBackground(unsigned long pixel_)
{
  XSetBackground(display(), gc_Normal(), pixel_);
}

//
// Find extents for Snip sequence (s, e). The terminating 'e' is not included.
//
// NOTE: THIS FUNCTION ASSUMES CORRECT CALLING: 'e' must follow 's'
//
void MSTextEditor::findExtents(MSTextEditorTypes::Snip *s, Snip *e, int *ascent, int *descent)
{
  int base = s->y; // Compute ascent/descent relative to this! 

  *ascent = 0;
  *descent = 0;
  for ( ; s && s != e; s = s->next)
  if (!s->floating)
   {
     int adj = base - s->y;

     if (s->ascent + adj > *ascent)
     *ascent = s->ascent + adj;
     if (s->descent - adj > *descent)
     *descent = s->descent - adj;
   }
}

//
//    Start (h != 0) or End (h == 0) rendition effect.
//

void MSTextEditor::updateTextRegions(MSBoolean bounds) 
{ 
  for (int n = _num_regions-1; n>=0; n--)
  updateTextRegion(&_regions[n], bounds); 
}

void MSTextEditor::doRefreshRegions() 
{ 
  if (_refresh && !XEmptyRegion(_refresh))
   {
     Region r = _refresh;
     _refresh = 0;
     redisplay((XExposeEvent *)NULL, r, 
	       double_buffering?pixmap():panner()->window());
     XDestroyRegion(r);
   }
}

static Time NoteTime(XEvent *event)
{
  if (event != NULL)
   {
     switch (event->type)
      {
      case ButtonPress:
      case ButtonRelease:
	return event->xbutton.time;
      case KeyPress:
      case KeyRelease:
	return event->xkey.time;
      case MotionNotify:
	return event->xmotion.time;
      case EnterNotify:
      case LeaveNotify:
	return event->xcrossing.time;
      }
   }
  return CurrentTime; // A kludge, should probably give warning --msa 
}

void MSTextEditor::notePosition(XEvent *event, XPoint *p)
{
  switch (event->type)
   {
   case ButtonPress:
   case ButtonRelease:
     p->x = event->xbutton.x + _origin.x;
     p->y = event->xbutton.y + _origin.y;
     break;
   case KeyPress:
   case KeyRelease:
     {
       XRectangle cursor;
       _cursor->getBounds(&cursor);
       p->x = cursor.x + _origin.x + cursor.width / 2;;
       p->y = cursor.y + _origin.y + cursor.height / 2;;
     }
     break;
   case MotionNotify:
     p->x = event->xmotion.x + _origin.x;
     p->y = event->xmotion.y + _origin.y;
     break;
   case EnterNotify:
   case LeaveNotify:
     p->x = event->xcrossing.x + _origin.x;
     p->y = event->xcrossing.y + _origin.y;
     break;
   default:
     p->x = 0;
     p->y = 0;
     break;
   }
}


int MSTextEditor::startEditing(InsertMode mode_, XEvent *event_)
{
  // do callback....

  unsetSelection();
  
  initRefreshRegion();
  if (event_)
  if (_allow_edit) { 
    XBell(display(), 0); 
    return 0; 
  }
  else _time = NoteTime(event_);

  if (!_inserting)
   {
     adjustSnipFirst(&_cursor->location);
     _inserting = insertContent(_cursor->location.snip, 
				_cursor->location.offset, mode_);
     _cursor->valid = MSFalse;
     if (_insert_prefix)
      {
	_inserting->lock(MSFalse);
	_inserting->feedContent(_insert_prefix, _insert_prefix_length);
	_inserting->lock(MSTrue);
      }
   }
  return 1;
}

void MSTextEditor::endEditing(int deleted_)
{
  int i, n;

  _mult = 1;
  long ref = _cursor->position;
  if (_inserting)
   {
     _inserting->insertLocation(&_cursor->location, _refresh);
     if (_enable_display >= 0)
      {
	layout(_column_width > 0 ? _column_width : pWidth(),
	       //
	       // The beginning of text often gets
	       // marked by NULL location pointer. Use
	       // 'first' in that case to prevent
	       // unnecessary total relayout...
	       _cursor->location.snip ? _cursor->location.snip : _first,
	       _refresh);
	//if (_num_children > 0) configureChildren();
      }			
     _cursor->position = offset(&_cursor->location);
     _cursor->valid = MSTrue;
   }
  long position = _cursor->position;
  //
  // Do a feeble attempt to update text regions
  
  if (deleted_ < 0) { deleted_ = -deleted_; ref = position; }

  for (n = _num_regions-1; n >= 0; n--)
   {
     TextRegion *r = &_regions[n];
     if (r->mode == UNUSED)
     continue;
     for (i = 0; i < NumberOf(r->range); ++i)
     if (ref <= r->range[i])
     if (deleted_ == 0)
     r->range[i] += _cursor->position-ref;
     else if (ref + deleted_ > r->range[i])
     r->range[i] = ref;
     else
     r->range[i] -= deleted_;
     updateTextRegion(r, MSFalse); // Should fine tune? --msa 
   }
  // changeWidgetSize(MSTrue); 
  updateCursorPosition(MSTrue, MSTrue, MSTrue);
  doRefreshRegions();
  updateScrollbars();
}

//
// AdjustSnipFirst
//	adjust the location to point the first editable Snip in the
//	content, if location is pointing to the beginning of the content.
//
void MSTextEditor::adjustSnipFirst(MSTextEditor::TextLocation *location_)
{
  if (location_->snip) return;
  location_->offset = 0;
  for (Snip *s = _first; s; s = s->next)
  if (s->hasEditableContents()) { location_->snip = s; break; }
}

long MSTextEditor::offset(MSTextEditor::TextLocation *dot)
{
  if (!dot->snip) return 0;

  long voffset = 0;

  for (Snip *s = _first; s; s = s->next)
  if (dot->snip == s) { voffset += dot->offset; break; }
  else if (s->hasEditableContents()) voffset += s->virtualLength();

  return voffset;
}

//
// initSnipMode
//	initial SnipMode (size_modification has to have the default 100!)
//
static MSTextEditorTypes::SnipMode initSnipMode(0,0,100);

static void Gn_Feed(void *client_data, ccf_Gs, char *s, int n) 
{
  MSTextEditor::copy1_GL((MSTextEditorTypes::InsertContext*)client_data, 
			    n, (unsigned char *)s, 
			    NULL, MSTextEditor::charsetDefault());
}

#if 0
const int BS  = 8;	/* Backspace */
const int HT  = 9;	/* Horizontal tabulation */
const int LF  = 10;	/* Line Feed */
const int FF  = 12;	/* Form Feed */
const int CR  = 13;	/* Carriage Return */
const int SUB = 26;	/* Substitute */
const int RS  = 0x1E;	/* Record Separator, used as Paragraph end (not ODA) */
#endif

//
// IgnoreFormattedContent
//	is MSTrue, when inserting Formatted content and not specially
//	allowed by locking into it (only done in _XeMakeLayoutContext)
//
#define IgnoreFormattedContent(cx) \
(((cx)->_mode.bits & Content_MASK) == Content_FORMATTED && \
 !((cx)->_lock.bits & Content_MASK))

static void Do_C0(MSTextEditorTypes::InsertContext *cx, int c)
{
  register MSTextEditorTypes::Snip *t;
  if (IgnoreFormattedContent(cx))
  return;  // Completely Ignore Formatted content for now! //
  switch (c)
   {
   case HT:
     // HT is not allowed in ODA, but to support traditional
     // text files, convert this into a special STAB
     // sequence. Each HT will end up into differenct
     // sequence.
     //
     //t = cx->appendSnip();
     //t->tab = MSTrue;
     //t->tabref = NEXTTABSTOP;
     //t->endseq = MSTextEditor::End;
     break;  
   case CR:
     //
     // In processable format a CR can only appear as an
     // ITEM terminator or immediately preceding an LF.
     //
     t = cx->_last;
     if (t == NULL || t->endseq || t->mode.tag != cx->_mode.tag ||
	 ((t->mode.bits ^ cx->_mode.bits) & Content_MASK))
     t = cx->appendSnip();
     else
     t->valid = MSFalse;
     t->endseq = MSTextEditor::End;
     break;
   case FF:
     // Treat FF as LF for now //
   case LF:
     //
     // Each LF will terminate the current line. If line is
     // empty or the previous character already terminated
     // the line, an empty sequence will be created. An LF
     // will reset current sequence to TEXT.
     //
     t = cx->_last;
     if (t == NULL || t->hasEndLine() || t->space || t->widget ||
	 t->mode.tag != cx->_mode.tag ||
	 ((t->mode.bits ^ cx->_mode.bits) & Content_MASK))
     t = cx->appendSnip();
     else
     t->valid = MSFalse;
     t->endseq = MSTextEditor::EndLine;
     break;
   case RS:
     //
     // RS is used experimentally to mark end of paragraph.
     // (Some other coding may get used in future.)
     //
     t = cx->_last;
     if (t == NULL || t->hasEndLine() || t->space || t->widget ||
	 t->mode.tag != cx->_mode.tag ||
	 ((t->mode.bits ^ cx->_mode.bits) & Content_MASK))
     t = cx->appendSnip();
     else
     t->valid = MSFalse;
     t->endseq = MSTextEditor::EndParagraph;
     break;
   default:
     break;
   }
}

#if 0
const int BPH = 2;
const int NBH = 3;
const int PLD = 11;
const int PLU = 12;
const int SOS = 24;
const int ST  = 28;
#endif

static void Do_C1(MSTextEditorTypes::InsertContext *cx, int c)
{
  if (IgnoreFormattedContent(cx))
   {
     if (c == ST && !(cx->_lock.bits & Content_MASK))
      {
	cx->_mode.bits &= ~Content_MASK;
	cx->_mode.bits |= Content_PROCESSABLE;
      }
     return;  // Completely Ignore Formatted content for now! //
   }
  switch (c)
   {
   case PLU:
     //
     // A PLU can only follow PLD, or it can start Partial
     // line up from neutral state (NOALIGNMENT). If preceded by
     // PLU, this one is ignored (PLU state does not change).
     //
     if (!cx->_locked)
     cx->_lock.bits |= PartialLine_MASK;
     else if (cx->_lock.bits & PartialLine_MASK)
     break;  // Locked Mode, do not change //
     if (cx->_mode.bits & PartialLine_DOWN)
     cx->_mode.bits &= ~PartialLine_DOWN;
     else
     cx->_mode.bits |= PartialLine_UP;
     break;
   case PLD:
     //
     // a PLD can only follow PLU, or it can start Partial
     // line down from neutral state (NOALIGNMENT). If preceded by
     // PLD, this one is ignored (PLD state does not change).
     //
     if (!cx->_locked)
     cx->_lock.bits |= PartialLine_MASK;
     else if (cx->_lock.bits & PartialLine_MASK)
     break;  // Locked Mode, do not change //
     if (cx->_mode.bits & PartialLine_UP)
     cx->_mode.bits &= ~PartialLine_UP;
     else
     cx->_mode.bits |= PartialLine_DOWN;
     break;
   case BPH:
     //
     // BPH is sensible only if there is a MSTextEditorTypes::Snip to attach
     // (ignored otherwise)
     //
     if (cx->_last != NULL)
      {
	cx->_last->brk = MSTextEditor::Break_BPH;
	cx->_last->valid = MSFalse;
      }
     break;
   case NBH:
     //
     // NBH is sensible only if there is a MSTextEditorTypes::Snip to attach
     // (ignored otherwise)
     //
     if (cx->_last != NULL)
      {
	cx->_last->brk = MSTextEditor::Break_NBH;
	cx->_last->valid = MSFalse;
      }
     break;
   case SOS:
     if (!(cx->_lock.bits & Content_MASK))
      {
	cx->_mode.bits &= ~Content_MASK;
	cx->_mode.bits |= Content_FORMATTED;
      }
     break;
   default:
     break;
   }
}

static void Cn_Feed(void *client_data, ccf_Cs Cn, int c)
{
  if (client_data == NULL)
  return; // FATAL ERROR //
  else if (Cn == ccf_C0)
  Do_C0((MSTextEditorTypes::InsertContext *)client_data, c);
  else if (Cn == ccf_C1)
  Do_C1((MSTextEditorTypes::InsertContext *)client_data, c);
}
// uncomment if need to use
// const int HPB = 0x6A; // Character Position Backward //
// const int HPR = 0x61; // Character Position Relative //
// const int PTX = 0x5C; // Parallel Texts //
const int SGR = 0x6D; // Select Graphic Rendition //
// const int SRS = 0x5B; // Start Reverse String //
const int VPB = 0x6B; // Line Position Backward //
const int VPR = 0x65; // Line Position Relative //
const int DAQ = 0x6F; // Define Area Qualification //

// const int GCC = 0x5F; // Graphic Character Composition
const int GSM = 0x42; // Graphic Size Modification
// const int IGS = 0x4D; // Identify Graphic Subrepertoire
const int JFY = 0x46; // Justify
const int QUAD= 0x48; // Quad
// const int SCS = 0x67; // Set Character Spacing
// const int SHS = 0x4B; // Select Character Spacing
// const int SACS= 0x5C; // Set Additional Character Spacing
const int SLS = 0x68; // Set Line Spacing
// const int SRCS= 0x66; // Set Reduced Character Spacing
// const int SSW = 0x5B; // Set SPACE Width
const int STAB= 0x5E; // Selective Tabulation
const int SVS = 0x4C; // Select Line Spacing

static void ESC_Feed(void *, char *, int, int) { }


//
//  Process single parameter value of SGR or "graphic rendition".
//
//  (ISO 8613-6  11.1.8 Table 4)
//
static void selectGraphicRendition(MSTextEditorTypes::InsertContext *cx, int r)
{
  unsigned int mask, value;

  switch (r)
   {
   case 0: 
     //
     // reset to default rendition 
     // Unfortunately, mode contains flags that make it not
     // possible to just do mode = init. This is indication of
     // incorrect grouping.. should be fixed --msa 
     //
     mask = Weight_MASK | Italicized_MASK | Underline_MASK | 
     Blinking_MASK | ImageInversion_MASK | CrossedOut_MASK | Font_MASK | 
     Foreground_MASK | Background_MASK | Framing_MASK | Overlined_MASK;

     if (cx->_locked)
     mask &= ~cx->_lock.bits;
     value = 0;
     break;
   case 1: // bold or increased density //
     mask = Weight_MASK;
     value = Weight_BOLD;
     break;
   case 2: // faint or decreased density //
     mask = Weight_MASK;
     value = Weight_FAINT;
     break;
   case 3: // italicized //
     mask = value = Italicized_MASK;
     break;
   case 4: // underlined //
     mask = Underline_MASK;
     value = Underline_SINGLE;
     break;
   case 5: // slowly blinking //
     mask = Blinking_MASK;
     value = Blinking_SLOWLY;
     break;
   case 6: // rapidly blinking //
     mask = Blinking_MASK;
     value = Blinking_RAPIDLY;
     break;
   case 7: // negative image //
     mask = value = ImageInversion_MASK;
     break;
   case 9: // crossed-out //
     mask = value = CrossedOut_MASK;
     break;
   case 10: // primary (default) font //
   case 11: // first alternative font //
   case 12: // second alternative font //
   case 13: // third alternative font //
   case 14: // fourth alternative font //
   case 15: // fifth alternative font //
   case 16: // sixth alternative font //
   case 17: // seventh alternative font //
   case 18: // eigth alternative font //
   case 19: // ninth alternative font //
     mask = Font_MASK;
     value = Font_VALUE(r - 10);
     break;
   case 21: // doubly underlined //
     mask = Underline_MASK;
     value = Underline_DOUBLE;
     break;
   case 22: // normal intensity (neither bold nor faint) //
     mask = Weight_MASK;
     value = 0;
     break;
   case 23: // not italicized //
     mask = Italicized_MASK;
     value = 0;
     break;
   case 24: // not underlined (neither singly nor doubly) //
     mask = Underline_MASK;
     value = 0;
     break;
   case 25: // steady (not blinking) //
     mask = Blinking_MASK;
     value = 0;
     break;
   case 26: // variable spacing //
     mask = value = 0;
     break;
   case 27: // positive image //
     mask = ImageInversion_MASK;
     value = 0;
     break;
   case 29: // not crossed-out //
     mask = CrossedOut_MASK;
     value = 0;
     break;
   case 30: // ISO 6429, black display //
   case 31: // ISO 6429, red display //
   case 32: // ISO 6429, green display //
   case 33: // ISO 6429, yellow display //
   case 34: // ISO 6429, blue display //
   case 35: // ISO 6429, magenta display //
   case 36: // ISO 6429, cyan display //
   case 37: // ISO 6429, white display //
     mask = Foreground_MASK;
     value = Foreground_VALUE(r - 29);
     break;
   case 39: // ISO 6429, default display color //
     mask = Foreground_MASK;
     value = 0;
     break;
   case 40: // ISO 6429, black background //
   case 41: // ISO 6429, red background //
   case 42: // ISO 6429, green background //
   case 43: // ISO 6429, yellow background //
   case 44: // ISO 6429, blue background //
   case 45: // ISO 6429, magenta background //
   case 46: // ISO 6429, cyan background //
   case 47: // ISO 6429, white background //
     mask = Background_MASK;
     value = Background_VALUE(r - 39);
     break;  
   case 49: // ISO 6429, default background color //
     mask = Background_MASK;
     value = 0;
     break;
   case 50: // not variable spacing //
     mask = value = 0;
     break;
   case 51: // ISO 6429, framed //
     mask = Framing_MASK;
     value = Framing_FRAMED;
     break;
   case 52: // ISO 6429, encircled //
     mask = Framing_MASK;
     value = Framing_ENCIRCLED;
     break;
   case 53: // ISO 6429, overlined //
     mask = value = Overlined_MASK;
     break;
   case 54: // ISO 6429, not framed, not encircled //
     mask = Framing_MASK;
     value = 0;
     break;
   case 55: // ISO 6429, not overlined //
     mask = Overlined_MASK;
     value = 0;
     break;
   default:
     mask = value = 0;
     break;
   }
  if (!cx->_locked)
  cx->_lock.bits |= mask;
  else if (cx->_lock.bits & mask)
  return;  // Locked mode, do not change. Ignore control //
  cx->_mode.bits &= ~mask;
  cx->_mode.bits |= value;
}

static void Do_CSI(MSTextEditorTypes::InsertContext *cx, int n, int *p, int F)
{
  switch (F)
   {
   case VPB:
     // Not implemented //
     break;
   case VPR:
     // Not implemented //
     break;
   case SGR:
     if (n == 0)
     selectGraphicRendition(cx, 0);
     else while (--n >= 0)
     selectGraphicRendition(cx, *p++);
     break;
   case DAQ: // Define Area Qualification //
     if (!cx->_locked)
     cx->_lock.bits |= Content_MASK;
     else if (cx->_lock.bits & Content_MASK)
     break;  // Locked Mode, do not change //
     while (--n >= 0)
     // Only parameters 0 and 1 implemented now!! //
     switch (*p++)
      {
      default:
        break;
      case 0: // Accept All Input (default) //
        cx->_mode.bits &= ~Content_MASK;
        cx->_mode.bits |= Content_PROCESSABLE;
        break;
      case 1: // Accept No Input and do not transmit //
        cx->_mode.bits &= ~Content_MASK;
        cx->_mode.bits |= Content_PROTECTED;
        break;
      }
     break;
   default:
     break;
   }
}

static void Do_CSI1(MSTextEditorTypes::InsertContext *cx, int n, 
		    int *p,int F,int I)
{
  register MSTextEditorTypes::Snip *t;
  MSTextEditor::Alignment align;

  //
  // In ODA, only intermediate used is SPACE--ignore all others.
  //
  if (I != 0x20)
  return;
  switch (F)
   {
   case GSM: // ISO 6429 : 1988 //
     //
     // Graphic Size Modification defines the percentage of
     // the vertical and horizontal size change for subsequent
     // characters. ONLY VERTICAL CHANGE IMPLEMENTED HERE!
     //
     if (!cx->_locked)
     cx->_lock.size_modification = ~0;
     else if (cx->_lock.size_modification)
     break;
     cx->_mode.size_modification = *p ? *p : 100;
     break;
   case JFY: // ISO 6429 : 1988 //
     if (!cx->_locked)
     cx->_lock.bits |= Justify_MASK;
     else if (cx->_lock.bits & Justify_MASK)
     break;  // Locked Mode, do not change //
     cx->_mode.bits &= ~Justify_MASK;
     align = MSTextEditor::NOALIGNMENT;
     while (--n >= 0)
     switch (*p++)
      {
      default:
      case 0: // no justification (use NOALIGNMENT default) //
      case 1: // word fill (ignored) //
      case 4: // hyphenation (ignored) //
      case 8: // italian hyphenation (ignored) //
        break;
      case 2: // word space //
      case 3: // letter space (assume word space) //
        align = MSTextEditor::JUSTIFIED;
        break;
      case 5: // flush to line home position //
        align = MSTextEditor::START;
        break;
      case 6: // centre between home and limit //
        align = MSTextEditor::CENTER;
        break;
      case 7: // flush to line limit position //
        align = MSTextEditor::END;
        break;
      }
     cx->_mode.bits |= Justify_VALUE(align);
     break;
   case QUAD: // ISO 6429 : 1988 //
     //
     // Multiple QUAD on same spot just overwrite the previous
     // value.
     //
     t = cx->_last;
     if (t == NULL || !t->endseq)
      {
	t = cx->appendSnip();
	t->endseq = MSTextEditor::End;
      }
     switch (n > 0 ? *p : 0)
      {
      default:
      case 0: // Start //
      case 1: // Start with leader fill //
	align = MSTextEditor::START;
	break;
      case 2: // Center //
      case 3: // Center with leader fill //
	align = MSTextEditor::CENTER;
	break;
      case 4: // End //
      case 5: // End with leader fill //
	align = MSTextEditor::END;
	break;
      case 6: // Justify //
	align = MSTextEditor::JUSTIFIED;
	break;
      }
     t->quad = align;
     break;
   case SLS:
     // Fix later --msa //
     break;
   case SVS:
     // Fix later --msa //
     break;
   case STAB:
     t = cx->_last;
     if (t && !t->endseq)
     t->valid = MSFalse;
     else
     t = cx->appendSnip();
     t->endseq = MSTextEditor::End;
     t->tab = MSTrue;
     t->tabref = *p;
     break;
   default:
     break;
   }
}

static void CSI_Feed(void *client_data,int P,
		     int *p,int n,char *I,int nI,int F)
{
  MSTextEditorTypes::InsertContext *cx = (MSTextEditor::InsertContext *)client_data;

  if (P)
  return;  // No use for Private sequences now //
  if (cx == NULL)
  return;
  if (IgnoreFormattedContent(cx))
  return;  // Completely Ignore Formatted content for now! //
  if (nI == 0)
  Do_CSI(cx, n, p, F);
  else if (nI == 1)
  Do_CSI1(cx, n, p, F, *I & 0xFF);
}


static int Designate_G(void *, ccf_Gs, int, int, int) 
{ return 0; }

static int Designate_C( void *, ccf_Cs, int) { return 0; }

//
// MSTextEditorTypes::InsertContext *MSTextEditor::insertContent(...)
//
//	Start inserting character content after the specified snip. Returns
//	pointer to a new "context" block, and this context is implicitly
//	set after this call.
//
//	*NOTE*	after should point to a Snip representing original text,
//		not anything generated by layout.
//
//	New insertion point is opened in Snip specified by the position
//	parameter and length of the Snip:
//
//	position > length,	insert totally after the snip (e.g, if
//				the snip is ending a line, the new data
//				will be inserted after the line end).
//	position == length,	insert at end of snip, but before the
//				possible end of line mark.
//	position < length,	insert *before* the character specified
//				by the position (0==first character and
//				<length-1>==last character).
//
//	'mode' parameter controls the initial mode, before applying the
//	value from the initialState resource:
//
//	mode == 0,	initial mode for the snip is EMPTY (all zeroes).
//	mode > 0,	if the insert point at end of the Snip, the mode
//			will be taken from the next editable Snip after
//			this Snip. (If insert point is inside the snip,
//			the mode is taken from the snip).
//	mode < 0,	Use the mode from insert point Snip (after).
//
MSTextEditorTypes::InsertContext *MSTextEditor::insertContent(
								    Snip *after, int position, int mode)
{
  InsertContext *cx;
  Snip *t;
  int n;
  char *s;

  if (after == NULL) return beginContent(); /* Bad call, easy way out.. */
  cx = new InsertContext;
  if (!cx) return NULL;
  if (position < 0) position = 0; 
  if (!after->hasEditableContents()) return NULL;
  cx->_editor = this;
  cx->_list   = &_first;
  cx->_last   = after;
  cx->_mode   = after->mode;

  //
  // Split content of snip After into two parts
  //
  n = after->length - position;
  s = after->data; 		// Needs to be saved, can be changed below 
  if (n > 0 || (n == 0 && after->hasEndPosition()))
   {
     //
     // Inserting within current Snip. The snip needs to be split.
     // This is achieved by changing the 'after' to reflect only
     // the tail part and creating a new empty snip in front of it.
     // After which the the new data will be inserted.
     //
     cx->adjustLastPointer(after);
     if (after->layout)
      {
	cx->updateExposeArea(after->x,
			     after->y - after->ascent,
			     after->xWidth,
			     after->ascent + after->descent);
	after->layout = after->valid = MSFalse;
      }
     if (after->data && n > 0)
      {
	after->data += after->content.head->bytes * position;
	after->length = n;
      }
     else
      {
	after->data = NULL;
	after->length = 0;
      }

     //
     // If there was data in 'after' *before* the insert point,
     // it now must be saved away.
     //
     if (position > 0 && s &&
	 (t = cx->beginAppend(position,
			      after->content.head->character_set,
			      after->content.head->bytes)) != NULL)
      {
	memcpy((void *)t->data,(void *)s,
	       position * after->content.head->bytes);
	t->length += position;
      }
   }
  else if (mode > 0)
   {
     //
     // Inserting totally after the current Snip and request is
     // to use mode from following Snip. Do that.
     //
     t = after;
     while ((t = t->next) != NULL)
     if (t->hasEditableContents()) { 
       cx->_mode = t->mode; 
       break; 
     }
   }

  if (mode == 0)
  cx->_mode = initSnipMode;
  cx->initGxMap();
  cx->_ccf = ccf_Open(cx, Gn_Feed, Cn_Feed, ESC_Feed, CSI_Feed,
		      Designate_G, Designate_C);
  if (_initial_state != NULL)
  cx->feedContent(_initial_state, strlen(_initial_state));
  cx->lock(MSTrue);
  return cx;
}


//
//  First check if a sequence of characters can be appended to the
//  current run of data being collected into context block.
//  This can be done, if
//		- context data has sufficient available space and
//		- character set is same and
//		- font selection is the same.
//  If not, then flush out the current collected data and start
//  a new empty collection (which must be sufficiently large for
//  the offered sequence).
//
//  Second, check if a new Snip block needs to be allocated or if
//  the current one can be used (If flush was required, a new Snip
//  is always needed).
//
#define HEAD_MIN_SIZE 512 // Minimum initial data allocation 
#define HEAD_MIN_FUZZ 32  // Don't bother to shrink if less than this free 

MSTextEditorTypes::Snip *MSTextEditorTypes::InsertContext::beginAppend(
									     int n_chars_, char *cset_, int bytes_per_char_)
{
  MSTextEditorTypes::Snip *t;
  unsigned long fontinfo = (unsigned long)fontSelectInfo(_mode);

  int n = n_chars_ * bytes_per_char_;
  if (_used+n > _size || (_head && _head->character_set != cset_) || 
      fontinfo != _fontinfo)
   {
     flushAppend();
     if (_size < n)
      {
	/* Need to reallocate larger head block */
	_head = derefDataHeader(_head);
	_size = n > HEAD_MIN_SIZE ? n : HEAD_MIN_SIZE;
	_head = (MSTextEditorTypes::SnipData *)
	calloc(1, OffsetOf(MSTextEditorTypes::SnipData, s[0]) + _size);
	_head->refs = 1; /* cx ref is counted ! */
      }    
     _fontinfo = fontinfo;
     _head->bytes = bytes_per_char_;
     _head->font = NULL;
     _head->character_set = cset_;
     t = _first = appendSnip();
     t->content.head = _head;
     t->data = &_head->s[_used];
     _head->refs += 1;
   }
  else 
  if ((t = _last) == NULL ||  t->content.head != _head ||
      t->space || t->endseq || t->brk || !IsSameMode(t->mode, _mode))
   {
     t = appendSnip();
     t->content.head = _head;
     t->data = &_head->s[_used];
     _head->refs += 1;
   }
  _used += n;
  t->valid = MSFalse;
  return t;
}

//
//    Release extra buffer space
//
void MSTextEditorTypes::InsertContext::flushAppend()
{
  SnipData *head, *h;
  int size = OffsetOf(SnipData, s[0]) + _used;
  Snip *s = _first;

  h = derefDataHeader(_head);
  if (s && h && _used + HEAD_MIN_FUZZ <  _size &&
      (head = (SnipData *)realloc((char *)h, size)) != h)
   {
     //
     // This branch should not be taken usually. It means
     // that realloc moved the block, even if we just
     // truncated the allocation! (This happens if you run purify,
     // also seems to occur with Apollo sr10.2 --msa)
     //
     for (size = head->refs; s; s = s->next)
     if (s->content.head == NULL || s->widget)
     continue;
     else if (s->content.head == h)
      {
	int i;
	i = (int)(s->data - &h->s[0]);
	s->data = &head->s[i];
	s->content.head = head;
	if (--size == 0)
	break;
      }
   }
  _first = NULL;
  _used = 0;
  _size = 0;
  _head = NULL;
}

//
//  Add a new Snip to the Block of the current context. 
//
MSTextEditorTypes::Snip *MSTextEditorTypes::InsertContext::appendSnip()
{
  Snip **where = _last ? &(_last->next) : _list;
  Snip *s = insertSnip(where);
  if (s != NULL) _last = s;
  s->mode = _mode;
  return s;
}

//
//  insert a new empty Snip before the Snip pointed by 'h'.
//  Return a pointer to the newly created Snip.
//
MSTextEditorTypes::Snip *MSTextEditorTypes::InsertContext::insertSnip(MSTextEditorTypes::Snip **list_)
{
  Snip *s = (Snip *)calloc(1, sizeof(Snip));
  if (!s) return NULL; 
  s->next = *list_;
  if (s->next) s->next->back = &s->next;
  *list_ = s;
  s->back = list_;
  s->content.head = NULL;
  s->data = NULL;
  return s;
}

//
// Start processing a new unit of character content. Returns a
// pointer to a new "context" block, and this context is implicitly
// set after this call.
//
MSTextEditorTypes::InsertContext *MSTextEditor::beginContent()
{
  InsertContext *cx = new InsertContext();

  if (cx == NULL) return NULL;
  cx->_editor = this;
  cx->_last   = NULL;
  cx->_list   = &_first;

  //
  // Delete previous internal structures, if any present
  //
  while (_first) Snip::Delete(&_first);
  cx->initGxMap();
  cx->_ccf = ccf_Open(cx, Gn_Feed, Cn_Feed, ESC_Feed, CSI_Feed,
		      Designate_G, Designate_C);
  if (_initial_state)
  cx->feedContent(_initial_state, strlen(_initial_state));
  if (_graphic_rendition)
   {
     cx->feedContent("\233", 1);
     cx->feedContent(_graphic_rendition, strlen(_graphic_rendition));
     cx->feedContent("\155", 1); // ASCII "m" 
   }
  cx->lock(MSTrue);
  //
  // Using the resource graphicRendition as initial state may not
  // be quite correct, and might be needed to be made an option --msa
  //
  return cx;
}

//
// AdjustLastPointer
//
//      Adjust last pointer backward to the first editable Snip
//      starting backwards from the given Snip (t).
//
void MSTextEditorTypes::InsertContext::adjustLastPointer(MSTextEditorTypes::Snip *r)
{
  if (r)
   {
     r->valid = MSFalse;
     do
      {
	if (r->back == _list) { r = NULL; break; }
	r = r->previous();
	r->valid = MSFalse;
      } while (!r->hasEditableContents());
   }
  _last = r;
}

//
// updateExposeArea
//      Extend expose area from the Snip
//
void MSTextEditorTypes::InsertContext::updateExposeArea(int x, int y, 
							   int width, int height)
{
  _expose = updateXRegion(_expose, x, y, width, height);
}

//
//  Recompute the rectangles covering the specified Text Region, and
//  update the area requiring refresh/expose.
//
void MSTextEditor::updateTextRegion(TextRegion *region, MSBoolean bounds)
{
  XRectangle rect[3];
  int nrect, i, ascent1, descent1, ascent2, descent2, ascent3, base, adj;
  int in_one_line;
  TextLocation dot[2];
  XPoint xy[2];
  long range[2];
  Snip *s;

  //if (_enable_display < 0 || region->mode <= NOHIGHLIGHT) return;
  nrect = 0;
  if (region->range[0] != region->range[1])
   {
     range[0] = region->range[0];
     range[1] = region->range[1];
     locations(range, 2, dot, xy);
     if (dot[0].snip == NULL || dot[1].snip == NULL)
     goto no_editable_content;
     in_one_line = MSFalse;
     for (s = dot[0].snip, ascent1=descent1=0, base = s->y; s; s = s->next)
      {
	if (!s->floating)
	 {
	   adj = base - s->y;
	   if (s->ascent + adj > ascent1)   ascent1 = s->ascent + adj;
	   if (s->descent - adj > descent1) descent1 = s->descent - adj;
	 }
	if (s == dot[1].snip) { in_one_line = MSTrue; break; }
	if (s->hasEndLine()) break;
      }
     for (s=dot[1].snip, ascent2=descent2=0, base = s->y; s; s=s->previous())
      {
	if (!s->floating)
	 {
	   adj = base - s->y;
	   if (s->ascent + adj > ascent2)   ascent2 = s->ascent + adj;
	   if (s->descent - adj > descent2) descent2 = s->descent - adj;
	 }
	if (s == dot[0].snip || s->back == &_first || s->hasEndLine())
	break;
      }
     for (s = dot[1].snip, ascent3 = 0, base = s->y; s; s = s->next)
      {
        if (!s->floating && s->ascent + base - s->y > ascent3)
	ascent3 = s->ascent + base - s->y;
        if (s->hasEndLine()) break;
      }
     rect[0].x = xy[0].x;
     rect[0].y = xy[0].y - ascent1;
     rect[0].height = ascent1 + descent1;
     if (in_one_line)
      {
	nrect = 1;
	if (xy[0].x < xy[1].x)
        rect[0].width = xy[1].x - xy[0].x;
	else if (xy[0].x > xy[1].x)
        rect[0].width = xy[0].x - xy[1].x;
	else
        nrect = 0;
      }
     else
      {
	if (pWidth() <= (int)rect[0].x)
        rect[0].width = 1;
	else
        rect[0].width = pWidth() - rect[0].x;
	rect[1].x = 0;
	rect[1].y = xy[1].y - ascent3;
	if (xy[1].x > 0)
        rect[1].width = xy[1].x;
	else
        rect[1].width = 1;
	rect[1].height = ascent3 + descent2;
	if (rect[1].height == 0)
        rect[1].height = 1;
	rect[2].x = 0;
	rect[2].y = rect[0].y + rect[0].height;
	rect[2].width = pWidth() > 0 ? pWidth() : 1;
	i = rect[1].y - rect[0].y - rect[0].height;
	if (i > 0)
	 {
	   rect[2].height = i;
	   nrect = 3;
	 }
	else
	 {
	   i = rect[1].y - rect[0].y;
	   rect[0].height = i > 0 ? i : 1;
	   nrect = 2;
	 }
      }
   }
 no_editable_content:
  if (1 || mapped())		// widget needs to be redrawn
   {
     Region r_old = XCreateRegion();
     Region r_new = XCreateRegion();
     Region r_xor = XCreateRegion();
     
     for (i = 0; i < region->nrect; ++i)
       XUnionRectWithRegion(&region->rect[i], r_old, r_old);
     for (i = 0; i < nrect; ++i)
      {
	XUnionRectWithRegion(&rect[i], r_new, r_new);
	region->rect[i] = rect[i];
      }
     region->nrect = nrect;
     XXorRegion(r_new, r_old, r_xor);
     if (_refresh)
       XUnionRegion(r_xor, _refresh, _refresh);
     XDestroyRegion(r_old);
     XDestroyRegion(r_new);
     XDestroyRegion(r_xor);
     if (bounds && nrect > 0)
      {
	XRectangle area;
	i = range[1] == region->range[1] ? 1 : 0;
	area.x = xy[i].x;
	area.y = xy[i].y;
	area.width = area.height = 1;
	if ((s = dot[i].snip) != NULL)
	 {
	   area.y -= s->ascent;
	   area.height += s->ascent + s->descent;
	 }
	checkBounds(&area);
      }
   }
  region->nrect = nrect;
}

//
//	Convert virtual offsets into TextLocations and matching
//	(x,y) coordinates.
//
//	NOTE:	The offsets will be rearranged to ascending order,
//		if they are not.
//
//	NOTE:	In the returned (snip, offset), it is always true
//		that "offset < snip->virtualLength()", except when the
//		converted offset was beyond the end of the file, this
//		is the only case when "offset == snip->virtualLength()".
//
void MSTextEditor::locations(
				long *v,		// Virtual offsets (points to convert) 
				int n,			// Number of points to convert 
				TextLocation *p,	// Return matching (Snip, offset) here 
				XPoint *xy)		// Return matching (x,y) here 
{
  int done = 0;      		// # of converted virtual offsets 
  long voffset = 0;    		// Cumulative Virtual offset 
  Snip *last_editable = NULL;  	// Last Editable Snip in content 
  long vlength = 0;    		// Virtual Length of the last snip 
  Snip *s;
  int x, y, i, j;

  //
  // First, ensure that the offsets are given in ascending
  // order. Rearrange them if this is not the case. Use brute
  // bubble sort. Never call this with more than 1-4 offsets!
  
  for (i = 0; i < n-1; ++i)
  for (j = i + 1; j < n; ++j)
  if (v[i] > v[j])
   {
     register long temp = v[j];

     v[j] = v[i];
     v[i] = temp;
   }
  //
  // Find the matching (Snip,Offset) and (x,y) information for
  // the each virtual offset.
  
  y = _y;
  x = _x;
  for (s = _first; done < n && s; s = s->next)
   {
     if (s->hasEditableContents())
      {
	last_editable = s;
	x = s->x + s->xWidth;
	y = s->y;
	voffset += (vlength = s->virtualLength());
	while (done < n && v[done] < voffset)
	 {
	   int l = v[done] - voffset + vlength;

	   p[done].snip = s;
	   p[done].offset = l;
	   coordinates(s, l, &xy[done]);
	   done += 1;
	 }
      }
   }
  //
  // The remaining un-done virtual offsets point past end of
  // content. Adjust them to point after the last editable
  // Snip.
  
  while (done < n)
   {
     v[done] = voffset;
     xy[done].x = x;
     xy[done].y = y;
     p[done].snip = last_editable;
     p[done].offset = vlength;
     done += 1;
   }
}

//
//	Return (x,y) coordinates matching the given (Snip, Offset)
//	This function uses some ad hoc rules in deciding the actual
//	position. The reasons for them are somewhat obscure, but they
//	have been arrived by trial and error and do give out least
//	surprises to the user...
//
//	If Snip == NULL, default to the first Snip in content, or
//	origin, if empty content.
//
//	* THIS WORKS ONLY IF THE CONTENT HAS BEEN LAID OUT *
//
void MSTextEditor::coordinates(MSTextEditorTypes::Snip *s, int offset, XPoint *xy)
{
  if (!s)
   {
     //
     // No Snip specified, default to position of the first
     // editable Snip, if any exists
     
     offset = 0;
     for (s = _first; ; s = s->next)
     if (s == NULL)
      {
        //
        // No Snip and empty content, default to
        // (text.x, text.y)
        xy->x = _x;
        xy->y = _y;
        return;  		// <<<<<<<<<<<< //* RETURN!! *** 
      }
     else if (s->hasEditableContents())
     break;
   }
  xy->x = s->x;
  xy->y = s->y;
  //
  // If this snip is a space and point would be in front of it,
  // then use the end of previous editable Snip instead, if that
  // snip exists and is not endseq. [This trickery is because
  // of the way adjustable spaces are treated in the layout
  // process: within line the extra space is added in *front* of
  // the space by adjusting the (x) of the space Snip, and soft
  // line breaks are added *after* the space and the spaces are
  // zero width elements before soft break.]
  
  if (offset == 0 && (s->space || (s->endseq && s->length == 0)))
   {
     if (&_first != s->back)
      {
	s = s->previous();
	if (!s->endseq && s->hasEditableContents())
	 {
	   xy->x = s->x + s->xWidth;
	   xy->y = s->y;
	 }
      }
   }
  else 
  if (offset < s->length) xy->x += snipWidth(s, s->data, offset);
  else 
  if (s->xWidth == 0 || offset > s->length)
   {
     //
     // If the point would be *after* visually empty
     // (zero width) element or point is after endseq
     // (offset > length), then use the position from the next
     // editable element. If there is no editable next
     // element, then use the end of widget for position or
     // last position from layout snips.
     Snip *r;
     if (s->hasEndLine())
      {
	xy->x = _x;
	xy->y += s->descent + s->ascent;
      }
     else xy->x += s->xWidth;

     for (r = s; (r = r->next) != NULL; )
      {
	xy->x = r->x;
	xy->y = r->y;
	if (r->hasEditableContents()) break;
      }
   }
  else
  xy->x += s->xWidth;
}

const int TABWIDTH = 96;

//
//  return the width of the text string.
//
int MSTextEditor::snipWidth(MSTextEditorTypes::Snip *snip_, char *str_, int len_)
{
  if (snip_->tab) {
    return (snip_->x + TABWIDTH) / TABWIDTH * TABWIDTH;
  }
  SnipData *h = snip_->content.head;

  if (h == NULL) return 0;
  if (h->font == NULL) snipExtents(snip_); 	// Force loading of Font 
  return h->bytes == 2 ? XTextWidth16(h->font, (XChar2b *)str_, len_) 
  : XTextWidth(h->font, str_, len_);
}




void MSTextEditor::checkBounds(XRectangle *)
{
}

int MSTextEditor::setupContent(int, int)
{
  return 1;
}

const unsigned int Cursor1W	   = 6;
const unsigned int Cursor1H	   = 4;
static char Cursor1Bits[] = {0x0c, 0x1e, 0x33}; 

MSTextEditorTypes::InsertCursor::InsertCursor(Display *dpy, Window window) 
{ 
  display = dpy;
  bits = Cursor1Bits;
  x = 0;
  y = 0;
  w = Cursor1W; 
  h = Cursor1H;
  pixmap = XCreateBitmapFromData(display, window, bits, w, h);
  valid = MSTrue;
  location.snip = NULL;
  location.offset = 0;
  goal = 0;
  position = 0L;
  draw_mode = Solid;
}

MSTextEditorTypes::InsertCursor::~InsertCursor() 
{ 
  XFreePixmap(display, pixmap);
}

void MSTextEditorTypes::InsertCursor::getBounds(XRectangle * rect)
{
  rect->width  = (unsigned short) w+1;
  rect->height = (unsigned short) h+1;
  rect->x      = x; 
  rect->y      = (draw_mode == Solid) ? y-2*h-0*Cursor1H  : y; 
}

void MSTextEditorTypes::InsertCursor::draw(GC gc, Drawable d, 
					      XPoint *origin, int lineSpacing, int width)
{
  int x1 = x-origin->x,		 y1 = y-origin->y + h;
  int x2 = x-origin->x + w/2,	 y2 = y-origin->y;
  int x3 = x-origin->x + w,	 y3 = y-origin->y + h;
  switch (draw_mode) {
  case UnderLine:
    ::XDrawLine(display, d, gc, x1, y1, x2, y2);
    ::XDrawLine(display, d, gc, x2, y2, x3, y3);
    break;
  case NoCursor:	// yes, no drawing
    break;
  case IBar:		// not yet implemented
    break;
  case Solid:
    w = width;
    h = lineSpacing;
    ::XFillRectangle(display, d, gc, 
		     x - origin->x, y - origin->y - 0*Cursor1H - 2*h , w, h);
    break;
  }
}

#include <stdio.h>

#define ExportFormatted(f) ((f)&1)
#define ExportOdif(f) ((f)&2)
#define ExportOdifFP(f) ((f)&4)

#define ESC 0x1b

#define PLD_Fe 0x4b  /* Fe for PLD in 7bit ESC Fe */
#define PLU_Fe 0x4c  /* Fe for PLU in 7bit ESC Fe */
#define PLD 0x8b  /* 8bit PLD (C1) */
#define PLU 0x8c  /* 8bit PLU (C1) */

typedef struct Rendition {
  unsigned int mask, value;
  int set;
} Rendition;

#define NO_VALUE (~0)

static Rendition renditions[] = {
  {Weight_MASK, Weight_BOLD, 1},
  {Weight_MASK, Weight_FAINT, 2},
  {Weight_MASK, Weight_NORMAL, 22},
  {Italicized_MASK, Italicized_MASK, 3},
  {Italicized_MASK, 0, 23},
  {Underline_MASK, Underline_SINGLE, 4},
  {Underline_MASK, Underline_DOUBLE, 21},
  {Underline_MASK, Underline_NONE, 24},
  {Blinking_MASK, Blinking_SLOWLY, 5},
  {Blinking_MASK, Blinking_RAPIDLY, 6},
  {Blinking_MASK, Blinking_STEADY, 25},
  {ImageInversion_MASK, ImageInversion_MASK, 7},
  {ImageInversion_MASK, 0, 27},
  {CrossedOut_MASK, CrossedOut_MASK, 9},
  {CrossedOut_MASK, 0, 29},
  {Framing_MASK, Framing_FRAMED, 51},
  {Framing_MASK, Framing_ENCIRCLED, 52},
  {Framing_MASK, Framing_NONE, 54},
  {Overlined_MASK, Overlined_MASK, 53},
  {Overlined_MASK, 0, 55},
  {Foreground_MASK, 0, 39},  /* Use default foreground */
  {Background_MASK, 0, 49},  /* Use default background */
  {Foreground_MASK, NO_VALUE, -1},
  {Background_MASK, NO_VALUE, -1},
  {Font_MASK, NO_VALUE, -1},
};

/*
** Change_ISO6429
**  Generate an ESC sequence to match the change in the state
**  defined by ISO 6429
**
**  NOTE:  The use of HEX character values is intentional.
**    These codes must not depend on C compiler characterset.
*/
static const char * change_ISO6429(MSTextEditorTypes::SnipMode *old, 
			     MSTextEditorTypes::SnipMode *new_mode)
{
  unsigned int change = old->bits ^ new_mode->bits;
  static char buf[3+NumberOf(renditions)*4+  /* Max use by SGR */
		  4+        /* Max use by PLD/PLU */
		  5+2*6+        /* Max use by GSM */
		  1];        /* NUL */
  Rendition *r = &renditions[0];
  char *s = buf, *mark;
  int delim;
  if (change == 0 && old->size_modification == new_mode->size_modification)
  return "";
  mark = s;
  *s++ = ESC;
  delim = 0x5b;  /* [   */
  for ( ;r < &renditions[NumberOf(renditions)]; r += 1)
  if (change & r->mask)
   {
     int par;

     if (r->value == (r->mask & new_mode->bits))
     par = r->set;
     else if (r->value != NO_VALUE)
     continue;
     else if (r->mask == Background_MASK)
     par = 39 + Background_COLOR(new_mode->bits);
     else if (r->mask == Foreground_MASK)
     par = 29 + Foreground_COLOR(new_mode->bits);
     else if (r->mask == Font_MASK)
     par = 10 + Font_NUMBER(new_mode->bits);
     else
     continue;
     sprintf(s, "%c%d", delim, par);
     s += strlen(s);
     delim = 0x3b;    /* ; */
     change &= ~r->mask;  /* This change processed */
   }
  if (s > mark+1) /* Anything really generated? */
  *s++ = 0x6d;  /* m  */
  else
  s = mark; /* Reset to empty buffer */
  if (change & Justify_MASK)
   {
     /* Generate JFY sequence (ISO 6429) */

     MSTextEditor::Alignment align = Justify_MODE(new_mode->bits);

     *s++ = ESC;
     *s++ = 0x5b;  /* [ */
     if (align == MSTextEditor::START)
     *s++ = 0x35;
     else if (align == MSTextEditor::CENTER)
     *s++ = 0x36;
     else if (align == MSTextEditor::END)
     *s++ = 0x37;
     else if (align == MSTextEditor::JUSTIFIED)
     *s++ = 0x32;  /* Word Space */
     *s++ = 0x20;
     *s++ = 0x46;
   }
  if (change & PartialLine_MASK)
   {
     /*
     ** Generate PLD/PLU with ESC Fe, not C1 control
     */
     int i = 0, Fe;

     if (old->bits & PartialLine_UP)
     i -= 1;
     else if (old->bits & PartialLine_DOWN)
     i += 1;
     if (new_mode->bits & PartialLine_UP)
     i += 1;
     else if (new_mode->bits & PartialLine_DOWN)
     i -= 1;
     if (i < 0)
      {
	Fe = PLD_Fe;
	i = -i;
      }
     else
     Fe = PLU_Fe;
     while (--i >= 0)
      {
	*s++ = ESC;
	*s++ = Fe;
      }
   }
  if (old->size_modification != new_mode->size_modification)
   {
     /*
     ** Graphic Size Modification (ISO 6429)
     ** (Only vertical size change supported)
     */
     sprintf(s,"\033[%d B", (int)new_mode->size_modification);
     s += strlen(s);
   }
  *s = '\0';
  return (s > buf) ? buf : "";
}

//
//  Save the current content of MSTextEditor into a file. The format
//  of the exported file is determined from by the 'exportFormat'
//  resource (_export_format). Returns MSTrue, if save succeeds
//  and MSFalse otherwise.
//
MSBoolean MSTextEditor::saveAsFile(const char *name)
{
  SnipMode init_mode;
  SnipMode *prev_mode = &init_mode;
  if (name == NULL) return MSFalse;
  FILE *fp=0;
  if ((fp = fopen(name, "wb")) == NULL) return MSFalse;
  for (Snip *s = _first; s; s = s->next) {
    const char *iso = change_ISO6429(prev_mode, &s->mode);
    fwrite(iso, 1, strlen(iso), fp);
    if (s->content.head && s->content.head->bytes * s->length > 0) 
    fwrite((char *)(&s->data[s->content.head->bytes * s->offset]), 1, 
	   s->content.head->bytes * s->length, fp);
    if (s->endseq) { fwrite("\n", 1, 1, fp); }
    prev_mode = &s->mode;
  }
  fclose(fp);
  return MSTrue;
}

MSBoolean MSTextEditor::saveInString(MSString &string_, MSBoolean textonly_)
{
  SnipMode init_mode;
  SnipMode *prev_mode = &init_mode;
  for (Snip *s = _first; s; s = s->next) {
    if (textonly_ == MSFalse) string_ << change_ISO6429(prev_mode, &s->mode);
    if (s->content.head && s->content.head->bytes * s->length > 0) {
      int len = s->content.head->bytes * s->length;
      char *buf = new char[len + 1];
      strncpy(buf, (char *)(&s->data[s->content.head->bytes * s->offset]), len);
      buf[len] = '\0';
      string_ << buf;
      delete [] buf;
    }
    if (s->endseq) string_ << "\n";
    prev_mode = &s->mode;
  }
  return MSTrue;
}

MSBoolean MSTextEditor::loadFromFile(const char *name)
{
  ifstream fin(name);
  if (!fin) return MSFalse;
  fin.unsetf(ios::skipws);
  freeze();
  if (!startEditing(CURRENT, 0)) return MSFalse;
  char s[BUFSIZ];
  fin.getline(s, BUFSIZ);
  while (fin && !fin.eof()) {
    _inserting->feedContent(s, strlen(s));
    _inserting->feedContent("\n", 1);
    fin.getline(s, BUFSIZ);
  }
  unfreeze();
  endEditing(0);
  fin.close();
  setOrigin();
  return MSTrue;
}

////////////////////////////////////////////////////////////////////
//
// handling selections (cut and paste)
//
////////////////////////////////////////////////////////////////////

typedef enum 	// used in modifySelection()
{ 
  SelectStart, SelectEnd, SelectExtendStart, SelectMove, SelectNone
} SelectType;

void MSTextEditor::unsetSelection() 
{ 
  modifySelection(0, SelectNone); 
}

void MSTextEditor::selectionStart(const XEvent *e)  
{
  modifySelection(e, SelectStart); 
}

void MSTextEditor::selectionAdjust(const XEvent *e) 
{
  modifySelection(e, SelectMove); 
}

void MSTextEditor::selectionExtendAdjust(const XEvent *e) 
{
  modifySelection(e, SelectMove); 
}

void MSTextEditor::selectionEnd(const XEvent *e)    
{
  modifySelection(e, SelectEnd); 
  if (_regions[0].range[0] != -1) {
    MSString selection = extractSelectedContent();
    if (selection.length() > 0) {
      XStoreBytes(display(), selection, selection.length());
    }
    if (_refresh) { XDestroyRegion(_refresh); _refresh = 0; }
  }
}

void MSTextEditor::selectionExtendStart(const XEvent *e)  
{
  modifySelection(e, SelectExtendStart); 
}

void MSTextEditor::insertSelection() 
{ 
  int n;
  char *buffer = XFetchBytes(display(), &n);
  if (n) {
    insert(buffer, n);
    XFree(buffer);
  }
}

void MSTextEditor::selectWord(const XEvent *)
{
  //_time = noteTime(e);
  startMoving();
  endMoving();
}

void MSTextEditor::selectAll(const XEvent *)
{
  //_time = noteTime(e);
  startMoving();
  _regions[0].range[0] = 0;
  _regions[0].range[1] = LONG_MAX;
  updateTextRegion(&_regions[0], MSFalse);
  endMoving();
}

void MSTextEditor::modifySelection(const XEvent *event, int mode)
{
  XPoint p;
  long position;
  long a, b;
  TextLocation dot;

  if (event) {
    //_time = noteTime(event);
    initRefreshRegion();
    notePosition((XEvent *)event, &p);
    dot.snip = NULL;
    dot.offset = 0;
    adjustSnipFirst(&dot);
    position = findPosition(&p, &dot);
    if (dot.snip && dot.snip->callback()) {
      if (mode == SelectStart) {
        dot.snip->activateCallback();
      }
      _regions[0].range[0] = _regions[0].range[1] = -1;
      return;
    }
  }
  switch (mode) {
  case SelectNone:
    _regions[0].range[0] = _regions[0].range[1] = 0;
    return;
  case SelectStart:
    _regions[0].range[0] = _regions[0].range[1] = position;
    break;
  case SelectExtendStart:
    a = position - _regions[0].range[0];
    b = position - _regions[0].range[1];
    if (labs(b) > labs(a))
      _regions[0].range[0] = _regions[0].range[1];
    _regions[0].range[1] = position;
    break;
  case SelectMove:
    _regions[0].range[1] = position;
    break;
  case SelectEnd:
    _regions[0].range[1] = position;
    if (_regions[0].range[0] == position)
     {
       if (position != _cursor->position ||
	   _cursor->location.snip != dot.snip)
        {
	  // ..because startMoving actually closes
	  // the insertion point, this is done only
	  // if position has really changed. This is
	  // a kludge that doesn't really work, because
	  // insert point gets closed in so many other
	  // ways too, even if cursor is not moved!!!
	  // Need to re-think the concept --msa 
	  startMoving();
	  _cursor->position = position;
	  _cursor->location = dot;
	  updateCursorPosition(MSTrue, MSTrue, MSTrue);
	  endMoving();
        }
     }
    break;
  default:
    break;
  }
  if (_regions[0].range[0] >= 0)
  updateTextRegion(&_regions[0], MSTrue);
  if (p.y > _origin.y + pHeight()) {
    _origin.y = p.y - pHeight();
    redraw();
  }
  if (p.y < _origin.y) {
    _origin.y = p.y;
    if (_origin.y < 0) _origin.y = 0;
    redraw();
  }
  doRefreshRegions();
}

/*
MSBoolean MSTextEditor::insideSelection(int x, int y)
{
  for (int n = 0; n < _num_regions; n++) {
    TextRegion *r = &_regions[n];
    for (int i = 0; i < r->nrect; i++)
    if (x >= r->rect[i].x && x <= r->rect[i].x + r->rect[i].width - 1 &&
	y >= r->rect[i].y && y <= r->rect[i].y + r->rect[i].height - 1)
    return MSTrue;
  }
  return MSFalse;
}
*/

void MSTextEditor::markSelection(Drawable d)
{
  int n, i;

  XSetForeground(display(), gc_Normal(), 1-background());

  for (n = _num_regions; --n >= 0;) {
    TextRegion *r = &_regions[n];
    int n;

    if (r->nrect > 0) {
      switch (r->mode) {
      case REVERSE:
	if (r->range[0] == -1) break;
        initRefreshRegion();
	for (n=0; n<r->nrect; n++) {
	  r->rect[n].x -= _origin.x;
	  r->rect[n].y -= _origin.y;
	  _refresh = updateXRegion(_refresh, r->rect[n].x, r->rect[n].y, 
				   r->rect[n].width, r->rect[n].height);
	}
        XFillRectangles(display(), d, gc_Normal(), r->rect, r->nrect);
	for (n=0; n<r->nrect; n++) {
	  r->rect[n].x += _origin.x;
	  r->rect[n].y += _origin.y;
	}
        break;
      case FRAME:
	if (r->range[0] == -1) break;
        //
        // Have to draw the rectangles individually
        // becuase the lines must be totally inside
        // the specified area (thus, assuming the
        // line width of 1, the following is result).
        // [change the code to cope with different
        // line widths later -- msa]
        //
        for (i = 0; i < r->nrect; ++i)
	XDrawRectangle(display(), d, gc_XOR(),
		       r->rect[i].x, r->rect[i].y,
		       r->rect[i].width - 1, r->rect[i].height - 1);
	break;
      default:
	break;
      }
    }
  }

  XSetForeground(display(), gc_Normal(), foreground());
}

////////////////////////////////////////////////////////////////////
//
// handling vertical and horizontal scrollbars
//
////////////////////////////////////////////////////////////////////


int MSTextEditor::pWidth()
{
  return width() - 2*offset() - vsb()->width();
}

int MSTextEditor::pHeight()
{
  return height() - 2*offset() - hsb()->height();
}

int MSTextEditor::offset()
{
  return 1;
}

MSTextEditorTypes::Vsb::Vsb(MSWidget *owner_) : MSVScrollBar(owner_)
{
  inc(20);  
  _highlightThickness=0;
  acceptFocus(MSFalse);
  width(DefaultScrollBarSize);
}

MSTextEditorTypes::Vsb::~Vsb(void) {}

void MSTextEditorTypes::Vsb::change(void)
{
  MSTextEditor *p=(MSTextEditor *)owner();
  p->scrollbarsUpdated();
}

void MSTextEditorTypes::Vsb::drag(void)
{ change(); }

MSTextEditorTypes::Hsb::Hsb(MSWidget *owner_) : MSHScrollBar(owner_)
{
  inc(5);  
  _highlightThickness=0;
  acceptFocus(MSFalse);
  height(DefaultScrollBarSize);
}

MSTextEditorTypes::Hsb::~Hsb(void) {}

void MSTextEditorTypes::Hsb::change(void)
{
  MSTextEditor *p=(MSTextEditor *)owner();
  p->scrollbarsUpdated();
}

void MSTextEditorTypes::Hsb::drag(void)
{ change(); }

MSTextEditor::Panner::Panner(MSWidget *owner_) : MSPrimitive(owner_)
{
  _shadowThickness=2;
  _highlightThickness=2;
  shadowStyle(MSSunken);
  //selectInput(ExposureMask|ButtonPressMask|ButtonMotionMask|ButtonReleaseMask);
}

MSTextEditor::Panner::~Panner(void) {}

void MSTextEditor::Panner::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     MSTextEditor *p=(MSTextEditor *)owner();
     XEvent er;
     while (XCheckWindowEvent(display(),window(),ExposureMask,&er)==MSTrue);
     p->redraw();
   }
}

void MSTextEditor::resetScrollbars(void)
{ 
  if (vsb()->value() != firstLine())   vsb()->valueChange(firstLine());
  if (hsb()->value() != firstColumn()) hsb()->valueChange(firstColumn());
}

void MSTextEditor::updateScrollbars(void)
{
  if (vsb()->width()>1)
   {
     int last_line_y = 0;
     Snip *s;
     for (s = _first; s && s->next; s = s->next) {
     }
     if (s) {
       last_line_y = s->y;
     }
     if (last_line_y < pHeight()) last_line_y = pHeight() - 1;

     if (_origin.y > last_line_y) {	// we're scrolled down too far
       _origin.y = last_line_y - pHeight() + 14;
       if (_origin.y < 0) _origin.y = 0;
     }

     vsb()->viewSize(pHeight());
     vsb()->max(last_line_y-1);
     vsb()->pageInc(pHeight()-vsb()->inc());
     vsb()->valueChange(_origin.y);
     if (vsb()->mapped()==MSFalse) vsb()->map();
   }
  else if (vsb()->mapped()==MSTrue) vsb()->unmap();

  if (hsb()->height()>1)
   {
     int maxx = pWidth()+1;
     for (Snip *s = _first; s; s = s->next)
     if (s->x > maxx) maxx = s->x;
     
     hsb()->viewSize(pWidth());
     hsb()->max(maxx-1);
     hsb()->pageInc(pWidth());
     hsb()->valueChange(_origin.x);

     if (hsb()->mapped()==MSFalse) hsb()->map();
   }
  else if (hsb()->mapped()==MSTrue) hsb()->unmap();
}

void MSTextEditor::scrollbarsUpdated(void)
{
  _origin.y = vsb()->value();
  _origin.x = hsb()->value();
  int rh=vsb()->inc();
  if(_cursor->y-rh<_origin.y || _cursor->y-_origin.y>vsb()->pageInc())
   {
     startMoving();
     XPoint p;
     p.x = _cursor->x;
     if(_cursor->y-rh<_origin.y) p.y = _origin.y+rh+5;
     else p.y = _origin.y+vsb()->pageInc()-rh;
     _cursor->location.snip=0;
     _cursor->location.offset=0;
     adjustSnipFirst(&_cursor->location);
     _cursor->position = findPosition(&p,&_cursor->location);
     _cursor->valid = MSTrue;
     updateCursorPosition(MSTrue, MSTrue, MSTrue);
     endMoving();
     drawCursor();
   }
   redrawImmediately();
}

int MSTextEditor::numLines(void)
{
  int n_lines = 0;
  for (Snip *s = _first; s && s->y < _origin.y; s = s->next)
  if (s->hasEndLine()) n_lines ++;
  return n_lines;
}

int MSTextEditor::computeMaxNumLines(void) { 
  return 0; 
}

int MSTextEditor::numColumns(void)
{
  return pWidth();
}

int MSTextEditor::computeMaxNumColumns(void)
{
  return pWidth() * 4; // wrong
}

int MSTextEditor::firstLine(void)
{
  return _origin.y;
}

int MSTextEditor::firstColumn(void)
{
  return _origin.x;
}

////////////////////////////////////////////////////////////////////
//
// background and foreground color setting API
//
////////////////////////////////////////////////////////////////////


void MSTextEditor::setColorBlack() 	{ insert(GRSetForegroundBlack, 6); }
void MSTextEditor::setColorRed() 	{ insert(GRSetForegroundRed, 6); }
void MSTextEditor::setColorGreen() 	{ insert(GRSetForegroundGreen, 5); }
void MSTextEditor::setColorYellow() 	{ insert(GRSetForegroundYellow, 5); }
void MSTextEditor::setColorBlue() 	{ insert(GRSetForegroundBlue, 5); }
void MSTextEditor::setColorMagenta() 	{ insert(GRSetForegroundMagenta, 5); }
void MSTextEditor::setColorCyan() 	{ insert(GRSetForegroundCyan, 5); }
void MSTextEditor::setColorWhite() 	{ insert(GRSetForegroundWhite, 5); }
void MSTextEditor::setColorDefault() 	{ insert(GRSetForegroundDefault, 5); }

void MSTextEditor::setBackColorBlack() 	{ insert(GRSetBackgroundBlack, 5); }
void MSTextEditor::setBackColorRed() 	{ insert(GRSetBackgroundRed, 5); }
void MSTextEditor::setBackColorGreen() 	{ insert(GRSetBackgroundGreen, 5); }
void MSTextEditor::setBackColorYellow() { insert(GRSetBackgroundYellow, 5); }
void MSTextEditor::setBackColorBlue() 	{ insert(GRSetBackgroundBlue, 5); }
void MSTextEditor::setBackColorMagenta(){ insert(GRSetBackgroundMagenta, 5); }
void MSTextEditor::setBackColorCyan() 	{ insert(GRSetBackgroundCyan, 5); }
void MSTextEditor::setBackColorWhite() 	{ insert(GRSetBackgroundWhite, 5); }
void MSTextEditor::setBackColorDefault(){ insert(GRSetBackgroundDefault, 5); }

void MSTextEditor::setFontDefault() 	{ insert(GRSetFontDefault, 5); }
void MSTextEditor::setFont1() 	{ insert(GRSetFont1, 5); }
void MSTextEditor::setFont2() 	{ insert(GRSetFont2, 5); }
void MSTextEditor::setFont3() 	{ insert(GRSetFont3, 5); }
void MSTextEditor::setFont4() 	{ insert(GRSetFont4, 5); }
void MSTextEditor::setFont5() 	{ insert(GRSetFont5, 5); }
void MSTextEditor::setFont6() 	{ insert(GRSetFont6, 5); }
void MSTextEditor::setFont7() 	{ insert(GRSetFont7, 5); }
void MSTextEditor::setFont8() 	{ insert(GRSetFont8, 5); }
void MSTextEditor::setFont9() 	{ insert(GRSetFont9, 5); }

////////////////////////////////////////////////////////////////////
//
// widget insertion
//
////////////////////////////////////////////////////////////////////

void MSTextEditor::insertWidget(MSWidget *widget_)
{ 
  if (readonly()) { XBell(display(), 0); return; }
  if (!startEditing(CURRENT, 0)) return;
  widget_->reparent(panner()); 
  widget_->show();
  Snip *s = _inserting->appendSnip();
  if (s) {
    s->widget = MSTrue;
    s->length = 0;
    s->data   = 0;
    s->endseq = MSTextEditor::End;
    s->content.widget = widget_;
    s->content.widget->moveTo(s->x - _origin.x, 
			      s->y - _origin.y - (s->ascent + s->descent));
    s->xWidth  = s->content.widget->width();
    s->ascent  = s->content.widget->height();
    s->descent = 0;
  }
  endEditing(0);
}

extern Bool double_buffering;

void MSTextEditor::deleteForwardChar()
{ deleteOrKill(POSITION, 1, TEXT_DELETE); }
void MSTextEditor::deleteBackwardChar() 
{ deleteOrKill(POSITION, -1, TEXT_DELETE); }
void MSTextEditor::deleteBackwardWord() 
{ deleteOrKill(WORD, -1, TEXT_DELETE); }
void MSTextEditor::deleteForwardWord() 
{ deleteOrKill(WORD, 1, TEXT_DELETE); }
void MSTextEditor::killBackwardWord() 
{ deleteOrKill(WORD, -1, TEXT_KILL); }
void MSTextEditor::killForwardWord() 
{ deleteOrKill(WORD, 1, TEXT_KILL); }
void MSTextEditor::killToEndOfLine() 
{ deleteOrKill(LINE_END, 1, TEXT_KILL); }

void MSTextEditor::deleteWholeContents() 
{ 
  freeze();
  if (_inserting) _inserting->endContent(0);
  while (_first) Snip::Delete(&_first);
  if (_inserting) _inserting->_last = 0;
//  moveBeginningOfFile();
   _cursor->location.snip = NULL;
  unfreeze();
}

void MSTextEditor::deleteCurrentSelection() { }
void MSTextEditor::killCurrentSelection() {}
void MSTextEditor::insertNewLineAndBackup() { }
void MSTextEditor::killToEndOfParagraph() { }

void MSTextEditor::moveBackwardParagraph(){moveCursor(PARAGRAPH,-1);}
void MSTextEditor::moveBackwardChar()    { moveCursor(POSITION,-1); }
void MSTextEditor::moveBackwardWord()    { moveCursor(WORD,-1); }
void MSTextEditor::moveBackwardPage()    { moveCursor(PAGE,-1); }
void MSTextEditor::moveBeginningOfFile() { moveCursor(ALL,-1); }
void MSTextEditor::moveEndOfFile()       { moveCursor(ALL,1); }
void MSTextEditor::moveForwardChar() 	 { moveCursor(POSITION,1); }
void MSTextEditor::moveForwardParagraph(){ moveCursor(PARAGRAPH,1); }
void MSTextEditor::moveForwardWord()     { moveCursor(WORD,1); }
void MSTextEditor::moveForwardPage()     { moveCursor(PAGE,1); }
void MSTextEditor::moveNextLine()        { moveCursor(LINE,1); }
void MSTextEditor::movePreviousLine()    { moveCursor(LINE,-1); } 
void MSTextEditor::moveToLineEnd()       { moveCursor(LINE_END,1); }
void MSTextEditor::moveToLineStart()     { moveCursor(LINE_BEGIN,1); }

void MSTextEditor::moveToXY(int x,int y) { moveCursor(x,y); }
void MSTextEditor::redrawDisplay() 	 { redraw(); }

void MSTextEditor::insert(const char *str_, long len_)
{
  if (readonly()) { XBell(display(), 0); return; }
  if (len_ <= 0) return;
  if (!startEditing(CURRENT, 0)) return;
  if (len_ > 0 && str_) _inserting->feedContent(str_, len_);
  endEditing(0);
}

void MSTextEditor::insert(const char *str_)
{
  insert(str_, strlen(str_));
}

void MSTextEditor::insert(const char *str_, 
			     MSTextEditorTypes::TextFlowCallback *callback_)
{
  if (readonly()) { XBell(display(), 0); return; }
  insert(str_, strlen(str_));
  _cursor->location.snip->callback(callback_);
  //FIXME:
  //The above function copies the callback. So to maintain
  //MStk standard we just delete this callback here to avoid memory
  //leak.
  delete callback_;
}


MSTextEditor& operator<<(MSTextEditor& editor_, const char *str_)
{
  editor_.insert(str_, strlen(str_));
  return editor_;
}

void MSTextEditor::insertNewLine()
{
  if (readonly()) { XBell(display(), 0); return; }
  if (!startEditing(CURRENT, 0)) return;
  _inserting->feedContent("\n", 1);
  endEditing(0);
  setOrigin();
}

long MSTextEditor::findPosition(int x, int y)
{
  XPoint p;
  p.x = x;
  p.y = y;
  TextLocation location;
  location.snip = NULL;
  location.offset = 0;
  adjustSnipFirst(&location);

  long position = findPosition(&p, &location);
  return position;
}

void MSTextEditor::moveCursor(int x, int y)
{
  startMoving();
  XRectangle rect;
  _cursor->getBounds(&rect); // to clear out old cursor mark 
  XUnionRectWithRegion(&rect, _refresh, _refresh);
  endMoving();

  startMoving();
  XPoint p;
  p.x = x;
  p.y = y - 4;
  _cursor->location.snip = NULL;
  _cursor->location.offset = 0;
  adjustSnipFirst(&_cursor->location);
  _cursor->position = findPosition(&p, &_cursor->location);
  _cursor->valid = MSTrue;
  updateCursorPosition(MSTrue, MSTrue, MSTrue);
  endMoving();
  drawCursor();
}

void MSTextEditor::moveCursor(MSTextEditor::TextUnit moving_type, int amount)
{
  if(moving_type==PAGE)
   {
    int v =_origin.y;
    int max=vsb()->max()-vsb()->viewSize();
    if((amount<0&&v>vsb()->inc())||(amount>0&&v<max-vsb()->inc()))
     {
       v+=amount*vsb()->pageInc();
       if(v<0) v=0;
       else if(v>max) v= max;
       vsb()->value(v);
     }
    else moveCursor(ALL,amount);
    return;
   }
  startMoving();
  XRectangle rect;
  _cursor->getBounds(&rect); // to clear out old cursor mark 
  XUnionRectWithRegion(&rect, _refresh, _refresh);
  endMoving();
  
  startMoving();
  long position = scanLocation(moving_type, amount, &_cursor->location);
  if (position < 0)
   {
     position = 0;
     _cursor->location.snip = NULL;
     _cursor->location.offset = 0;
     XBell(display(), 0);
   }
  _cursor->position = position;
  _cursor->valid = MSTrue;
  updateCursorPosition(MSBoolean(moving_type != LINE), MSTrue, MSTrue);
  endMoving();
  drawCursor();
}

void MSTextEditor::moveInsertionPoint(unsigned long location_)
{
  freeze();
  moveCursor(ALL,-1); 
  moveCursor(POSITION, location_); 
  unfreeze();

  moveForwardChar(); moveBackwardChar(); // hack for getting attributes right

  setOrigin();
}

MSTextEditorTypes::TextFlowCallback *MSTextEditor::NoCallback = 0;

MSTextEditorTypes::TextFlowCallback::TextFlowCallback(
							 MSTextEditorTypes::TextFlowCallback *cb_)
{
  if (cb_) {
    _function = cb_->_function;
    _name = cb_->_name;
    _owner = cb_->_owner;
  }
  else {
    _function = 0;
    _name = "";
    _owner = 0;
  }
}


MSTextEditorTypes::TextFlowCallback *
MSTextEditor::getInsertionPointCurrentCallback() const
{
  Snip *s = _cursor->location.snip;
  if (s) return s->callback();
  return NoCallback;
}

static char *GRSetForegroundTable[] = {
  "\033[39m", "\033[30m", "\033[31m", "\033[32m", "\033[33m",
  "\033[34m", "\033[35m", "\033[36m", "\033[37m",
};

const char * MSTextEditor::getInsertionPointCurrentForeground() const
{
  Snip *s = _cursor->location.snip;
  int fn = 0;
  if (s) fn = Foreground_COLOR(s->mode.bits);
  if (fn <0 || fn>9) fn = 0;
  return GRSetForegroundTable[ fn ];
}

static const char *GRSetBackgroundTable[] = {
  "\033[49m", "\033[40m", "\033[41m", "\033[42m", "\033[43m",
  "\033[44m", "\033[45m", "\033[46m", "\033[47m",
};

const char * MSTextEditor::getInsertionPointCurrentBackground() const
{
  Snip *s = _cursor->location.snip;
  int fn = 0;
  if (s) fn = Background_COLOR(s->mode.bits);
  if (fn <0 || fn>9) fn = 0;
  return GRSetBackgroundTable[ fn ];
}

static const char *GRSetFontTable[] = {
  MSTextEditor::GRSetFontDefault, 	MSTextEditor::GRSetFont1,
  MSTextEditor::GRSetFont2, 	MSTextEditor::GRSetFont3,
  MSTextEditor::GRSetFont4, 	MSTextEditor::GRSetFont5,
  MSTextEditor::GRSetFont6, 	MSTextEditor::GRSetFont7,
  MSTextEditor::GRSetFont8,	MSTextEditor::GRSetFont9,
};

const char * MSTextEditor::getInsertionPointCurrentFont() const
{
  Snip *s = _cursor->location.snip;
  int fn = 0;
  if (s) fn = Font_NUMBER(s->mode.bits);
  if (fn <0 || fn>9) fn = 0;
  return GRSetFontTable[ fn ];
}

void MSTextEditor::moveCursor(unsigned long location_)
{
  moveInsertionPoint(location_);
}

void MSTextEditor::startMoving()
{
  initRefreshRegion();
  if (_inserting)
   {
     Snip *s = _inserting->endContent(_refresh);
     _cursor->location.snip = s;
     _cursor->location.offset = s ? s->virtualLength() : 0;
     _cursor->valid = MSFalse;
     delete _inserting;   //striv ???
     _inserting = NULL;
   }
}

extern Region updateXRegion(Region region_, int x, int y, int w, int h);

void MSTextEditor::setOrigin()
{
  XPoint save_origin = _origin;

  if (_cursor->x - _origin.x < 0) { 		// cursor too far to the left?
    _origin.x = _cursor->x; 	
  }

  if (_cursor->x > _origin.x + pWidth()) {	// cursor too far to the right?
    _origin.x += _cursor->x - _origin.x - pWidth() + 10;
  }

  int dy = _line_spacing;
  if (dy < 28) dy =28;
  if (_cursor->y - _origin.y < dy) {			    // up?
    _origin.y = _cursor->y - dy;
    if (_origin.y < dy) _origin.y = 0;
  }

  if (_cursor->y > _origin.y + pHeight() - dy/2) { 	    // down?
    _origin.y += _cursor->y - (_origin.y + pHeight() - 4); 
  }
  
  if (_origin.x != save_origin.x || _origin.y != save_origin.y) {
    initRefreshRegion();
    updateCursorPosition(MSFalse, MSTrue, MSFalse);
    updateTextRegions(MSFalse);
    _refresh = updateXRegion(_refresh, _origin.x,_origin.y, pWidth(),pHeight());
    doRefreshRegions();
    resetScrollbars();
  }
}

void MSTextEditor::endMoving()
{
  _mult = 1;
  setOrigin();
  doRefreshRegions();
  resetScrollbars();
}

//
// FreeSnipsAndReturnLength
//  Release a snip chain and return virtual length of the
//  released section.
//
// *BUGS*
//  This function is now used to count the actual length of the
//  deleted section. But, because SnipMode changes are counted
//  as one position, this will give the wrong result when the
//  deleted section is different from the surrounding modes, but
//  which are equal. In this case the count will be off by 2. This
//  only affects range selection which shifts place if active. --msa
//
static int freeSnipsAndReturnLength(MSTextEditorTypes::Snip *s)
{
  int length = 0;
  while (s)
   {
     if (s->hasEditableContents()) length += s->virtualLength();
     MSTextEditorTypes::Snip::Delete(&s);
   }
  return length;
}

//
// deleteOrKillRange
//  Remove a number of virtual chacters from the content.
//  Returns the *actual* number deleted (negative or positive
//  depending on the original direction of delete).
//
int MSTextEditor::deleteOrKillRange(int amount, DeleteOrKill mode)
{
  if (readonly()) { XBell(display(), 0); return 0; }
  Snip *s;
//   TextLocation dot[2];

  if (mode == TEXT_KILL) {
    // SelectionStruct sel;
    // put in selection buffer....
  }
  s = deleteContent(_inserting, amount);
  int len = freeSnipsAndReturnLength(s);
  return amount > 0 ? len : -len;
}

void MSTextEditor::deleteOrKill(MSTextEditor::TextUnit move, int amount, 
				   DeleteOrKill mode)
{
  if (readonly()) { XBell(display(), 0); return; }
  if (!startEditing(CURRENT, 0)) return;
  amount *= _mult;
  TextLocation dot;
  long end = scanLocation(move, amount, &dot);

  // Special handling for KILL to END LINE
  if (move == LINE_END && end == _cursor->position) end++;
  amount = deleteOrKillRange(end - _cursor->position, mode);
  endEditing(amount);
  setOrigin();
}



//
//  "Close" a block of character content from further input. Return
//  the pointer to the last Snip of the context.
//
MSTextEditorTypes::Snip *MSTextEditorTypes::InsertContext::endContent(Region expose)
{
  Snip *last = _last;
  if (expose && _expose) XUnionRegion(_expose, expose, expose);
  flushAppend();
  if (last && !last->endseq && last->length == 0)
   {
     //
     // Avoid leaving empty snips, delete this. Empty Snip should be
     // restricted  to transient existance while insert context is open.
     //
     Snip **h = last->back;
     adjustLastPointer(last);
     last = _last;
     Snip::Delete(h);
   }
  //freeContext();
  return last;
}

//
//  Find a new location within content starting from current cursor location.
//
long MSTextEditor::scanLocation( TextUnit moving_type, int direction, 
				    TextLocation *dot)  // New Location after the scan
{
  Snip *s;
  MSTextEditorTextTag tag;
  XPoint xy;
  int n, prev_space;
  long position;
  int amount = direction;

  if (_inserting)
  _inserting->insertLocation(dot, (Region)0);
  else
  *dot = _cursor->location;
  adjustSnipFirst(dot);
  coordinates(dot->snip, dot->offset, &xy);
  if (!_cursor->valid)
  position = offset(dot);
  else
  position = _cursor->position;  
  if ((s = dot->snip) == NULL)
  return position; // No real content //
  adjustSnipBeginning(dot);
  s = dot->snip;
  switch (moving_type)
   {
   case POSITION:
     position -= dot->offset;
     amount += dot->offset;
     dot->offset = 0;
     //
     // Move backwards one whole Snip at time until the position
     // requested is passed.
     //
     while (amount < 0 && s->back != &_first)
      {
	s = s->previous();
	if (s->hasEditableContents())
	 {
	   n = s->virtualLength();
	   amount += n;
	   position -= n;
	   dot->snip = s;
	 }
      }
     //
     // Move forwards whole Snip at time until the position
     // requested is within current snip.
     //
     while (amount > 0 && s)
      {
	if (s->hasEditableContents() ||
	    s->hasEndLine())		// chris.
	 {
	   dot->snip = s;
	   dot->offset = n = s->virtualLength();
	   if ((amount -= n) <= 0)
	    {
	      dot->offset += amount;
	      position += dot->offset;
	      break;
	    }
	   position += n;
	 }
	s = s->next;
      }
     break;
   case LINE_END:
     //
     // Scan location to the end of line. End of line can either
     // be real or just result of the line breaking algorithm.
     //
     position -= dot->offset;
     dot->offset = 0;
     for (prev_space = 0; s ; s = s->next)
      {
	if (s->hasEditableContents())
	 {
	   n = s->virtualLength();
	   position += n;
	   dot->snip = s;
	   dot->offset = n;
	   prev_space = s->space;
	 }
	if (s->hasEndLine())
	 {
	   if (s->hasEditableContents() || prev_space)
	    {
	      // Back over the NL or SPACE //
	      position -= 1;
	      dot->offset -= 1;
	    }
	   break;
	 }
      }
     break;
   case LINE_BEGIN:
     position -= dot->offset;
     dot->offset = 0;
     while (s->back != &_first)
      {
	s = s->previous();
	if (s->hasEndLine())
        break;
	if (s->hasEditableContents())
	 {
	   position -= s->virtualLength();
	   dot->snip = s;
	 }
      }
     break;
   case ALL:
     position -= dot->offset;
     dot->offset = 0;
     if (amount < 0)
      {
	position = 0;
	dot->snip = NULL;
      }
     else while (s)
      {
	if (s->hasEditableContents())
	 {
	   n = s->virtualLength();
	   position += n;
	   dot->offset = n;
	   dot->snip = s;
	 }
	s = s->next;
      }
     break;
   case WORD:
#define IsWhiteSpace(s,o) ((s)->space || ((s)->endseq && (o) == (s)->length))
     if (amount >= 0)
      {
	do
	 {
	   //
	   // Skip white space
	   //
	   while (s)
	    {
	      if (!s->hasEditableContents())
	       {
		 s = s->next;
		 continue;
	       }
	      dot->snip = s;
	      if (!IsWhiteSpace(s, dot->offset))
	      break;
	      if (dot->offset == s->length)
	       {
		 position += s->hasEndPosition();
		 dot->offset = 0;
		 s = s->next;
	       }
	      else
	       {
		 position += s->length -
		 dot->offset;
		 dot->offset = s->length;
	       }
	    }
	   while (s)
	    {
	      if (!s->hasEditableContents())
	       {
		 s = s->next;
		 continue;
	       }
	      dot->snip = s;
	      if (IsWhiteSpace(s, dot->offset))
	      break;
	      if (dot->offset == s->length)
	       {
		 position += s->hasEndPosition();
		 dot->offset = 0;
		 s = s->next;
	       }
	      else
	       {
		 position += s->length
		 - dot->offset;
		 dot->offset = s->length;
	       }
	    }
	 } while (--amount > 0);
      }
     else if (amount < 0)
      {
	do
	 {
	   //
	   // Skip white space backwards
	   //
	   while (s)
	    {
	      if (dot->offset == 0 || s->space)
	       {
		 position -= dot->offset;
		 dot->offset = 0;
		 adjustSnipEnding(dot);
		 s = dot->snip;
		 continue;
	       }
	      if (s->endseq && s->length<dot->offset)
	       {
		 position -= 1;
		 dot->offset = s->length;
	       }
	      else
	      break;
	    }
	   //
	   // Skip non-white space backwards
	   //
	   while (s)
	    {
	      if (dot->offset == 0)
	       {
		 adjustSnipEnding(dot);
		 s = dot->snip;
		 continue;
	       }
	      if (s->space||(s->endseq &&
			     s->length<dot->offset))
	      break;
	      position -= dot->offset;
	      dot->offset = 0;
	    }
	 } while (++amount < 0);
      }
     //
     // Finetune the location (does not affect the virtual
     // offset).
     //
     if (direction > 0)
     adjustSnipEnding(dot);
     else
     adjustSnipBeginning(dot);
     break;
   case LINE:
     //
     // Pass over as many Snip_EndLine's as indicated by
     // amount.
     //
     position -= dot->offset;
     dot->offset = 0;
     if (amount > 0)
     do
      {
        if (s->hasEditableContents())
	 {
	   n = s->virtualLength();
	   position += n;
	   dot->snip = s;
	   dot->offset = n;
	 }
        if (s->hasEndLine())
	amount -= 1;
        s = s->next;
      }
     while (s && amount > 0);
     else if (amount < 0)
     while (s->back != &_first)
      {
        s = s->previous();
        if (s->hasEndLine())
	 {
	   amount += 1;
	   if (amount > 0)
            {
	      s = s->next;
	      break;
            }
	 }
        if (s->hasEditableContents())
	 {
	   n = s->virtualLength();
	   position -= n;
	   dot->snip = s;
	   dot->offset = 0;
	 }
      }
     xy.x = _cursor->goal;
     xy.y = 0;
     // ..following is brute force, check sometime! --msa //
     adjustSnipBeginning(dot);
     position += findPosition(&xy, dot);
     break;
   case TAG:
     position -= dot->offset;
     tag = s->mode.tag;
     if (amount > 0)
     do
     if (s->hasEditableContents())
      {
	if (s->mode.tag != tag)
	 {
	   tag = s->mode.tag;
	   if (--amount == 0)
	   break;
	 }
	n = s->virtualLength();
	position += n;
	dot->snip = s;
	dot->offset = n;
      }
     while ((s = s->next) != NULL);
     else if (amount < 0)
      {
	while (s->back != &_first)
	 {
	   s = s->previous();
	   if (s->hasEditableContents())
	    {
	      if (s->mode.tag != tag)
	       {
		 tag = s->mode.tag;
		 if (++amount == 0)
		 break;
	       }
	      n = s->virtualLength();
	      position -= n;
	      dot->snip = s;
	      dot->offset = 0;
	    }
	 }
      }
     break;
   case PAGE:
     if (_first && _first->ascent + _first->descent != 0)
      {
        int h=_first->ascent + _first->descent;
        int lines=pHeight()/h;
        if(pHeight()-lines*h<h*2/3) lines--;
        position =scanLocation(LINE,amount*lines,dot);
        return position;
      }
     else return 0;
   case PARAGRAPH:
     XBell(display(), 0);
     break;
   default:
     break;
   }
  return position;
}

//
//  Return virtual length of the content starting from the specified
//  TextLocation up to the position defined by goal.
//
//  NOTE 1)  The code assumes that the starting point is Editable!
//
//  Returns the virtual length.
//
long MSTextEditor::findPosition(XPoint *goal, TextLocation *dot)
{
  int x, y;
  Snip *r, *s;
  long position = 0;
  int end_line = 0;

  s = r = dot->snip;
  if (s == NULL) 
   {
     return 0;
   }
  position -= dot->offset;
  dot->offset = 0;
  //
  // At head of loop, the following are true:
  //
  // r    current Snip.
  //
  // position  virtual offset difference from the beginning of
  //    the search to the beginning the current Snip
  //    being examined (r).
  //
  // ..it should not be this cryptic! Something is real messy here!!
  // Look into below someday.. --msa 
  //
  while (r && !end_line)
   {
     x = r->x;
     y = r->y;
     if (y >= goal->y)
      {
	if (x >= goal->x)
	break;  // Before current snip (r) //
	else if (!r->floating && x + r->xWidth >= goal->x)
	 {
	   int n = 0;
	   if (!r->hasEditableContents())
	   break; // Non-Editable //
	   
	   while (++n <= r->length)
	   if (x + snipWidth(r, r->data, n) > goal->x) break;
	   //
	   // Point within editable Snip, easy out...
	   //
	   dot->snip = r;
	   dot->offset = n - 1;
	   position += dot->offset;
	   return position;
	 }
	else if (r->hasEndLine())
	end_line = 1; // Break after this Snip //
      }
     if (r->hasEditableContents())
      {
        position += r->virtualLength();
        s = r;
      }
     r = r->next;
   }
  //
  // If the loop exits, it means that requested position lies
  // between Snips, before the one indicated by 'r' (if non-NULL).
  // Locate the two possible points (next and previous editable Snips)
  // and select the one that is closer to the goal point horizontally!
  //
  if (r == s) {
    return position; // messy!! //
  }
  x = s->x - goal->x;
  if (!s->floating)
  x += (int)s->xWidth;
  if (x < 0)
  x = -x;
  for ( ;r; r = r->next)
  if (r->hasEditableContents())
   {
     y = r->x - goal->x;
     if (y < 0)
     y = -y;
     if (y < x)
      {
        dot->snip = r;
        dot->offset = 0;
        return position;
      }
     break;
   }
  // The snip pointed by 's' is chosen, do some final "adjustements" //
  dot->snip = s;
  dot->offset = s->length;
  if (s->hasEndPosition())
  position -= 1;
  else if ((s->xWidth == 0 || s->floating) && dot->offset > 0)
   {
     position -= 1;
     dot->offset -= 1;
   }
  return position;
}

//
//  adjusts the location to point the beginning of the equivalent
//  editable snip, if the current location is beyond the current Snip.
//  (Skip over empty snips, if any).
//
void MSTextEditor::adjustSnipBeginning(TextLocation *dot)
{
  Snip *s = dot->snip;
  if (s && dot->offset >= s->virtualLength())
   {
     while ((s = s->next) != NULL)
     if (s->hasEditableContents())
      {
	dot->snip = s;
	dot->offset = 0;
	if (s->virtualLength() > 0) break;
      }
   }
}

//
//  adjusts the location to point the end of the previous editable
//  snip, if the current location is at beginning of a Snip.
//  (Skip over empty snips, if any).
//
void MSTextEditor::adjustSnipEnding(TextLocation *dot)
{
  Snip *s = dot->snip;
  while (s && dot->offset == 0)
   {
     if (s->back == &_first)
      {
	dot->snip = NULL;
	break;
      }
     s = s->previous();
     if (s->hasEditableContents())
      {
	dot->snip = s;
	dot->offset = s->virtualLength();
      }
   }
}

//
//  Cut abs(amount) number of *characters* from the current insertion
//  point. If amount > 0, then the characters are cut forward from the
//  insertion point. If amount < 0, then the characters are cut backward
//  from the instert position.
//
//  The function returns a pointer to the cut content that has
//  been deleted from the main chain. This function does not release
//  the space!
//
MSTextEditorTypes::Snip * MSTextEditor::deleteContent(InsertContext *cx, long amount)
{
  Snip *r, *t;
  int vlength;
  long delete_offset;

  if (cx == NULL)
  return NULL;
  Snip **h = cx->_last ? &cx->_last->next : cx->_list;
  if (amount > 0)
   {
     //
     // Delete/Cut forward. This operation never affects the
     // cx->_last pointer or the snip pointed by it (other
     // than possibly changing the following Snip after it).
     //
     for (t = *h; ; t = t->next)
      {
	if (t == NULL)
	 {
	   // return all from *h to end, cut chain at h //
	   if ((t = *h) != NULL)
	    {
	      t->back = NULL;
	      *h = NULL;
	    }  
	   goto update_expose;
	 }
	if (t->hasEditableContents())
	 {
	   amount -= (vlength = t->virtualLength());
	   if (amount < 0)
	   break;
	 }
      }
     delete_offset = amount + vlength;
     if (delete_offset > 0 && delete_offset < vlength) {
       (void) Snip::Split(cx, t, delete_offset);
     }
     //
     // Return all from *h up to t, excluding t. t is always
     // non-NULL at this point.
     //
     r = *h;
     if (r == t) return NULL;
     *t->back = NULL;  // Terminate chain (r) //
     r->back = NULL;
     *h = t;      // Link chain (t) to h //
     t->back = h;
     //
     // Need to clear the valid flag of the next snip to MSFalse
     // so that the layout process knows from where to start
     // processing in partial layout. (Just deleting snips
     // does not necessarily leave any mark on the chain).
     //
     t->valid = MSFalse;  // Hint for layout process //
     t = r;
   }
  else if (amount < 0)
   {
     //
     // Delete/Cut backward. This operation will always affect
     // cx->_last.
     //
     if ((t = cx->_last) == NULL) return NULL; // No delete backwards from begin 
     if (t->next) t->next->valid = MSFalse; // Hint for layout process 
     for (;; t = t->previous())
      {
	if (t->hasEditableContents())
	 {
	   amount += (vlength = t->virtualLength());
	   if (amount >= 0) break;
	 }
	else if (t->back == cx->_list)
	 {
	   //
	   // Reached beginning of the chain. Return
	   // all from beginning up to *h.
	   //
	   cx->flushAppend();
	   if ((*t->back = *h) != NULL)
	   (*h)->back = t->back;
	   t->back = NULL;
	   *h = NULL;
	   cx->_last = NULL;
	   goto update_expose;
	 }
      }
     delete_offset = amount;
     //
     // The following is a special kludge to deal the deletion
     // of the last character of super/subscripts --msa
     //
     if (delete_offset > 0 && delete_offset == t->length && t->hasEndPosition() && !t->endseq)
     delete_offset -= 1;
     if (delete_offset > 0 && delete_offset < vlength)
     (void)Snip::Split(cx, t, delete_offset);
     // return all from t to *h //
     cx->flushAppend();
     cx->adjustLastPointer(t);
     if ((*t->back = *h) != NULL)
     (*h)->back = t->back;
     *h = NULL;
     t->back = NULL;
   }
  else
  t = NULL;
 update_expose:
  h = cx->_last ? &cx->_last->next : cx->_list;
  for (r = t; r; )
   {
     Snip *q;

     if (r->layout)
      {
        cx->updateExposeArea(r->x, r->y - r->ascent,
			     r->xWidth, r->ascent + r->descent);
	r->layout = r->valid = MSFalse;
      }
     //
     // First fledgling attempt to deal with Content_PROTECTED.
     // Cut of the protected Snips from 't' chain and re-attach
     // them back to the main chain.
     //
     q = r;
     r = r->next;
     if (q->hasProtectedContents())
      {
	if (q != t)
	 {
	   if ((*q->back = r) != NULL)
	   r->back = q->back;
	 }
	else
	 {
	   if ((t = r) != NULL)
	   r->back = NULL;
	 }
	//
	// Add q back to chain
	//
	if ((q->next = *h) != NULL)
        (*h)->back = &q->next;
	*h = q;
	q->back = h;
	h = &q->next;
      }
   }
  return t;
}


void MSTextEditor::tab()      { }
void MSTextEditor::shiftTab() { }
void MSTextEditor::escape()   { }

void MSTextEditor::selectLine()      { }
void MSTextEditor::selectAll()      { }
void MSTextEditor::selectWord()      { }

////////////////////////////////////////////////////////////////////
//
// handling mouse selections (cut and paste)
//
////////////////////////////////////////////////////////////////////

void MSTextEditor::buttonPress(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.subwindow != panner()->window()) return;

 if (traverseFocus(this) == MSTrue) {
    switch (pEvent_->xbutton.button) {
    case Button1:		
      selectionStart(pEvent_);	// start a new selection
      break;
    case Button2:
      insertSelection();		// paste current selection
      break;
    case Button3:		
      selectionExtendStart(pEvent_);	// extend current selection
      break;
    }
  }
  else {
  }
}

void MSTextEditor::motionNotify(const XEvent *pEvent_)
{
  XEvent aEvent;
  if (XCheckWindowEvent(display(),window(),ButtonMotionMask,&aEvent)==MSTrue)
  return;
  if (pEvent_->xmotion.state & Button1Mask)
  selectionAdjust(pEvent_);		// adjust current selection
  if (pEvent_->xmotion.state & Button3Mask)
  selectionExtendAdjust(pEvent_);	// extend current selection
}

void MSTextEditor::buttonRelease(const XEvent *pEvent_)
{
  switch (pEvent_->xbutton.button) {
  case Button1:	
  case Button3:	
    selectionEnd(pEvent_);		// finish current selection
    break;
  }
}

MSTextEditorTypes::CursorTimer::CursorTimer(MSTextEditor *editor_,unsigned long interval_)
    : MSIntervalTimer(interval_) 
{ 
  _editor=editor_; 
}

MSTextEditorTypes::CursorTimer::~CursorTimer(void) 
{
}

void MSTextEditorTypes::CursorTimer::process(void)
{ 
  _editor->processCursorTimer(); 
}

void MSTextEditor::processCursorTimer(void)
{ 
  drawCursor(); 
}

