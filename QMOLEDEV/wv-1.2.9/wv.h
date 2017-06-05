#ifndef MSWORDVIEW_HEADER
#define MSWORDVIEW_HEADER

#include <stdio.h>
#include <time.h>

#include <gsf/gsf-input.h>

#ifdef __cplusplus
extern "C" {
#endif

/* The structure below is used to refer to a wvStream.  Usually,
 * kind = GSF_STREAM,
 * but if we can't open a file using LibGSF, we fall back to the old file-based
 * routines, in which case kind == FILE_STREAM.
 */
    typedef enum {
	GSF_STREAM,
	FILE_STREAM,
	MEMORY_STREAM
    } wvStreamKind;

    typedef struct {
      char *mem;
      unsigned long current;
      unsigned long size;
    } MemoryStream;
    
    typedef union {
	FILE *file_stream;
	GsfInput *gsf_stream;
	MemoryStream *memory_stream;
    } wvInternalStream;

    typedef struct {
	wvStreamKind kind;
	wvInternalStream stream;
    } wvStream;


#ifndef PATH_MAX
#define PATH_MAX 1024		/*seems a reasonable figure */
#endif

/* these really should be worked out in the configure script to be 100% correct */
#ifndef U32
#define U32 unsigned int
#endif

#ifndef S32
#define S32 int
#endif

#ifndef U16
#define U16 unsigned short
#endif

#ifndef S16
#define S16 signed short
#endif

#ifndef U8
#define U8 unsigned char
#endif

#ifndef S8
#define S8 char
#endif


#define DEFAULTINDENT 1800
#define TWIRPS_PER_BQ 1440
#define TWIRPS_PER_H_PIXEL 20
#define TWIRPS_PER_V_PIXEL 20

#define SPACEPIXELS 6

#ifndef isletter
#define isletter(c) (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z'))
#endif

#ifndef digit
#define digit(c)  ((c) >= '0' && (c) <= '9')
#endif

#define legal_variable_char(c)  (isletter (c) || digit (c) || c == '_')

#ifndef HAVE_WMF
    typedef struct tagRECT {
	S32 left;
	S32 right;
	S32 top;
	S32 bottom;
    } RECT;

    typedef struct tagPOINT {
	S32 x;
	S32 y;
    } POINT;
#else
#	include /**/ <wmfapi.h>
#endif



/*Wine Portions Begin*/

    typedef struct tagPANOSE {
	U8 bFamilyType;
	U8 bSerifStyle;
	U8 bWeight;
	U8 bProportion;
	U8 bContrast;
	U8 bStrokeVariation;
	U8 bArmStyle;
	U8 bLetterform;
	U8 bMidline;
	U8 bXHeight;
    } PANOSE;

    void wvGetPANOSE (PANOSE * panose, wvStream * fd);
    void wvInitPANOSE (PANOSE * item);


/*
 * The FONTSIGNATURE tells which Unicode ranges and which code pages
 * have glyphs in a font.
 *
 * fsUsb  128-bit bitmap. The most significant bits are 10 (magic number).
 *        The remaining 126 bits map the Unicode ISO 10646 subranges
 *        for which the font provides glyphs.
 *
 * fsCsb  64-bit bitmap. The low 32 bits map the Windows codepages for
 *        which the font provides glyphs. The high 32 bits are for
 *        non Windows codepages.
 */
    typedef struct {
	U32 fsUsb[4];
	U32 fsCsb[2];
    } FONTSIGNATURE;

    void wvGetFONTSIGNATURE (FONTSIGNATURE * fs, wvStream * fd);
    void wvInitFONTSIGNATURE (FONTSIGNATURE * fs);


#ifndef _FILETIME_
#define _FILETIME_
/* 64 bit number of 100 nanoseconds intervals since January 1, 1601 */
    typedef struct {
	U32 dwLowDateTime;
	U32 dwHighDateTime;
    } FILETIME;
#endif				/* _FILETIME_ */

    void wvGetFILETIME (FILETIME * ft, wvStream * fd);
    void wvInitFILETIME (FILETIME * ft);

    time_t wvDOSFS_FileTimeToUnixTime (const FILETIME * filetime,
				       U32 * remainder);
    int wvFileTimeToDosDateTime (const FILETIME * ft, U16 * fatdate,
				 U16 * fattime);
/*Wine Portions End*/

    char *wvFmtMsg (char *fmt, ...);

/** beginning of clean interface **/
    void wvRealError (char *file, int line, char *msg);
    void wvRealTrace (char *file, int line, char *msg);
#define wvError( args ) wvRealError(__FILE__,__LINE__, wvFmtMsg args )
    void wvWarning (char *fmt, ...);

void _wvFree (void *ptr);
#define wvFree(P) do { if (P) { _wvFree((void *)(P)); (P)=NULL; } } while (0)

    char *wvWideStrToMB (U16 * str);
    char *wvWideCharToMB (U16 char16);

    typedef enum {
	WORD1 = 0x0000,
	WORD2 = 0x0001,
	WORD3 = 0x0002,
	WORD4 = 0x0003,
	WORD5 = 0x0004,
	WORD6 = 0x0005,
	WORD7 = 0x0006,
	WORD8 = 0x0007
    } wvVersion;

    typedef enum {
	Dmain,
	Dfootnote,
	Dheader,
	Dannotation,
	Dendnote,
	Dtextbox,
	Dheader_textbox
    } subdocument;



    typedef struct _FIB {
	U16 wIdent;		/* 0x0000 */
	U16 nFib;		/* 0x0002 */
	U16 nProduct;		/* 0x0004 */
	U16 lid;		/* 0x0006 */
	S16 pnNext;		/* 0x0008 */

	U32 fDot:1;		/* Bitfield 0x0001 0x000A */
	U32 fGlsy:1;		/* Bitfield 0x0002 */
	U32 fComplex:1;		/* Bitfield 0x0004 */
	U32 fHasPic:1;		/* Bitfield 0x0008 */
	U32 cQuickSaves:4;	/* Bitfield 0x00F0 */
	U32 fEncrypted:1;	/* Bitfield 0x0100 */
	U32 fWhichTblStm:1;	/* Bitfield 0x0200 */
	U32 fReadOnlyRecommended:1;	/* Bitfield 0x0400 */
	U32 fWriteReservation:1;	/* Bitfield 0x0800 */
	U32 fExtChar:1;		/* Bitfield 0x1000 */
	U32 fLoadOverride:1;	/* Bitfield 0x2000 */
	U32 fFarEast:1;		/* Bitfield 0x4000 */
	U32 fCrypto:1;		/* Bitfield 0x8000 */
	U32 nFibBack:16;	/* 0x000C */
	U32 lKey;		/* 0x000E */
	U32 envr:8;		/* 0x0012 */
	U32 fMac:1;		/* Bitfield 0x01 0x0013 */
	U32 fEmptySpecial:1;	/* Bitfield 0x02 */
	U32 fLoadOverridePage:1;	/* Bitfield 0x04 */
	U32 fFutureSavedUndo:1;	/* Bitfield 0x08 */
	U32 fWord97Saved:1;	/* Bitfield 0x10 */
	U32 fSpare0:3;		/* Bitfield 0xFE */
	U32 chse:16;		/* 0x0014 *//*was chs */
	U16 chsTables;		/* 0x0016 */
	U32 fcMin;		/* 0x0018 */
	U32 fcMac;		/* 0x001C */
	U16 csw;		/* 0x0020 */
	U16 wMagicCreated;	/* 0x0022 */
	U16 wMagicRevised;	/* 0x0024 */
	U16 wMagicCreatedPrivate;	/* 0x0026 */
	U16 wMagicRevisedPrivate;	/* 0x0028 */
	S16 pnFbpChpFirst_W6;	/* 0x002A */
	S16 pnChpFirst_W6;	/* 0x002C */
	S16 cpnBteChp_W6;	/* 0x002E */
	S16 pnFbpPapFirst_W6;	/* 0x0030 */
	S16 pnPapFirst_W6;	/* 0x0032 */
	S16 cpnBtePap_W6;	/* 0x0034 */
	S16 pnFbpLvcFirst_W6;	/* 0x0036 */
	S16 pnLvcFirst_W6;	/* 0x0038 */
	S16 cpnBteLvc_W6;	/* 0x003A */
	S16 lidFE;		/* 0x003C */
	U16 clw;		/* 0x003E */
	S32 cbMac;		/* 0x0040 */
	U32 lProductCreated;	/* 0x0044 */
	U32 lProductRevised;	/* 0x0048 */
	U32 ccpText;		/* 0x004C */
	S32 ccpFtn;		/* 0x0050 */
	S32 ccpHdr;		/* 0x0054 */
	S32 ccpMcr;		/* 0x0058 */
	S32 ccpAtn;		/* 0x005C */
	S32 ccpEdn;		/* 0x0060 */
	S32 ccpTxbx;		/* 0x0064 */
	S32 ccpHdrTxbx;		/* 0x0068 */
	S32 pnFbpChpFirst;	/* 0x006C */
	S32 pnChpFirst;		/* 0x0070 */
	S32 cpnBteChp;		/* 0x0074 */
	S32 pnFbpPapFirst;	/* 0x0078 */
	S32 pnPapFirst;		/* 0x007C */
	S32 cpnBtePap;		/* 0x0080 */
	S32 pnFbpLvcFirst;	/* 0x0084 */
	S32 pnLvcFirst;		/* 0x0088 */
	S32 cpnBteLvc;		/* 0x008C */
	S32 fcIslandFirst;	/* 0x0090 */
	S32 fcIslandLim;	/* 0x0094 */
	U16 cfclcb;		/* 0x0098 */
	S32 fcStshfOrig;	/* 0x009A */
	U32 lcbStshfOrig;	/* 0x009E */
	S32 fcStshf;		/* 0x00A2 */
	U32 lcbStshf;		/* 0x00A6 */
	S32 fcPlcffndRef;	/* 0x00AA */
	U32 lcbPlcffndRef;	/* 0x00AE */
	S32 fcPlcffndTxt;	/* 0x00B2 */
	U32 lcbPlcffndTxt;	/* 0x00B6 */
	S32 fcPlcfandRef;	/* 0x00BA */
	U32 lcbPlcfandRef;	/* 0x00BE */
	S32 fcPlcfandTxt;	/* 0x00C2 */
	U32 lcbPlcfandTxt;	/* 0x00C6 */
	S32 fcPlcfsed;		/* 0x00CA */
	U32 lcbPlcfsed;		/* 0x00CE */
	S32 fcPlcpad;		/* 0x00D2 */
	U32 lcbPlcpad;		/* 0x00D6 */
	S32 fcPlcfphe;		/* 0x00DA */
	U32 lcbPlcfphe;		/* 0x00DE */
	S32 fcSttbfglsy;	/* 0x00E2 */
	U32 lcbSttbfglsy;	/* 0x00E6 */
	S32 fcPlcfglsy;		/* 0x00EA */
	U32 lcbPlcfglsy;	/* 0x00EE */
	S32 fcPlcfhdd;		/* 0x00F2 */
	U32 lcbPlcfhdd;		/* 0x00F6 */
	S32 fcPlcfbteChpx;	/* 0x00FA */
	U32 lcbPlcfbteChpx;	/* 0x00FE */
	S32 fcPlcfbtePapx;	/* 0x0102 */
	U32 lcbPlcfbtePapx;	/* 0x0106 */
	S32 fcPlcfsea;		/* 0x010A */
	U32 lcbPlcfsea;		/* 0x010E */
	S32 fcSttbfffn;		/* 0x0112 */
	U32 lcbSttbfffn;	/* 0x0116 */
	S32 fcPlcffldMom;	/* 0x011A */
	U32 lcbPlcffldMom;	/* 0x011E */
	S32 fcPlcffldHdr;	/* 0x0122 */
	U32 lcbPlcffldHdr;	/* 0x0126 */
	S32 fcPlcffldFtn;	/* 0x012A */
	U32 lcbPlcffldFtn;	/* 0x012E */
	S32 fcPlcffldAtn;	/* 0x0132 */
	U32 lcbPlcffldAtn;	/* 0x0136 */
	S32 fcPlcffldMcr;	/* 0x013A */
	U32 lcbPlcffldMcr;	/* 0x013E */
	S32 fcSttbfbkmk;	/* 0x0142 */
	U32 lcbSttbfbkmk;	/* 0x0146 */
	S32 fcPlcfbkf;		/* 0x014A */
	U32 lcbPlcfbkf;		/* 0x014E */
	S32 fcPlcfbkl;		/* 0x0152 */
	U32 lcbPlcfbkl;		/* 0x0156 */
	S32 fcCmds;		/* 0x015A */
	U32 lcbCmds;		/* 0x015E */
	S32 fcPlcmcr;		/* 0x0162 */
	U32 lcbPlcmcr;		/* 0x0166 */
	S32 fcSttbfmcr;		/* 0x016A */
	U32 lcbSttbfmcr;	/* 0x016E */
	S32 fcPrDrvr;		/* 0x0172 */
	U32 lcbPrDrvr;		/* 0x0176 */
	S32 fcPrEnvPort;	/* 0x017A */
	U32 lcbPrEnvPort;	/* 0x017E */
	S32 fcPrEnvLand;	/* 0x0182 */
	U32 lcbPrEnvLand;	/* 0x0186 */
	S32 fcWss;		/* 0x018A */
	U32 lcbWss;		/* 0x018E */
	S32 fcDop;		/* 0x0192 */
	U32 lcbDop;		/* 0x0196 */
	S32 fcSttbfAssoc;	/* 0x019A */
	U32 lcbSttbfAssoc;	/* 0x019E */
	S32 fcClx;		/* 0x01A2 */
	U32 lcbClx;		/* 0x01A6 */
	S32 fcPlcfpgdFtn;	/* 0x01AA */
	U32 lcbPlcfpgdFtn;	/* 0x01AE */
	S32 fcAutosaveSource;	/* 0x01B2 */
	U32 lcbAutosaveSource;	/* 0x01B6 */
	S32 fcGrpXstAtnOwners;	/* 0x01BA */
	U32 lcbGrpXstAtnOwners;	/* 0x01BE */
	S32 fcSttbfAtnbkmk;	/* 0x01C2 */
	U32 lcbSttbfAtnbkmk;	/* 0x01C6 */
	S32 fcPlcdoaMom;	/* 0x01CA */
	U32 lcbPlcdoaMom;	/* 0x01CE */
	S32 fcPlcdoaHdr;	/* 0x01D2 */
	U32 lcbPlcdoaHdr;	/* 0x01D6 */
	S32 fcPlcspaMom;	/* 0x01DA */
	U32 lcbPlcspaMom;	/* 0x01DE */
	S32 fcPlcspaHdr;	/* 0x01E2 */
	U32 lcbPlcspaHdr;	/* 0x01E6 */
	S32 fcPlcfAtnbkf;	/* 0x01EA */
	U32 lcbPlcfAtnbkf;	/* 0x01EE */
	S32 fcPlcfAtnbkl;	/* 0x01F2 */
	U32 lcbPlcfAtnbkl;	/* 0x01F6 */
	S32 fcPms;		/* 0x01FA */
	U32 lcbPms;		/* 0x01FE */
	S32 fcFormFldSttbs;	/* 0x0202 */
	U32 lcbFormFldSttbs;	/* 0x0206 */
	S32 fcPlcfendRef;	/* 0x020A */
	U32 lcbPlcfendRef;	/* 0x020E */
	S32 fcPlcfendTxt;	/* 0x0212 */
	U32 lcbPlcfendTxt;	/* 0x0216 */
	S32 fcPlcffldEdn;	/* 0x021A */
	U32 lcbPlcffldEdn;	/* 0x021E */
	S32 fcPlcfpgdEdn;	/* 0x0222 */
	U32 lcbPlcfpgdEdn;	/* 0x0226 */
	S32 fcDggInfo;		/* 0x022A */
	U32 lcbDggInfo;		/* 0x022E */
	S32 fcSttbfRMark;	/* 0x0232 */
	U32 lcbSttbfRMark;	/* 0x0236 */
	S32 fcSttbCaption;	/* 0x023A */
	U32 lcbSttbCaption;	/* 0x023E */
	S32 fcSttbAutoCaption;	/* 0x0242 */
	U32 lcbSttbAutoCaption;	/* 0x0246 */
	S32 fcPlcfwkb;		/* 0x024A */
	U32 lcbPlcfwkb;		/* 0x024E */
	S32 fcPlcfspl;		/* 0x0252 */
	U32 lcbPlcfspl;		/* 0x0256 */
	S32 fcPlcftxbxTxt;	/* 0x025A */
	U32 lcbPlcftxbxTxt;	/* 0x025E */
	S32 fcPlcffldTxbx;	/* 0x0262 */
	U32 lcbPlcffldTxbx;	/* 0x0266 */
	S32 fcPlcfhdrtxbxTxt;	/* 0x026A */
	U32 lcbPlcfhdrtxbxTxt;	/* 0x026E */
	S32 fcPlcffldHdrTxbx;	/* 0x0272 */
	U32 lcbPlcffldHdrTxbx;	/* 0x0276 */
	S32 fcStwUser;		/* 0x027A */
	U32 lcbStwUser;		/* 0x027E */
	S32 fcSttbttmbd;	/* 0x0282 */
	U32 cbSttbttmbd;	/* 0x0286 */
	S32 fcUnused;		/* 0x028A */
	U32 lcbUnused;		/* 0x028E */
	S32 fcPgdMother;	/* 0x0292 */
	U32 lcbPgdMother;	/* 0x0296 */
	S32 fcBkdMother;	/* 0x029A */
	U32 lcbBkdMother;	/* 0x029E */
	S32 fcPgdFtn;		/* 0x02A2 */
	U32 lcbPgdFtn;		/* 0x02A6 */
	S32 fcBkdFtn;		/* 0x02AA */
	U32 lcbBkdFtn;		/* 0x02AE */
	S32 fcPgdEdn;		/* 0x02B2 */
	U32 lcbPgdEdn;		/* 0x02B6 */
	S32 fcBkdEdn;		/* 0x02BA */
	U32 lcbBkdEdn;		/* 0x02BE */
	S32 fcSttbfIntlFld;	/* 0x02C2 */
	U32 lcbSttbfIntlFld;	/* 0x02C6 */
	S32 fcRouteSlip;	/* 0x02CA */
	U32 lcbRouteSlip;	/* 0x02CE */
	S32 fcSttbSavedBy;	/* 0x02D2 */
	U32 lcbSttbSavedBy;	/* 0x02D6 */
	S32 fcSttbFnm;		/* 0x02DA */
	U32 lcbSttbFnm;		/* 0x02DE */
	S32 fcPlcfLst;		/* 0x02E2 */
	U32 lcbPlcfLst;		/* 0x02E6 */
	S32 fcPlfLfo;		/* 0x02EA */
	U32 lcbPlfLfo;		/* 0x02EE */
	S32 fcPlcftxbxBkd;	/* 0x02F2 */
	U32 lcbPlcftxbxBkd;	/* 0x02F6 */
	S32 fcPlcftxbxHdrBkd;	/* 0x02FA */
	U32 lcbPlcftxbxHdrBkd;	/* 0x02FE */
	S32 fcDocUndo;		/* 0x0302 */
	U32 lcbDocUndo;		/* 0x0306 */
	S32 fcRgbuse;		/* 0x030A */
	U32 lcbRgbuse;		/* 0x030E */
	S32 fcUsp;		/* 0x0312 */
	U32 lcbUsp;		/* 0x0316 */
	S32 fcUskf;		/* 0x031A */
	U32 lcbUskf;		/* 0x031E */
	S32 fcPlcupcRgbuse;	/* 0x0322 */
	U32 lcbPlcupcRgbuse;	/* 0x0326 */
	S32 fcPlcupcUsp;	/* 0x032A */
	U32 lcbPlcupcUsp;	/* 0x032E */
	S32 fcSttbGlsyStyle;	/* 0x0332 */
	U32 lcbSttbGlsyStyle;	/* 0x0336 */
	S32 fcPlgosl;		/* 0x033A */
	U32 lcbPlgosl;		/* 0x033E */
	S32 fcPlcocx;		/* 0x0342 */
	U32 lcbPlcocx;		/* 0x0346 */
	S32 fcPlcfbteLvc;	/* 0x034A */
	U32 lcbPlcfbteLvc;	/* 0x034E */
	FILETIME ftModified;	/* 0x0352 */
	S32 fcPlcflvc;		/* 0x035A */
	U32 lcbPlcflvc;		/* 0x035E */
	S32 fcPlcasumy;		/* 0x0362 */
	U32 lcbPlcasumy;	/* 0x0366 */
	S32 fcPlcfgram;		/* 0x036A */
	U32 lcbPlcfgram;	/* 0x036E */
	S32 fcSttbListNames;	/* 0x0372 */
	U32 lcbSttbListNames;	/* 0x0376 */
	S32 fcSttbfUssr;	/* 0x037A */
	U32 lcbSttbfUssr;	/* 0x037E */

	/* Added for Word 2 */

	U32 Spare;		/* 0x000E */
	U16 rgwSpare0[3];	/* 0x0012 */
	U32 fcSpare0;		/* 0x0024 */
	U32 fcSpare1;		/* 0x0028 */
	U32 fcSpare2;		/* 0x002C */
	U32 fcSpare3;		/* 0x0030 */
	U32 ccpSpare0;		/* 0x0048 */
	U32 ccpSpare1;		/* 0x004C */
	U32 ccpSpare2;		/* 0x0050 */
	U32 ccpSpare3;		/* 0x0054 */

	U32 fcPlcfpgd;		/* 0x0082 */
	U16 cbPlcfpgd;		/* 0x0086 */

	U32 fcSpare5;		/* 0x0130 */
	U16 cbSpare5;		/* 0x0136 */
	U32 fcSpare6;		/* 0x0130 */
	U16 cbSpare6;		/* 0x0136 */
	U16 wSpare4;		/* 0x013C */

    } FIB;

    void wvGetFIB (FIB * item, wvStream * fd);
    void wvGetFIB2 (FIB * item, wvStream * fd);
    void wvGetFIB6 (FIB * item, wvStream * fd);
    void wvInitFIB (FIB * item);


    int wvGetEmpty_PLCF (U32 ** cp, U32 * nocps, U32 offset, U32 len,
			 wvStream * fd);

    typedef struct _FRD {
	S16 frd;
    } FRD;

    void wvGetFRD (FRD * item, wvStream * fd);
    int wvGetFRD_PLCF (FRD ** frd, U32 ** pos, U32 * nofrd, U32 offset, U32 len,
		       wvStream * fd);

    typedef U16 XCHAR;

    typedef struct _ATRD {
	XCHAR xstUsrInitl[10];
	S16 ibst;
	U16 ak;			/*unused */
	U16 grfbmc;		/*unused */
	S32 lTagBkmk;
    } ATRD;

    void wvGetATRD (ATRD * item, wvStream * fd);
    int wvGetATRD_PLCF (ATRD ** atrd, U32 ** pos, U32 * noatrd, U32 offset,
			U32 len, wvStream * fd);

    typedef struct _SED {
	S16 fn;
	U32 fcSepx;
	S16 fnMpr;
	U32 fcMpr;
    } SED;

    void wvGetSED (SED * item, wvStream * fd);
    int wvGetSED_PLCF (SED ** item, U32 ** pos, U32 * noitem, U32 offset,
		       U32 len, wvStream * fd);

    typedef struct _FFN {
	U32 cbFfnM1:8;
	U32 prq:2;
	U32 fTrueType:1;
	U32 reserved1:1;
	U32 ff:3;
	U32 reserved2:1;
	S32 wWeight:16;

	U8 chs;
	U8 ixchSzAlt;
	PANOSE panose;
	FONTSIGNATURE fs;
	XCHAR xszFfn[65];	/*max size */
    } FFN;

    void wvGetFFN (FFN * item, wvStream * fd);
    void wvGetFFN6 (FFN * item, wvStream * fd);


    typedef struct _FFN_STTBF {
	U16 extendedflag;
	U16 nostrings;
	U16 extradatalen;
	FFN *ffn;
    } FFN_STTBF;

    void wvGetFFN_STTBF (FFN_STTBF * item, U32 offset, U32 len, wvStream * fd);
    void wvGetFFN_STTBF6 (FFN_STTBF * item, U32 offset, U32 len, wvStream * fd);
    void wvReleaseFFN_STTBF (FFN_STTBF * item);
    char *wvGetFontnameFromCode (FFN_STTBF * item, int fontcode);


    typedef struct _STTBF {
	U16 extendedflag;
	U16 nostrings;
	U16 extradatalen;
	S8 **s8strings;
	U16 **u16strings;
	U8 **extradata;
    } STTBF;

    void wvGetSTTBF (STTBF * anS, U32 offset, U32 len, wvStream * fd);
    void wvGetSTTBF6 (STTBF * anS, U32 offset, U32 len, wvStream * fd);
    void wvListSTTBF (STTBF * item);
    void wvReleaseSTTBF (STTBF * item);
    void wvGetGrpXst (STTBF * anS, U32 offset, U32 len, wvStream * fd);


    U16 *UssrStrBegin (STTBF * sttbf, int no);


    typedef enum {
	ibstAssocFileNext = 0,
	ibstAssocDot = 1,
	ibstAssocTitle = 2,
	ibstAssocSubject = 3,
	ibstAssocKeyWords = 4,
	ibstAssocComments = 5,
	ibstAssocAuthor = 6,
	ibstAssocLastRevBy = 7,
	ibstAssocDataDoc = 8,
	ibstAssocHeaderDoc = 9,
	ibstAssocCriteria1 = 10,
	ibstAssocCriteria2 = 11,
	ibstAssocCriteria3 = 12,
	ibstAssocCriteria4 = 13,
	ibstAssocCriteria5 = 14,
	ibstAssocCriteria6 = 15,
	ibstAssocCriteria7 = 16,
	ibstAssocMax = 17,
	ibstAssocMaxWord6 = 17	/* just in case */
    } ibst;


    typedef struct _wv_var1 {
	/* 16 bits for bitfields */
	U32 ch:5;
	U32 reserved:3;
	U32 flt:8;
    } wv_var1;

    typedef struct _wv_var2 {
	/* 16 bits for bitfields */
	U32 ch:5;
	U32 reserved:3;
	U32 fDiffer:1;
	U32 fZombieEmbed:1;
	U32 fResultDirty:1;
	U32 fResultEdited:1;
	U32 fLocked:1;
	U32 fPrivateResult:1;
	U32 fNested:1;
	U32 fHasSep:1;
    } wv_var2;



    typedef union _FLD {
	wv_var1 var1;
	wv_var2 var2;
    } FLD;

    void wvGetFLD (FLD * item, wvStream * fd);
    int wvGetFLD_PLCF (FLD ** fld, U32 ** pos, U32 * nofld, U32 offset, U32 len,
		       wvStream * fd);

    typedef struct _COPTS {
	/* 16 bits for bitfields */
	U32 fNoTabForInd:1;
	U32 fNoSpaceRaiseLower:1;
	U32 fSuppressSpbfAfterPageBreak:1;
	U32 fWrapTrailSpaces:1;
	U32 fMapPrintTextColor:1;
	U32 fNoColumnBalance:1;
	U32 fConvMailMergeEsc:1;
	U32 fSuppressTopSpacing:1;
	U32 fOrigWordTableRules:1;
	U32 fTransparentMetafiles:1;
	U32 fShowBreaksInFrames:1;
	U32 fSwapBordersFacingPgs:1;
	U32 reserved:4;
    } COPTS;

    void wvGetCOPTS (COPTS * copts, wvStream * fd);

    typedef struct _DTTM {
	U32 mint:6;
	U32 hr:5;
	U32 dom:5;
	U32 mon:4;
	U32 yr:9;
	U32 wdy:3;
    } DTTM;

    void wvGetDTTM (DTTM * item, wvStream * fd);
    void wvGetDTTMFromBucket (DTTM * item, U8 * pointer);
    void wvCreateDTTM (DTTM * dttm, U16 one, U16 two);
    void wvCopyDTTM (DTTM * dest, DTTM * src);
    void wvInitDTTM (DTTM * dttm);
    char *wvDTTMtoUnix (DTTM * src);


    typedef struct _DOPTYPOGRAPHY {
	U32 fKerningPunct:1;
	U32 iJustification:2;
	U32 iLevelOfKinsoku:2;
	U32 f2on1:1;
	U32 reserved:10;
	U32 cchFollowingPunct:16;

	U16 cchLeadingPunct;
	U16 rgxchFPunct[101];
	U16 rgxchLPunct[51];
    } DOPTYPOGRAPHY;

    void wvGetDOPTYPOGRAPHY (DOPTYPOGRAPHY * dopt, wvStream * fd);
    void wvInitDOPTYPOGRAPHY (DOPTYPOGRAPHY * dopt);


    typedef struct _DOGRID {
	U16 xaGrid;
	U16 yaGrid;
	U16 dxaGrid;
	U32 dyaGrid:16;
	U32 dyGridDisplay:7;
	U32 fTurnItOff:1;
	U32 dxGridDisplay:7;
	U32 fFollowMargins:1;
    } DOGRID;

    typedef struct _ASUMY {
	S32 lLevel;
    } ASUMY;


    void wvGetDOGRID (DOGRID * dogrid, wvStream * fd);
    void wvInitDOGRID (DOGRID * dog);

    typedef struct _ASUMYI {
	U32 fValid:1;
	U32 fView:1;
	U32 iViewBy:2;
	U32 fUpdateProps:1;
	U32 reserved:11;
	U32 wDlgLevel:16;

	U32 lHighestLevel;
	U32 lCurrentLevel;
    } ASUMYI;

    void wvGetASUMYI (ASUMYI * asumyi, wvStream * fd);
    void wvInitASUMYI (ASUMYI * asu);

    typedef struct _DOP {
	U32 fFacingPages:1;
	U32 fWidowControl:1;
	U32 fPMHMainDoc:1;
	U32 grfSuppression:2;
	U32 fpc:2;		/*where footnotes are put */
	U32 reserved1:1;
	U32 grpfIhdt:8;
	U32 rncFtn:2;		/*how to restart footnotes */
	U32 fFtnRestart:1;	/* Word 2 */

	U32 nFtn:15;		/*first footnote no. WORD 2: int :15 */

	U8 irmBar;		/* W2 */
	U32 irmProps:7;		/* W2 */

	U32 fOutlineDirtySave:1;
	U32 reserved2:7;
	U32 fOnlyMacPics:1;
	U32 fOnlyWinPics:1;
	U32 fLabelDoc:1;
	U32 fHyphCapitals:1;
	U32 fAutoHyphen:1;
	U32 fFormNoFields:1;
	U32 fLinkStyles:1;

	U32 fRevMarking:1;
	U32 fBackup:1;
	U32 fExactCWords:1;
	U32 fPagHidden:1;
	U32 fPagResults:1;
	U32 fLockAtn:1;
	U32 fMirrorMargins:1;

	U32 fKeepFileFormat:1;	/* W2 */

	U32 reserved3:1;

	U32 fDfltTrueType:1;
	U32 fPagSuppressTopSpacing:1;

	U32 fRTLAlignment:1;	/* W2 */
	U32 reserved3a:6;	/* " */
	U32 reserved3b:7;	/* " */

	U32 fSpares:16;		/* W2 */

	U32 fProtEnabled:1;
	U32 fDispFormFldSel:1;
	U32 fRMView:1;
	U32 fRMPrint:1;
	U32 reserved4:1;
	U32 fLockRev:1;
	U32 fEmbedFonts:1;

	COPTS copts;

	U16 dxaTab;

	U32 ftcDefaultBi;	/* W2 */

	U16 wSpare;
	U16 dxaHotZ;
	U16 cConsecHypLim;
	U16 wSpare2;

	U32 wSpare3;		/* W2 */

	DTTM dttmCreated;
	DTTM dttmRevised;
	DTTM dttmLastPrint;

	U16 nRevision;
	U32 tmEdited;
	U32 cWords;
	U32 cCh;
	U16 cPg;
	U32 cParas;

	U16 rgwSpareDocSum[3];	/* W2 */

	U32 rncEdn:2;		/*how endnotes are restarted */
	U32 nEdn:14;		/*beginning endnote no */
	U32 epc:2;		/*where endnotes go */
	U32 nfcFtnRef:4;	/*number format code for auto footnotes, i think use the new_* instead */
	U32 nfcEdnRef:4;	/*number format code for auto endnotes, i thing use the new_* instead */
	U32 fPrintFormData:1;
	U32 fSaveFormData:1;
	U32 fShadeFormData:1;
	U32 reserved6:2;
	U32 fWCFtnEdn:1;

	U32 cLines;
	U32 cWordsFtnEnd;
	U32 cChFtnEdn;
	U16 cPgFtnEdn;
	U32 cParasFtnEdn;
	U32 cLinesFtnEdn;
	U32 lKeyProtDoc;	/*password protection key (! ?) */

	U32 wvkSaved:3;
	U32 wScaleSaved:9;
	U32 zkSaved:2;
	U32 fRotateFontW6:1;
	U32 iGutterPos:1;
	U32 fNoTabForInd:1;
	U32 fNoSpaceRaiseLower:1;
	U32 fSuppressSpbfAfterPageBreak:1;
	U32 fWrapTrailSpaces:1;
	U32 fMapPrintTextColor:1;
	U32 fNoColumnBalance:1;
	U32 fConvMailMergeEsc:1;
	U32 fSuppressTopSpacing:1;
	U32 fOrigWordTableRules:1;
	U32 fTransparentMetafiles:1;
	U32 fShowBreaksInFrames:1;
	U32 fSwapBordersFacingPgs:1;
	U32 reserved7:4;

	U32 fSuppressTopSpacingMac5:1;
	U32 fTruncDxaExpand:1;
	U32 fPrintBodyBeforeHdr:1;
	U32 fNoLeading:1;
	U32 reserved8:1;
	U32 fMWSmallCaps:1;
	U32 reserved9:10;
	U32 adt:16;

	DOPTYPOGRAPHY doptypography;
	DOGRID dogrid;

	U32 reserver11:1;
	U32 lvl:4;
	U32 fGramAllDone:1;
	U32 fGramAllClean:1;
	U32 fSubsetFonts:1;
	U32 fHideLastVersion:1;
	U32 fHtmlDoc:1;
	U32 reserved10:1;
	U32 fSnapBorder:1;
	U32 fIncludeHeader:1;
	U32 fIncludeFooter:1;
	U32 fForcePageSizePag:1;
	U32 fMinFontSizePag:1;
	U32 fHaveVersions:1;
	U32 fAutoVersion:1;
	U32 reserved11:14;

	ASUMYI asumyi;

	U32 cChWS;
	U32 cChWSFtnEdn;
	U32 grfDocEvents;
	U32 fVirusPrompted:1;
	U32 fVirusLoadSafe:1;
	U32 KeyVirusSession30:30;

	U8 Spare[30];
	U32 reserved12;
	U32 reserved13;

	U32 cDBC;
	U32 cDBCFtnEdn;
	U32 reserved14;
	U16 new_nfcFtnRef;	/*number format code for auto footnote references */
	U16 new_nfcEdnRef;	/*number format code for auto endnote references */
	U16 hpsZoonFontPag;
	U16 dywDispPag;
    } DOP;

    void wvGetDOP (wvVersion ver, DOP * dop, U32 fcDop, U32 lcbDop,
		   wvStream * tablefd);
    void wvInitDOP (DOP * dop);

    typedef struct _BKF {
	S32 ibkl:16;
	U32 itcFirst:7;
	U32 fPub:1;
	U32 itcLim:7;
	U32 fCol:1;
    } BKF;

    void wvGetBKF (BKF * item, wvStream * fd);
    int wvGetBKF_PLCF (BKF ** bkf, U32 ** pos, U32 * nobkf, U32 offset, U32 len,
		       wvStream * fd);
    void wvInitBKF (BKF * item);



    typedef struct _Xst {
	U16 *u16string;
	struct _Xst *next;
	U32 noofstrings;
    } Xst;

    void wvGetXst (Xst ** xst, U32 offset, U32 len, wvStream * fd);
    void wvFreeXst (Xst ** xst);

    typedef struct _FSPA {
	U32 spid;
	S32 xaLeft;
	S32 yaTop;
	S32 xaRight;
	S32 yaBottom;
	/* 16 bits for bitfields */
	U32 fHdr:1;
	U32 bx:2;
	U32 by:2;
	U32 wr:4;
	U32 wrk:4;
	U32 fRcaSimple:1;
	U32 fBelowText:1;
	U32 fAnchorLock:1;
	S32 cTxbx;
    } FSPA;

    void wvGetFSPA (FSPA * item, wvStream * fd);
    int wvGetFSPA_PLCF (FSPA ** fspa, U32 ** pos, U32 * nofspa, U32 offset,
			U32 len, wvStream * fd);
    FSPA *wvGetFSPAFromCP (U32 currentcp, FSPA * fspa, U32 * pos, U32 nofspa);
    void wvInitFSPA (FSPA * item);

    typedef struct _LSTF {
	U32 lsid;
	U32 tplc;
	U16 rgistd[9];
	/* 16 bits for bitfields */
	U32 fSimpleList:1;
	U32 fRestartHdn:1;
	U32 reserved1:6;
	U32 reserved2:8;
    } LSTF;

    void wvGetLSTF (LSTF * item, wvStream * fd);
    void wvInitLSTF (LSTF * item);
    int wvGetLSTF_PLCF (LSTF ** lstf, U32 ** pos, U32 * nolst, U32 offset,
			U32 len, wvStream * fd);

    typedef struct _LVLF {
	U32 iStartAt;
	/* 16 bits for bitfield */
	U32 nfc:8;
	U32 jc:2;
	U32 fLegal:1;
	U32 fNoRestart:1;
	U32 fPrev:1;
	U32 fPrevSpace:1;
	U32 fWord6:1;
	U32 reserved1:1;
	U8 rgbxchNums[9];
	U8 ixchFollow;
	U32 dxaSpace;
	U32 dxaIndent;
	U8 cbGrpprlChpx;
	U8 cbGrpprlPapx;
	U16 reserved2;
    } LVLF;

    void wvGetLVLF (LVLF * item, wvStream * fd);
    void wvInitLVLF (LVLF * item);
    void wvCopyLVLF (LVLF * dest, LVLF * src);


/*
A LVL structure contains two parts to it:

(1) an LVLF, which stores all static data such as the start-at value for the
list level, the numbering type (arabic or roman), the alignment (left, right or
centered) of the number, and several Word 6.0 compatibility options; and

(2) a set of pointers to variable length data:

(a) a grpprlChpx, which gives character formatting to the paragraph number text
itself

(b) a grpprlPapx, which gives paragraph formatting to the paragraph containing
the number, such as indenting and tab information, and

(c) the number text itself.
*/
    typedef struct _LVL {
	LVLF lvlf;
	U8 *grpprlPapx;
	U8 *grpprlChpx;
	XCHAR *numbertext;
    } LVL;

    void wvGetLVL (LVL * lvl, wvStream * fd);
    void wvCopyLVL (LVL * dest, LVL * src);
    void wvReleaseLVL (LVL * lvl);
    void wvInitLVL (LVL * lvl);


/*
An LST consists of two main parts:

(1) an LSTF, which is stored on disk and contains formatting properties which
apply to the entire list, such as whether the list is simple or multilevel,
the list's unique list index and template code, the istd's of the styles (if
any) that each level in the list is linked to, and a number of Word 6
compatilibity option;

(2) an array of LVL structures, which describe the appearance of each
individual level in the LST.
*/

/*
(3) I have added a list of values which i will use to determine what
number to use for each list entry, Caolan
*/

    typedef struct _LST {
	LSTF lstf;
	LVL *lvl;
	U32 *current_no;
    } LST;

    int wvGetLST (LST ** lst, U16 * noofLST, U32 offset, U32 len,
		  wvStream * fd);
    void wvReleaseLST (LST ** lst, U16 noofLST);
    LST *wvSearchLST (U32 id, LST * lst, U16 noofLST);
    int wvInitLST (LST * lst);

    typedef struct _LFO {
	U32 lsid;
	U32 reserved1;
	U32 reserved2;
	U8 clfolvl;
	U8 reserved3[3];
    } LFO;

    void wvGetLFO (LFO * item, wvStream * fd);
    void wvInitLFO (LFO * item);
    int wvGetLFO_PLF (LFO ** lfo, U32 * nolfo, U32 offset, U32 len,
		      wvStream * fd);

    typedef struct _LFOLVL {
	U32 iStartAt;
	U32 ilvl:4;
	U32 fStartAt:1;
	U32 fFormatting:1;
	U32 reserved1:2;
	U32 reserved2:8;
	U32 reserved3:8;
	U32 reserved4:8;
    } LFOLVL;

    void wvGetLFOLVL (LFOLVL * item, wvStream * fd);
    void wvInitLFOLVL (LFOLVL * item);
    int wvInvalidLFOLVL (LFOLVL * item);

    int wvGetLFO_records (LFO ** lfo, LFOLVL ** lfolvl, LVL ** lvl, U32 * nolfo,
			  U32 * nooflvl, U32 offset, U32 len, wvStream * fd);
    int wvReleaseLFO_records (LFO ** lfo, LFOLVL ** lfolvl, LVL ** lvl,
			      U32 nooflvl);

    U16 *wvListString (int ilfo, int ilvl, LST * alst);

    typedef U16 LID;

    typedef struct _SHD {
	/*16 bits in total */
	U32 icoFore:5;
	U32 icoBack:5;
	U32 ipat:6;
    } SHD;

    void wvGetSHD (SHD * item, wvStream * fd);
    void wvGetSHDFromBucket (SHD * item, U8 * pointer);
    void wvInitSHD (SHD * item);
    void wvCopySHD (SHD * dest, SHD * src);


    typedef struct _DCS {
	/* 16 bits for bitfields */
	U32 fdct:3;
	U32 count:5;
	U32 reserved:8;
    } DCS;

    void wvGetDCS (DCS * item, wvStream * fd);
    void wvGetDCSFromBucket (DCS * item, U8 * pointer);
    void wvInitDCS (DCS * item);
    void wvCopyDCS (DCS * dest, DCS * src);

    typedef struct _BRC {
	U32 dptLineWidth:8;
	U32 brcType:8;
	U32 ico:8;
	U32 dptSpace:5;
	U32 fShadow:1;
	U32 fFrame:1;
	U32 reserved:1;
    } BRC;

    void wvGetBRC (wvVersion ver, BRC * abrc, wvStream * fd);
    int wvGetBRCFromBucket (wvVersion ver, BRC * abrc, U8 * pointer);
    void wvInitBRC (BRC * abrc);
    void wvCopyBRC (BRC * dest, BRC * src);
    int wvEqualBRC (BRC * a, BRC * b);


    typedef struct _BRC10 {
	/* 16 bits in total */
	U32 dxpLine2Width:3;
	U32 dxpSpaceBetween:3;
	U32 dxpLine1Width:3;
	U32 dxpSpace:5;
	U32 fShadow:1;
	U32 fSpare:1;
    } BRC10;

    int wvGetBRC10FromBucket (BRC10 * item, U8 * pointer);
    void wvInitBRC10 (BRC10 * item);
    void wvConvertBRC10ToBRC (BRC * item, BRC10 * in);


/*
The seven types of border lines that Windows Word 1.0 supports are coded
with different sets of values for dxpLine1Width, dxpSpaceBetween, and
dxpLine2 Width.

The border lines and their brc10 settings follow:

 line type        dxpLine1Width               dxpSpaceBetween dxpLine2Width
 no border        0                           0               0

 single line      1                           0               0
 border

 two single line  1                           1               1
 border

 fat solid border 4                           0               0

 thick solid      2                           0               0
 border

 dotted border    6 (special value meaning    0               0
                  dotted line)

 hairline border  7(special value meaning     0               0
                  hairline)

When the no border settings are stored in the BRC, brc.fShadow and
brc.dxpSpace should be set to 0.
*/

    typedef struct _LSPD {
	S16 dyaLine;
	S16 fMultLinespace;
    } LSPD;

    void wvCopyLSPD (LSPD * dest, LSPD * src);
    void wvInitLSPD (LSPD * item);
    void wvGetLSPDFromBucket (LSPD * item, U8 * pointer);

    typedef union _PHE {
	struct {
	    U32 fSpare:1;
	    U32 fUnk:1;
	    U32 fDiffLines:1;
	    U32 reserved1:5;
	    U32 clMac:8;
	    U32 reserved2:16;
	    S32 dxaCol;
	    S32 dymHeight;	/*also known as dymLine and dymTableHeight in docs */
	} var1;
	struct {
	    U32 fSpare:1;
	    U32 fUnk:1;
	    U32 dcpTtpNext:30;
	    S32 dxaCol;
	    S32 dymHeight;	/*also known as dymLine and dymTableHeight in docs */
	} var2;
    } PHE;


    void wvCopyPHE (PHE * dest, PHE * src, int which);
    void wvInitPHE (PHE * item, int which);
    void wvGetPHE (PHE * dest, int which, U8 * page, U16 * pos);
    void wvGetPHE6 (PHE * dest, U8 * page, U16 * pos);

    typedef struct _NUMRM {
	U8 fNumRM;
	U8 Spare1;
	S16 ibstNumRM;
	DTTM dttmNumRM;
	U8 rgbxchNums[9];
	U8 rgnfc[9];
	S16 Spare2;
	S32 PNBR[9];
	XCHAR xst[32];
    } NUMRM;

    void wvGetNUMRM (NUMRM * item, wvStream * fd);
    void wvGetNUMRMFromBucket (NUMRM * item, U8 * pointer);
    void wvCopyNUMRM (NUMRM * dest, NUMRM * src);
    void wvInitNUMRM (NUMRM * item);

    typedef struct _ANLD {
	U8 nfc;
	U8 cxchTextBefore;

	U32 cxchTextAfter:8;
	U32 jc:2;
	U32 fPrev:1;
	U32 fHang:1;
	U32 fSetBold:1;
	U32 fSetItalic:1;
	U32 fSetSmallCaps:1;
	U32 fSetCaps:1;
	U32 fSetStrike:1;
	U32 fSetKul:1;
	U32 fPrevSpace:1;
	U32 fBold:1;
	U32 fItalic:1;
	U32 fSmallCaps:1;
	U32 fCaps:1;
	U32 fStrike:1;
	U32 kul:3;
	U32 ico:5;

	S16 ftc;
	U16 hps;
	U16 iStartAt;
	S16 dxaIndent;
	U16 dxaSpace;
	U8 fNumber1;
	U8 fNumberAcross;
	U8 fRestartHdn;
	U8 fSpareX;
	XCHAR rgxch[32];
    } ANLD;

    void wvGetANLD (wvVersion ver, ANLD * item, wvStream * fd);
    void wvGetANLD_FromBucket (wvVersion ver, ANLD * item, U8 * pointer8);
    void wvCopyANLD (ANLD * dest, ANLD * src);
    void wvInitANLD (ANLD * item);
    U32 wvCheckSumANLD (ANLD * item);

#define istdNormalChar 10

    typedef struct _CHP {
	U32 fBold:1;
	U32 fItalic:1;
	U32 fRMarkDel:1;
	U32 fOutline:1;
	U32 fFldVanish:1;
	U32 fSmallCaps:1;
	U32 fCaps:1;
	U32 fVanish:1;
	U32 fRMark:1;
	U32 fSpec:1;
	U32 fStrike:1;
	U32 fObj:1;
	U32 fShadow:1;
	U32 fLowerCase:1;
	U32 fData:1;
	U32 fOle2:1;

	U32 fEmboss:1;
	U32 fImprint:1;
	U32 fDStrike:1;
	S32 fUsePgsuSettings:1;	/*? */
	U32 reserved1:12;
	U32 reserved2;

	U16 reserved11;
	U16 ftc;
	U16 ftcAscii;
	U16 ftcFE;
	U16 ftcOther;
	U16 hps;
	S32 dxaSpace;

	U32 iss:3;
	U32 kul:4;
	U32 fSpecSymbol:1;
	U32 ico:5;
	U32 reserved3:1;
	U32 fSysVanish:1;
	U32 hpsPos:1;
	S32 super_sub:16;

	LID lid;
	LID lidDefault;
	LID lidFE;
	U8 idct;
	U8 idctHint;
	U8 wCharScale;
	S32 fcPic_fcObj_lTagObj;
	S16 ibstRMark;
	S16 ibstRMarkDel;
	DTTM dttmRMark;
	DTTM dttmRMarkDel;
	S16 reserved4;
	U16 istd;
	S16 ftcSym;
	XCHAR xchSym;
	S16 idslRMReason;
	S16 idslReasonDel;
	U8 ysr;
	U8 chYsr;
	U16 cpg;
	U16 hpsKern;

	U32 icoHighlight:5;
	U32 fHighlight:1;
	U32 kcd:3;
	U32 fNavHighlight:1;
	U32 fChsDiff:1;
	U32 fMacChs:1;
	U32 fFtcAsciSym:1;
	U32 reserved5:3;
	U32 fPropRMark:16;	/*was typo of fPropMark in documentation */

	S16 ibstPropRMark;
	DTTM dttmPropRMark;
	U8 sfxtText;
	U8 reserved6;
	U8 reserved7;
	U16 reserved8;
	U16 reserved9;
	DTTM reserved10;
	U8 fDispFldRMark;
	S16 ibstDispFldRMark;
	DTTM dttmDispFldRMark;
	XCHAR xstDispFldRMark[16];
	SHD shd;
	BRC brc;

      /* BiDi properties */
      U32  fBidi:1;
      U32  fBoldBidi:1;
      U32  fItalicBidi:1;
      U16 ftcBidi;
      U16 hpsBidi;
      U8  icoBidi;
      LID lidBidi;

	  char stylename[100];
		
    } CHP;

    void wvInitCHP (CHP * item);
    void wvCopyCHP (CHP * dest, CHP * src);

#define itcMax 64

    typedef struct _TC {
	U32 fFirstMerged:1;
	U32 fMerged:1;
	U32 fVertical:1;
	U32 fBackward:1;
	U32 fRotateFont:1;
	U32 fVertMerge:1;
	U32 fVertRestart:1;
	U32 vertAlign:2;
	U32 fUnused:7;
	U32 wUnused:16;
	BRC brcTop;
	BRC brcLeft;
	BRC brcBottom;
	BRC brcRight;
    } TC;

    void wvCopyTC (TC * dest, TC * src);
    int wvGetTCFromBucket (wvVersion ver, TC * abrc, U8 * pointer);
    void wvInitTC (TC * item);

    typedef struct _TLP {
	S32 itl:16;
	U32 fBorders:1;
	U32 fShading:1;
	U32 fFont:1;
	U32 fColor:1;
	U32 fBestFit:1;
	U32 fHdrRows:1;
	U32 fLastRow:1;
	U32 fHdrCols:1;
	U32 fLastCol:1;
	U32 unused:7;
    } TLP;

    void wvCopyTLP (TLP * dest, TLP * src);
    void wvInitTLP (TLP * item);
    void wvGetTLP (TLP * item, wvStream * fd);
    void wvGetTLPFromBucket (TLP * item, U8 * pointer);

    typedef struct _TAP {
	S16 jc;
	S32 dxaGapHalf;
	S32 dyaRowHeight;
	U8 fCantSplit;
	U8 fTableHeader;
	TLP tlp;
	S32 lwHTMLProps;

	U32 fCaFull:1;
	U32 fFirstRow:1;
	U32 fLastRow:1;
	U32 fOutline:1;
	U32 reserved:12;
	S32 itcMac:16;

	S32 dxaAdjust;
	S32 dxaScale;
	S32 dxsInch;
	S16 rgdxaCenter[itcMax + 1];
	S16 rgdxaCenterPrint[itcMax + 1];
	TC rgtc[itcMax];
	SHD rgshd[itcMax];
	BRC rgbrcTable[6];
    } TAP;

#define itbdMax 64

    void wvCopyTAP (TAP * dest, TAP * src);
    void wvInitTAP (TAP * item);

    typedef struct _TBD		/* 8 bits */
    {
	U32 jc:3;
	U32 tlc:3;
	U32 reserved:2;
    } TBD;

    void wvInitTBD (TBD * item);
    void wvCopyTBD (TBD * dest, TBD * src);
    void wvGetTBD (TBD * item, wvStream * fd);
    void wvGetTBDFromBucket (TBD * item, U8 * pointer);

	/* list information: this is an wv extension to the PAP struct */
	typedef struct 
	{
		U32     id;
		S32     start;
		XCHAR * numberstr;
		U32     numberstr_size;
		U8      format;
		U8      align;
		U8      ixchFollow;
		CHP     chp;
	}wvListInfo;


    typedef struct _PAP {
	U16 istd;
	U8 jc;
	U8 fKeep;
	U8 fKeepFollow;

	U32 fPageBreakBefore:8;
	U32 fBrLnAbove:1;
	U32 fBrLnBelow:1;
	U32 fUnused:2;
	U32 pcVert:2;
	U32 pcHorz:2;
	U32 brcp:8;
	U32 brcl:8;

	U8 reserved1;
	U8 ilvl;
	U8 fNoLnn;
	S16 ilfo;
	U8 nLvlAnm;
	U8 reserved2;
	U8 fSideBySide;
	U8 reserved3;
	U8 fNoAutoHyph;
	U8 fWidowControl;
	S32 dxaRight;
	S32 dxaLeft;
	S32 dxaLeft1;
	LSPD lspd;
	U32 dyaBefore;
	U32 dyaAfter;
	PHE phe;
	U8 fCrLf;
	U8 fUsePgsuSettings;
	U8 fAdjustRight;
	U8 reserved4;
	U8 fKinsoku;
	U8 fWordWrap;
	U8 fOverflowPunct;
	U8 fTopLinePunct;
	U8 fAutoSpaceDE;
	U8 fAtuoSpaceDN;
	U16 wAlignFont;

	U32 fVertical:1;
	U32 fBackward:1;
	U32 fRotateFont:1;
	U32 reserved5:13;
	U32 reserved6:16;

	S8 fInTable;
	S8 fTtp;
	U8 wr;
	U8 fLocked;
	TAP ptap;
	S32 dxaAbs;
	S32 dyaAbs;
	S32 dxaWidth;
	BRC brcTop;
	BRC brcLeft;
	BRC brcBottom;
	BRC brcRight;
	BRC brcBetween;
	BRC brcBar;
	S32 dxaFromText;
	S32 dyaFromText;
	/*16 bits for the next two entries */
	S32 dyaHeight:15;
	S32 fMinHeight:1;
	SHD shd;
	DCS dcs;
	S8 lvl;
	S8 fNumRMIns;
	ANLD anld;
	S16 fPropRMark;
	S16 ibstPropRMark;
	DTTM dttmPropRMark;
	NUMRM numrm;
	S16 itbdMac;
	S16 rgdxaTab[itbdMax];
	TBD rgtbd[itbdMax];

  /* TODO: Enable Word 2002 extensions; Note this will break wv ABI
	S8 fNoAllowOverlap;
	S32 ipgb;
	S32 rsid;
	S16 istdList;
	S8 fContextualSpacing;
	S8 fHasOldProps;
	S8 rpf;
	S32 hplcnf;
	S8 yfti[13];
  */
  
/* >>------------------PATCH */
	char stylename[100];
/* -----------------------<< */
      /* BiDi */
      U32 fBidi:1;
		wvListInfo linfo;
    } PAP;

#define istdNil 4095

    void wvCopyPAP (PAP * dest, PAP * src);
    void wvCopyConformPAP (PAP * dest, PAP * src);
    void wvInitPAP (PAP * item);
    int wvIsListEntry (PAP * apap, wvVersion ver);
    int isPAPConform (PAP * current, PAP * previous);


    typedef U16 BF;
    typedef U16 FTC;

/*
  STSHI: STyleSHeet Information, as stored in a file
  Note that new fields can be added to the STSHI without invalidating
  the file format, because it is stored preceded by it's length.
  When reading a STSHI from an older version, new fields will be zero.
*/
    typedef struct _STSHI {
	U16 cstd;		/* Count of styles in stylesheet */
	U16 cbSTDBaseInFile;	/* Length of STD Base as stored in a file */
	U32 fStdStylenamesWritten:1;	/* Are built-in stylenames stored? */
	U32 reserved:15;	/* Spare flags */
	U32 stiMaxWhenSaved:16;	/* Max sti known when this file was written */
	U16 istdMaxFixedWhenSaved;	/* How many fixed-index istds are there? */
	U16 nVerBuiltInNamesWhenSaved;	/* Current version of built-in stylenames */
	FTC rgftcStandardChpStsh[3];	/* ftc used by StandardChpStsh for this document */
    } STSHI;

    void wvGetSTSHI (STSHI * item, U16 cbSTSHI, wvStream * fd);
    void wvInitSTSHI (STSHI * item);


    typedef union _UPX {
	struct {
	    U8 *grpprl;
	} chpx;
	struct {
	    U16 istd;
	    U8 *grpprl;
	} papx;
	U8 *rgb;
    } UPX;

    typedef struct _UPXF {
	U16 cbUPX;
	UPX upx;
    } UPXF;

    typedef struct _CHPX {
	U16 istd;
	U8 cbGrpprl;
	U8 *grpprl;
    } CHPX;

    void wvInitCHPX (CHPX * item);
    void wvCopyCHPX (CHPX * dest, CHPX * src);
    void wvReleaseCHPX (CHPX * item);
    void wvGetCHPX (wvVersion ver, CHPX * item, U8 * page, U16 * pos);
	
   typedef struct _CHPX_FKP {
	U32 *rgfc;
	U8 *rgb;
	CHPX *grpchpx;
	U8 crun;
    } CHPX_FKP;

    void wvGetCHPX_FKP (wvVersion ver, CHPX_FKP * fkp, U32 pn, wvStream * fd);
    void wvReleaseCHPX_FKP (CHPX_FKP * fkp);
    void wvInitCHPX_FKP (CHPX_FKP * fkp);





    typedef union _UPD {
	PAP apap;
	CHP achp;
	CHPX chpx;
    } UPD;

/*
The UPE structure is the non-zero prefix of a UPD structure

For my purposes we'll call them the same, and when we get around
to writing word files, when we'll make a distinction.
*/
    typedef UPD UPE;



/*
   STD: STyle Definition
   The STD contains the entire definition of a style.
   It has two parts, a fixed-length base (cbSTDBase bytes long)
   and a variable length remainder holding the name, and the upx and upe
   arrays (a upx and upe for each type stored in the style, std.cupx)
   Note that new fields can be added to the BASE of the STD without
   invalidating the file format, because the STSHI contains the length
   that is stored in the file.  When reading STDs from an older version,
   new fields will be zero.
*/
    typedef struct _wvSTD {
	/* Base part of STD: */
	U32 sti:12;		/* invariant style identifier */
	U32 fScratch:1;		/* spare field for any temporary use,
				   always reset back to zero! */
	U32 fInvalHeight:1;	/* PHEs of all text with this style are wrong */
	U32 fHasUpe:1;		/* UPEs have been generated */
	U32 fMassCopy:1;	/* std has been mass-copied; if unused at
				   save time, style should be deleted */
	U32 sgc:4;		/* style type code */
	U32 istdBase:12;	/* base style */

	U32 cupx:4;		/* # of UPXs (and UPEs) */
	U32 istdNext:12;	/* next style */
	U32 bchUpe:16;		/* offset to end of upx's, start of upe's */

	/* 16 bits in the following bitfields */
	U32 fAutoRedef:1;	/* auto redefine style when appropriate */
	U32 fHidden:1;		/* hidden from UI? */
	U32 reserved:14;	/* unused bits */

		/* Variable length part of STD: */
		/*	XCHAR *xstzName;*/	/* sub-names are separated by chDelimStyle */
		char *xstzName;


		
		UPXF *grupxf;		/*was UPX *grupx in the spec, but for my
							  purposes its different */

	/* the UPEs are not stored on the file; they are a cache of the based-on
	   chain */
	UPE *grupe;
    } STD;

    typedef enum {
	sgcPara = 1,
	sgcChp,
	sgcPic,
	sgcSep,
	sgcTap
    } sgcval;

    int wvGetSTD (STD * item, U16 baselen, U16 fixedlen, wvStream * fd);
    void wvInitSTD (STD * item);
    void wvReleaseSTD (STD * item);


/*
The style sheet (STSH) is stored in the file in two parts, a STSHI and then
an array of STDs. The STSHI contains general information about the following
stylesheet, including how many styles are in it. After the STSHI, each style
is written as an STD. Both the STSHI and each STD are preceded by a U16
that indicates their length.

 Field     Size        Comment
 cbStshi   2 bytes     size of the following STSHI structure
 STSHI     (cbStshi)   Stylesheet Information
 Then for each style in the stylesheet (stshi.cstd), the following is
 stored:
 cbStd     2 bytes     size of the following STD structure
 STD       (cbStd)     the style description
*/

    typedef struct _STSH {
	STSHI Stshi;
	STD *std;
    } STSH;

    void wvGetSTSH (STSH * item, U32 offset, U32 len, wvStream * fd);
    void wvReleaseSTSH (STSH * item);
    void wvGenerateStyle (STSH * item, U16 i, U16 type);


    void wvInitPAPFromIstd (PAP * apap, U16 istdBase, STSH * stsh);
    void wvAddPAPXFromBucket (PAP * apap, UPXF * upxf, STSH * stsh,
			      wvStream * data);
    void wvAddPAPXFromBucket6 (PAP * apap, UPXF * upxf, STSH * stsh);

    void wvInitCHPFromIstd (CHP * achp, U16 istdBase, STSH * stsh);
    void wvAddCHPXFromBucket (CHP * achp, UPXF * upxf, STSH * stsh);
    void wvAddCHPXFromBucket6 (CHP * achp, UPXF * upxf, STSH * stsh);

    void wvInitCHPXFromIstd (CHPX * chpx, U16 istdBase, STSH * stsh);
    void wvMergeCHPXFromBucket (CHPX * dest, UPXF * upxf);
    void wvUpdateCHPXBucket (UPXF * src);

    void wvApplyCHPXFromBucket (CHP * achp, CHPX * chpx, STSH * stsh);

    typedef struct _ANLV {
	U8 nfc;
	U8 cxchTextBefore;

	U32 cxchTextAfter:8;
	U32 jc:2;
	U32 fPrev:1;
	U32 fHang:1;
	U32 fSetBold:1;
	U32 fSetItalic:1;
	U32 fSetSmallCaps:1;
	U32 fSetCaps:1;
	U32 fSetStrike:1;
	U32 fSetKul:1;
	U32 fPrevSpace:1;
	U32 fBold:1;
	U32 fItalic:1;
	U32 fSmallCaps:1;
	U32 fCaps:1;
	U32 fStrike:1;
	U32 kul:3;
	U32 ico:5;

	S16 ftc;
	U16 hps;
	U16 iStartAt;
	U16 dxaIndent;
	U16 dxaSpace;
    } ANLV;

    void wvInitANLV (ANLV * item);

    void wvGetANLV (ANLV * item, wvStream * fd);
    void wvGetANLVFromBucket (ANLV * item, U8 * pointer);



    typedef struct _OLST {
	ANLV rganlv[9];
	U8 fRestartHdr;
	U8 fSpareOlst2;
	U8 fSpareOlst3;
	U8 fSpareOlst4;
	XCHAR rgxch[64];
    } OLST;

    void wvInitOLST (OLST *);
    void wvGetOLST (wvVersion ver, OLST * item, wvStream * fd);
    void wvGetOLSTFromBucket (wvVersion ver, OLST * item, U8 * pointer);


    typedef struct _SEP {
	U8 bkc;
	U8 fTitlePage;
	S8 fAutoPgn;
	U8 nfcPgn;
	U8 fUnlocked;
	U8 cnsPgn;
	U8 fPgnRestart;
	U8 fEndNote;
	U8 lnc;
	S8 grpfIhdt;
	U16 nLnnMod;
	S32 dxaLnn;
	S16 dxaPgn;
	S16 dyaPgn;
	S8 fLBetween;
	S8 vjc;
	U16 dmBinFirst;
	U16 dmBinOther;
	U16 dmPaperReq;
	BRC brcTop;
	BRC brcLeft;
	BRC brcBottom;
	BRC brcRight;
	S16 fPropRMark;
	S16 ibstPropRMark;
	DTTM dttmPropRMark;
	S32 dxtCharSpace;
	S32 dyaLinePitch;
	U16 clm;
	S16 reserved1;
	U8 dmOrientPage;
	U8 iHeadingPgn;
	U16 pgnStart;
	S16 lnnMin;
	S16 wTextFlow;
	S16 reserved2;

	S32 pgbProp:16;
	U32 pgbApplyTo:3;
	U32 pgbPageDepth:2;
	U32 pgbOffsetFrom:3;
	U32 reserved:8;

	U32 xaPage;
	U32 yaPage;
	U32 xaPageNUp;
	U32 yaPageNUp;
	U32 dxaLeft;
	U32 dxaRight;
	S32 dyaTop;
	S32 dyaBottom;
	U32 dzaGutter;
	U32 dyaHdrTop;
	U32 dyaHdrBottom;
	S16 ccolM1;
	S8 fEvenlySpaced;
	S8 reserved3;
	S32 dxaColumns;
	S32 rgdxaColumnWidthSpacing[89];
	S32 dxaColumnWidth;
	U8 dmOrientFirst;
	U8 fLayout;
	S16 reserved4;
	OLST olstAnm;
	U8 fBidi;	
    } SEP;

    void wvInitSEP (SEP * item);

    typedef struct _SEPX {
	U16 cb;
	U8 *grpprl;
    } SEPX;

    void wvGetSEPX (wvVersion ver, SEPX * item, wvStream * fd);
    void wvReleaseSEPX (SEPX * item);
    int wvAddSEPXFromBucket (SEP * asep, SEPX * item, STSH * stsh);
    int wvAddSEPXFromBucket6 (SEP * asep, SEPX * item, STSH * stsh);

    typedef struct _Sprm {
	/*16 bits in total */
	U32 ispmd:9;		/*ispmd unique identifier within sgc group */
	U32 fSpec:1;		/*fSpec sprm requires special handling */
	U32 sgc:3;		/*sgc   sprm group; type of sprm (PAP, CHP, etc) */
	U32 spra:3;		/*spra  size of sprm argument */
    } Sprm;



    Sprm wvApplySprmFromBucket (wvVersion ver, U16 sprm, PAP * apap, CHP * achp,
				SEP * asep, STSH * stsh, U8 * pointer,
				U16 * pos, wvStream * data);

    int wvSprmLen (int spra);
    void wvGetSprmFromU16 (Sprm * Sprm, U16 sprm);
    U8 wvEatSprm (U16 sprm, U8 * pointer, U16 * pos);

    typedef enum _SprmName {
	/*
	   these ones are ones I made up entirely to match
	   unnamed patterns in word 95 files, whose
	   purpose is currently unknown
	 */
	sprmTUNKNOWN1 = 0xD400,
	sprmPUNKNOWN2 = 0x2400,	/* word 7 0x39 */
	sprmPUNKNOWN3 = 0x2401,	/* word 7 0x3a */
	sprmPUNKNOWN4 = 0x4400,	/* word 7 0x3b */
	sprmCUNKNOWN5 = 0x4800,	/* word 7 0x6f */
	sprmCUNKNOWN6 = 0x4801,	/* word 7 0x70 */
	sprmCUNKNOWN7 = 0x4802,	/* word 7 0x71 */


	/*
	   these ones showed up in rgsprmPrm and are mostly
	   out of date i reckon
	 */
	sprmNoop = 0x0000,	/* this makes sense */
	sprmPPnbrRMarkNot = 0x0000,	/* never seen this one */

	/*
	   this subset were not listed in word 8, but i recreated them
	   from the word 8 guidelines and the original word 6, so
	   basically they will blow things up when ms decides to reuse them
	   in word 2000 or later versions, but what the hell...
	 */
	sprmCFStrikeRM = 0x0841,
	sprmPNLvlAnm = 0x240D,
	sprmCFtc = 0x483D,
	/*end subset */

	/*
	   one of the sprm's that shows up in word 6 docs is "0", which
	   appears to be either the pap.istd or just an index, seeing
	   as the word 6 people didn't list it, lets just ignore it.
	   as it only happens in word 6 docs, our code happens to
	   function fine in the current setup, but at some stage
	   im sure it will bite me hard
	 */

	sprmPIstd = 0x4600,
	sprmPIstdPermute = 0xC601,
	sprmPIncLvl = 0x2602,
	sprmPJc = 0x2403,
	sprmPFSideBySide = 0x2404,
	sprmPFKeep = 0x2405,
	sprmPFKeepFollow = 0x2406,
	sprmPFPageBreakBefore = 0x2407,
	sprmPBrcl = 0x2408,
	sprmPBrcp = 0x2409,
	sprmPIlvl = 0x260A,
	sprmPIlfo = 0x460B,
	sprmPFNoLineNumb = 0x240C,
	sprmPChgTabsPapx = 0xC60D,
	sprmPDxaRight = 0x840E,
	sprmPDxaLeft = 0x840F,
	sprmPNest = 0x4610,
	sprmPDxaLeft1 = 0x8411,
	sprmPDyaLine = 0x6412,
	sprmPDyaBefore = 0xA413,
	sprmPDyaAfter = 0xA414,
	sprmPChgTabs = 0xC615,
	sprmPFInTable = 0x2416,
	sprmPFTtp = 0x2417,
	sprmPDxaAbs = 0x8418,
	sprmPDyaAbs = 0x8419,
	sprmPDxaWidth = 0x841A,
	sprmPPc = 0x261B,
	sprmPBrcTop10 = 0x461C,
	sprmPBrcLeft10 = 0x461D,
	sprmPBrcBottom10 = 0x461E,
	sprmPBrcRight10 = 0x461F,
	sprmPBrcBetween10 = 0x4620,
	sprmPBrcBar10 = 0x4621,
	sprmPDxaFromText10 = 0x4622,
	sprmPWr = 0x2423,
	sprmPBrcTop = 0x6424,
	sprmPBrcLeft = 0x6425,
	sprmPBrcBottom = 0x6426,
	sprmPBrcRight = 0x6427,
	sprmPBrcBetween = 0x6428,
	sprmPBrcBar = 0x6629,
	sprmPFNoAutoHyph = 0x242A,
	sprmPWHeightAbs = 0x442B,
	sprmPDcs = 0x442C,
	sprmPShd = 0x442D,
	sprmPDyaFromText = 0x842E,
	sprmPDxaFromText = 0x842F,
	sprmPFLocked = 0x2430,
	sprmPFWidowControl = 0x2431,
	sprmPRuler = 0xC632,
	sprmPFKinsoku = 0x2433,
	sprmPFWordWrap = 0x2434,
	sprmPFOverflowPunct = 0x2435,
	sprmPFTopLinePunct = 0x2436,
	sprmPFAutoSpaceDE = 0x2437,
	sprmPFAutoSpaceDN = 0x2438,
	sprmPWAlignFont = 0x4439,
	sprmPFrameTextFlow = 0x443A,
	sprmPISnapBaseLine = 0x243B,
	sprmPAnld = 0xC63E,
	sprmPPropRMark = 0xC63F,
	sprmPOutLvl = 0x2640,
	sprmPFBiDi = 0x2441,
	sprmPFNumRMIns = 0x2443,
	sprmPCrLf = 0x2444,
	sprmPNumRM = 0xC645,
	sprmPHugePapx = 0x6645,
	sprmPHugePapx2 = 0x6646,	/* this is the one I have found in
					   the wild, maybe the doc is incorrect
					   in numbering it 6645 C. */
	sprmPFUsePgsuSettings = 0x2447,
	sprmPFAdjustRight = 0x2448,
	sprmPItap = 0x6649,
	sprmPRsid = 0x6467,
	sprmCRsidText = 0x6816,

	sprmCFRMarkDel = 0x0800,
	sprmCFRMark = 0x0801,
	sprmCFFldVanish = 0x0802,
	sprmCPicLocation = 0x6A03,
	sprmCIbstRMark = 0x4804,
	sprmCDttmRMark = 0x6805,
	sprmCFData = 0x0806,
	sprmCIdslRMark = 0x4807,
	sprmCChs = 0xEA08,
	sprmCSymbol = 0x6A09,
	sprmCFOle2 = 0x080A,
	sprmCIdCharType = 0x480B,
	sprmCHighlight = 0x2A0C,
	sprmCObjLocation = 0x680E,
	sprmCFFtcAsciSymb = 0x2A10,
	sprmCIstd = 0x4A30,
	sprmCIstdPermute = 0xCA31,
	sprmCDefault = 0x2A32,
	sprmCPlain = 0x2A33,
	sprmCKcd = 0x2A34,
	sprmCFBold = 0x0835,
	sprmCFItalic = 0x0836,
	sprmCFStrike = 0x0837,
	sprmCFOutline = 0x0838,
	sprmCFShadow = 0x0839,
	sprmCFSmallCaps = 0x083A,
	sprmCFCaps = 0x083B,
	sprmCFVanish = 0x083C,
	sprmCFtcDefault = 0x4A3D,
	sprmCKul = 0x2A3E,
	sprmCSizePos = 0xEA3F,
	sprmCDxaSpace = 0x8840,
	sprmCLid = 0x4A41,
	sprmCIco = 0x2A42,
	sprmCHps = 0x4A43,
	sprmCHpsInc = 0x2A44,
	sprmCHpsPos = 0x4845,
	sprmCHpsPosAdj = 0x2A46,
	sprmCMajority = 0xCA47,
	sprmCIss = 0x2A48,
	sprmCHpsNew50 = 0xCA49,
	sprmCHpsInc1 = 0xCA4A,
	sprmCHpsKern = 0x484B,
	sprmCMajority50 = 0xCA4C,
	sprmCHpsMul = 0x4A4D,
	sprmCYsri = 0x484E,
	sprmCRgFtc0 = 0x4A4F,
	sprmCRgFtc1 = 0x4A50,
	sprmCRgFtc2 = 0x4A51,
	sprmCCharScale = 0x4852,
	sprmCFDStrike = 0x2A53,
	sprmCFImprint = 0x0854,
	sprmCFSpec = 0x0855,
	sprmCFObj = 0x0856,
	sprmCPropRMark = 0xCA57,
	sprmCFEmboss = 0x0858,
	sprmCSfxText = 0x2859,
	sprmCFBiDi = 0x085A,
	sprmCFDiacColor = 0x085B,
	sprmCFBoldBi = 0x085C,
	sprmCFItalicBi = 0x085D,
	sprmCFtcBi = 0x4A5E,
	sprmCLidBi = 0x485F,
	sprmCIcoBi = 0x4A60,
	sprmCHpsBi = 0x4A61,
	sprmCDispFldRMark = 0xCA62,
	sprmCIbstRMarkDel = 0x4863,
	sprmCDttmRMarkDel = 0x6864,
	sprmCBrc = 0x6865,
	sprmCShd = 0x4866,
	sprmCIdslRMarkDel = 0x4867,
	sprmCFUsePgsuSettings = 0x0868,
	sprmCCpg = 0x486B,
	sprmCRgLid0 = 0x486D,
	sprmCRgLid1 = 0x486E,
	sprmCIdctHint = 0x286F,

	sprmPicBrcl = 0x2E00,
	sprmPicScale = 0xCE01,
	sprmPicBrcTop = 0x6C02,
	sprmPicBrcLeft = 0x6C03,
	sprmPicBrcBottom = 0x6C04,
	sprmPicBrcRight = 0x6C05,

	sprmScnsPgn = 0x3000,
	sprmSiHeadingPgn = 0x3001,
	sprmSOlstAnm = 0xD202,
	sprmSDxaColWidth = 0xF203,
	sprmSDxaColSpacing = 0xF204,
	sprmSFEvenlySpaced = 0x3005,
	sprmSFProtected = 0x3006,
	sprmSDmBinFirst = 0x5007,
	sprmSDmBinOther = 0x5008,
	sprmSBkc = 0x3009,
	sprmSFTitlePage = 0x300A,
	sprmSCcolumns = 0x500B,
	sprmSDxaColumns = 0x900C,
	sprmSFAutoPgn = 0x300D,
	sprmSNfcPgn = 0x300E,
	sprmSDyaPgn = 0xB00F,
	sprmSDxaPgn = 0xB010,
	sprmSFPgnRestart = 0x3011,
	sprmSFEndnote = 0x3012,
	sprmSLnc = 0x3013,
	sprmSGprfIhdt = 0x3014,
	sprmSNLnnMod = 0x5015,
	sprmSDxaLnn = 0x9016,
	sprmSDyaHdrTop = 0xB017,
	sprmSDyaHdrBottom = 0xB018,
	sprmSLBetween = 0x3019,
	sprmSVjc = 0x301A,
	sprmSLnnMin = 0x501B,
	sprmSPgnStart = 0x501C,
	sprmSBOrientation = 0x301D,
	sprmSBCustomize = 0x301E,
	sprmSXaPage = 0xB01F,
	sprmSYaPage = 0xB020,
	sprmSDxaLeft = 0xB021,
	sprmSDxaRight = 0xB022,
	sprmSDyaTop = 0x9023,
	sprmSDyaBottom = 0x9024,
	sprmSDzaGutter = 0xB025,
	sprmSDmPaperReq = 0x5026,
	sprmSPropRMark = 0xD227,
	sprmSFBiDi = 0x3228,
	sprmSFFacingCol = 0x3229,
	sprmSFRTLGutter = 0x322A,
	sprmSBrcTop = 0x702B,
	sprmSBrcLeft = 0x702C,
	sprmSBrcBottom = 0x702D,
	sprmSBrcRight = 0x702E,
	sprmSPgbProp = 0x522F,
	sprmSDxtCharSpace = 0x7030,
	sprmSDyaLinePitch = 0x9031,
	sprmSClm = 0x5032,
	sprmSTextFlow = 0x5033,

	sprmTJc = 0x5400,
	sprmTDxaLeft = 0x9601,
	sprmTDxaGapHalf = 0x9602,
	sprmTFCantSplit = 0x3403,
	sprmTTableHeader = 0x3404,
	sprmTTableBorders = 0xD605,
	sprmTDefTable10 = 0xD606,
	sprmTDyaRowHeight = 0x9407,
	sprmTDefTable = 0xD608,
	sprmTDefTableShd = 0xD609,
	sprmTTlp = 0x740A,
	sprmTFBiDi = 0x560B,
	sprmTHTMLProps = 0x740C,
	sprmTSetBrc = 0xD620,
	sprmTInsert = 0x7621,
	sprmTDelete = 0x5622,
	sprmTDxaCol = 0x7623,
	sprmTMerge = 0x5624,
	sprmTSplit = 0x5625,
	sprmTSetBrc10 = 0xD626,
	sprmTSetShd = 0x7627,
	sprmTSetShdOdd = 0x7628,
	sprmTTextFlow = 0x7629,
	sprmTDiagLine = 0xD62A,
	sprmTVertMerge = 0xD62B,
	sprmTVertAlign = 0xD62C
    } SprmName;

    SprmName wvGetrgsprmWord6 (U8 in);

    void wvApplysprmPIstdPermute (PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPIncLvl (PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPChgTabsPapx (PAP * apap, U8 * pointer, U16 * pos);
    int wvApplysprmPChgTabs (PAP * apap, U8 * pointer, U16 * len);
    void wvApplysprmPPc (PAP * apap, U8 * pointer, U16 * len);
    void wvApplysprmPFrameTextFlow (PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPAnld (wvVersion ver, PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPPropRMark (PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPNumRM (PAP * apap, U8 * pointer, U16 * pos);
    void wvApplysprmPHugePapx (PAP * apap, U8 * pointer, U16 * pos,
			       wvStream * data, STSH * stsh);

    void wvApplysprmCChs (CHP * achp, U8 * pointer, U16 * pos);	/*unfinished */
    void wvApplysprmCSymbol (wvVersion ver, CHP * achp, U8 * pointer,
			     U16 * pos);
    void wvApplysprmCIstdPermute (CHP * achp, U8 * pointer, U16 * pos);	/*unfinished */
    void wvApplysprmCDefault (CHP * achp, U8 * pointer, U16 * pos);
    void wvApplysprmCPlain (CHP * achp, STSH * stsh);
    void wvApplysprmCHpsInc (CHP * achp, U8 * pointer, U16 * pos);	/*unfinished */
    void wvApplysprmCSizePos (CHP * achp, U8 * pointer, U16 * pos);	/*unfinished */
    void wvApplysprmCHpsPosAdj (CHP * achp, U8 * pointer, U16 * pos);	/*unfinished */
    void wvApplysprmCMajority (CHP * achp, STSH * stsh, U8 * pointer, U16 * pos);	/*possibly wrong */
    void wvApplysprmCMajority50 (CHP * achp, STSH * stsh, U8 * pointer, U16 * pos);	/*possibly wrong */
    void wvApplysprmCHpsInc1 (CHP * achp, U8 * pointer, U16 * pos);
    void wvApplysprmCPropRMark (CHP * achp, U8 * pointer, U16 * pos);
    void wvApplysprmCDispFldRMark (CHP * achp, U8 * pointer, U16 * pos);

    void wvApplysprmSOlstAnm (wvVersion ver, SEP * asep, U8 * pointer,
			      U16 * pos);
    void wvApplysprmSPropRMark (SEP * asep, U8 * pointer, U16 * pos);

    void wvApplysprmTDxaLeft (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTDxaGapHalf (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTTableBorders (wvVersion ver, TAP * tap, U8 * pointer,
				   U16 * pos);
    void wvApplysprmTDefTable (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTDefTable10 (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTDefTableShd (TAP * tap, U8 * pointer, U16 * pos);
    void wv2ApplysprmTDefTableShd (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTSetBrc (wvVersion ver, TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTInsert (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTDelete (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTDxaCol (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTMerge (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTSplit (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTSetBrc10 (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTSetShd (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTSetShdOdd (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTTextFlow (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTVertMerge (TAP * tap, U8 * pointer, U16 * pos);
    void wvApplysprmTVertAlign (TAP * tap, U8 * pointer, U16 * pos);


    U8 wvToggle (U8 in, U8 toggle);

    typedef enum {
	UTF8,
	ISO_8859_15,
	KOI8,
	TIS620,
	/*add your own charset here */
	CharsetTableSize	/* must be last entry on pain of death */
    } wvCharset;


    typedef enum _FIELDCODE {
	FC_OTHER = 0,
	FC_TIME,
	FC_DateTimePicture,
	FC_HYPERLINK,
	FC_EDITTIME,
	FC_TOC,
	FC_TOC_FROM_RANGE,
	FC_PAGEREF,
	FC_EMBED,
	FC_SPEICHERDAT,
	FC_DATEINAME,
	FieldCodeTableSize	/*must be last entry on pain of death */
    } FIELDCODE;

    typedef enum _TT {
	TT_OTHER = 0,
	TT_DOCUMENT,
	TT_BEGIN,
	TT_END,
	TT_TITLE,
	TT_PARA,
	TT_CHARSET,
	TT_VERSION,
	TT_JUSTIFICATION,
	TT_JUST,
	TT_LEFT,
	TT_RIGHT,
	TT_CENTER,
	TT_BLOCK,
	TT_ASIAN,
	TT_SECTION,
	TT_BOLD,
	TT_CHAR,
	TT_BOLDB,
	TT_BOLDE,
	TT_ITALIC,
	TT_ITALICB,
	TT_ITALICE,
	TT_STRIKE,
	TT_STRIKEB,
	TT_STRIKEE,
	TT_RMarkDel,
	TT_RMarkDelB,
	TT_RMarkDelE,
	TT_OUTLINE,
	TT_OUTLINEB,
	TT_OUTLINEE,
	TT_SMALLCAPS,
	TT_SMALLCAPSB,
	TT_SMALLCAPSE,
	TT_CAPS,
	TT_CAPSB,
	TT_CAPSE,
	TT_VANISH,
	TT_VANISHB,
	TT_VANISHE,
	TT_RMark,
	TT_RMarkB,
	TT_RMarkE,
	TT_SHADOW,
	TT_SHADOWB,
	TT_SHADOWE,
	TT_LOWERCASE,
	TT_LOWERCASEB,
	TT_LOWERCASEE,
	TT_EMBOSS,
	TT_EMBOSSB,
	TT_EMBOSSE,
	TT_IMPRINT,
	TT_IMPRINTB,
	TT_IMPRINTE,
	TT_DSTRIKE,
	TT_DSTRIKEB,
	TT_DSTRIKEE,
	TT_SUPER,
	TT_SUPERB,
	TT_SUPERE,
	TT_SUB,
	TT_SUBB,
	TT_SUBE,
	TT_SINGLEU,
	TT_SINGLEUB,
	TT_SINGLEUE,
	TT_WORDU,
	TT_WORDUB,
	TT_WORDUE,
	TT_DOUBLEU,
	TT_DOUBLEUB,
	TT_DOUBLEUE,
	TT_DOTTEDU,
	TT_DOTTEDUB,
	TT_DOTTEDUE,
	TT_HIDDENU,
	TT_HIDDENUB,
	TT_HIDDENUE,
	TT_THICKU,
	TT_THICKUB,
	TT_THICKUE,
	TT_DASHU,
	TT_DASHUB,
	TT_DASHUE,
	TT_DOTU,
	TT_DOTUB,
	TT_DOTUE,
	TT_DOTDASHU,
	TT_DOTDASHUB,
	TT_DOTDASHUE,
	TT_DOTDOTDASHU,
	TT_DOTDOTDASHUB,
	TT_DOTDOTDASHUE,
	TT_WAVEU,
	TT_WAVEUB,
	TT_WAVEUE,
	TT_BLACK,
	TT_BLACKB,
	TT_BLACKE,
	TT_BLUE,
	TT_BLUEB,
	TT_BLUEE,
	TT_CYAN,
	TT_CYANB,
	TT_CYANE,
	TT_GREEN,
	TT_GREENB,
	TT_GREENE,
	TT_MAGENTA,
	TT_MAGENTAB,
	TT_MAGENTAE,
	TT_RED,
	TT_REDB,
	TT_REDE,
	TT_YELLOW,
	TT_YELLOWB,
	TT_YELLOWE,
	TT_WHITE,
	TT_WHITEB,
	TT_WHITEE,
	TT_DKBLUE,
	TT_DKBLUEB,
	TT_DKBLUEE,
	TT_DKCYAN,
	TT_DKCYANB,
	TT_DKCYANE,
	TT_DKGREEN,
	TT_DKGREENB,
	TT_DKGREENE,
	TT_DKMAGENTA,
	TT_DKMAGENTAB,
	TT_DKMAGENTAE,
	TT_DKRED,
	TT_DKREDB,
	TT_DKREDE,
	TT_DKYELLOW,
	TT_DKYELLOWB,
	TT_DKYELLOWE,
	TT_DKGRAY,
	TT_DKGRAYB,
	TT_DKGRAYE,
	TT_LTGRAY,
	TT_LTGRAYB,
	TT_LTGRAYE,
	TT_FONTSTR,
	TT_FONTSTRB,
	TT_FONTSTRE,
	TT_COLOR,
	TT_COLORB,
	TT_COLORE,
	TT_ibstRMark,
	TT_ibstRMarkDel,
	TT_dttmRMark,
	TT_dttmRMarkDel,
	TT_PropRMark,
	TT_PropRMarkB,
	TT_PropRMarkE,
	TT_ibstPropRMark,
	TT_dttmPropRMark,
	TT_LasVegas,
	TT_LasVegasB,
	TT_LasVegasE,
	TT_BackgroundBlink,
	TT_BackgroundBlinkB,
	TT_BackgroundBlinkE,
	TT_SparkleText,
	TT_SparkleTextB,
	TT_SparkleTextE,
	TT_MarchingAnts,
	TT_MarchingAntsB,
	TT_MarchingAntsE,
	TT_MarchingRedAnts,
	TT_MarchingRedAntsB,
	TT_MarchingRedAntsE,
	TT_Shimmer,
	TT_ShimmerB,
	TT_ShimmerE,
	TT_ANIMATION,
	TT_ANIMATIONB,
	TT_ANIMATIONE,
	TT_DispFldRMark,
	TT_DispFldRMarkB,
	TT_DispFldRMarkE,
	TT_ibstDispFldRMark,
	TT_dttmDispFldRMark,
	TT_xstDispFldRMark,
	TT_OLIST,
	TT_OLISTB,
	TT_OLISTE,
	TT_ULIST,
	TT_ULISTB,
	TT_ULISTE,
	TT_ENTRY,
	TT_ENTRYB,
	TT_ENTRYE,
	TT_numbering,
	TT_Arabic,
	TT_UpperRoman,
	TT_LowerRoman,
	TT_UpperCaseN,
	TT_LowerCaseN,
	TT_nfc,
	TT_START,
	TT_TABLE,
	TT_TABLEB,
	TT_TABLEE,
	TT_ROW,
	TT_ROWB,
	TT_ROWE,
	TT_CELL,
	TT_CELLB,
	TT_CELLE,
	TT_LASTCELL,
	TT_LASTCELLB,
	TT_LASTCELLE,
	TT_COLSPAN,
	TT_ROWSPAN,
	TT_TEXT,
	TT_TEXTB,
	TT_TEXTE,
	TT_CELLRELWIDTH,
	TT_CELLRELPAGEWIDTH,
	TT_CELLBGCOLOR,
	TT_TABLERELWIDTH,
	TT_STYLE,
	TT_COMMENT,
	TT_IBSTANNO,
	TT_xstUsrInitl,
	TT_mmParaBefore,
	TT_mmParaAfter,
	TT_mmParaLeft,
	TT_mmParaRight,
	TT_mmParaLeft1,

	TT_BORDER,
	TT_NONED,
	TT_SINGLED,
	TT_THICKD,
	TT_DOUBLED,
	TT_NUMBER4D,
	TT_HAIRLINED,
	TT_DOTD,
	TT_DASHLARGEGAPD,
	TT_DOTDASHD,
	TT_DOTDOTDASHD,
	TT_TRIPLED,
	TT_thin_thicksmallgapD,
	TT_thick_thinsmallgapD,
	TT_thin_thick_thinsmallgapD,
	TT_thin_thickmediumgapD,
	TT_thick_thinmediumgapD,
	TT_thin_thick_thinmediumgapD,
	TT_thin_thicklargegapD,
	TT_thick_thinlargegapD,
	TT_thin_thick_thinlargegapD,
	TT_WAVED,
	TT_DOUBLEWAVED,
	TT_DASHSMALLGAPD,
	TT_DASHDOTSTROKEDD,
	TT_EMBOSS3DD,
	TT_ENGRAVE3DD,
	TT_DEFAULTD,
	TT_BORDERTopSTYLE,
	TT_BORDERTopCOLOR,
	TT_BORDERLeftSTYLE,
	TT_BORDERLeftCOLOR,
	TT_BORDERRightSTYLE,
	TT_BORDERRightCOLOR,
	TT_BORDERBottomSTYLE,
	TT_BORDERBottomCOLOR,
	TT_mmPadTop,
	TT_mmPadRight,
	TT_mmPadBottom,
	TT_mmPadLeft,
	TT_mmLineHeight,
	TT_PARABGCOLOR,
	TT_PARAFGCOLOR,
	TT_PICTURE,
	TT_pixPicWidth,
	TT_pixPicHeight,
	TT_htmlAlignGuess,
	TT_htmlNextLineGuess,
	TT_PMARGIN,
	TT_PBORDER,
	TT_PARAMARGIN,
	TT_PARABORDER,
	TT_TABLEOVERRIDES,
	TT_ParaBefore,
	TT_ParaAfter,
	TT_ParaLeft,
	TT_ParaRight,
	TT_ParaLeft1,
	TT_FILENAME,
	TT_htmlgraphic,
	TT_no_rows,
	TT_no_cols,
	TT_CHARENTITY,
	TT_VertMergedCells,
	TT_DIRECTION,
	TT_DIR,
/* >>---------- PATCH */
	TT_stylename,
/* << ---------------- */
	TokenTableSize		/*must be last entry on pain of death */
    } TT;



    typedef struct _TokenTable {
	const char *m_name;
	int m_type;
    } TokenTable, CharsetTable, ReasonTable;

/* support for ternary tree lookup of tokens */
    typedef struct tokennode *Tokenptr;
    typedef struct tokennode {
	char splitchar;
	Tokenptr lokid, eqkid, hikid;
	int token;		/* indexes into the token table */
    } Tokennode;

    void tokenTreeFreeAll (void);


    typedef struct _wvEle {
	int nostr;
	char **str;
    } wvEle;


    const char *wvGetCharset (U16 charset);
    U16 wvLookupCharset (char *optarg);

    typedef struct _state_data {
	wvEle elements[TokenTableSize];
	U32 state;
	wvEle *currentele;
	char **current;
	U32 currentlen;
	FILE *fp;
	const char *path;
    } state_data;


    typedef struct _PRM {
	/*full total of bits should be 16 */
	U32 fComplex:1;
	union {
	    struct {
		U32 isprm:7;
		U32 val:8;
	    } var1;
	    struct {
		U32 igrpprl:15;
	    } var2;
	} para;
    } PRM;

    void wvGetPRM (PRM * item, wvStream * fd);
    void wvInitPRM (PRM * item);

    typedef struct _PCD {
	/*this should be 16 bits for bitfields */
	U32 fNoParaLast:1;
	U32 fPaphNil:1;
	U32 fCopied:1;
	U32 reserved:5;
	U32 fn:8;
	U32 fc;
	PRM prm;
    } PCD;

    void wvGetPCD (PCD * item, wvStream * fd);
    void wvInitPCD (PCD * item);
    int wvGetPCD_PLCF (PCD ** pcd, U32 ** pos, U32 * nopcd, U32 offset, U32 len,
		       wvStream * fd);
    int wvReleasePCD_PLCF (PCD * pcd, U32 * pos);
    int wvGuess16bit (PCD * pcd, U32 * pos, U32 nopcd);

    typedef struct _CLX {
	PCD *pcd;
	U32 *pos;
	U32 nopcd;

	U16 grpprl_count;
	U16 *cbGrpprl;
	U8 **grpprl;
    } CLX;



    void wvInitCLX (CLX * item);
    void wvGetCLX (wvVersion ver, CLX * clx, U32 offset, U32 len, U8 fExtChar,
		   wvStream * fd);
    void wvReleaseCLX (CLX * clx);
    void wvBuildCLXForSimple6 (CLX * clx, FIB * fib);

    typedef struct _FDOA {
	S32 fc;
	S16 ctxbx;
    } FDOA;

    void wvGetFDOA (FDOA * item, wvStream * fd);
    int wvGetFDOA_PLCF (FDOA ** fdoa, U32 ** pos, U32 * nofdoa, U32 offset,
			U32 len, wvStream * fd);
    FDOA *wvGetFDOAFromCP (U32 currentcp, FDOA * fdoa, U32 * pos, U32 nofdoa);

    typedef enum {
	DOCBEGIN,
	DOCEND,
	SECTIONBEGIN,
	SECTIONEND,
	PARABEGIN,
	PARAEND,
	CHARPROPBEGIN,
	CHARPROPEND,
	COMMENTBEGIN,
	COMMENTEND
    } wvTag;

    typedef struct _wvParseStruct {
	/*public */
	void *userData;

	/*protected */
        GsfInput *ole_file;
	wvStream *mainfd;
	wvStream *tablefd;
	wvStream *data;
	wvStream *summary;
	FIB fib;
	DOP dop;
	STTBF anSttbfAssoc;
	STTBF Sttbfbkmk;
	LFO *lfo;
	LFOLVL *lfolvl;
	LVL *lvl;
	U32 nolfo;
	U32 nooflvl;
	LST *lst;
	U16 noofLST;
	CLX clx;
	FFN_STTBF fonts;
	STSH stsh;

	LVL *finallvl;
	U32 *liststartnos;
	U8 *listnfcs;

        int (*charhandler) (struct _wvParseStruct * ps, U16 eachchar,
			    U8 chartype, U16 lid);
        int (*scharhandler) (struct _wvParseStruct * ps, U16 eachchar,
			     CHP * achp);
        int (*elehandler) (struct _wvParseStruct * ps, wvTag tag, void *props,
			   int dirty);
        int (*dochandler) (struct _wvParseStruct * ps, wvTag tag);

	/*private */
	wvStream *tablefd0;
	wvStream *tablefd1;
	U16 password[16];
	U8 intable;
	S16 *cellbounds;
	int nocellbounds;
	S16 **vmerges;
	U16 norows;
	U8 endcell;
	U32 currentcp;
	PAP nextpap;

	FSPA *fspa;
	U32 *fspapos;
	U32 nooffspa;

	FDOA *fdoa;
	U32 *fdoapos;
	U32 nooffdoa;

	int fieldstate;
	int fieldmiddle;
	char *filename;
	char *dir;

      /* see abiword bug 10247 */
        GsfInput *input;
    } wvParseStruct;

    void wvSetPassword (const char *password, wvParseStruct * ps);
    void wvSetTableInfo (wvParseStruct * ps, TAP * ptap, int no);
    int wvDecrypt95 (wvParseStruct * ps);
    int wvDecrypt97 (wvParseStruct * ps);

    void wvPrintTitle (wvParseStruct * ps, STTBF * item);

    wvStream *wvWhichTableStream (FIB * fib, wvParseStruct * ps);

    char *wvAutoCharset (wvParseStruct * ps);

    typedef struct _expand_data {
	STTBF *anSttbfAssoc;	/* associated strings */
	STSH *stsh;
	LFO **lfo;		/* list tables */
	LFOLVL *lfolvl;
	LVL *lvl;
	U32 *nolfo;
	U32 *nooflvl;
	LST **lst;
	U16 *noofLST;
	U8 *intable;
	U8 *endcell;
	S16 **cellbounds;
	int *nocellbounds;
	S16 ***vmerges;
	int whichcell;
	int whichrow;

	U32 **liststartnos;
	U8 **listnfcs;
	LVL **finallvl;
	U16 *norows;

	FIB *fib;
	DOP *dop;

	void *props;		/* holds PAP/CHP/etc */
	char *charset;

	char *retstring;
	U32 currentlen;
	state_data *sd;
	SEP *asep;
	PAP *nextpap;
	PAP lastpap;
	char *filename;

	wvParseStruct *ps;
    } expand_data;

    void wvInitExpandData (expand_data * data);
/*
returns the same as wvOLEDecode with the addition that
4 means that it isnt a word document
*/
  int wvInitParser (wvParseStruct * ps, char *path);
  int wvInitParser_gsf (wvParseStruct * ps, GsfInput *path);

  wvParseStruct * wvCreateParser (void);
  void wvDeleteParser (wvParseStruct * ps);

  int wvInit (void);
  void wvShutdown (void);

    void wvDecodeSimple (wvParseStruct * ps, subdocument whichdoc);
    U32 wvGetBeginFC (wvParseStruct * ps, subdocument whichdoc);

    typedef enum {
	cbATRD = 30,
	cbANLD = 84,
	cbANLV = 16,
	cbASUMY = 4,
	cbASUMYI = 12,
	cbBTE = 4,
	cbBKD = 6,
	cbBKF = 4,
	cbBKL = 2,
	cbBRC = 4,
	cbBRC10 = 2,
	cbCHP = 136,
	cbDTTM = 4,
	cbDCS = 2,
	cbDOGRID = 10,
	cbDOPTYPOGRAPHY = 310,
	cbFSPA = 26,
	cbFIB = 898,
	cbLSPD = 4,
	cbOLST = 212,
	cbNUMRM = 128,
	cbPGD = 10,
	cbPHE = 12,
	cbPAP = 610,
	cbPCD = 8,
	/*
	   cbPLC
	 */
	cbPRM = 2,
	cbRS = 16,
	cbRR = 4,
	cbSED = 12,
	cbSEP = 704,
	cbSHD = 2,
	cbTBD = 1,
	cbTC = 20,
	cbTLP = 4,
	cbTAP = 1728,
	cbWKB = 12,
	cbLSTF = 28,
	cbFDOA = 6,
	cbFTXBXS = 22,

	cb7DOP = 88,

	cb6BTE = 2,
	cb6FIB = 682,
	cb6PHE = 6,
	cb6ANLD = 52,
	cb6BRC = 2,
	cb6DOP = 84,
	cb6PGD = 6,
	cb6TC = 10,
	cb6CHP = 42
    } cbStruct;

    U32 wvNormFC (U32 fc, int *flag);
    int wvGetPieceBoundsFC (U32 * begin, U32 * end, CLX * clx, U32 piececount);
    int wvGetPieceBoundsCP (U32 * begin, U32 * end, CLX * clx, U32 piececount);
    U16 wvGetChar (wvStream * fd, U8 chartype);
    void *wvMalloc (U32 size);

    typedef struct _BTE {
	U32 pn:22;
	U32 unused:10;
    } BTE;

    void wvGetBTE (BTE * bte, wvStream * fd);
    void wvInitBTE (BTE * bte);
    int wvGetBTE_PLCF (BTE ** bte, U32 ** pos, U32 * nobte, U32 offset, U32 len,
		       wvStream * fd);
    int wvGetBTE_PLCF6 (BTE ** bte, U32 ** pos, U32 * nobte, U32 offset,
			U32 len, wvStream * fd);
    void wvCopyBTE (BTE * dest, BTE * src);
    int wvGetBTE_FromFC (BTE * bte, U32 currentfc, BTE * list, U32 * fcs,
			 int nobte);
    void wvListBTE_PLCF (BTE ** bte, U32 ** pos, U32 * nobte);

#define WV_PAGESIZE 512


    typedef struct _BX {
	U8 offset;
	PHE phe;
    } BX;

    void wvGetBX (BX * item, U8 * page, U16 * pos);
    void wvGetBX6 (BX * item, U8 * page, U16 * pos);


    typedef struct _PAPX {
	U16 cb;
	U16 istd;
	U8 *grpprl;
    } PAPX;

    void wvGetPAPX (wvVersion ver, PAPX * item, U8 * page, U16 * pos);
    void wvReleasePAPX (PAPX * item);
    void wvInitPAPX (PAPX * item);

    typedef struct _PAPX_FKP {
	U32 *rgfc;
	BX *rgbx;
	PAPX *grppapx;
	U8 crun;
    } PAPX_FKP;

    void wvGetPAPX_FKP (wvVersion ver, PAPX_FKP * fkp, U32 pn, wvStream * fd);
    void wvReleasePAPX_FKP (PAPX_FKP * fkp);
    void wvInitPAPX_FKP (PAPX_FKP * fkp);

    int wvGetIntervalBounds (U32 * fcFirst, U32 * fcLim, U32 currentfc,
			     U32 * pos, U32 nopos);
    int wvIncFC (U8 chartype);

    int wvGetSimpleParaBounds (wvVersion ver, PAPX_FKP * fkp, U32 * fcFirst,
			       U32 * fcLim, U32 currentfc,	/*CLX *clx, */
			       BTE * bte, U32 * pos, int nobte, wvStream * fd);

    int wvOutputTextChar (U16 eachchar, U8 chartype, wvParseStruct * ps,
			  CHP * achp);
    void wvOutputFromUnicode (U16 eachchar, char *outputtype);

    int wvConvertUnicodeToHtml (U16 char16);
    int wvConvertUnicodeToXml (U16 char16);
    char *wvConvertStylename(char *stylename, char *outputtype);
    int wvConvertUnicodeToLaTeX (U16 char16);
    U16 wvConvertSymbolToUnicode (U16 char16);
    U16 wvConvertMTExtraToUnicode (U16 char16);

    U16 wvHandleCodePage (U16 eachchar, U16 lid);

    void wvDecodeComplex (wvParseStruct * ps);
    int wvGetComplexParaBounds (wvVersion ver, PAPX_FKP * fkp, U32 * fcFirst,
				U32 * fcLim, U32 currentfc, CLX * clx,
				BTE * bte, U32 * pos, int nobte, U32 piece,
				wvStream * fd);
    U32 wvSearchNextLargestFCPAPX_FKP (PAPX_FKP * fkp, U32 currentfc);
    U32 wvSearchNextLargestFCCHPX_FKP (CHPX_FKP * fkp, U32 currentfc);
    int wvQuerySamePiece (U32 fcTest, CLX * clx, U32 piece);
    int wvGetComplexParafcFirst (wvVersion ver, U32 * fcFirst, U32 currentfc,
				 CLX * clx, BTE * bte, U32 * pos, int nobte,
				 U32 piece, PAPX_FKP * fkp, wvStream * fd);
    U32 wvSearchNextSmallestFCPAPX_FKP (PAPX_FKP * fkp, U32 currentfc);
    U32 wvGetPieceFromCP (U32 cp, CLX * clx);
    int wvGetIndexFCInFKP_PAPX (PAPX_FKP * fkp, U32 currentfc);

    void wvOLEFree (wvParseStruct * ps);



    int wvText (wvParseStruct * ps);
    int wvHtml (wvParseStruct * ps);

#ifdef DEBUG
#define wvTrace( args ) wvRealTrace(__FILE__,__LINE__, wvFmtMsg args )
#else
#define wvTrace( args )
#endif

    int wvAssembleSimplePAP (wvVersion ver, PAP * apap, U32 fc, PAPX_FKP * fkp, wvParseStruct * ps);
    int wvAssembleComplexCHP (wvVersion ver, CHP * achp, U32 cpiece,STSH * stsh, CLX * clx);

    void wvAppendStr (char **orig, const char *add);
    int wvParseConfig (state_data * myhandle);

    void wvBeginDocument (expand_data * data);
    void wvEndDocument (expand_data * data);

    void wvInitStateData (state_data * data);
    void wvListStateData (state_data * data);

    int wvExpand (expand_data * myhandle, char *buf, int len);
    int wvStrlen (const char *str);
    char *wvStrcat (char *dest, const char *src);
    void wvReleaseStateData (state_data * data);

    U32 wvConvertCPToFC (U32 currentcp, CLX * clx);

    int wvIsEmptyPara (PAP * apap, expand_data * data, int inc);
    void wvBeginPara (expand_data * data);
    void wvEndPara (expand_data * data);

    void wvBeginCharProp (expand_data * data, PAP * apap);
    void wvEndCharProp (expand_data * data);

    void wvBeginSection (expand_data * data);
    void wvEndSection (expand_data * data);

    void wvBeginComment (expand_data * data);
    void wvEndComment (expand_data * data);

    int wvGetComplexParafcLim (wvVersion ver, U32 * fcLim, U32 currentfc,
			       CLX * clx, BTE * bte, U32 * pos, int nobte,
			       U32 piece, PAPX_FKP * fkp, wvStream * fd);

    wvVersion wvQuerySupported (FIB * fib, int *reason);

    const char *wvReason (int reason);

    void
    wvSetCharHandler (wvParseStruct * ps,
		      int (*proc) (wvParseStruct *, U16, U8, U16));
    void
    wvSetSpecialCharHandler (wvParseStruct * ps,
			     int (*proc) (wvParseStruct *, U16, CHP *));
    void
    wvSetElementHandler (wvParseStruct * ps,
			 int (*proc) (wvParseStruct *, wvTag, void *, int));
    void
    wvSetDocumentHandler (wvParseStruct * ps,
			  int (*proc) (wvParseStruct *, wvTag));

    int wvHandleElement (wvParseStruct * ps, wvTag tag, void *props,
			 int dirty);
    int wvHandleDocument (wvParseStruct * ps, wvTag tag);

    SprmName wvGetrgsprmPrm (U16 in);
    int wvAssembleComplexPAP (wvVersion ver, PAP * apap, U32 cpiece, wvParseStruct *ps);
    U32 wvGetEndFCPiece (U32 piece, CLX * clx);
    void wvInitSprm (Sprm * Sprm);

    void wvInitError (void);

    typedef struct _BKD {
	S16 ipgd_itxbxs;
	S32 dcpDepend:16;
	U32 icol:8;
	U32 fTableBreak:1;
	U32 fColumnBreak:1;
	U32 fMarked:1;
	U32 fUnk:1;
	U32 fTextOverflow:1;
	U32 reserved1:3;
    } BKD;

    void wvGetBKD (BKD * item, wvStream * fd);
    int wvGetBKD_PLCF (BKD ** bkd, U32 ** pos, U32 * nobkd, U32 offset, U32 len,
		       wvStream * fd);



    typedef struct _BKL {
	S16 ibkf;
    } BKL;

    void wvGetBKL (BKL * item, wvStream * fd);
    int wvGetBKL_PLCF (BKL ** bkl, U32 ** pos, U32 * nobkl, U32 bkloffset, U32 bkllen,
               U32 bkfoffset, U32 bkflen, wvStream * fd);


    typedef struct _PGD {
	U32 fContinue:1;
	U32 fUnk:1;
	U32 fRight:1;
	U32 fPgnRestart:1;

	/*
	   U32 fGhost:2;   fGhost is fEmptyPage && fAllFtn, and is unioned (sort of with them
	   in word 97) its existance serves no bloody purpose. The word 6
	   spec has a different location for fGhost, but i reckon the word97
	   is right for word 6 as well, but its not like i intend to use
	   this anyway :-)
	 */
	U32 fEmptyPage:1;
	U32 fAllFtn:1;

	U32 fColOnly:1;		/* unused in word 97, but ive retained the name */
	U32 fTableBreaks:1;
	U32 fMarked:1;
	U32 fColumnBreaks:1;
	U32 fTableHeader:1;
	U32 fNewPage:1;
	U32 bkc:4;

	U32 lnn:16;
	U16 pgn;
	S32 dym;
    } PGD;


    void wvGetPGD (wvVersion ver, PGD * item, wvStream * fd);


    typedef struct _RS {
	S16 fRouted;
	S16 fReturnOrig;
	S16 fTrackStatus;
	S16 fDirty;
	S16 nProtect;
	S16 iStage;
	S16 delOption;
	S16 cRecip;
    } RS;

    void wvGetRS (RS * item, wvStream * fd);

    typedef struct _RR {
	S16 cb;
	S16 cbSzRecip;
    } RR;

    void wvGetRR (RR * item, wvStream * fd);

    typedef struct _FTXBXS {
	S32 cTxbx_iNextReuse;
	S32 cReusable;
	S16 fReusable;
	S32 reserved;
	S32 lid;
	S32 txidUndo;
    } FTXBXS;

    void wvGetFTXBXS (FTXBXS * item, wvStream * fd);
    int wvGetFTXBXS_PLCF (FTXBXS ** ftxbxs, U32 ** pos, U32 * noftxbxs,
			  U32 offset, U32 len, wvStream * fd);


    typedef struct _WKB {
	S16 fn;
	U16 grfwkb;
	S32 lvl:16;
	U32 fnpt:4;
	U32 fnpd:12;
	S32 doc;
    } WKB;

    void wvGetWKB (WKB * item, wvStream * fd);

    int wvGetSimpleSectionBounds (wvVersion ver, wvParseStruct * ps, SEP * sep,
				  U32 * fcFirst, U32 * fcLim, U32 cp, CLX * clx,
				  SED * sed, U32 * spiece, U32 * posSedx,
				  U32 section_intervals, STSH * stsh,
				  wvStream * fd);
    int wvGetComplexSEP (wvVersion ver, SEP * sep, U32 cpiece, STSH * stsh,
			 CLX * clx);

    int wvGetSimpleCharBounds (wvVersion ver, CHPX_FKP * fkp, U32 * fcFirst,
			       U32 * fcLim, U32 currentcp, CLX * clx, BTE * bte,
			       U32 * pos, int nobte, wvStream * fd);
    int wvAssembleSimpleCHP (wvVersion ver, CHP * achp, const PAP * apap,
							 U32 fc, CHPX_FKP * fkp, STSH * stsh);
    int wvGetComplexCharfcLim (wvVersion ver, U32 * fcLim, U32 currentfc,
			       CLX * clx, BTE * bte, U32 * pos, int nobte,
			       U32 piece, CHPX_FKP * fkp, wvStream * fd);
    int wvGetComplexCharfcFirst (wvVersion ver, U32 * fcFirst, U32 currentfc,
				 CLX * clx, BTE * bte, U32 * pos, int nobte,
				 U32 piece, CHPX_FKP * fkp, wvStream * fd);

    void wvOutputHtmlChar (U16 eachchar, U8 chartype, char *outputtype,
			   U16 lid);

    int wvGetListEntryInfo (wvVersion ver, LVL ** rlvl, U32 ** nos, U8 ** nfcs,
			    LVL * retlvl, LFO ** retlfo, PAP * apap, LFO ** lfo,
			    LFOLVL * lfolvl, LVL * lvl, U32 * nolfo, LST ** lst,
			    U16 * noofLST);


    void wvSetPixelsPerInch (S16 hpixels, S16 vpixels);
    float wvTwipsToHPixels (S16 twips);
    float wvTwipsToVPixels (S16 twips);
    float wvTwipsToMM (S16 twips);
    float wvPointsToMM (S16 points);

    int wvCellBgColor (int whichrow, int whichcell, int nocells, int norows,
		       TLP * tlp);

#define isodd(a)  ((a/2) != ((a+1)/2))

    float wvRelativeWidth (S16 width, SEP * asep);

    int fieldCharProc (wvParseStruct * ps, U16 eachchar, U8 chartype, U16 lid);

    ATRD *wvGetCommentBounds (U32 * comment_cpFirst, U32 * comment_cpLim,
			      U32 currentcp, ATRD * atrd, U32 * pos, U32 noatrd,
			      STTBF * bookmarks, BKF * bkf, U32 * posBKF,
			      U32 bkf_intervals, BKL * bkl, U32 * posBKL,
			      U32 bkl_intervals);

    int cellCompEQ (void *a, void *b);
    int cellCompLT (void *a, void *b);

    typedef size_t (*wvConvertToUnicode) (const char **, size_t *, char **,
					  size_t *);

    const char *wvLIDToCodePageConverter (U16 lid);
    const char *wvLIDToLangConverter (U16 lid);
  U16 wvLangToLIDConverter ( const char * lang );

    typedef struct _MSOFBH {
	U32 ver:4;
	U32 inst:12;
	U32 fbt:16;
	U32 cbLength;
    } MSOFBH;

    U32 wvGetMSOFBH (MSOFBH * amsofbh, wvStream * fd);
    U32 wvEatmsofbt (MSOFBH * amsofbh, wvStream * fd);

/* FDGG - File DGG */
    typedef struct _FDGG {
	U32 spidMax;		/* The current maximum shape ID */
	U32 cidcl;		/* The number of ID clusters (FIDCLs) */
	U32 cspSaved;		/* The total number of shapes saved */
	/* (including deleted shapes, if undo */
	/* information was saved) */
	U32 cdgSaved;		/* The total number of drawings saved */
    } FDGG;

/* File ID Cluster - used to save IDCLs */
    typedef struct _FIDCL {
	U32 dgid;		/* DG owning the SPIDs in this cluster */
	U32 cspidCur;		/* number of SPIDs used so far */
    } FIDCL;

/* FBSE - File Blip Store Entry */
    typedef struct _FBSE {
	U8 btWin32;		/* Required type on Win32 */
	U8 btMacOS;		/* Required type on Mac */
	U8 rgbUid[16];		/* Identifier of blip */
	U16 tag;		/* currently unused */
	U32 size;		/* Blip size in stream */
	U32 cRef;		/* Reference count on the blip */
	U32 foDelay;		/* File offset in the delay stream */
	U8 usage;		/* How this blip is used (MSOBLIPUSAGE) */
	U8 cbName;		/* length of the blip name */
	U8 unused2;		/* for the future */
	U8 unused3;		/* for the future */
    } FBSE;

    U32 wvGetFBSE (FBSE * afbse, wvStream * fd);
    void wvCopyFBSE (FBSE * dest, FBSE * src);


    typedef enum {
	msoblipusagedefault,	/* all non-texture fill blips get this. */
	msoblipusagetexture,
	msoblipusagemax = 255	/* since this is stored in a byte */
    } msoblipusage;

    typedef enum {		/* GEL provided types... */
	msoblipERROR = 0,	/* An error occured during loading */
	msoblipUNKNOWN,		/* An unknown blip type */
	msoblipEMF,		/* Windows Enhanced Metafile */
	msoblipWMF,		/* Windows Metafile */
	msoblipPICT,		/* Macintosh PICT */
	msoblipJPEG,		/* JFIF */
	msoblipPNG,		/* PNG */
	msoblipDIB,		/* Windows DIB */
	msoblipFirstClient = 32,	/* First client defined blip type */
	msoblipLastClient = 255	/* Last client defined blip type */
    } MSOBLIPTYPE;

    typedef enum {
	msobiUNKNOWN = 0,
	msobiWMF = 0x216,	/* Metafile header then compressed WMF */
	msobiEMF = 0x3D4,	/* Metafile header then compressed EMF */
	msobiPICT = 0x542,	/* Metafile header then compressed PICT */
	msobiPNG = 0x6E0,	/* One byte tag then PNG data */
	msobiJFIF = 0x46A,	/* One byte tag then JFIF data */
	msobiJPEG = msobiJFIF,
	msobiDIB = 0x7A8,	/* One byte tag then DIB data */
	msobiClient = 0x800	/* Clients should set this bit */
    } MSOBI;			/* Blip signature as encoded in the MSOFBH.inst */

    typedef enum {
	msofbtDggContainer = 0xF000,
	msofbtBstoreContainer = 0xF001,
	msofbtDgContainer = 0xF002,
	msofbtSpgrContainer = 0xF003,
	msofbtSpContainer = 0xF004,
	msofbtDgg = 0xF006,
	msofbtBSE = 0xF007,
	msofbtDg = 0xF008,
	msofbtSpgr = 0xF009,
	msofbtSp = 0xF00A,
	msofbtOPT = 0xF00B,
	msofbtTextbox = 0xF00C,
	msofbtClientTextbox = 0xF00D,
	msofbtAnchor = 0xF00E,
	msofbtChildAnchor = 0xF00F,
	msofbtClientAnchor = 0xF010,
	msofbtClientData = 0xF011,
	msofbtBlipFirst = 0xF018,
	msofbtDeletedPspl = 0xF11D,
	msofbtSplitMenuColors = 0xF11E,
	msofbtOleObject = 0xF11F,
	msofbtUserDefined = 0xF122
    } MSOFBT;

    typedef enum {
	msocompressionDeflate = 0,
	msocompressionNone = 254,	/* Used only if compression fails */
	msocompressionTest = 255	/* For testing only */
    } MSOBLIPCOMPRESSION;

    typedef enum {
	msofilterAdaptive = 0,	/* PNG type - not used/supported for metafile */
	msofilterNone = 254,
	msofilterTest = 255	/* For testing only */
    } MSOBLIPFILTER;

    typedef struct _MetaFileBlip {
	/* The secondary, or data, UID - should always be set. */
	U8 m_rgbUid[16];
	/* The primary UID - this defaults to 0, in which case the primary ID is
	   that of the internal data. NOTE!: The primary UID is only saved to disk
	   if (blip_instance ^ blip_signature == 1). Blip_instance is MSOFBH.inst and
	   blip_signature is one of the values defined in MSOBI */
	U8 m_rgbUidPrimary[16];	/* optional based on the above check */

	/* Metafile Blip overhead = 34 bytes. m_cb gives the number of
	   bytes required to store an uncompressed version of the file, m_cbSave
	   is the compressed size.  m_mfBounds gives the boundary of all the
	   drawing calls within the metafile (this may just be the bounding box
	   or it may allow some whitespace, for a WMF this comes from the
	   SetWindowOrg and SetWindowExt records of the metafile). */
	U32 m_cb;		/* Cache of the metafile size */
	RECT m_rcBounds;	/* Boundary of metafile drawing commands */
	POINT m_ptSize;		/* Size of metafile in EMUs */
	U32 m_cbSave;		/* Cache of saved size (size of m_pvBits) */
	U8 m_fCompression;	/* MSOBLIPCOMPRESSION */
	U8 m_fFilter;		/* always msofilterNone */
	wvStream *m_pvBits;		/* Compressed bits of metafile. */
    } MetaFileBlip;

    typedef struct _BitmapBlip {
	/* The secondary, or data, UID - should always be set. */
	U8 m_rgbUid[16];
	/* The primary UID - this defaults to 0, in which case the primary ID is
	   that of the internal data. NOTE!: The primary UID is only saved to disk
	   if (blip_instance ^ blip_signature == 1). Blip_instance is MSOFBH.finst and
	   blip_signature is one of the values defined in MSOBI */
	U8 m_rgbUidPrimary[16];	/* optional based on the above check */
	U8 m_bTag;
	wvStream *m_pvBits;		/* raster bits of the blip */
    } BitmapBlip;


    typedef struct _Blip {
	FBSE fbse;
	U16 type;
	U16 *name;
	union {
	    MetaFileBlip metafile;
	    BitmapBlip bitmap;
	} blip;
    } Blip;

    void wvCopyBlip (Blip * dest, Blip * src);
    U32 wvGetBlip (Blip * blip, wvStream * fd, wvStream * delay);
    void wvReleaseBlip (Blip * blip);

    U32 wvGetMetafile (MetaFileBlip * amf, MSOFBH * amsofbh, wvStream * fd);
    void wvCopyMetafile (MetaFileBlip * dest, MetaFileBlip * src);
    U32 wvGetBitmap (BitmapBlip * abm, MSOFBH * amsofbh, wvStream * fd);
    void wvCopyBitmap (BitmapBlip * dest, BitmapBlip * src);

    typedef struct _FOPTE {
	/* this should be 16 bits for bitfields, and then 32 bit op */
	U32 pid:14;		/* Property ID */
	U32 fBid:1;		/* value is a blip ID - only valid if fComplex is FALSE */
	U32 fComplex:1;		/* complex property, value is length */
	U32 op;
	U8 *entry;
    } FOPTE;

    U32 wvGetFOPTE (FOPTE * afopte, wvStream * fd);
    void wvReleaseFOPTE (FOPTE * afopte);
    U32 wvGetFOPTEArray (FOPTE ** fopte, MSOFBH * msofbh, wvStream * fd);
    void wvReleaseFOPTEArray (FOPTE ** fopte);
    void wvInitFOPTEArray (FOPTE ** fopte);

    typedef struct _FSP {
	U32 spid;		/* The shape id */
	U32 grfPersistent;
    } FSP;

    U32 wvGetFSP (FSP * fsp, wvStream * fd);

/* FDG - File DG */
    typedef struct _FDG {
	U32 csp;		/* The number of shapes in this drawing */
	U32 spidCur;		/* The last MSOSPID given to an SP in this DG */
    } FDG;

    typedef struct _FSPGR {
	RECT rcgBounds;
    } FSPGR;

    typedef RECT FAnchor, FChildAnchor, FClientAnchor;
    U32 wvGetFAnchor (FAnchor * fanchor, wvStream * fd);

    typedef struct _ClientData {
	U8 *data;
    } ClientData;

    typedef struct _ClientTextbox {
	U32 *textid;
    } ClientTextbox;

    void wvInitClientTextbox (ClientTextbox * item);
    void wvReleaseClientTextbox (ClientTextbox * item);
    U32 wvGetClientTextbox (ClientTextbox * item, MSOFBH * amsofbh,
			    wvStream * fd);

    typedef struct _FSPContainer {
	FSPGR fspgr;		/*may not exist */
	FSP fsp;		/*always will exist */
	FOPTE *fopte;		/*always */
	FAnchor fanchor;	/* one of these will be there */
	ClientData clientdata;	/*always */
	ClientTextbox clienttextbox;	/*maybe */

#if 0
	Textbox OleObject	/*maybe */
	  DeletedPspl		/*maybe */
#endif
    } FSPContainer;

    int wv0x01 (Blip * blip, wvStream * fd, U32 len);
    char *wvHtmlGraphic (wvParseStruct * ps, Blip * blip);

    U32 wvGetFSPContainer (FSPContainer * item, MSOFBH * msofbh, wvStream * fd);
    void wvReleaseFSPContainer (FSPContainer * item);

/* begin temp */
    typedef struct _BITMAP {
	U8 bm[14];
    } BITMAP;

    void wvGetBITMAP (BITMAP * bmp, wvStream * fd);

    typedef struct _rc {
	U8 bm[14];
    } rc;

    void wvGetrc (rc * arc, wvStream * fd);
/* end temp */

    typedef struct _PICF {
	U32 lcb;
	U16 cbHeader;
	S16 mfp_mm;
	S16 mfp_xExt;
	S16 mfp_yExt;
	S16 mfp_hMF;
	union {
	    BITMAP bitmap;
	    rc arc;
	} obj;
	S16 dxaGoal;
	S16 dyaGoal;
	U16 mx;
	U16 my;
	S16 dxaCropLeft;
	S16 dyaCropTop;
	S16 dxaCropRight;

	S32 dyaCropBottom:16;
	U32 brcl:4;
	U32 fFrameEmpty:1;
	U32 fBitmap:1;
	U32 fDrawHatch:1;
	U32 fError:1;
	U32 bpp:8;

	BRC brcTop;
	BRC brcLeft;
	BRC brcBottom;
	BRC brcRight;
	S16 dxaOrigin;
	S16 dyaOrigin;
	S16 cProps;
	wvStream *rgb;
    } PICF;

    int wvGetPICF (wvVersion ver, PICF * apicf, wvStream * fd);

    void remove_suffix (char *name, const char *suffix);
    char *base_name (char const *name);

    U32 wvEatOldGraphicHeader (wvStream * fd, U32 len);
    int bmptopng (char *prefix);

    int wv0x08 (Blip * blip, S32 spid, wvParseStruct * ps);

    typedef struct _SplitMenuColors {
	U32 noofcolors;
	U32 *colors;
    } SplitMenuColors;

    typedef struct _Dgg {
	FDGG fdgg;
	FIDCL *fidcl;
    } Dgg;

    typedef struct _BstoreContainer {
	U32 no_fbse;
	Blip *blip;
    } BstoreContainer;

    typedef struct _DggContainer {
	SplitMenuColors splitmenucolors;
	Dgg dgg;
	BstoreContainer bstorecontainer;
    } DggContainer;


    U32 wvGetDggContainer (DggContainer * item, MSOFBH * msofbh, wvStream * fd,
			   wvStream * delay);
    void wvReleaseDggContainer (DggContainer * item);
    void wvInitDggContainer (DggContainer * item);
    U32 wvGetBstoreContainer (BstoreContainer * item, MSOFBH * msofbh,
			      wvStream * fd, wvStream * delay);
    void wvReleaseBstoreContainer (BstoreContainer * item);
    void wvInitBstoreContainer (BstoreContainer * item);


    U32 wvGetDgg (Dgg * dgg, MSOFBH * amsofbh, wvStream * fd);
    void wvReleaseDgg (Dgg * dgg);
    void wvInitDgg (Dgg * dgg);

    U32 wvGetFDGG (FDGG * afdgg, wvStream * fd);
    U32 wvGetFIDCL (FIDCL * afidcl, wvStream * fd);


    U32 wvGetSplitMenuColors (SplitMenuColors * splitmenucolors, MSOFBH
			      * amsofbh, wvStream * fd);
    void wvReleaseSplitMenuColors (SplitMenuColors * splitmenucolors);
    void wvInitSplitMenuColors (SplitMenuColors * splitmenucolors);

    typedef struct _SpgrContainer {
	U32 no_spcontainer;
	FSPContainer *spcontainer;
	U32 no_spgrcontainer;
	struct _SpgrContainer *spgrcontainer;
    } SpgrContainer;

    typedef struct _DgContainer {
	FDG fdg;
	U32 no_spgrcontainer;
	SpgrContainer *spgrcontainer;
	U32 no_spcontainer;
	FSPContainer *spcontainer;
#if 0
	SolverContainer solvercontainer;
	ColorScheme colorscheme;
	RegroupItems regroupitems;
#endif
    } DgContainer;

    U32 wvGetDgContainer (DgContainer * item, MSOFBH * msofbh, wvStream * fd);
    void wvReleaseDgContainer (DgContainer * item);
    void wvInitDgContainer (DgContainer * item);
    U32 wvGetFDG (FDG * afdg, wvStream * fd);
    U32 wvGetSpgrContainer (SpgrContainer * item, MSOFBH * msofbh,
			    wvStream * fd);
    void wvReleaseSpgrContainer (SpgrContainer * item);
    U32 wvGetFSPGR (FSPGR * item, wvStream * fd);

    U32 wvGetClientData (ClientData * item, MSOFBH * msofbh, wvStream * fd);
    void wvReleaseClientData (ClientData * item);
    void wvInitClientData (ClientData * item);
    FSPContainer *wvFindSPID (SpgrContainer * item, S32 spid);

    typedef struct _escherstruct {
	DggContainer dggcontainer;
	DgContainer dgcontainer;
    } escherstruct;

    void wvGetEscher (escherstruct * item, U32 offset, U32 len, wvStream * fd,
		      wvStream * delay);
    void wvInitEscher (escherstruct * item);
    void wvReleaseEscher (escherstruct * item);
    void wvStrToUpper (char *str);
    int decompress (FILE * inputfile, FILE * outputfile, U32 inlen, U32 outlen);

/*current insertion position*/

/*
Property       PID            Type            Default        Description
*/
    typedef enum _pid {
	rotation = 4,		/*LONG            0              fixed point: */
	fLockRotation = 119,	/*BOOL           FALSE           No rotation */
	fLockAspectRatio = 120,	/*BOOL           FALSE           Don't allow */
	fLockPosition = 121,	/*BOOL           FALSE           Don't allow */
	fLockAgainstSelect = 122,	/*BOOL           FALSE           Shape may not */
	fLockCropping = 123,	/*BOOL           FALSE           No cropping */
	fLockVertices = 124,	/*BOOL           FALSE           Edit Points */
	fLockText = 125,	/*BOOL           FALSE           Do not edit */
	fLockAdjustHandles = 126,	/*BOOL           FALSE           Do not adjust */
	fLockAgainstGrouping = 127,	/*BOOL           FALSE           Do not group */
	lTxid = 128,		/*LONG           0               id for the text, */
	dxTextLeft = 129,	/*LONG           1/10 inch       margins relative */
	dyTextTop = 130,	/*LONG           1/20 inch */
	dxTextRight = 131,	/*LONG           1/10 inch */
	dyTextBottom = 132,	/*LONG           1/20 inch */
	WrapText = 133,		/*MSOWRAPMODE    FALSE           Wrap text at */
	scaleText = 134,	/*LONG           0               Text zoom/scale */
	anchorText = 135,	/*MSOANCHOR      Top             How to anchor */
	txflTextFlow = 136,	/*MSOTXFL        HorzN           Text flow */
	cdirFont = 137,		/*MSOCDIR        msocdir0        Font rotation */
	hspNext = 138,		/*MSOHSP         NULL            ID of the next */
	txdir = 139,		/*MSOTXDIR       LTR             Bi-Di Text */
	fSelectText = 187,	/*BOOL           TRUE            TRUE if single */
	fAutoTextMargin = 188,	/*BOOL           FALSE           use host's */
	fRotateText = 189,	/*BOOL           FALSE           Rotate text with */
	fFitShapeToText = 190,	/*BOOL           FALSE           Size shape to */
	fFitTextToShape = 191,	/*BOOL           FALSE           Size text to fit */
	gtextUNICODE = 192,	/*WCHAR*           NULL           UNICODE text */
	gtextRTF = 193,		/*char*            NULL           RTF text */
	gtextAlign = 194,	/*MSOGEOTEXTALIGN  Center         alignment on */
	gtextSize = 195,	/*LONG             36<<16         default point */
	gtextSpacing = 196,	/*LONG             1<<16          fixed point */
	gtextFont = 197,	/*WCHAR*           NULL           font family */
	gtextFReverseRows = 240,	/*BOOL             FALSE          Reverse row */
	fGtext = 241,		/*BOOL             FALSE          Has text */
	gtextFVertical = 242,	/*BOOL             FALSE          Rotate */
	gtextFKern = 243,	/*BOOL             FALSE          Kern */
	gtextFTight = 244,	/*BOOL             FALSE          Tightening or */
	gtextFStretch = 245,	/*BOOL             FALSE          Stretch to */
	gtextFShrinkFit = 246,	/*BOOL             FALSE          Char bounding */
	gtextFBestFit = 247,	/*BOOL             FALSE          Scale */
	gtextFNormalize = 248,	/*BOOL             FALSE          Stretch char */
	gtextFDxMeasure = 249,	/*BOOL             FALSE          Do not */
	gtextFBold = 250,	/*BOOL             FALSE          Bold font */
	gtextFItalic = 251,	/*BOOL             FALSE          Italic font */
	gtextFUnderline = 252,	/*BOOL             FALSE          Underline */
	gtextFShadow = 253,	/*BOOL             FALSE          Shadow font */
	gtextFSmallcaps = 254,	/*BOOL             FALSE          Small caps */
	gtextFStrikethrough = 255,	/*BOOL             FALSE          Strike */
	cropFromTop = 256,	/*LONG          0                      16.16 fraction times total image */
	cropFromBottom = 257,	/*LONG          0 */
	cropFromLeft = 258,	/*LONG          0 */
	cropFromRight = 259,	/*LONG          0 */
	pib = 260,		/*IMsoBlip*     NULL                   Blip to display */
	pibName = 261,		/*WCHAR*        NULL                   Blip file name */
	pibFlags = 262,		/*MSOBLIPFLAGS  Comment                Blip flags */
	pictureTransparent = 263,	/*LONG          ~0                     transparent color (none if ~0UL) */
	pictureContrast = 264,	/*LONG          1<<16                  contrast setting */
	pictureBrightness = 265,	/*LONG          0                      brightness setting */
	pictureGamma = 266,	/*LONG          0                      16.16 gamma */
	pictureId = 267,	/*LONG          0                      Host-defined ID for OLE objects */
	pictureDblCrMod = 268,	/*MSOCLR        This                   Modification used if shape has */
	pictureFillCrMod = 269,	/*MSOCLR        undefined */
	pictureLineCrMod = 270,	/*MSOCLR        undefined */
	pibPrint = 271,		/*IMsoBlip*     NULL                   Blip to display when printing */
	pibPrintName = 272,	/*WCHAR*        NULL                   Blip file name */
	pibPrintFlags = 273,	/*MSOBLIPFLAGS  Comment                Blip flags */
	fNoHitTestPicture = 316,	/*BOOL          FALSE                  Do not hit test the picture */
	pictureGray = 317,	/*BOOL          FALSE                  grayscale display */
	pictureBiLevel = 318,	/*BOOL          FALSE                  bi-level display */
	pictureActive = 319,	/*BOOL          FALSE                  Server is active (OLE objects */
	geoLeft = 320,		/*LONG           0                   Defines the G */
	geoTop = 321,		/*LONG           0 */
	geoRight = 322,		/*LONG           21600 */
	geoBottom = 323,	/*LONG           21600 */
	shapePath = 324,	/*MSOSHAPEPATH   msoshapeLinesClosed */
	pVertices = 325,	/*IMsoArray      NULL                An array of */
	pSegmentInfo = 326,	/*IMsoArray      NULL */
	adjustValue = 327,	/*LONG           0                   Adjustment */
	adjust2Value = 328,	/*LONG           0 */
	adjust3Value = 329,	/*LONG           0 */
	adjust4Value = 330,	/*LONG           0 */
	adjust5Value = 331,	/*LONG           0 */
	adjust6Value = 332,	/*LONG           0 */
	adjust7Value = 333,	/*LONG           0 */
	adjust8Value = 334,	/*LONG           0 */
	adjust9Value = 335,	/*LONG           0 */
	adjust10Value = 336,	/*LONG           0 */
	fShadowOK = 378,	/*BOOL           TRUE                Shadow may be */
	f3DOK = 379,		/*BOOL           TRUE                3D may be set */
	fLineOK = 380,		/*BOOL           TRUE                Line style may */
	fGtextOK = 381,		/*BOOL           FALSE               Text effect */
	fFillShadeShapeOK = 382,	/*BOOL           FALSE */
	fFillOK = 383,		/*BOOL           TRUE                OK to fill the */
	fillType = 384,		/*MSOFILLTYPE   Solid       Type of fill */
	fillColor = 385,	/*MSOCLR        white       Foreground color */
	fillOpacity = 386,	/*LONG          1<<16       Fixed 16.16 */
	fillBackColor = 387,	/*MSOCLR        white       Background color */
	fillBackOpacity = 388,	/*LONG          1<<16       Shades only */
	fillCrMod = 389,	/*MSOCLR        undefined   Modification for BW */
	fillBlip = 390,		/*IMsoBlip*     NULL        Pattern/texture */
	fillBlipName = 391,	/*WCHAR*        NULL        Blip file name */
	fillBlipFlags = 392,	/*MSOBLIPFLAGS  Comment     Blip flags */
	fillWidth = 393,	/*LONG          0           How big (A units) to */
	fillHeight = 394,	/*LONG          0 */
	fillAngle = 395,	/*LONG          0           Fade angle - degrees in */
	fillFocus = 396,	/*LONG          0           Linear shaded fill focus */
	fillToLeft = 397,	/*LONG          0           Fraction 16.16 */
	fillToTop = 398,	/*LONG          0           Fraction 16.16 */
	fillToRight = 399,	/*LONG          0           Fraction 16.16 */
	fillToBottom = 400,	/*LONG          0           Fraction 16.16 */
	fillRectLeft = 401,	/*LONG          0           For shaded fills, use */
	fillRectTop = 402,	/*LONG          0 */
	fillRectRight = 403,	/*LONG          0 */
	fillRectBottom = 404,	/*LONG          0 */
	fillDztype = 405,	/*MSODZTYPE     Default */
	fillShadePreset = 406,	/*LONG          0           Special shades */
	fillShadeColors = 407,	/*IMsoArray     NULL        a preset array of colors */
	fillOriginX = 408,	/*LONG          0 */
	fillOriginY = 409,	/*LONG          0 */
	fillShapeOriginX = 410,	/*LONG          0 */
	fillShapeOriginY = 411,	/*LONG          0 */
	fillShadeType = 412,	/*MSOSHADETYPE  Default    Type of */
	fFilled = 443,		/*BOOL          TRUE        Is shape filled? */
	fHitTestFill = 444,	/*BOOL          TRUE        Should we hit test fill? */
	fillShape = 445,	/*BOOL          TRUE        Register pattern on */
	fillUseRect = 446,	/*BOOL          FALSE       Use the large rect? */
	fNoFillHitTest = 447,	/*BOOL          FALSE       Hit test a shape as */
	lineColor = 448,	/*MSOCLR            black             Color of line */
	lineOpacity = 449,	/*LONG              1<<16             Not implemented */
	lineBackColor = 450,	/*MSOCLR            white             Background color */
	lineCrMod = 451,	/*MSOCLR            undefined         Modification for */
	lineType = 452,		/*MSOLINETYPE       Solid             Type of line */
	lineFillBlip = 453,	/*IMsoBlip*         NULL              Pattern/texture */
	lineFillBlipName = 454,	/*WCHAR*            NULL              Blip file name */
	lineFillBlipFlags = 455,	/*MSOBLIPFLAGS      Comment           Blip flags */
	lineFillWidth = 456,	/*LONG              0                 How big (A */
	lineFillHeight = 457,	/*LONG              0 */
	lineFillDztype = 458,	/*MSODZTYPE         Default           How to interpret */
	lineWidth = 459,	/*LONG              9525              A units; 1pt == */
	lineMiterLimit = 460,	/*LONG              8<<16             ratio (16.16) of */
	lineStyle = 461,	/*MSOLINESTYLE      Simple            Draw parallel */
	lineDashing = 462,	/*MSOLINEDASHING    Solid             Can be */
	lineDashStyle = 463,	/*IMsoArray         NULL              As Win32 */
	lineStartArrowhead = 464,	/*MSOLINEEND        NoEnd             Arrow at start */
	lineEndArrowhead = 465,	/*MSOLINEEND        NoEnd             Arrow at end */
	lineStartArrowWidth = 466,	/*MSOLINEENDWIDTH   MediumWidthArrow  Arrow at start */
	lineStartArrowLength = 467,	/*MSOLINEENDLENGTH  MediumLenArrow    Arrow at end */
	lineEndArrowWidth = 468,	/*MSOLINEENDWIDTH   MediumWidthArrow  Arrow at start */
	lineEndArrowLength = 469,	/*MSOLINEENDLENGTH  MediumLenArrow    Arrow at end */
	lineJoinStyle = 470,	/*MSOLINEJOIN       JoinRound         How to join */
	lineEndCapStyle = 471,	/*MSOLINECAP        EndCapFlat        How to end lines */
	fArrowheadsOK = 507,	/*BOOL              FALSE             Allow arrowheads */
	fLine = 508,		/*BOOL              TRUE              Any line? */
	fHitTestLine = 509,	/*BOOL              TRUE              Should we hit */
	lineFillShape = 510,	/*BOOL              TRUE              Register pattern */
	fNoLineDrawDash = 511,	/*BOOL              FALSE             Draw a dashed */
	shadowType = 512,	/*MSOSHADOWTYPE  Offset          Type of */
	shadowColor = 513,	/*MSOCLR         0x808080        Foreground */
	shadowHighlight = 514,	/*MSOCLR         0xCBCBCB        Embossed */
	shadowCrMod = 515,	/*MSOCLR         undefined       Modification */
	shadowOpacity = 516,	/*LONG           1<<16           Fixed 16.16 */
	shadowOffsetX = 517,	/*LONG           25400           Offset shadow */
	shadowOffsetY = 518,	/*LONG           25400           Offset shadow */
	shadowSecondOffsetX = 519,	/*LONG           0               Double offset */
	shadowSecondOffsetY = 520,	/*LONG           0               Double offset */
	shadowScaleXToX = 521,	/*LONG           1<<16           16.16 */
	shadowScaleYToX = 522,	/*LONG           0               16.16 */
	shadowScaleXToY = 523,	/*LONG           0               16.16 */
	shadowScaleYToY = 524,	/*LONG           1<<16           16.16 */
	shadowPerspectiveX = 525,	/*LONG           0               16.16 */
	shadowPerspectiveY = 526,	/*LONG           0               16.16 */
	shadowWeight = 527,	/*LONG           1<<8            scaling */
	shadowOriginX = 528,	/*LONG           0 */
	shadowOriginY = 529,	/*LONG           0 */
	fShadow = 574,		/*BOOL           FALSE           Any shadow? */
	fshadowObscured = 575,	/*BOOL           FALSE           Excel5-style */
	perspectiveType = 576,	/*MSOXFORMTYPE   Shape           Where transform */
	perspectiveOffsetX = 577,	/*LONG           0               The LONG values */
	perspectiveOffsetY = 578,	/*LONG           0 */
	perspectiveScaleXToX = 579,	/*LONG           1<<16 */
	perspectiveScaleYToX = 580,	/*LONG           0 */
	perspectiveScaleXToY = 581,	/*LONG           0 */
	perspectiveScaleYToY = 582,	/*LONG           1<<16 */
	perspectivePerspectiveX = 583,	/*LONG           0 */
	perspectivePerspectiveY = 584,	/*LONG           0 */
	perspectiveWeight = 585,	/*LONG           1<<8            Scaling factor */
	perspectiveOriginX = 586,	/*LONG           1<<15 */
	perspectiveOriginY = 587,	/*LONG           1<<15 */
	fPerspective = 639,	/*BOOL           FALSE           On/off */
	c3DSpecularAmt = 640,	/*LONG    0               Fixed-point 16.16 */
	c3DDiffuseAmt = 641,	/*LONG    65536           Fixed-point 16.16 */
	c3DShininess = 642,	/*LONG    5               Default gives OK */
	c3DEdgeThickness = 643,	/*LONG    12700           Specular edge */
	c3DExtrudeForward = 644,	/*LONG    0               Distance of extrusion */
	c3DExtrudeBackward = 645,	/*LONG    457200 */
	c3DExtrudePlane = 646,	/*LONG    0               Extrusion direction */
	c3DExtrusionColor = 647,	/*MSOCLR  FillThenLine   Basic color */
	c3DCrMod = 648,		/*MSOCLR  undefined       Modification for BW */
	f3D = 700,		/*BOOL    FALSE           Does this shape have a */
	fc3DMetallic = 701,	/*BOOL    0               Use metallic */
	fc3DUseExtrusionColor = 702,	/*BOOL    FALSE */
	fc3DLightFace = 703,	/*BOOL    TRUE */
	c3DYRotationAngle = 704,	/*LONG             0              degrees (16.16) */
	c3DXRotationAngle = 705,	/*LONG             0              degrees (16.16) */
	c3DRotationAxisX = 706,	/*LONG             100            These specify */
	c3DRotationAxisY = 707,	/*LONG             0 */
	c3DRotationAxisZ = 708,	/*LONG             0 */
	c3DRotationAngle = 709,	/*LONG             0              degrees (16.16) */
	c3DRotationCenterX = 710,	/*LONG             0              rotation center */
	c3DRotationCenterY = 711,	/*LONG             0              rotation center */
	c3DRotationCenterZ = 712,	/*LONG             0              rotation center */
	c3DRenderMode = 713,	/*MSO3DRENDERMODE  FullRender     Full,wireframe, */
	c3DTolerance = 714,	/*LONG             30000          pixels (16.16) */
	c3DXViewpoint = 715,	/*LONG             1250000        X view point */
	c3DYViewpoint = 716,	/*LONG             -1250000       Y view point */
	c3DZViewpoint = 717,	/*LONG             9000000        Z view distance */
	c3DOriginX = 718,	/*LONG             32768 */
	c3DOriginY = 719,	/*LONG             -32768 */
	c3DSkewAngle = 720,	/*LONG             -8847360       degree (16.16) */
	c3DSkewAmount = 721,	/*LONG             50             Percentage skew */
	c3DAmbientIntensity = 722,	/*LONG             20000          Fixed point */
	c3DKeyX = 723,		/*LONG             50000          Key light */
	c3DKeyY = 724,		/*LONG             0              tion; only */
	c3DKeyZ = 725,		/*LONG             10000          magnitudes */
	c3DKeyIntensity = 726,	/*LONG             38000          Fixed point */
	c3DFillX = 727,		/*LONG             -50000         Fill light */
	c3DFillY = 728,		/*LONG             0              tion; only */
	c3DFillZ = 729,		/*LONG             10000          magnitudes */
	c3DFillIntensity = 730,	/*LONG             38000          Fixed point */
	fc3DConstrainRotation = 763,	/*BOOL             TRUE */
	fc3DRotationCenterAuto = 764,	/*BOOL             FALSE */
	fc3DParallel = 765,	/*BOOL             1              Parallel */
	fc3DKeyHarsh = 766,	/*BOOL             1              Is key lighting */
	fc3DFillHarsh = 767,	/*BOOL             0              Is fill */
	hspMaster = 769,	/*MSOHSP      NULL        master shape */
	cxstyle = 771,		/*MSOCXSTYLE  None       Type of */
	bWMode = 772,		/*MSOBWMODE   Automatic  Settings for */
	bWModePureBW = 773,	/*MSOBWMODE   Automatic */
	bWModeBW = 774,		/*MSOBWMODE   Automatic */
	fOleIcon = 826,		/*BOOL        FALSE       For OLE objects, */
	fPreferRelativeResize = 827,	/*BOOL        FALSE       For UI only. Prefer */
	fLockShapeType = 828,	/*BOOL        FALSE       Lock the shape type */
	fDeleteAttachedObject = 830,	/*BOOL        FALSE */
	fBackground = 831,	/*BOOL        FALSE       If TRUE, this is the */
	spcot = 832,		/*MSOSPCOT  TwoSegment  Callout type */
	dxyCalloutGap = 833,	/*LONG      1/12 inch   Distance from box to */
	spcoa = 834,		/*MSOSPCOA  Any         Callout angle */
	spcod = 835,		/*MSOSPCOD  Specified   Callout drop type */
	dxyCalloutDropSpecified = 836,	/*LONG      9 points    if msospcodSpecified, the */
	dxyCalloutLengthSpecified = 837,	/*LONG      0           if */
	fCallout = 889,		/*BOOL      FALSE       Is the shape a callout? */
	fCalloutAccentBar = 890,	/*BOOL      FALSE       does callout have accent */
	fCalloutTextBorder = 891,	/*BOOL      TRUE        does callout have a text */
	fCalloutMinusX = 892,	/*BOOL      FALSE */
	fCalloutMinusY = 893,	/*BOOL      FALSE */
	fCalloutDropAuto = 894,	/*BOOL      FALSE       If true, then we */
	fCalloutLengthSpecified = 895,	/*BOOL      FALSE       if true, we look at */
	wzName = 896,		/*WCHAR*         NULL            Shape Name */
	wzDescription = 897,	/*WCHAR*         NULL            alternate */
	pihlShape = 898,	/*IHlink*        NULL            The hyperlink */
	pWrapPolygonVertices = 899,	/*IMsoArray      NULL            The polygon */
	dxWrapDistLeft = 900,	/*LONG           1/8 inch        Left wrapping */
	dyWrapDistTop = 901,	/*LONG           0               Top wrapping */
	dxWrapDistRight = 902,	/*LONG           1/8 inch        Right */
	dyWrapDistBottom = 903,	/*LONG           0               Bottom */
	lidRegroup = 904,	/*LONG           0               Regroup ID */
	fEditedWrap = 953,	/*BOOL           FALSE           Has the wrap */
	fBehindDocument = 954,	/*BOOL           FALSE           Word-only */
	fOnDblClickNotify = 955,	/*BOOL           FALSE           Notify client */
	fIsButton = 956,	/*BOOL           FALSE           A button */
	fOneD = 957,		/*BOOL           FALSE           1D adjustment */
	fHidden = 958,		/*BOOL           FALSE           Do not */
	fPrint = 959		/*BOOL           TRUE            Print this */
    } pid;


    struct _fopte_list {
	FOPTE afopte;
	struct _fopte_list *next;
    };

    typedef struct _fopte_list fopte_list;

    struct _fsp_list {
	FSP afsp;
	fopte_list *afopte_list;
	struct _fsp_list *next;
    };

    typedef struct _fsp_list fsp_list;

    struct _fbse_list {
	FBSE afbse;
	char filename[4096];
	struct _fbse_list *next;
    };

    typedef struct _fbse_list fbse_list;



    fsp_list *wvParseEscher (fbse_list ** pic_list, U32 fcDggInfo,
			     U32 lcbDggInfo, wvStream * escherstream,
			     FILE * delaystream);
    fbse_list *wvGetSPID (U32 spid, fsp_list * afsp_list,
			  fbse_list * afbse_list);
    U32 twvGetFBSE (FBSE * item, wvStream * fd);









/*Summary Information Stream*/

    typedef struct _PropHeader {
	U16 byteOrder;
	U16 wFormat;
	U16 osVersion1;
	U16 osVersion2;
	U8 classId[16];
	U32 cSections;
    } PropHeader;

    void wvGetPropHeader (PropHeader * header, wvStream * file);

    typedef struct _FIDAndOffset {
	U32 dwords[4];
	U32 dwOffset;
    } FIDAndOffset;

    void wvGetFIDAndOffset (FIDAndOffset * fid, wvStream * file);

    typedef struct _aPro {
	U32 propID;
	U32 dwOffset;
    } aPro;

    typedef struct _SummaryInfo {
	U32 cBytes;
	U32 cProps;
	aPro *aProps;
	U8 *data;
    } SummaryInfo;

    int wvSumInfoOpenStream (SummaryInfo * si, wvStream * stream);

    void wvGetSummaryInfo (SummaryInfo * si, wvStream * file, U32 offset);
    void wvReleaseSummaryInfo (SummaryInfo * si);

    typedef struct _vtB {
	U32 cBytes;
	char *ch;
    } vtB;

#define VT_I4           0x03
#define VT_LPSTR        0x1E
#define VT_FILETIME     0x40
#define VT_WMF			0x47

    typedef struct _PropValue {
	U32 vtType;
	union {
	    FILETIME vtTime;
	    S32 vtLong;
	    vtB vtBSTR;
	} vtValue;
    } PropValue;

/*String properties*/
#define PID_TITLE          0x02
#define PID_SUBJECT        0x03
#define PID_AUTHOR         0x04
#define PID_KEYWORDS       0x05
#define PID_COMMENTS       0x06
#define PID_TEMPLATE       0x07
#define PID_LASTAUTHOR     0x08
#define PID_REVNUMBER      0x09
#define PID_APPNAME        0x12

/*Time properties*/
#define PID_TOTAL_EDITTIME 0x0A
#define PID_LASTPRINTED    0x0B
#define PID_CREATED        0x0C
#define PID_LASTSAVED      0x0D

/*Long integer properties*/
#define PID_PAGECOUNT      0x0E
#define PID_WORDCOUNT      0x0F
#define PID_CHARCOUNT      0x10
#define PID_SECURITY       0x13

#define PID_THUMBNAIL	   0x11

/*bit masks for security long integer*/
#define AllSecurityFlagsEqNone         0x00
#define fSecurityPassworded            0x01
#define fSecurityRORecommended         0x02
#define fSecurityRO                    0x04
#define fSecurityLockedForAnnotations  0x08

    int wvGetProperty (PropValue * Prop, SummaryInfo * si, U32 pid);
    void wvReleaseProperty (PropValue * Prop);

    int wvSumInfoGetString (char *lpStr, U16 cbStr, U32 pid, SummaryInfo * si);
    int wvSumInfoGetLong (U32 * lpLong, U32 pid, SummaryInfo * si);
    int wvSumInfoGetTime (U16 * yr, U16 * mon, U16 * day, U16 * hr, U16 * min,
			  U16 * sec, U32 pid, SummaryInfo * si);
    int wvSumInfoGetPreview (char *lpStr, U16 cbStr, U32 pid, SummaryInfo * si);

    void wvGetRowTap (wvParseStruct * ps, PAP * dpap, U32 para_intervals,
		      BTE * btePapx, U32 * posPapx);
    void wvGetComplexRowTap (wvParseStruct * ps, PAP * dpap, U32 para_intervals,
			     BTE * btePapx, U32 * posPapx, U32 piececount);
    void wvGetFullTableInit (wvParseStruct * ps, U32 para_intervals,
			     BTE * btePapx, U32 * posPapx);
    void wvGetComplexFullTableInit (wvParseStruct * ps, U32 para_intervals,
				    BTE * btePapx, U32 * posPapx, U32 piece);


/*end of clean interface*/
















    struct tTLP {
	U16 itl;
	U8 fShading;
	U8 fColor;
	U8 fHdrRows;
	U8 fLastRow;
	U8 fHdrCols;
	U8 fLastCol;
    };

    typedef struct tTLP oTLP;

    struct ttablelook {
	/* TOP BAR
	   color for the top left entry
	   color for the otherwise odd top entries
	   color for the even top entries
	 */

	/* EVEN ROWS
	   color for the left entry
	   color for the otherwise odd row entries
	   color for the even entries
	 */

	/* ODD ROWS
	   color for the left entry
	   color for the otherwise odd row entries
	   color for the even entries
	 */

	char *color[9];
    };

    typedef struct ttablelook tablelook;



    struct tobj_by_spid {
	U16 spid;
	char *filename;
	struct tobj_by_spid *next;
    };

    typedef struct tobj_by_spid obj_by_spid;

    struct node {
	char streamname[255];
	char filename[PATH_MAX];
	struct node *next;
	int level;
    };

    typedef struct node olestream;

    struct tsep {
	U8 bkc;			/*break code */
	U8 fTitlePage;		/*want title page */
	U8 fAutoPgn;		/*unused */
	U8 nfcPgn;		/*page no format */
	U8 fUnlocked;		/*huh ? */
	U8 cnsPgn;		/*huh ? */
	U8 fPgnRestart;		/*restart pg numering */
	U8 fEndNote;		/*footnotes at end of sec or page */
	U8 lnc;			/*huh ? */
	U8 grpfIhdt;		/*not used */
	U8 nLnnMod;
	U16 ccolM1;
	U16 pgnStart;
	U32 xaPage;
	U32 yaPage;
	U32 dxaLeft;
	U32 dxaRight;
	U32 dyaTop;
	U32 dyaBottom;
	U32 dzaGutter;
    };

    typedef struct tsep sep;

    struct tchp {
	unsigned short istd;

	U32 fBold:1;
	U32 fItalic:1;
	U32 fRMarkDel:1;
	U32 fOutline:1;		/*not imp yet */
	U32 fFldVanish:1;	/*not imp yet, internal to word */
	U32 fSmallCaps:1;
	U32 fCaps:1;
	U32 fVanish:1;		/*not imp yet */
	U32 fRMark:1;		/*not imp yet */
	U32 fSpec:1;
	U32 fStrike:1;
	U32 fObj:1;		/*not imp yet */
	U32 fShadow:1;		/*not imp yet */
	U32 fLowerCase:1;	/*not imp yet */
	U32 fData:1;
	U32 fOle2:1;		/*not imp yet */
	U32 fEmboss:1;		/*not imp yet */
	U32 fImprint:1;		/*not imp yet */
	U32 fDStrike:1;
	U32 fUsePgsuSettings:1;	/*not imp yet, dont know what it means */
	U32 Reserved1:12;	/*unused */

	U32 Reserved2;		/*unused */

	U16 ftc;		/*not used in word 8 */
	U16 ftcAscii;
	U16 ftcFE;
	U16 ftcOther;

	U16 fontsize;		/*half points */
	U8 supersubscript;
	S16 fontcode;
	U16 fontspec;
	char color[8];
	U16 underline;
	U8 idctHint;
	U32 fcPic;

	U16 ibstRMark;
	U16 ibstRMarkDel;
	DTTM dttmRMark;
	DTTM dttmRMarkDel;

	U16 fPropRMark;
	U16 ibstPropRMark;
	DTTM dttmPropRMark;
	U8 sfxtText;
    };

    typedef struct tchp chp;

    struct tlist_def {
	U16 *list_string;
	int len;
	S16 begin_no;
	int no_type;
	int fPrev;
	U32 id;
	chp achp;
	struct tlist_def *sub_def_list;
    };

    typedef struct tlist_def list_def;

    struct ttap {
	oTLP tlp;
	int tablewidth;
	S16 cellwidth[65];
	int cell_no;
	int shade_no;
	int cell_backs[65];
	int cell_fronts[65];
	int cell_pattern[65];
	int rowheight;
    };

    typedef struct ttap tap;

    struct tpap {
	unsigned short istd;
	U8 fInTable;
	U8 fTtp;
	U8 tableflag;
	int justify;
	int ilvl;		/*list level, 0 to 8 */
	long ilfo;		/*list index */
	/*link to list information */
	list_def *list_data;
	ANLD anld;
	tap ourtap;
	S16 leftmargin;
	S16 rightmargin;
	S16 firstline;
	U32 brcBottom;
	U32 brcLeft;
	U32 brcRight;
	U32 brcBetween;
	U16 dxaWidth;
	U32 dyaBefore;
	U32 dyaAfter;
	char *begin;
	char *end;
	char *prespace;
	char *postspace;
    };

    typedef struct tpap pap;


    struct field_pro {
	U32 *cps;
	FLD *flds;
	U32 no;
    };

    typedef struct field_pro field_info;

    struct tlist_info {
	/*
	   now this is very hairy, i not sure how this is supposed to work
	   so lists are a bit tentitive, basically theres no many things you
	   *can* do with lists, but hopefully this will sort out whether they
	   are bulleted or enumerated, and ignore all sorts of shite like
	   what kind of bullet were talking about, and whether some list
	   items are numbered etc etc
	 */
	U8 *array;
	int count;
	int nooflsts;
	U32 *o_lst_ids;
	int **current_index_nos;
	list_def *o_list_def;
	U8 *level;

	U8 *lstarray;
	int lstcount;
	U32 nooflfos;
	U32 *lst_ids;
	list_def *a_list_def;
	int *overridecount;


	/*
	   temp placed here, will eventually replace the other rubbish
	 */
	LFO *lfo;
	U32 nolfo;
	LFOLVL *lfolvl;
	U32 nooflvl;
	LVL *lvl;

	LST *lst;
	U16 noofLST;
    };

    typedef struct tlist_info list_info;

    struct tsprm {
	U8 *list;
	struct tsprm *next;
	int len;
    };

    typedef struct tsprm tSprm;

    struct tcstyle {
	pap thepap;
	chp thechp;
	char *begin;
	char *end;
	char *name;
	char *prespace;
	char *postspace;
	char *Default;
	char *bold;
	char *italic;
	char *font;
	struct tcstyle *next;
    };

    typedef struct tcstyle config_style;

    struct _document_style {
	char *begin;
	int htwips;
	int vtwips;
	char *end;
    };

    typedef struct _document_style document_style;

    struct _element_style {
	char *begin;
	char *end;
    };

    typedef struct _element_style element_style;

    typedef enum {
	BOLD,
	ITALIC,
	FONT
    } ele_type;

    struct tstyle {
	/*temp put in hooks for new stylesheet */
	STSH stsh;
	pap thepap;
	chp thechp;
	char *name;
	char *begin;
	char *end;
	char *Default;
	char *prespace;
	char *postspace;
	char *font;
	char *bold;
	char *italic;
    };

    typedef struct tstyle style;

    struct tbookmark_limits {
	U32 bookmark_b_no;
	U32 *bookmark_b_cps;
	BKF *bookmark_b_bkfs;
	U32 bookmark_e_no;
	U32 *bookmark_e_cps;
    };

    typedef struct tbookmark_limits bookmark_limits;


    struct ttextportions {
	U32 fcMin;
	U32 fcMac;
	U32 ccpText;
	U32 ccpFtn;
	U32 ccpHdr;
	U32 ccpAtn;
	U32 ccpEdn;
	U32 fcPlcfhdd;
	U32 lcbPlcfhdd;
	U32 *headercplist;
	U8 headercpno;

	U32 fndref_no;
	U32 fndtxt_no;
	U32 *fndRef;
	FRD *fndFRD;
	U32 *fndTxt;
	int list_footnotes[256];
	int list_foot_no;
	int auto_foot;
	int last_foot;

	U32 endref_no;
	U32 endtxt_no;
	U32 *endRef;
	FRD *endFRD;
	S16 *endTrueFRD;
	U32 *endTxt;
	int list_endnotes[256];
	int list_end_no;
	int auto_end;

	U32 andref_no;
	U32 *andRef;
	U32 andtxt_no;
	U32 *andTxt;
	int list_annotations[256];
	int list_anno_no;
	Xst *authors;
	STTBF annotations;
	bookmark_limits a_bookmarks;
	ATRD *the_atrd;
	int last_anno;

	bookmark_limits l_bookmarks;
	STTBF bookmarks;

	/*
	   STTBF revisions;
	 */

	U32 *section_cps;
	SED *section_fcs;
	U32 section_nos;

	U32 noofficedraw;
	U32 *officedrawcps;
	FSPA *fspas;		/*im ignoring the rest of the FSPA for now */

	int noofblipdata;
	obj_by_spid *ablipdata;
    };

    typedef struct ttextportions textportions;

#define IGNORENUM 0
#define DONTIGNORENUM 1
#define IGNOREALL 2

    U32 read_32ubit (wvStream * in);
    U16 read_16ubit (wvStream * in);
    U8 read_8ubit (wvStream * in);

    U32 sread_32ubit (const U8 * in);
    U16 sread_16ubit (const U8 * in);
    U8 sread_8ubit (const U8 * in);

    U32 dread_32ubit (wvStream * in, U8 ** list);
    U16 dread_16ubit (wvStream * in, U8 ** list);
    U8 dread_8ubit (wvStream * in, U8 ** list);

    U32 bread_32ubit (U8 * in, U16 * pos);
    U16 bread_16ubit (U8 * in, U16 * pos);
    U8 bread_8ubit (U8 * in, U16 * pos);

/* Perform file-I/O-like operations on wvStreams. */
    U32 wvStream_read (void *ptr, size_t size, size_t nmemb, wvStream * stream);
    void wvStream_rewind (wvStream * stream);
    U32 wvStream_goto (wvStream * stream, long position);
    U32 wvStream_offset (wvStream * stream, long offset);
    U32 wvStream_offset_from_end (wvStream * stream, long offset);
    U32 wvStream_size (wvStream * stream);
    U32 wvStream_tell (wvStream * stream);

/* These functions take care of memory/file management for wvStreams */
    void wvStream_FILE_create (wvStream ** in, FILE * inner);
  wvStream * wvStream_TMP_create (size_t size);
    void wvStream_gsf_create (wvStream ** in, GsfInput * inner);
    void wvStream_memory_create (wvStream ** in, char *buf, size_t size);
    void wvStream_create (wvStream ** in, wvStreamKind kind,
			  wvInternalStream inner);
    U32 wvStream_close (wvStream * stream);

/* The above functions store all the streams we open in one of these, so that
 * we can clean up nicely.
 */
    struct twvStream_list {
	wvStream *stream;
	struct twvStream_list *next;
    };
    typedef struct twvStream_list wvStream_list;

    void external_wvReleasePAPX_FKP (void);
    void external_wvReleaseCHPX_FKP (void);

    void cleanupstreams (char *analyze, char *slashtmp);
    olestream *divide_streams (char *filename, char **analyze, char **slashtmp,
			       char *argv0);
    int decode_word8 (wvParseStruct * ps, int core);
    void get_table_info (wvStream * tablefd, list_info * a_list_info,
			 U32 fcSttbFnm, U32 lcbSttbFnm, U32 fcPlcfLst,
			 U32 lcbPlcfLst, U32 fcPlfLfo, U32 lcbPlfLfo,
			 style * sheet);

    pap *get_pap (U32 pageindex, wvStream * in, U32 charindex, U32 * nextfc,
		  style * sheet, list_info * a_list_info);
    chp *get_chp (U32 pageindex, wvStream * in, FILE * data, U32 charindex,
		  U32 * nextfc, style * sheet, U16 istd);
    sep *get_sep (U32 offset, wvStream * in);

    void decode_clx (U32 startpiece, U32 begincp, U32 endcp, wvStream * in,
		     FILE * main, FILE * data, U32 fcClx, U32 lcbClx,
		     U32 intervals, U32 chpintervals, U32 * plcfbtePapx,
		     U32 * plcfbteChpx, field_info * all_fields[5],
		     list_info * a_list_info, style * sheet,
		     textportions * portions, FFN_STTBF * ffn_sttbf,
		     int headfooterflag);
    void decode_clx_header (U32 * rgfc, sep * asep, int nopieces,
			    U32 startpiece, U32 begincp, U32 endcp,
			    wvStream * in, FILE * main, FILE * data, U32 fcClx,
			    U32 lcbClx, U32 intervals, U32 chpintervals,
			    U32 * plcfbtePapx, U32 * plcfbteChpx,
			    field_info * all_fields[5], list_info * a_list_info,
			    style * sheet, textportions * portions,
			    FFN_STTBF * ffn_sttbf, int headerfooterflag);
    void decode_clx_footer (U32 * rgfc, sep * asep, int nopieces,
			    U32 startpiece, U32 begincp, U32 endcp,
			    wvStream * in, FILE * main, FILE * data, U32 fcClx,
			    U32 lcbClx, U32 intervals, U32 chpintervals,
			    U32 * plcfbtePapx, U32 * plcfbteChpx,
			    field_info * all_fields[5], list_info * a_list_info,
			    style * sheet, textportions * portions,
			    FFN_STTBF * ffn_sttbf, int headerfooterflag);
    int decode_clx_endnote (U32 * rgfc, sep * asep, int nopieces,
			    U32 startpiece, U32 begincp, U32 endcp,
			    wvStream * in, FILE * main, FILE * data, U32 fcClx,
			    U32 lcbClx, U32 intervals, U32 chpintervals,
			    U32 * plcfbtePapx, U32 * plcfbteChpx,
			    field_info * all_fields[5], list_info * a_list_info,
			    style * sheet, textportions * portions,
			    FFN_STTBF * ffn_sttbf, int headerfooterflag);

    void decode_simple (wvStream * mafd, FILE * tablefd, FILE * data, U32 fcClx,
			U32 fcMin, U32 fcMac, U32 intervals, U32 chpintervals,
			U32 * plcfbtePapx, U32 * plcfbteChpx,
			field_info * all_fields[5], list_info * a_list_info,
			style * sheet, textportions * portions,
			FFN_STTBF * ffn_sttbf, int flag);
    int decode_simple_footer (wvStream * mafd, FILE * tablefd, FILE * data,
			      sep * asep, U32 fcClx, U32 fcMin, U32 fcMac,
			      U32 intervals, U32 chpintervals,
			      U32 * plcfbtePapx, U32 * plcfbteChpx,
			      field_info * all_fields[5],
			      list_info * a_list_info, style * sheet,
			      textportions * portions, FFN_STTBF * ffn_sttbf,
			      int flag);
    int decode_simple_endnote (wvStream * mafd, FILE * tablefd, FILE * data,
			       sep * asep, U32 fcClx, U32 fcMin, U32 fcMac,
			       U32 intervals, U32 chpintervals,
			       U32 * plcfbtePapx, U32 * plcfbteChpx,
			       field_info * all_fields[5],
			       list_info * a_list_info, style * sheet,
			       textportions * portions, FFN_STTBF * ffn_sttbf,
			       int flag);
    void decode_simple_header (wvStream * mafd, FILE * tablefd, FILE * data,
			       sep * asep, U32 fcClx, U32 fcMin, U32 fcMac,
			       U32 intervals, U32 chpintervals,
			       U32 * plcfbtePapx, U32 * plcfbteChpx,
			       field_info * all_fields[5],
			       list_info * a_list_info, style * sheet,
			       textportions * portions, FFN_STTBF * ffn_sttbf,
			       int flag);

    int decode_letter (int letter, int flag, pap * apap, chp * achp,
		       field_info * magic_fields, wvStream * main, FILE * data,
		       FFN_STTBF * ffn_sttbf, list_info * a_list_info,
		       textportions * portions, int *issection, style * sheet);
    void get_next_f_ref (textportions * portions, signed long *nextfootnote);
    void get_next_e_ref (textportions * portions, signed long *nextendnote);

    void decode_s_specials (pap * apap, chp * achp, list_info * a_list_info);
    int decode_s_table (pap * apap, chp * achp, list_info * a_list_info,
			int silent);
    void decode_e_specials (pap * apap, chp * achp, list_info * a_list_info);
    int decode_e_table (pap * apap, chp * achp, list_info * a_list_info,
			int silent);

    void decode_s_chp (chp * achp, FFN_STTBF *, style *);
    void decode_e_chp (chp * achp);

    void chpsoff (void);
    void chpson (void);

    void decode_list_nfc (int value, int no_type);
    void decode_list_level (pap * apap, int inalist, int num);

    int flushbreaks (int);

    void decode_s_anld (pap * apap, chp * achp, list_info * a_list_info,
			FFN_STTBF * ffn_sttbf, style * sheet);
    void decode_s_list (pap * apap, chp * achp, list_info * a_list_info,
			FFN_STTBF * ffn_sttbf, int num, style * sheet);
    void decode_e_list (pap * apap, chp * achp, list_info * a_list_info);

    void decode_field (wvStream * main, field_info * magic_fields, long *cp,
		       U8 * fieldwas, unsigned long *swallowcp1,
		       unsigned long *swallowcp2);

    int find_FKPno_papx (U32 fc, U32 * plcfbtePapx, U32 intervals);
    int find_FKPno_chpx (U32 fc, U32 * plcfbteChpx, U32 intervals);
    U32 find_FC_sepx (U32 cp, U32 * sepcp, textportions * portions);
    U32 find_next_smallest_fc (U32 charindex, U32 pageindex, wvStream * in,
			       S16 * location, long *pos);
    U32 find_next_biggest_fc (U32 charindex, U32 pageindex, wvStream * in,
			      U16 * location, long *pos);
    U32 find_next_biggest_orequal_fc (U32 charindex, U32 pageindex,
				      wvStream * in, U16 * location, long *pos);

    pap *get_complex_pap (U32 fc, U32 * plcfbtePapx, U16 i, U16 nopieces,
			  U32 intervals, U32 * rgfc, wvStream * main,
			  U32 * avalrgfc, U32 * thenextone, U32 * paraendfc,
			  int *paraendpiece, style * sheet,
			  list_info * a_list_info);
    chp *get_complex_chp (U32 fc, U32 * plcfbteChpx, U16 i, U16 nopieces,
			  U32 chpintervals, U32 * rgfc, wvStream * main,
			  U32 * avalrgfc, U32 * thenextone, style * sheet,
			  U16 istd);

#if 0
    void decode_gpprls (pap * apap, chp * achp, sep * asep, U16 * gpprlindex,
			int index, tSprm * sprmlists, style * sheet);
#endif

    style *decode_stylesheet (wvStream * tablefd, U32 stsh, U32 stshlen,
			      config_style * in_style);
    void fill_pap (style * stylelist, int m, int b);

    void decode_sprm (FILE * in, U16 clist, pap * retpap, chp * retchp,
		      sep * retsep, U16 * pos, U8 ** list, style * sheet,
		      U16 istd);

    void error (FILE * stream, char *fmt, ...);
    void oprintf (int silentflag, char *fmt, ...);

    int decode_symbol (U16 fontspec);
    char *symbolfontdir (void);

    int decode_wingding (U16 fontspec);
    char *wingdingfontdir (void);

    char *patterndir (void);

    void decode_header (U32 * begin, U32 * len, textportions * portions,
			sep * asep);
    void decode_header2 (U32 * begin, U32 * len, textportions * portions);
    void decode_footer (U32 * begin, U32 * len, textportions * portions,
			sep * asep);
    void decode_footnote (U32 * begin, U32 * len, textportions * portions,
			  int i);
    void decode_endnote (U32 * begin, U32 * len, textportions * portions,
			 int i);
    void decode_footanno (U32 * begin, U32 * len, textportions * portions,
			  int i);

    int find_piece_cp (U32 sepcp, U32 * rgfc, int nopieces);

    obj_by_spid *get_blips (U32 fcDggInfo, U32 lcbDggInfo, wvStream * tablefd,
			    FILE * mafd, int *noofblips, int streamtype,
			    obj_by_spid ** realhead);
    void output_draw (U32 cp, textportions * portions);

    void do_indent (pap * apap);

    U32 get_fc_from_cp (U32 acp, U32 * rgfc, U32 * avalrgfc, int nopieces);

    void end_para (pap * apap, pap * newpap);

/*
returns slot to use in index array which keeps track of how far each list
has got
*/
    int decode_ilfo (pap * retpap, chp * achp, list_info * a_list_info,
		     style * sheet, FFN_STTBF * ffn_sttbf);

    void init_chp (chp * achp);
    void init_pap (pap * apap);

/*result += modified - blank*/
    void merge_chps (chp * blank, chp * modified, chp * result);

    void init_chp_from_istd (U16 istd, style * sheet, chp * retchp);
    void init_pap_from_istd (U16 istd, style * sheet, pap * retpap);

    void get_para_bounds (int currentpiece, U32 fc, U32 * rgfc, U32 * avalrgfc,
			  int nopieces, U32 * plcfbtePapx, U32 intervals,
			  wvStream * main);

    char *ms_strlower (char *in);

/* returns
0 for no error
1 for file doesn't exist
2 if it isnt an ole file
3 if its corrupt
*/
    int wvOLEDecode (wvParseStruct * ps,
		     char *path, wvStream ** mafd, wvStream ** tablefd0,
		     wvStream ** tablefd1, wvStream ** data,
		     wvStream ** summary);
    int wvOLESummaryStream (char *filename, wvStream ** summary);

    long get_picture_header (U32 fcPic, wvStream * data, U32 * len,
			     U16 * datatype);

    void cleanupglobals (void);
    char *ms_basename (char *filename);
    void outputimgsrc (char *filename, int width, int height);


    U32 decode_b_bookmark (bookmark_limits * l_bookmarks, STTBF * bookmarks);
    U32 decode_e_bookmark (bookmark_limits * l_bookmarks);

    void output_tablebg (pap * apap);
    int do_tablelooks (pap * apap);

    int setdecom (void);

    void pagebreak (void);
    void columnbreak (void);
    void sectionbreak (sep * asep);
    void copy_tap (tap * rettap, tap * intap);
    void check_auto_color (chp * achp);

    void extract_bookm_limits (bookmark_limits * l_bookmarks,
			       wvStream * tablefd, U32 fcPlcfbkf,
			       U32 lcbPlcfbkf, U32 fcPlcfbkl, U32 lcbPlcfbkl);

    int use_fontfacequery (chp * achp);

    char *notoday (int no);

    void convertwmf (char *filename);

    int Parse (wvStream * in, config_style ** in_style,
	       document_style ** doc_style, element_style * ele_style);
    int do_output_start (U32 * avalrgfc, int nopieces,
			 document_style * doc_style);
    void do_output_end (document_style * doc_style, int core, int tail);
    char *argument (void);

    void fill_table_info (pap * apap, U32 tapfc1, U32 * plcfbtePapx,
			  U32 intervals, wvStream * mafd, style * sheet,
			  list_info * a_list_info);

    char *expand_variables (char *in, pap * apap);
    char *expand_element (char *in, char *fontface, char *color, char *size);
    void init_sep (sep * asep);
    char *get_image_prefix (void);

    int add_t (int **vals, S16 * p, int plen);
    int gcf (int high, int low);
    int gcf_list (int *vals, int cno);
    int allowedfont (style * sheet, U16 istd);

#define NOOFIDS 8

/*interim*/
    U32 wvGetSPIDfromCP (U32 cp, textportions * portions);
    void oldwvGetPICF (PICF * apicf, wvStream * fd, U32 offset);

/* have to have pap replaced with PAP, and change the text output code to the new ones, whenever they are ready*/
    void wvGetListInfo (pap * apap, chp * achp, LFO * lfo, LFOLVL * lfolvl,
			LVL * lvl, U32 nolfo, LST * lst, U16 noofLST,
			style * sheet, FFN_STTBF * ffn_sttbf);
/* have to have pap replaced with PAP*/
    void wvAddPAP_FromBucket (pap * pap, U8 * pointer8, U16 len, style * sheet);

/*we have to replace chp with CHP*/
    void wvAddCHP_FromBucket (chp * achp, U8 * pointer8, U16 len,
			      style * sheet);
    void twvCopyCHP (chp * dest, chp * src);

    void wvSetEntityConverter (expand_data * data);

	int wvIsBidiDocument(wvParseStruct * ps);

	int wvGetPLCF (void ** plcf, U32 offset, U32 len, wvStream * fd);
	
/* & finally */
    extern const char* wv_version;
	
#ifdef __cplusplus
}
#endif
#endif
