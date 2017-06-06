#ifndef _DEFAULTFONT_H_
#define _DEFAULTFONT_H_

# ifdef HAVE_X11_LOCALE_H
#  include <X11/Xlocale.h>
# else
#  include <locale.h>
# endif

#ifdef HAVE_NL_LANGINFO
# include <langinfo.h>
#endif


extern void eterm_default_font_locale(char ***fonts, char ***mfonts, char **mencoding, int *def_idx);

/*
 * List of encoding labels.
 * Note "encoding" is not "character set" nor "encoding method".
 *
 * In Eterm, "encoding" is implemented as a pair of "encoding method"
 * (implemented as ENC_METHOD in screen.h) and font specification,
 * i.e., defaultfont[] in this file.
 *
 * This type is used only in this file.
 */

enum enc_label {
  ENC_SJIS, ENC_EUCJ, ENC_GB, ENC_BIG5, ENC_EUCKR,
  ENC_ISO8859_1, ENC_ISO8859_2, ENC_ISO8859_3, ENC_ISO8859_4,
  ENC_ISO8859_5, ENC_ISO8859_6, ENC_ISO8859_7, ENC_ISO8859_8,
  ENC_ISO8859_9, ENC_ISO8859_10, ENC_ISO8859_11, ENC_ISO8859_12,
  ENC_ISO8859_13, ENC_ISO8859_14, ENC_ISO8859_15,
  ENC_KOI8R, ENC_KOI8U, ENC_UTF8,
  /* ENC_VISCII, ENC_VSCII, ENC_TIS620, ENC_UTF8, */
  ENC_DUMMY
};

#define ENC_ISO8859_LAST ENC_ISO8859_15


/*
 * Used for tables of locale/encoding names -> encodng labels.
 */

struct name2encoding {
  const char     *name;
  const enum enc_label encoding;
};

/*
 * Used for tables of encoding labels -> Eterm internal informations.
 *
 */

struct defaultfont {
  const enum enc_label enc_label;
  const char     *encoding_method;
  const int      def_idx; 
  const char     *font[NFONTS];
  const char     *mfont[NFONTS];
};


/*
 *****************************************************************************
 * FONT DEFINITIONS
 *****************************************************************************
 */

/*
 *  FONTS TO BE USED
 *
 * ----------------------------------------------------------------------
 * font name       font package name      available from
 * ----------------------------------------------------------------------
 * k12             shinonome
 *                        http://www.on.cs.keio.ac.jp/~yasu/jp_fonts.html
 * k18             intlfonts              ftp.gnu.org/pub/gnu
 * -*-ming-medium-r-normal-*-big5-0
 *                 cmexfonts
 *                            http://metalab.unc.edu/pub/Linux/X11/fonts/
 * -*-gulim-medium-r-normal-*-ksc5601.1987-0
 *                 baekmuk                ftp.mizi.co.kr/pub/baekmuk/
 * -misc-grfixed-medium-r-*-iso8859-7
 *                 hrnet&hcr
 *       http://www.hellug.gr/gr/howto/howto/pub/html/Hellenic-HOWTO.html
 * -cronyx-fixed-medium-r-*-koi8-u
 *                 Xcyr                   http://sawsoft.newmail.ru/LS/
 * all others      XFree86                http://www.xfree86.org/
 * ----------------------------------------------------------------------
 */

#define DEF_DUMMY DEF_10646
#define NFONT_LIST_NULL NFONT_LIST_10646
#define MFONT_LIST_NULL MFONT_LIST_10646

#define DEF_EUCJ 1
#define NFONT_LIST_EUCJ "6x12","7x14", "8x16", "9x18", "12x24"
#define MFONT_LIST_EUCJ "k12", "k14", "kanji16", "k18", "kanji24"

#define DEF_GB 0
#define NFONT_LIST_GB   "8x16", "12x24", "", "", ""
#define MFONT_LIST_GB \
  "-isas-song ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0", \
  "-isas-song ti-medium-r-normal--24-240-72-72-c-240-gb2312.1980-0", \
  "", "", ""

#define DEF_BIG5 0
#define NFONT_LIST_BIG5 "8x16", "9x18", "10x20", "12x24", "13x26"
#define MFONT_LIST_BIG5 \
  "-*-ming-medium-r-normal-*-16-*-*-*-c-*-big5-0",\
  "-*-ming-medium-r-normal-*-18-*-*-*-c-*-big5-0",\
  "-*-ming-medium-r-normal-*-20-*-*-*-c-*-big5-0",\
  "-*-ming-medium-r-normal-*-24-*-*-*-c-*-big5-0",\
  "-*-ming-medium-r-normal-*-26-*-*-*-c-*-big5-0"

#define DEF_EUCKR 1
#define NFONT_LIST_EUCKR "7x14", "8x16", "9x18", "10x20", "12x24"
#define MFONT_LIST_EUCKR \
  "-*-gulim-medium-r-normal--14-*-*-*-*-140-ksc5601.1987-0",\
  "-*-gulim-medium-r-normal--16-*-*-*-*-160-ksc5601.1987-0",\
  "-*-gulim-medium-r-normal--18-*-*-*-*-180-ksc5601.1987-0",\
  "-*-gulim-medium-r-normal--20-*-*-*-*-200-ksc5601.1987-0",\
  "-*-gulim-medium-r-normal--24-*-*-*-*-240-ksc5601.1987-0"

#define DEF_7 1
#define NFONT_LIST_7 \
  "-misc-grfixed-medium-r-semicondensed--10-100-75-75-c-60-iso8859-7",\
  "-misc-grfixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-7",\
  "-misc-grfixed-medium-r-normal--14-110-75-75-c-75-iso8859-7",\
  "-misc-grfixed-medium-r-normal--16-120-75-75-c-75-iso8859-7",\
  "-misc-grfixed-medium-r-normal--25-190-75-75-c-90-iso8859-7"

#define DEF_KOI8R 1
#define NFONT_LIST_KOI8R \
  "-cronyx-fixed-medium-r-normal--10-100-75-75-c-60-koi8-r",\
  "-cronyx-fixed-medium-r-semicondensed--13-120-75-75-c-60-koi8-r",\
  "-cronyx-fixed-medium-r-normal--13-120-75-75-c-80-koi8-r",\
  "-cronyx-fixed-medium-r-normal--15-140-75-75-c-90-koi8-r",\
  "-cronyx-fixed-medium-r-normal--20-200-75-75-c-100-koi8-r"

#define DEF_KOI8U 1
#define NFONT_LIST_KOI8U \
  "-cronyx-fixed-medium-r-normal--10-100-75-75-c-60-koi8-u",\
  "-cronyx-fixed-medium-r-semicondensed--13-120-75-75-c-60-koi8-u",\
  "-cronyx-fixed-medium-r-normal--13-120-75-75-c-80-koi8-u",\
  "-cronyx-fixed-medium-r-normal--14-130-75-75-c-70-koi8-u",\
  "-cronyx-fixed-medium-r-normal--15-140-75-75-c-90-koi8-u"

#define DEF_10646 2
#define NFONT_LIST_10646 "5x7", "6x10", "fixed", "8x13", "9x15"
#define MFONT_LIST_10646 \
  "-misc-fixed-medium-r-normal--7-*-*-*-c-*-iso10646-1",\
  "-misc-fixed-medium-r-normal--10-*-*-*-c-*-iso10646-1",\
  "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-*-iso10646-1",\
  "-misc-fixed-medium-r-normal--14-*-*-*-c-*-iso10646-1",\
  "-misc-fixed-medium-r-normal--15-*-*-*-c-*-iso10646-1"

/* special common rule for ISO-8859-* */

#define DEF_8859 2
#define NFONT_LIST_ISO8859X \
  "-misc-fixed-medium-r-normal--7-*-*-*-c-*-iso8859-%d",\
  "-misc-fixed-medium-r-normal--10-*-*-*-c-*-iso8859-%d",\
  "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-*-iso8859-%d",\
  "-misc-fixed-medium-r-normal--13-*-*-*-c-*-iso8859-%d",\
  "-misc-fixed-medium-r-normal--18-*-*-*-c-*-iso8859-%d"


#endif /* _DEFAULTFONT_H_ */

