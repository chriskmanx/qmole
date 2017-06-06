/*
 * Setting default font and encoding according to user's locale (LC_CTYPE).
 */

#include "config.h"
#include "feature.h"

#include "font.h"
#include "defaultfont.h"

#define ENCODINGBUFLEN 100

#ifdef AUTO_ENCODING

/*
 * Table to convert from encoding names to enc_label.
 * This table is useful to normalize encoding names
 * and support various return value from nl_langinfo(3).
 *
 * The encoding names here are "truncated" names,
 * where all alphabets are uppercase and all '-' and
 * '_' are eliminated.
 */
const struct name2encoding n2e[] = {
#ifdef MULTI_CHARSET
    {"EUCJP", ENC_EUCJ},
    {"UJIS", ENC_EUCJ},
    {"SHIFTJIS", ENC_SJIS},
    {"SJIS", ENC_SJIS},
    {"EUCKR", ENC_EUCKR},
    {"EUCCN", ENC_GB},
    {"GB2312", ENC_GB},
    {"GB", ENC_GB},
    {"BIG5", ENC_BIG5},
    {"BIGFIVE", ENC_BIG5},
    {"BIG5HKSCS", ENC_BIG5},
    {"UTF8", ENC_UTF8},
#endif /* MULTI_CHARSET */
    {"KOI8R", ENC_KOI8R},
    {"KOI8U", ENC_KOI8U},
    {"ISO88591", ENC_ISO8859_1},
    {"ISO88592", ENC_ISO8859_2},
    {"ISO88593", ENC_ISO8859_3},
    {"ISO88594", ENC_ISO8859_4},
    {"ISO88595", ENC_ISO8859_5},
    {"ISO88596", ENC_ISO8859_6},
    {"ISO88597", ENC_ISO8859_7},
    {"ISO88598", ENC_ISO8859_8},
    {"ISO88599", ENC_ISO8859_9},
    {"ISO885910", ENC_ISO8859_10},
    {"ISO885911", ENC_ISO8859_11},
    {"ISO885912", ENC_ISO8859_12},
    {"ISO885913", ENC_ISO8859_13},
    {"ISO885914", ENC_ISO8859_14},
    {"ISO885915", ENC_ISO8859_15},
    {NULL, ENC_DUMMY}
};


/*
 * This table converts from locale names to enc_label.
 *
 * This table is used to know which encoding is used
 * as the default in the current user environment
 * (LC_CTYPE locale), since it is the standard way
 * for users to specify encoding by LANG/LC_CTYPE/LC_ALL
 * variables (i.e., LC_CTYPE locale).  Consult locale(7).
 *
 * This table is used when nl_langinfo(3) is not available
 * or it fails.
 *
 * locale names whose "encoding" part are listed in n2e[]
 * can be omitted here, because "encoding" part is checked
 * separately before l2e[] check.
 *
 * Note that longer locale names must be written earlier
 * than shorter locale names in this table, because
 * strncmp(3) is used for seek for this table.
 */
const struct name2encoding l2e[] = {
#ifdef MULTI_CHARSET
    {"ja_JP.EUC", ENC_EUCJ},
    {"ja_JP", ENC_EUCJ},
    {"ko_KR.EUC", ENC_EUCKR},
    {"ko_KR", ENC_EUCKR},
    {"zh_CN.EUC", ENC_GB},
    {"zh_CN", ENC_GB},
    {"zh_TW", ENC_BIG5},
#endif /* MULTI_CHARSET */
    {"da", ENC_ISO8859_1},
    {"de", ENC_ISO8859_1},
    {"en", ENC_ISO8859_1},
    {"fi", ENC_ISO8859_1},
    {"fr", ENC_ISO8859_1},
    {"is", ENC_ISO8859_1},
    {"it", ENC_ISO8859_1},
    {"la", ENC_ISO8859_1},
    {"lt", ENC_ISO8859_1},
    {"nl", ENC_ISO8859_1},
    {"no", ENC_ISO8859_1},
    {"pt", ENC_ISO8859_1},
    {"sv", ENC_ISO8859_1},
    {"cs", ENC_ISO8859_2},
    {"hr", ENC_ISO8859_2},
    {"hu", ENC_ISO8859_2},
    {"la", ENC_ISO8859_2},
    {"lt", ENC_ISO8859_2},
    {"pl", ENC_ISO8859_2},
    {"ro", ENC_ISO8859_2},
    {"sk", ENC_ISO8859_2},
    {"sl", ENC_ISO8859_2},
    {"ar", ENC_ISO8859_6},
    {"el", ENC_ISO8859_7},
    {"tr", ENC_ISO8859_9},
    {"lt", ENC_ISO8859_13},
    {"lv", ENC_ISO8859_13},
    {"mi", ENC_ISO8859_13},
    {"ru", ENC_KOI8R},          /* ISO8859-5 ? */
    {"uk", ENC_KOI8U},
#if 0
    {"vi", ENC_VISCII},
    {"th", ENC_TIS620},
#endif
    {NULL, ENC_DUMMY}
};


/*
 *  Default font name for each language.
 *  I'd like these names edited by native speakers.
 *
 *  enc_label   -->   ENCODING_METHOD and font informations
 *                    which as needed for Eterm to work.
 */
const struct defaultfont defaultfont[] = {
#ifdef MULTI_CHARSET
    {ENC_EUCJ, "eucj", DEF_EUCJ, {NFONT_LIST_EUCJ}, {MFONT_LIST_EUCJ}},
    {ENC_SJIS, "sjis", DEF_EUCJ, {NFONT_LIST_EUCJ}, {MFONT_LIST_EUCJ}},
    {ENC_GB, "gb", DEF_GB, {NFONT_LIST_GB}, {MFONT_LIST_GB}},
    {ENC_BIG5, "big5", DEF_BIG5, {NFONT_LIST_BIG5}, {MFONT_LIST_BIG5}},
    {ENC_EUCKR, "euckr", DEF_EUCKR, {NFONT_LIST_EUCKR}, {MFONT_LIST_EUCKR}},
    {ENC_UTF8, "iso-10646", DEF_10646, {NFONT_LIST_10646}, {MFONT_LIST_10646}},
#endif /* MULTI_CHARSET */
    {ENC_ISO8859_7, "none", DEF_7, {NFONT_LIST_7}, {MFONT_LIST_NULL}},
    {ENC_KOI8R, "none", DEF_KOI8R, {NFONT_LIST_KOI8R}, {MFONT_LIST_NULL}},
    {ENC_KOI8U, "none", DEF_KOI8U, {NFONT_LIST_KOI8U}, {MFONT_LIST_NULL}},
    {ENC_DUMMY, "none", DEF_DUMMY, {MFONT_LIST_NULL}, {MFONT_LIST_NULL}}
};

/* special common rule for ISO-8859-x */
const char *const defaultfont_8859[] = {
    NFONT_LIST_ISO8859X
};

/* fallback defaults */
const int def_def_idx = DEF_10646;

const char *const def_fonts[] = {
    NFONT_LIST_10646
};

#ifdef MULTI_CHARSET
const char *const def_mfonts[] = {
    MFONT_LIST_10646
};
#endif

/*----------------------------------------------------------------------*/
/* EXTPROTO */
void
eterm_default_font_locale(char ***fonts, char ***mfonts, char **mencoding, int *def_idx)
{
    char *locale;
    char *encoding_str = NULL;
    char encoding_buf[ENCODINGBUFLEN];
    char *p, *p2;
    enum enc_label encoding = ENC_DUMMY;
    int j, k;

    locale = setlocale(LC_CTYPE, "");
    if (!locale)
        if (!(locale = getenv("LC_ALL")))
            if (!(locale = getenv("LC_CTYPE")))
                if (!(locale = getenv("LANG")))
                    locale = "C";       /* failsafe */

    /* Obtain a "normalized" name of current encoding.
     * The current encoding is available via nl_langinfo().
     * Otherwise, it comes from locale name.
     */
#if defined(HAVE_NL_LANGINFO) && defined(CODESET)
    encoding_str = nl_langinfo(CODESET);
#else
    encoding_str = NULL;
#endif
    if (encoding_str && *encoding_str) {
        for (j = 0; n2e[j].name; j++) {
            if (!strcmp(encoding_str, n2e[j].name)) {
                encoding = n2e[j].encoding;
                break;
            }
        }
    }

    if (encoding == ENC_DUMMY) {
        p = strchr(locale, '.');
        if (p) {
            strncpy(encoding_buf, p + 1, ENCODINGBUFLEN);
            p = strchr(encoding_buf, '@');
            if (p)
                *p = 0;
        } else {
            strncpy(encoding_buf, locale, ENCODINGBUFLEN);
        }
        encoding_buf[ENCODINGBUFLEN - 1] = 0;
        for (p = p2 = encoding_buf; 1; p++, p2++) {
            while (*p2 == '_' || *p2 == '-')
                p2++;
            if (!*p2)
                break;
            *p = toupper(*p2);
        }
        *p = 0;
        for (j = 0; n2e[j].name; j++) {
            if (!strcmp(encoding_buf, n2e[j].name)) {
                encoding = n2e[j].encoding;
                break;
            }
        }
    }

    /* If the conversion fails, try using "language"/"country"
     * part of the locale name.
     */
    if (encoding == ENC_DUMMY) {
        for (j = 0; l2e[j].name; j++) {
            if (!strncmp(locale, l2e[j].name, strlen(l2e[j].name))) {
                encoding = l2e[j].encoding;
                break;
            }
        }
    }

    /* Now, the encoding to be used has been determined.
     * Fonts and encoding will be determined according to the encoding.
     */
    for (j = 0; defaultfont[j].enc_label != ENC_DUMMY; j++) {
        if (encoding == defaultfont[j].enc_label) {
            *def_idx = defaultfont[j].def_idx;
#ifdef MULTI_CHARSET
            *mencoding = STRDUP(defaultfont[j].encoding_method);
#endif
            for (k = 0; k < NFONTS; k++) {
                eterm_font_add(fonts, defaultfont[j].font[k], k);
#ifdef MULTI_CHARSET
                eterm_font_add(mfonts, defaultfont[j].mfont[k], k);
#endif
            }
            return;
        }
    }

/*
 * fallback for unknown encodings.  ISO-8559-* gets special treatment
 */

#ifdef MULTI_CHARSET
    *mencoding = STRDUP("none");
#endif
    if (encoding >= ENC_ISO8859_1 && encoding <= ENC_ISO8859_LAST) {
        /* fallback for ISO-8859-* encodings */
        k = encoding - ENC_ISO8859_1 + 1;
        *def_idx = DEF_8859;
    } else {
        /* fallback for "C", "POSIX", and invalid locales */
        k = 0;
        *def_idx = def_def_idx;
    }

    for (j = 0; j < NFONTS; j++) {
        if (k == 0)
            eterm_font_add(fonts, def_fonts[j], j);
        else {
            /* couple of wasted bytes each but lots of future expansion */
            sprintf(encoding_buf, defaultfont_8859[j], k);
            eterm_font_add(fonts, encoding_buf, j);
        }
#ifdef MULTI_CHARSET
        eterm_font_add(mfonts, def_mfonts[j], j);
#endif
    }
}
#endif /* AUTO_ENCODING */
