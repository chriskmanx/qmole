/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/**
 * @file strings.c
 * String Utility Routine Source File
 *
 * This file contains string utility functions.
 *
 * @author Michael Jennings <mej@eterm.org>
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: strings.c,v 1.25 2005/03/07 22:29:07 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

#if !(HAVE_MEMMEM)
/* Find first occurance of bytestring needle of size needlelen in memory region
   haystack of size haystacklen */
void *
memmem(const void *haystack, register size_t haystacklen, const void *needle, register size_t needlelen)
{
    register char *hs = (char *) haystack;
    register char *n = (char *) needle;
    register unsigned long i;
    register size_t len = haystacklen - needlelen;

    REQUIRE_RVAL(needle != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(haystack != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(needlelen > 0, SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(haystacklen > 0, SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(haystacklen > needlelen, SPIF_NULL_TYPE(ptr));
    for (i = 0; i < len; i++) {
        if (!memcmp(hs + i, n, needlelen)) {
            return (hs + i);
        }
    }
    return (NULL);
}
#endif

#if !(HAVE_STRNLEN)
size_t
strnlen(register const char *s, size_t maxlen)
{
    register size_t n;

    REQUIRE_RVAL(s, SPIF_CAST_C(size_t) 0);
    for (n = 0; *s && n < maxlen; s++, n++);
    return n;
}
#endif

#if !(HAVE_USLEEP)
void
usleep(unsigned long usec)
{
    struct timeval delay;

    delay.tv_sec = 0;
    delay.tv_usec = usec;
    select(0, NULL, NULL, NULL, &delay);

}

#endif

/***** Not needed ******
#if !(HAVE_NANOSLEEP)
__inline__ void
nanosleep(unsigned long nsec) {
    usleep(nsec / 1000);
}
#endif
************************/

#if !(HAVE_STRCASESTR)
char *
strcasestr(const char *haystack, register const char *needle)
{
    register const spif_uchar_t *t;
    register size_t len;

    REQUIRE_RVAL(needle != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(haystack != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    len = strlen(needle);
    for (t = SPIF_CAST_PTR(uchar) haystack; t && *t; t++) {
        if (!strncasecmp(t, needle, len)) {
            return ((char *) t);
        }
    }
    return (NULL);
}
#endif

#if !(HAVE_STRCASECHR)
char *
strcasechr(const char *haystack, register const char needle)
{
    register const char *t;

    REQUIRE_RVAL(haystack != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (t = haystack; t && *t; t++) {
        if (tolower(*t) == tolower(needle)) {
            return ((char *) t);
        }
    }
    return (NULL);
}
#endif

#if !(HAVE_STRCASEPBRK)
char *
strcasepbrk(const char *haystack, register const char *needle)
{
    register const char *t;

    REQUIRE_RVAL(needle != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(haystack != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (t = haystack; t && *t; t++) {
        if (strcasechr(needle, *t)) {
            return ((char *) t);
        }
    }
    return (NULL);
}
#endif

#if !(HAVE_STRREV)
char *
strrev(register char *str)
{
    register int i, j;

    REQUIRE_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    i = strlen(str);
    for (j = 0, i--; i > j; i--, j++) {
        (void) SWAP(str[j], str[i]);
    }
    return (str);

}
#endif

#if !(HAVE_STRSEP)
char *
strsep(char **str, register char *sep)
{
    register char *s = *str;
    char *sptr;

    REQUIRE_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(sep != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    D_STRINGS(("strsep(%s, %s) called.\n", *str, sep));
    sptr = s;
    for (; *s && !strchr(sep, *s); s++);
    if (!*s) {
        if (s != sptr) {
            *str = s;
            D_STRINGS(("Reached end of string with token \"%s\" in buffer\n", sptr));
            return (sptr);
        } else {
            D_STRINGS(("Reached end of string\n"));
            return ((char *) NULL);
        }
    }
    *s = 0;
    *str = s + 1;
    D_STRINGS(("Got token \"%s\", *str == \"%s\"\n", sptr, *str));
    return (sptr);
}
#endif

/**
 * Safer strncpy() with no NUL padding or wasted calculations.
 */
spif_bool_t
spiftool_safe_strncpy(spif_charptr_t dest, const spif_charptr_t src, spif_int32_t size)
{
    spif_char_t c;
    spif_charptr_t s = src, pbuff = dest;
    spif_charptr_t max_pbuff = dest + size - 1;

    ASSERT_RVAL(!SPIF_PTR_ISNULL(dest), FALSE);
    REQUIRE_RVAL(!SPIF_PTR_ISNULL(src), FALSE);
    REQUIRE_RVAL(size > 0, FALSE);

    for (; (c = *s) && (pbuff < max_pbuff); s++, pbuff++) {
        *pbuff = c;
    }
    *pbuff = 0;
    return ((c == 0) ? (TRUE) : (FALSE));
}

/**
 * Variant of strncat() which uses the safe strncpy() replacement above.
 */
spif_bool_t
spiftool_safe_strncat(spif_charptr_t dest, const spif_charptr_t src, spif_int32_t size)
{
    spif_int32_t len;

    ASSERT_RVAL(!SPIF_PTR_ISNULL(dest), FALSE);
    REQUIRE_RVAL(!SPIF_PTR_ISNULL(src), FALSE);
    REQUIRE_RVAL(size > 0, FALSE);

    len = strnlen(SPIF_CHARPTR_C(dest), size);
    if (len >= size) {
        return FALSE;
    } else {
        return spiftool_safe_strncpy(dest + len, src, size - len);
    }
}

/**
 * Returns a portion of a larger string.
 */
spif_charptr_t
spiftool_substr(spif_charptr_t str, spif_int32_t idx, spif_int32_t cnt)
{
    spif_charptr_t newstr;
    spif_uint32_t start_pos, char_count;
    spif_uint32_t len;

    REQUIRE_RVAL(str != SPIF_NULL_TYPE(charptr), SPIF_NULL_TYPE(charptr));

    len = SPIF_CAST(uint32) strlen(SPIF_CHARPTR_C(str));

    if (idx < 0) {
        start_pos = len + idx;
    } else {
        start_pos = idx;
    }
    REQUIRE_RVAL(start_pos < len, SPIF_NULL_TYPE(charptr));

    if (cnt <= 0) {
        char_count = len - start_pos + cnt;
    } else {
        char_count = cnt;
    }
    UPPER_BOUND(char_count, len - start_pos);

    newstr = SPIF_CAST(charptr) MALLOC(char_count + 1);
    memcpy(newstr, str + start_pos, char_count);
    newstr[char_count] = 0;
    return newstr;
}

#if HAVE_REGEX_H
/**
 * Compare a string to a regular expression.
 */
spif_bool_t
spiftool_regexp_match(const spif_charptr_t str, const spif_charptr_t pattern)
{
    static regex_t *rexp = NULL;
    register int result;
    char errbuf[256];

    if (!str) {
        /* If we're passed a NULL str, we want to free our static storage. */
        FREE(rexp);
        return FALSE;
    } else if (!rexp) {
        /* If we don't have static storage yet, make some. */
        rexp = (regex_t *) MALLOC(sizeof(regex_t));
    }

    if (pattern) {
        /* We have a pattern, so we need to compile it. */
        if ((result = regcomp(rexp, pattern, REG_EXTENDED)) != 0) {
            regerror(result, rexp, errbuf, 256);
            libast_print_error("Unable to compile regexp %s -- %s.\n", pattern, errbuf);
            return (FALSE);
        }
    }

    /* Compare the string to the compiled pattern. */
    if (((result = regexec(rexp, str, (size_t) 0, (regmatch_t *) NULL, 0)) != 0)
        && (result != REG_NOMATCH)) {
        regerror(result, rexp, errbuf, 256);
        libast_print_error("Error testing input string %s -- %s.\n", str, errbuf);
        return (FALSE);
    }
    return ((result == REG_NOMATCH) ? (FALSE) : (TRUE));
}

/**
 * Thread-safe way to compare a string to a regular expression.
 */
spif_bool_t
spiftool_regexp_match_r(register const spif_charptr_t str, register const spif_charptr_t pattern, register regex_t **rexp)
{
    register int result;
    char errbuf[256];

    ASSERT_RVAL(rexp != NULL, FALSE);
    if (*rexp == NULL) {
        *rexp = (regex_t *) MALLOC(sizeof(regex_t));
    }

    if (pattern) {
        if ((result = regcomp(*rexp, pattern, REG_EXTENDED)) != 0) {
            regerror(result, *rexp, errbuf, 256);
            libast_print_error("Unable to compile regexp %s -- %s.\n", pattern, errbuf);
            FREE(*rexp);
            return (FALSE);
        }
    }

    if (((result = regexec(*rexp, str, (size_t) 0, (regmatch_t *) NULL, 0))
         != 0) && (result != REG_NOMATCH)) {
        regerror(result, *rexp, errbuf, 256);
        libast_print_error("Error testing input string %s -- %s.\n", str, errbuf);
        return (FALSE);
    }
    return ((result == REG_NOMATCH) ? (FALSE) : (TRUE));
}
#endif

#define IS_DELIM(c)  ((delim != NULL) ? (strchr(SPIF_CHARPTR_C(delim), (c)) != NULL) : (isspace(c)))
#define IS_QUOTE(c)  (quote && quote == (c))

spif_charptr_t *
spiftool_split(const spif_charptr_t delim, const spif_charptr_t str)
{
    spif_charptr_t *slist;
    register spif_charptr_t pstr;
    register spif_charptr_t pdest;
    char quote = 0;
    unsigned short cnt = 0;
    unsigned long len;

    REQUIRE_RVAL(str != NULL, (spif_charptr_t *) NULL);

    if ((slist = (spif_charptr_t *) MALLOC(sizeof(spif_charptr_t))) == NULL) {
        libast_print_error("split():  Unable to allocate memory -- %s\n", strerror(errno));
        return ((spif_charptr_t *) NULL);
    }

    /* Before we do anything, skip leading "whitespace." */
    for (pstr = SPIF_CAST(charptr) str; *pstr && IS_DELIM(*pstr); pstr++);

    /* The outermost for loop is where we traverse the string.  Each new
       word brings us back to the top where we resize our string list. */
    for (; *pstr; cnt++) {
        /* First, resize the list to two bigger than our count.  Why two?
           One for the string we're about to do, and one for a trailing NULL. */
        if ((slist = (spif_charptr_t *) REALLOC(slist, sizeof(spif_charptr_t) * (cnt + 2))) == NULL) {
            libast_print_error("split():  Unable to allocate memory -- %s\n", strerror(errno));
            return ((spif_charptr_t *) NULL);
        }

        /* The string we're about to create can't possibly be larger than the remainder
           of the string we have yet to parse, so allocate that much space to start. */
        len = strlen(SPIF_CHARPTR_C(pstr)) + 1;
        if ((slist[cnt] = SPIF_CAST(charptr) MALLOC(len)) == NULL) {
            libast_print_error("split():  Unable to allocate memory -- %s.\n", strerror(errno));
            return ((spif_charptr_t *) NULL);
        }
        pdest = slist[cnt];

        /* This for loop is where we process each character. */
        for (; *pstr && (quote || !IS_DELIM(*pstr));) {
            if (*pstr == '\"' || *pstr == '\'') {
                /* It's a quote character, so set or reset the quote variable. */
                if (quote) {
                    if (quote == *pstr) {
                        quote = 0;
                    } else {
                        /* It's a single quote inside double quotes, or vice versa.  Leave it alone. */
                        *pdest++ = *pstr++;
                    }
                } else {
                    quote = *pstr;
                }
                pstr++;
            } else {
                /* Handle any backslashes that are escaping delimiters or quotes. */
                if ((*pstr == '\\') && (IS_DELIM(*(pstr + 1)) || IS_QUOTE(*(pstr + 1)))) {
                    /* Incrementing pstr here moves us past the backslash so that the line
                       below will copy the next character to the new token, no questions asked. */
                    pstr++;
                }
                *pdest++ = *pstr++;
            }
        }
        /* Add the trailing \0 to terminate the new string. */
        *pdest = 0;

        /* Reallocate the new string to be just the right size. */
        len = strlen(SPIF_CHARPTR_C(slist[cnt])) + 1;
        slist[cnt] = SPIF_CAST(charptr) REALLOC(slist[cnt], len);

        /* Move past any trailing "whitespace." */
        for (; *pstr && IS_DELIM(*pstr); pstr++);
    }
    if (cnt == 0) {
        FREE(slist);
        return NULL;
    } else {
        /* The last element of slist[] should be NULL. */
        slist[cnt] = 0;
        return slist;
    }
}

spif_charptr_t *
spiftool_split_regexp(const spif_charptr_t regexp, const spif_charptr_t str)
{
    USE_VAR(regexp);
    USE_VAR(str);
    return (NULL);
}

spif_charptr_t 
spiftool_join(spif_charptr_t sep, spif_charptr_t *slist)
{
    register unsigned long i;
    size_t len, slen;
    spif_charptr_t new_str;

    ASSERT_RVAL(slist != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    REQUIRE_RVAL(*slist != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    if (sep == NULL) {
        sep = SPIF_CHARPTR("");
    }
    slen = strlen(SPIF_CHARPTR_C(sep));
    for (i = len = 0; slist[i]; i++) {
        len += strlen(SPIF_CHARPTR_C(slist[i]));
    }
    len += slen * (i - 1);
    new_str = SPIF_CAST(charptr) MALLOC(len);
    strcpy(SPIF_CHARPTR_C(new_str), SPIF_CHARPTR_C(slist[0]));
    for (i = 1; slist[i]; i++) {
        if (slen) {
            strcat(SPIF_CHARPTR_C(new_str), SPIF_CHARPTR_C(sep));
        }
        strcat(SPIF_CHARPTR_C(new_str), SPIF_CHARPTR_C(slist[i]));
    }
    return new_str;
}

/* Return malloc'd pointer to index-th word in str.  "..." counts as 1 word. */
#undef IS_DELIM
#define IS_DELIM(c)  (delim ? ((c) == delim) : isspace(c))

spif_charptr_t 
spiftool_get_word(unsigned long index, const spif_charptr_t str)
{
    spif_charptr_t tmpstr;
    char delim = 0;
    register unsigned long i, j, k;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    k = strlen(SPIF_CHARPTR_C(str)) + 1;
    if ((tmpstr = SPIF_CAST(charptr) MALLOC(k)) == NULL) {
        libast_print_error("get_word(%lu, %s):  Unable to allocate memory -- %s.\n", index, str, strerror(errno));
        return (SPIF_CAST(charptr) NULL);
    }
    *tmpstr = 0;
    for (i = 0, j = 0; j < index && str[i]; j++) {
        for (; isspace(str[i]); i++);
        switch (str[i]) {
          case '\"':
              delim = '\"';
              i++;
              break;
          case '\'':
              delim = '\'';
              i++;
              break;
          default:
              delim = 0;
        }
        for (k = 0; str[i] && !IS_DELIM(str[i]);) {
            if (str[i] == '\\') {
                if (str[i + 1] == '\'' || str[i + 1] == '\"') {
                    i++;
                }
            }
            tmpstr[k++] = str[i++];
        }
        switch (str[i]) {
          case '\"':
          case '\'':
              i++;
              break;
        }
        tmpstr[k] = 0;
    }

    if (j != index) {
        FREE(tmpstr);
        D_STRINGS(("get_word(%lu, %s) returning NULL.\n", index, str));
        return (SPIF_CAST(charptr) NULL);
    } else {
        tmpstr = SPIF_CAST(charptr) REALLOC(tmpstr, strlen(SPIF_CHARPTR_C(tmpstr)) + 1);
        D_STRINGS(("get_word(%lu, %s) returning \"%s\".\n", index, str, tmpstr));
        return (tmpstr);
    }
}

/* Return pointer into str to index-th word in str.  "..." counts as 1 word. */
spif_charptr_t 
spiftool_get_pword(unsigned long index, const spif_charptr_t str)
{
    register spif_charptr_t tmpstr = SPIF_CAST(charptr) str;
    register unsigned long j;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (; isspace(*tmpstr) && *tmpstr; tmpstr++);
    for (j = 1; j < index && *tmpstr; j++) {
        for (; !isspace(*tmpstr) && *tmpstr; tmpstr++);
        for (; isspace(*tmpstr) && *tmpstr; tmpstr++);
    }

    if (*tmpstr == '\"' || *tmpstr == '\'') {
        tmpstr++;
    }
    if (*tmpstr == '\0') {
        D_STRINGS(("get_pword(%lu, %s) returning NULL.\n", index, str));
        return (SPIF_CAST(charptr) NULL);
    } else {
        D_STRINGS(("get_pword(%lu, %s) returning \"%s\"\n", index, str, tmpstr));
        return SPIF_CAST(charptr) tmpstr;
    }
}

/* Returns the number of words in str, for use with get_word() and get_pword().  "..." counts as 1 word. */
unsigned long
spiftool_num_words(const spif_charptr_t str)
{
    register unsigned long cnt = 0;
    char delim = 0;
    register unsigned long i;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_CAST_C(unsigned long) -1);
    for (i = 0; str[i] && IS_DELIM(str[i]); i++);
    for (; str[i]; cnt++) {
        switch (str[i]) {
          case '\"':
              delim = '\"';
              i++;
              break;
          case '\'':
              delim = '\'';
              i++;
              break;
          default:
              delim = 0;
        }
        for (; str[i] && !IS_DELIM(str[i]); i++);
        switch (str[i]) {
          case '\"':
          case '\'':
              i++;
              break;
        }
        for (; str[i] && isspace(str[i]); i++);
    }

    D_STRINGS(("num_words() returning %lu\n", cnt));
    return (cnt);
}

/* chomp() removes leading and trailing whitespace from a string */
spif_charptr_t 
spiftool_chomp(spif_charptr_t s)
{
    register spif_charptr_t front, back;

    ASSERT_RVAL(s != NULL, NULL);
    REQUIRE_RVAL(*s, s);

    for (front = s; *front && isspace(*front); front++);
    for (back = s + strlen(SPIF_CHARPTR_C(s)) - 1; *back && isspace(*back) && back > front; back--);

    *(++back) = 0;
    if (front != s) {
        memmove(s, front, back - front + 1);
    }
    return (s);
}

spif_charptr_t 
spiftool_downcase_str(spif_charptr_t str)
{
    register spif_charptr_t tmp;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (tmp = str; *tmp; tmp++) {
        *tmp = tolower(*tmp);
    }
    D_STRINGS(("downcase_str() returning %s\n", str));
    return (str);
}

spif_charptr_t 
spiftool_upcase_str(spif_charptr_t str)
{
    register spif_charptr_t tmp;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (tmp = str; *tmp; tmp++) {
        *tmp = toupper(*tmp);
    }
    D_STRINGS(("upcase_str() returning %s\n", str));
    return (str);
}

spif_charptr_t 
spiftool_condense_whitespace(spif_charptr_t s)
{

    register unsigned char gotspc = 0;
    register spif_charptr_t pbuff = s, pbuff2 = s;

    ASSERT_RVAL(s != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    D_STRINGS(("condense_whitespace(%s) called.\n", s));
    for (; *pbuff2; pbuff2++) {
        if (isspace(*pbuff2)) {
            if (!gotspc) {
                *pbuff = ' ';
                gotspc = 1;
                pbuff++;
            }
        } else {
            *pbuff = *pbuff2;
            gotspc = 0;
            pbuff++;
        }
    }
    if ((pbuff >= s) && (isspace(*(pbuff - 1))))
        pbuff--;
    *pbuff = 0;
    D_STRINGS(("condense_whitespace() returning \"%s\".\n", s));
    return (SPIF_CAST(charptr) REALLOC(s, strlen(SPIF_CHARPTR_C(s)) + 1));
}

spif_charptr_t 
spiftool_safe_str(register spif_charptr_t str, unsigned short len)
{
    register unsigned short i;

    ASSERT_RVAL(str != SPIF_NULL_TYPE(ptr), SPIF_NULL_TYPE(ptr));
    for (i = 0; i < len; i++) {
        if (iscntrl(str[i])) {
            str[i] = '.';
        }
    }

    return (str);
}

void
spiftool_hex_dump(void *buff, register size_t count)
{
    register unsigned long j, k, l;
    register spif_charptr_t ptr;
    spif_char_t buffr[9];

    ASSERT(buff != SPIF_NULL_TYPE(ptr));
    fprintf(stderr, "  Address  |  Size  | Offset  | 00 01 02 03 04 05 06 07 |  ASCII  \n");
    fprintf(stderr, "-----------+--------+---------+-------------------------+---------\n");
    for (ptr = buff, j = 0; j < count; j += 8) {
        fprintf(stderr, " %10p | %06lu | %07x | ", buff, (unsigned long) count, (unsigned int) j);
        l = ((count - j < 8) ? (count - j) : (8));
        memcpy(buffr, ptr + j, l);
        memset(buffr + l, 0, 9 - l);
        for (k = 0; k < l; k++) {
            fprintf(stderr, "%02x ", buffr[k]);
        }
        for (; k < 8; k++) {
            fprintf(stderr, "   ");
        }
        fprintf(stderr, "| %-8s\n", spiftool_safe_str(SPIF_CAST(charptr) buffr, l));
    }
}

#define CHAR_CLASS_MATCH(a, b)      ((isalpha(a) && isalpha(b)) \
                                     || (isdigit(a) && isdigit(b)) \
                                     || (!isalnum(a) && !isalnum(b)))
spif_cmp_t
spiftool_version_compare(spif_charptr_t v1, spif_charptr_t v2)
{
    spif_char_t buff1[128], buff2[128];

    D_CONF(("Comparing version strings \"%s\" and \"%s\"\n", NONULL(v1), NONULL(v2)));
    SPIF_COMP_CHECK_NULL(v1, v2);

    for (; *v1 && *v2; ) {
        D_CONF((" -> Looking at \"%s\" and \"%s\"\n", v1, v2));
        if (isalpha(*v1) && isalpha(*v2)) {
            spif_charptr_t p1 = buff1, p2 = buff2;
            spif_int8_t ival1 = 6, ival2 = 6;

            /* Compare words.  First, copy each word into buffers. */
            for (; *v1 && isalpha(*v1); v1++, p1++) *p1 = *v1;
            for (; *v2 && isalpha(*v2); v2++, p2++) *p2 = *v2;
            *p1 = *p2 = 0;

            /* Change the buffered strings to lowercase for easier comparison. */
            spiftool_downcase_str(buff1);
            spiftool_downcase_str(buff2);
            D_CONF(("     -> Comparing as words \"%s\" vs. \"%s\"\n", buff1, buff2));

            /* Some strings require special handling. */
            if (!strcmp(SPIF_CHARPTR_C(buff1), "snap")) {
                ival1 = 1;
            } else if (!strcmp(SPIF_CHARPTR_C(buff1), "pre")) {
                ival1 = 2;
            } else if (!strcmp(SPIF_CHARPTR_C(buff1), "alpha")) {
                ival1 = 3;
            } else if (!strcmp(SPIF_CHARPTR_C(buff1), "beta")) {
                ival1 = 4;
            } else if (!strcmp(SPIF_CHARPTR_C(buff1), "rc")) {
                ival1 = 5;
            }
            if (!strcmp(SPIF_CHARPTR_C(buff2), "snap")) {
                ival2 = 1;
            } else if (!strcmp(SPIF_CHARPTR_C(buff2), "pre")) {
                ival2 = 2;
            } else if (!strcmp(SPIF_CHARPTR_C(buff2), "alpha")) {
                ival2 = 3;
            } else if (!strcmp(SPIF_CHARPTR_C(buff2), "beta")) {
                ival2 = 4;
            } else if (!strcmp(SPIF_CHARPTR_C(buff2), "rc")) {
                ival2 = 5;
            }
            if (ival1 != ival2) {
                /* If the values are different, compare them. */
                D_CONF(("     -> %d\n", (int) SPIF_CMP_FROM_INT(ival1 - ival2)));
                return SPIF_CMP_FROM_INT(ival1 - ival2);
            } else if (ival1 == 6) {
                int c;

                /* Two arbitrary strings.  Compare them too. */
                if ((c = strcmp(SPIF_CHARPTR_C(buff1), SPIF_CHARPTR_C(buff2))) != 0) {
                    D_CONF(("     -> %d\n", (int) SPIF_CMP_FROM_INT(c)));
                    return SPIF_CMP_FROM_INT(c);
                }
            }
        } else if (isdigit(*v1) && isdigit(*v2)) {
            spif_charptr_t p1 = buff1, p2 = buff2;
            spif_int32_t ival1, ival2;
            spif_cmp_t c;

            /* Compare numbers.  First, copy each number into buffers. */
            for (; *v1 && isdigit(*v1); v1++, p1++) *p1 = *v1;
            for (; *v2 && isdigit(*v2); v2++, p2++) *p2 = *v2;
            *p1 = *p2 = 0;

            /* Convert the strings into actual integers. */
            ival1 = SPIF_CAST(int32) strtol(SPIF_CHARPTR_C(buff1), (char **) NULL, 10);
            ival2 = SPIF_CAST(int32) strtol(SPIF_CHARPTR_C(buff2), (char **) NULL, 10);
            D_CONF(("     -> Comparing as integers %d vs. %d\n", SPIF_CAST_C(int) ival1, SPIF_CAST_C(int) ival2));

            /* Compare the integers and return if not equal. */
            c = SPIF_CMP_FROM_INT(ival1 - ival2);
            if (!SPIF_CMP_IS_EQUAL(c)) {
                D_CONF(("     -> %d\n", (int) c));
                return c;
            }
        } else if (!isalnum(*v1) && !isalnum(*v2)) {
            spif_charptr_t p1 = buff1, p2 = buff2;
            spif_cmp_t c;

            /* Compare non-alphanumeric strings. */
            for (; *v1 && !isalnum(*v1); v1++, p1++) *p1 = *v1;
            for (; *v2 && !isalnum(*v2); v2++, p2++) *p2 = *v2;
            *p1 = *p2 = 0;

            D_CONF(("     -> Comparing as non-alphanumeric strings \"%s\" vs. \"%s\"\n", buff1, buff2));
            c = SPIF_CMP_FROM_INT(strcasecmp(SPIF_CHARPTR_C(buff1), SPIF_CHARPTR_C(buff2)));
            if (!SPIF_CMP_IS_EQUAL(c)) {
                D_CONF(("     -> %d\n", (int) c));
                return c;
            }
        } else {
            D_CONF(("     -> Comparing as alphanumeric strings \"%s\" vs. \"%s\"\n", buff1, buff2));
            D_CONF(("     -> %d\n", (int) SPIF_CMP_FROM_INT(strcasecmp(SPIF_CHARPTR_C(buff1), SPIF_CHARPTR_C(buff2)))));
            return SPIF_CMP_FROM_INT(strcasecmp(SPIF_CHARPTR_C(buff1), SPIF_CHARPTR_C(buff2)));
        }
    }

    /* We've reached the end of one of the strings. */
    if (*v1) {
        if (!BEG_STRCASECMP(SPIF_CHARPTR_C(v1), "snap") || !BEG_STRCASECMP(SPIF_CHARPTR_C(v1), "pre")
            || !BEG_STRCASECMP(SPIF_CHARPTR_C(v1), "alpha") || !BEG_STRCASECMP(SPIF_CHARPTR_C(v1), "beta")) {
            D_CONF(("     -> <\n"));
            return SPIF_CMP_LESS;
        } else {
            D_CONF(("     -> >\n"));
            return SPIF_CMP_GREATER;
        }
    } else if (*v2) {
        if (!BEG_STRCASECMP(SPIF_CHARPTR_C(v2), "snap") || !BEG_STRCASECMP(SPIF_CHARPTR_C(v2), "pre")
            || !BEG_STRCASECMP(SPIF_CHARPTR_C(v2), "alpha") || !BEG_STRCASECMP(SPIF_CHARPTR_C(v2), "beta")) {
            D_CONF(("     -> >\n"));
            return SPIF_CMP_GREATER;
        } else {
            D_CONF(("     -> <\n"));
            return SPIF_CMP_LESS;
        }
    }
    D_CONF(("     -> ==\n"));
    return SPIF_CMP_EQUAL;
}

/**
 * @defgroup DOXGRP_STRINGS String Utility Routines
 *
 * This group of functions/defines/macros provides oft-needed string
 * manipulation functionality which is not found (at least not
 * portably) in libc.
 *
 * Over the past several years, I have had to implement some simple
 * string routines which, for whatever reason, are not part of the
 * Standard C Library.  I have gathered these macros, functions,
 * etc. into LibAST in the hopes that others might not have to
 * reimplement them themselves.  Unlike the memory/debugging/object
 * stuff, there really isn't a well-defined architecture surrounding
 * these.  They're just utility functions and implementations for
 * non-portable functionality.
 *
 * A small sample program demonstrating some of these routines can be
 * found @link strings_example.c here @endlink.
 */

/**
 * @example strings_example.c
 * Example code for using the string routines.
 *
 */
