#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <errno.h>
#if defined(HAVE_STDARG_H) && (HAVE_STDARG_H != 0)
# include <stdarg.h>
#else
# undef HAVE_STDARG_H
#endif

static const char __attribute__((unused)) cvs_ident[] = "$Id: snprintf.c,v 1.6 2004/07/23 21:38:39 mej Exp $";

/* 
 * Shamelessly snarfed from Enlightenment...
 * which shamelessly snarfed from sane... 
 * which shamelessly snarfed from LPR.
 * 
 * Moved comments to end so I can actually read the code.. cleaned out useless
 * junk....
 */

#if !(HAVE_SNPRINTF) || (HAVE_SNPRINTF_BUG == 1)

#define VA_LOCAL_DECL va_list ap
#define VA_START(f) va_start(ap, f)
#define VA_SHIFT(v,t) ;         /* no-op for ANSI */
#define VA_END va_end(ap)

/*
 * dopr(): poor man's version of doprintf
 */

static void dopr(char *buffer, const char *format, va_list args);
static void fmtstr(char *value, int ljust, int len, int zpad, int precision);
static void fmtnum(long value, int base, int dosign, int ljust, int len, int zpad, int precision);
static void fmtdouble(int fmt, double value, int ljust, int len, int zpad, int precision);
static void dostr(char *);
static char *output;
static void dopr_outch(int c);
static char *end;
int visible_control = 1;

int
vsnprintf(char *str, size_t count, const char *fmt, va_list args)
{
    str[0] = 0;
    end = str + count - 1;
    dopr(str, fmt, args);
    if (count > 0) {
        end[0] = 0;
    }
    return (strlen(str));
}

#ifdef HAVE_STDARG_H
int
snprintf(char *str, size_t count, const char *fmt, ...)
#else
int
snprintf(va_alist)
     va_dcl
#endif
{
#ifndef HAVE_STDARG_H
    char *str;
    size_t count;
    char *fmt;

#endif
    VA_LOCAL_DECL;

    VA_START(fmt);
    VA_SHIFT(str, char *);

    VA_SHIFT(count, size_t);
    VA_SHIFT(fmt, char *);

    (void) vsnprintf(str, count, fmt, ap);
    VA_END;
    return (strlen(str));
}

static void
dopr(char *buffer, const char *format, va_list args)
{
    int ch;
    long value;
    int longflag = 0;
    char *strvalue;
    int ljust;
    int len;
    int zpad;
    int precision;
    int set_precision;
    double dval;

    output = buffer;
    while ((ch = *format++)) {
        switch (ch) {
          case '%':
              ljust = len = zpad = 0;
              precision = -1;
              set_precision = 0;
            nextch:
              ch = *format++;
              switch (ch) {
                case 0:
                    dostr("**end of format**");
                    return;
                case '-':
                    ljust = 1;
                    goto nextch;
                case '.':
                    set_precision = 1;
                    precision = 0;
                    goto nextch;
                case '*':
                    len = va_arg(args, int);

                    goto nextch;
                case '0':      /* set zero padding if len not set */
                    if (len == 0 && set_precision == 0)
                        zpad = '0';
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    if (set_precision) {
                        precision = precision * 10 + ch - '0';
                    } else {
                        len = len * 10 + ch - '0';
                    }
                    goto nextch;
                case 'l':
                    longflag = 1;
                    goto nextch;
                case 'u':
                case 'U':
                    /*fmtnum(value,base,dosign,ljust,len, zpad, precision) */
                    if (longflag) {
                        value = va_arg(args, long);
                    } else {
                        value = va_arg(args, int);
                    }
                    fmtnum(value, 10, 0, ljust, len, zpad, precision);
                    break;
                case 'o':
                case 'O':
                    /*fmtnum(value,base,dosign,ljust,len, zpad, precision) */
                    if (longflag) {
                        value = va_arg(args, long);
                    } else {
                        value = va_arg(args, int);
                    }
                    fmtnum(value, 8, 0, ljust, len, zpad, precision);
                    break;
                case 'd':
                case 'i':
                case 'D':
                    if (longflag) {
                        value = va_arg(args, long);
                    } else {
                        value = va_arg(args, int);
                    }
                    fmtnum(value, 10, 1, ljust, len, zpad, precision);
                    break;
                case 'x':
                    if (longflag) {
                        value = va_arg(args, long);
                    } else {
                        value = va_arg(args, int);
                    }
                    fmtnum(value, 16, 0, ljust, len, zpad, precision);
                    break;
                case 'X':
                    if (longflag) {
                        value = va_arg(args, long);
                    } else {
                        value = va_arg(args, int);
                    }
                    fmtnum(value, -16, 0, ljust, len, zpad, precision);
                    break;
                case 's':
                    strvalue = va_arg(args, char *);

                    fmtstr(strvalue, ljust, len, zpad, precision);
                    break;
                case 'c':
                    ch = va_arg(args, int);

                    {
                        char b[2];
                        int vsb = visible_control;

                        b[0] = ch;
                        b[1] = 0;
                        visible_control = 0;
                        fmtstr(b, ljust, len, zpad, precision);
                        visible_control = vsb;
                    }
                    break;
                case 'f':
                case 'g':
                    dval = va_arg(args, double);

                    fmtdouble(ch, dval, ljust, len, zpad, precision);
                    break;
                case '%':
                    dopr_outch(ch);
                    continue;
                default:
                    dostr("???????");
              }
              longflag = 0;
              break;
          default:
              dopr_outch(ch);
              break;
        }
    }
    *output = 0;
}

/*
 * Format '%[-]len[.precision]s'
 * -   = left justify (ljust)
 * len = minimum length
 * precision = numbers of chars in string to use
 */
static void
fmtstr(char *value, int ljust, int len, int zpad, int precision)
{
    int padlen, strlen, i, c;   /* amount to pad */

    zpad = 0;
    if (value == 0) {
        value = "<NULL>";
    }
    if (precision > 0) {
        strlen = precision;
    } else {
        /* cheap strlen so you do not have library call */
        for (strlen = 0; (c = value[strlen]); ++strlen) {
            if (visible_control && iscntrl(c) && !isspace(c)) {
                ++strlen;
            }
        }
    }
    padlen = len - strlen;
    if (padlen < 0)
        padlen = 0;
    if (ljust)
        padlen = -padlen;
    while (padlen > 0) {
        dopr_outch(' ');
        --padlen;
    }
    /* output characters */
    for (i = 0; (c = value[i]); ++i) {
        if (visible_control && iscntrl(c) && !isspace(c)) {
            dopr_outch('^');
            c = ('@' | (c & 0x1F));
        }
        dopr_outch(c);
    }
    while (padlen < 0) {
        dopr_outch(' ');
        ++padlen;
    }
}

static void
fmtnum(long value, int base, int dosign, int ljust, int len, int zpad, int precision)
{
    int signvalue = 0;
    unsigned long uvalue;
    char convert[20];
    int place = 0;
    int padlen = 0;             /* amount to pad */
    int caps = 0;

    precision = 0;
    /* DEBUGP(("value 0x%x, base %d, dosign %d, ljust %d, len %d, zpad %d\n",
     * value, base, dosign, ljust, len, zpad )); */
    uvalue = value;
    if (dosign) {
        if (value < 0) {
            signvalue = '-';
            uvalue = -value;
        }
    }
    if (base < 0) {
        caps = 1;
        base = -base;
    }
    do {
        convert[place++] = (caps ? "0123456789ABCDEF" : "0123456789abcdef")
            [uvalue % (unsigned) base];
        uvalue = (uvalue / (unsigned) base);
    }
    while (uvalue);
    convert[place] = 0;
    padlen = len - place;
    if (padlen < 0)
        padlen = 0;
    if (ljust)
        padlen = -padlen;
    /* DEBUGP(( "str '%s', place %d, sign %c, padlen %d\n",
     * convert,place,signvalue,padlen)); */
    if (zpad && padlen > 0) {
        if (signvalue) {
            dopr_outch(signvalue);
            --padlen;
            signvalue = 0;
        }
        while (padlen > 0) {
            dopr_outch(zpad);
            --padlen;
        }
    }
    while (padlen > 0) {
        dopr_outch(' ');
        --padlen;
    }
    if (signvalue)
        dopr_outch(signvalue);
    while (place > 0)
        dopr_outch(convert[--place]);
    while (padlen < 0) {
        dopr_outch(' ');
        ++padlen;
    }
}

static void
fmtdouble(int fmt, double value, int ljust, int len, int zpad, int precision)
{
    char convert[128];
    char fmtstr[128];
    int l;

    zpad = 0;
    if (len == 0)
        len = 10;
    if (len > (int) sizeof(convert) - 10) {
        len = (int) sizeof(convert) - 10;
    }
    if (precision > (int) sizeof(convert) - 10) {
        precision = (int) sizeof(convert) - 10;
    }
    if (precision > len)
        precision = len;
    strcpy(fmtstr, "%");
    if (ljust)
        strcat(fmtstr, "-");
    if (len) {
        sprintf(fmtstr + strlen(fmtstr), "%d", len);
    }
    if (precision > 0) {
        sprintf(fmtstr + strlen(fmtstr), ".%d", precision);
    }
    l = strlen(fmtstr);
    fmtstr[l] = fmt;
    fmtstr[l + 1] = 0;
    sprintf(convert, fmtstr, value);
    dostr(convert);
}

static void
dostr(char *str)
{
    while (*str)
        dopr_outch(*str++);
}

static void
dopr_outch(int c)
{
    if (end == 0 || output < end) {
        *output++ = c;
    }
}

/**************************************************************
 * Original:
 * Patrick Powell Tue Apr 11 09:48:21 PDT 1995
 * A bombproof version of doprnt (dopr) included.
 * Sigh.  This sort of thing is always nasty do deal with.  Note that
 * the version here does not include floating point...
 *
 * plp_snprintf() is used instead of sprintf() as it does limit checks
 * for string length.  This covers a nasty loophole.
 *
 * The other functions are there to prevent NULL pointers from
 * causing nast effects.
 **************************************************************/

/***************************************************************************
 * LPRng - An Extended Print Spooler System
 *
 * Copyright 1988-1997, Patrick Powell, San Diego, CA
 *     papowell@sdsu.edu
 * See below for conditions of use.
 *
 ***************************************************************************
 * MODULE: snprintf.c
 * PURPOSE: LPRng version of printf - absolutely bombproof (hopefully!)
 **************************************************************************/

/*
 * The "Artistic License"
 * 
 * Preamble
 * 
 * The intent of this document is to state the conditions under which a
 * Package may be copied, such that the Copyright Holder maintains some
 * semblance of artistic control over the development of the package,
 * while giving the users of the package the right to use and distribute
 * the Package in a more-or-less customary fashion, plus the right to make
 * reasonable modifications.
 * 
 * Definitions:
 * 
 * "Package" refers to the collection of files distributed by the
 * Copyright Holder, and derivatives of that collection of files
 * created through textual modification.
 * 
 * "Standard Version" refers to such a Package if it has not been
 * modified, or has been modified in accordance with the wishes
 * of the Copyright Holder as specified below.
 * 
 * "Copyright Holder" is whoever is named in the copyright or
 * copyrights for the package.
 * 
 * "You" is you, if you are thinking about copying or distributing
 * this Package.
 * 
 * "Reasonable copying fee" is whatever you can justify on the
 * basis of media cost, duplication charges, time of people involved,
 * and so on.  (You will not be required to justify it to the
 * Copyright Holder, but only to the computing community at large
 * as a market that must bear the fee.)
 * 
 * "libast_freely Available" means that no fee is charged for the item
 * itself, though there may be fees involved in handling the item.
 * It also means that recipients of the item may redistribute it
 * under the same conditions they received it.
 * 
 * 1. You may make and give away verbatim copies of the source form of the
 * Standard Version of this Package without restriction, provided that you
 * duplicate all of the original copyright notices and associated disclaimers.
 * 
 * 2. You may apply bug fixes, portability fixes and other modifications
 * derived from the Public Domain or from the Copyright Holder.  A Package
 * modified in such a way shall still be considered the Standard Version.
 * 
 * 3. You may otherwise modify your copy of this Package in any way, provided
 * that you insert a prominent notice in each changed file stating how and
 * when you changed that file, and provided that you do at least ONE of the
 * following:
 * 
 * a) place your modifications in the Public Domain or otherwise make them
 * Freely Available, such as by posting said modifications to Usenet or
 * an equivalent medium, or placing the modifications on a major archive
 * site such as uunet.uu.net, or by allowing the Copyright Holder to include
 * your modifications in the Standard Version of the Package.
 * 
 * b) use the modified Package only within your corporation or organization.
 * 
 * c) rename any non-standard executables so the names do not conflict
 * with standard executables, which must also be provided, and provide
 * a separate manual page for each non-standard executable that clearly
 * documents how it differs from the Standard Version.
 * 
 * d) make other distribution arrangements with the Copyright Holder.
 * 
 * 4. You may distribute the programs of this Package in object code or
 * executable form, provided that you do at least ONE of the following:
 * 
 * a) distribute a Standard Version of the executables and library files,
 * together with instructions (in the manual page or equivalent) on where
 * to get the Standard Version.
 * 
 * b) accompany the distribution with the machine-readable source of
 * the Package with your modifications.
 * 
 * c) give non-standard executables non-standard names, and clearly
 * document the differences in manual pages (or equivalent), together
 * with instructions on where to get the Standard Version.
 * 
 * d) make other distribution arrangements with the Copyright Holder.
 * 
 * 5. You may charge a reasonable copying fee for any distribution of this
 * Package.  You may charge any fee you choose for support of this
 * Package.  You may not charge a fee for this Package itself.  However,
 * you may distribute this Package in aggregate with other (possibly
 * commercial) programs as part of a larger (possibly commercial) software
 * distribution provided that you do not advertise this Package as a
 * product of your own. 
 * 
 * 6. The name of the Copyright Holder may not be used to endorse or promote
 * products derived from this software without specific prior written permission.
 * 
 * 7. THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * The End
 */

#endif /* HAVE_SNPRINTF */
