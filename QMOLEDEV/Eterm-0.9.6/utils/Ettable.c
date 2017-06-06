/* Ettable -- Eterm ASCII Table Display Utility
 *
 * Copyright (C) 1997-2009, Michael Jennings
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

static const char cvs_ident[] = "$Id: Ettable.c 38480 2009-01-06 09:08:48Z mej $";

#include <stdio.h>

const char *lookup[] = {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",     /*  0-7  */
    "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",     /*  8-15 */
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",     /* 16-23 */
    "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"   /* 24-31 */
};

int
main(void)
{

    unsigned short i;

    printf("+-----------+---------+-------------+--------+\n");
    printf("| Character | Decimal | Hexadecimal |  Octal |\n");
    printf("+-----------+---------+-------------+--------+\n");

    for (i = 0; i < 32; i++) {
        printf("|  %3s  ^%c  |   %3d   |    0x%02x     |   %03o  |\n", lookup[i], ('@' + i), i, i, i);
    }
    for (; i < 256; i++) {
        printf("|    '%c'    |   %3d   |    0x%02x     |  %c%03o  |\n", (i == 127 ? ' ' : i), i, i, (i > '\077' ? '0' : ' '), i);
    }
    printf("+-----------+---------+-------------+--------+\n");

    printf("+---------------+---------+-------------+-------+\n");
    printf("| ACS Character | Decimal | Hexadecimal | Octal |\n");
    printf("+---------------+---------+-------------+-------+\n");
    printf("\033)0");

    for (i = 1; i < 32; i++) {
        printf("|    \016%c\017   (%c)    |   %3d   |    0x%02x     |  %03o  |\n", i + 0x5e, i + 0x5e, i, i, i);
    }
    printf("+---------------+---------+-------------+-------+\n");
    return 0;
}
