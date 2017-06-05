/* -*- Mode: C; indent-tabs-mode:nil; c-basic-offset:8 -*- */

/*
 * This file is part of The Croco Library
 *
 * Copyright (C) 2002-2003 Dodji Seketeli <dodji@seketeli.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms 
 * of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the 
 * GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

/*
 *$Id: test6-main.c 244 2004-03-07 13:22:49Z dodji $
 */

#include <string.h>
#include "libcroco.h"
#include "cr-test-utils.h"

const guchar *gv_cssbuf =
        ".str0 {stroke:#007844;stroke-width:44}" ".fil0 {fill:url(#id0)}";

static enum CRStatus
  test_cr_parser_parse (void);

/**
 *The test of the cr_input_read_byte() method.
 *Reads the each byte of a_file_uri using the
 *cr_input_read_byte() method. Each byte is send to
 *stdout.
 *@param a_file_uri the file to read.
 *@return CR_OK upon successfull completion of the
 *function, an error code otherwise.
 */
static enum CRStatus
test_cr_parser_parse (void)
{
        enum CRStatus status = CR_OK;
        CROMParser *parser = NULL;
        CRStyleSheet *stylesheet = NULL;

        parser = cr_om_parser_new (NULL);
        status = cr_om_parser_parse_buf (parser, (guchar *) gv_cssbuf,
                                         strlen (gv_cssbuf),
                                         CR_ASCII, &stylesheet);

        if (status == CR_OK && stylesheet) {
                cr_stylesheet_dump (stylesheet, stdout);
                cr_stylesheet_destroy (stylesheet);
        }
        cr_om_parser_destroy (parser);

        return status;
}

/**
 *The entry point of the testing routine.
 */
int
main (int argc, char **argv)
{
        enum CRStatus status = CR_OK;

        status = test_cr_parser_parse ();

        if (status != CR_OK) {
                g_print ("\nKO\n");
        }

        return 0;
}
