/* UNARJ.C, UNARJ, R JUNG, 06/05/02
 * Main Extractor routine
 * Copyright (c) 1991-2002 by ARJ Software, Inc.  All rights reserved.
 *
 *   This code may be freely used in programs that are NOT ARJ archivers
 *   (both compress and extract ARJ archives).
 *
 *   If you wish to distribute a modified version of this program, you
 *   MUST indicate that it is a modified version both in the program and
 *   source code.
 *
 *   We are holding the copyright on the source code, so please do not
 *   delete our name from the program files or from the documentation.
 *
 *   We wish to give credit to Haruhiko Okumura for providing the
 *   basic ideas for ARJ and UNARJ in his program AR.  Please note
 *   that UNARJ is significantly different from AR from an archive
 *   structural point of view.
 *
 * Modification history:
 * Date      Programmer  Description of modification.
 * 09/27/00  R. Jung     Added additional header data checks.
 * 04/04/98  R. Jung     Added minor comments.
 * 04/05/91  R. Jung     Rewrote code.
 * 04/23/91  M. Adler    Portabilized.
 * 04/29/91  R. Jung     Added l command.  Removed 16 bit dependency in
 *                       fillbuf().
 * 05/19/91  R. Jung     Fixed extended header skipping code.
 * 05/25/91  R. Jung     Improved find_header().
 * 06/03/91  R. Jung     Changed arguments in get_mode_str() and
 *                       set_ftime_mode().
 * 06/19/81  R. Jung     Added two more %c in printf() in list_arc().
 * 07/07/91  R. Jung     Added default_case_path() to extract().
 *                       Added strlower().
 * 07/20/91  R. Jung     Changed uint ratio() to static uint ratio().
 * 07/21/91  R. Jung     Added #ifdef VMS.
 * 08/28/91  R. Jung     Changed M_DIFFHOST message.
 * 08/31/91  R. Jung     Added changes to support MAC THINK_C compiler
 *                       per Eric Larson.
 * 10/07/91  R. Jung     Added missing ; to THINK_C additions.
 * 11/11/91  R. Jung     Added host_os test to fwrite_txt_crc().
 * 11/24/91  R. Jung     Added more error_count processing.
 * 12/03/91  R. Jung     Added backup file processing.
 * 02/17/93  R. Jung     Added archive modified date support.
 * 01/22/94  R. Jung     Changed copyright message.
 * 07/29/96  R. Jung     Added "/" to list of path separators.
 * 06/05/02  R. Jung     Changed version number.
 *
 */

#include "unarj.h"

/* Global variables */
/* Messages */

char M_VERSION [] = "UNARJ (Demo version) 2.65 Copyright (c) 1991-2002 ARJ Software, Inc.\n\n";

char M_ARCDATE [] = "Archive created: %s";
char M_ARCDATEM[] = ", modified: %s";
char M_BADCOMND[] = "Bad UNARJ command: %s";
char M_BADCOMNT[] = "Invalid comment header";
char M_BADHEADR[] = "Bad header";
char M_BADTABLE[] = "Bad file data";
char M_CANTOPEN[] = "Can't open %s";
char M_CANTREAD[] = "Can't read file or unexpected end of file";
char M_CANTWRIT[] = "Can't write file. Disk full?";
char M_CRCERROR[] = "CRC error!\n";
char M_CRCOK   [] = "CRC OK\n";
char M_DIFFHOST[] = "  Binary file!";
char M_ENCRYPT [] = "File is password encrypted, ";
char M_ERRORCNT[] = "%sFound %5d error(s)!";
char M_EXTRACT [] = "Extracting %-25s";
char M_FEXISTS [] = "%-25s exists, ";
char M_HEADRCRC[] = "Header CRC error!";
char M_NBRFILES[] = "%5d file(s)\n";
char M_NOMEMORY[] = "Out of memory";
char M_NOTARJ  [] = "%s is not an ARJ archive";
char M_PROCARC [] = "Processing archive: %s\n";
char M_SKIPPED [] = "Skipped %s\n";
char M_SUFFIX  [] = ARJ_SUFFIX;
char M_TESTING [] = "Testing    %-25s";
char M_UNKNMETH[] = "Unsupported method: %d, ";
char M_UNKNTYPE[] = "Unsupported file type: %d, ";
char M_UNKNVERS[] = "Unsupported version: %d, ";

/*
int
main(int argc, char *argv[])
{
    int i, j, lastc;
    char *arc_p;

#ifdef THINK_C
    argc = ccommand(&argv);
#endif

    printf(M_VERSION);

    if (argc == 1)
    {
        //help();
        return EXIT_SUCCESS;
    }
    else if (argc == 2)
    {
        command = 'L';
        arc_p = argv[1];
    }
    else if (argc == 3)
    {
        if (strlen(argv[1]) > 1)
            error(M_BADCOMND, argv[1]);
        command = toupper(*argv[1]);
        if (strchr("ELTX", command) == NULL)
            error(M_BADCOMND, argv[1]);
        arc_p = argv[2];
    }
    else
    {
        //help();
        return EXIT_FAILURE;
    }

    strncopy(arc_name, arc_p, FNAME_MAX);
    case_path(arc_name);
    i = strlen(arc_name);
    j = parse_path(arc_name, (char *)NULL, (char *)NULL);
    lastc = arc_name[i - 1];
    if (lastc == ARJ_DOT)
        arc_name[i - 1] = NULL_CHAR;
    else if (strchr(&arc_name[j], ARJ_DOT) == NULL)
        strcat(arc_name, M_SUFFIX);

    make_crctable();

    error_count = 0;
    clock_inx = 0;
    arcfile = NULL;
    outfile = NULL;

    execute_cmd();

    if (error_count > 0)
        error(M_ERRORCNT, "");

    return EXIT_SUCCESS;
}
*/

/* end UNARJ.C */
