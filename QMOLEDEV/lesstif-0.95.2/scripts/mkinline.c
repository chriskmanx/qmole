/**
 *
 * $Id: mkinline.c,v 1.1 2001/08/21 14:30:35 amai Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Id: mkinline.c,v 1.1 2001/08/21 14:30:35 amai Exp $";

/* this is not a source used for libXm, but we might need it anyway */

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int
main(int argc, char *argv[])
{
	FILE	*in, *out;
	char	t[1024], f[1024];
	char	line[200];
	int	count = 0;

	/* Get the names of the input and output files. */
	if (argc != 3) {
		fprintf(stderr, "Usage: %s INLINE-IN INLINE-OUT\n", argv[0]);
		return 1;
	}

	in = fopen(argv[1], "r");
	if (in == NULL) {
		perror("fopen");
		fprintf(stderr, "%s: something's wrong reading %s\n",
		        argv[0], argv[1]);
		return 2;
	}

	out = fopen(argv[2], "w");
	if (out == NULL) {
		perror("fopen");
		fprintf(stderr, "%s: something's wrong writing %s\n",
		        argv[0], argv[2]);
		return 3;
	}

	fprintf(out, "#ifndef LESSTIF_PRODUCTION\n");
	fprintf(out, "#include <Xm/XmAll.h>\n");
	fprintf(out, "#include <XmI/XmI.h>\n");
	fprintf(out, "#include <Xm/ArrowBGP.h>\n");
	fprintf(out, "#include <Xm/ArrowBP.h>\n");
	fprintf(out, "#include <Xm/BaseClassP.h>\n");
	fprintf(out, "#include <Xm/BulletinBP.h>\n");
	fprintf(out, "#include <Xm/CacheP.h>\n");
	fprintf(out, "#include <Xm/CascadeBGP.h>\n");
	fprintf(out, "#include <Xm/CascadeBP.h>\n");
	fprintf(out, "#include <Xm/CommandP.h>\n");
	fprintf(out, "#include <Xm/CutPasteP.h>\n");
	fprintf(out, "#include <Xm/DesktopP.h>\n");
	fprintf(out, "#include <Xm/DialogSEP.h>\n");
	fprintf(out, "#include <Xm/DialogSP.h>\n");
	fprintf(out, "#include <Xm/DisplayP.h>\n");
	fprintf(out, "#include <Xm/DragCP.h>\n");
	fprintf(out, "#include <Xm/DragIconP.h>\n");
	fprintf(out, "#include <Xm/DragOverSP.h>\n");
	fprintf(out, "#include <Xm/DrawP.h>\n");
	fprintf(out, "#include <Xm/DrawingAP.h>\n");
	fprintf(out, "#include <Xm/DrawnBP.h>\n");
	fprintf(out, "#include <Xm/DropSMgrP.h>\n");
	fprintf(out, "#include <Xm/DropTransP.h>\n");
	fprintf(out, "#include <Xm/ExtObjectP.h>\n");
	fprintf(out, "#include <Xm/FileSBP.h>\n");
	fprintf(out, "#include <Xm/FormP.h>\n");
	fprintf(out, "#include <Xm/FrameP.h>\n");
	fprintf(out, "#include <Xm/GadgetP.h>\n");
	fprintf(out, "#include <Xm/LabelGP.h>\n");
	fprintf(out, "#include <Xm/LabelP.h>\n");
	fprintf(out, "#include <Xm/ListP.h>\n");
	fprintf(out, "#include <Xm/MainWP.h>\n");
	fprintf(out, "#include <Xm/ManagerP.h>\n");
	fprintf(out, "#include <Xm/MenuShellP.h>\n");
	fprintf(out, "#include <Xm/MenuUtilP.h>\n");
	fprintf(out, "#include <Xm/MessageBP.h>\n");
	fprintf(out, "#include <Xm/PanedWP.h>\n");
	fprintf(out, "#include <Xm/PrimitiveP.h>\n");
	fprintf(out, "#include <Xm/ProtocolsP.h>\n");
	fprintf(out, "#include <Xm/PushBGP.h>\n");
	fprintf(out, "#include <Xm/PushBP.h>\n");
	fprintf(out, "#include <Xm/RCUtilsP.h>\n");
	fprintf(out, "#include <Xm/RowColumnP.h>\n");
	fprintf(out, "#include <Xm/SashP.h>\n");
	fprintf(out, "#include <Xm/ScaleP.h>\n");
	fprintf(out, "#include <Xm/ScreenP.h>\n");
	fprintf(out, "#include <Xm/ScrollBarP.h>\n");
	fprintf(out, "#include <Xm/ScrolledWP.h>\n");
	fprintf(out, "#include <Xm/SelectioBP.h>\n");
	fprintf(out, "#include <Xm/SeparatoGP.h>\n");
	fprintf(out, "#include <Xm/SeparatorP.h>\n");
	fprintf(out, "#include <Xm/ShellEP.h>\n");
	fprintf(out, "#include <Xm/TearOffBP.h>\n");
	fprintf(out, "#include <Xm/TearOffP.h>\n");
	fprintf(out, "#include <Xm/TextFP.h>\n");
	fprintf(out, "#include <Xm/TextFSelP.h>\n");
	fprintf(out, "#include <Xm/TextInP.h>\n");
	fprintf(out, "#include <Xm/TextOutP.h>\n");
	fprintf(out, "#include <Xm/TextP.h>\n");
	fprintf(out, "#include <Xm/TextSelP.h>\n");
	fprintf(out, "#include <Xm/TextStrSoP.h>\n");
	fprintf(out, "#include <Xm/ToggleBGP.h>\n");
	fprintf(out, "#include <Xm/ToggleBP.h>\n");
	fprintf(out, "#include <Xm/TransltnsP.h>\n");
	fprintf(out, "#include <Xm/VaSimpleP.h>\n");
	fprintf(out, "#include <Xm/VendorSEP.h>\n");
	fprintf(out, "#include <Xm/VendorSP.h>\n");
	fprintf(out, "#include <Xm/VirtKeysP.h>\n");
	fprintf(out, "#include <Xm/WorldP.h>\n");
	fprintf(out, "#include <Xm/XmP.h>\n");
	fprintf(out, "#include <Xm/XmosP.h>\n");
	fprintf(out, "\n");

	while (fgets(line, sizeof(line), in) != NULL) {
	        /* "security", try to avoid buffer overflows */
		if (strlen(line)>(sizeof(t)-1))
			continue;
		if (sscanf(line, "%[^:]:%s\n", t, f) != 2)
			continue;
		if (t[0] == '#')
			continue;

		count++;

		fprintf(out, "%s i%s(Widget w)\n{\n\treturn %s(w);\n}\n\n",
			t, f, f);
	}
	fprintf(out, "#endif\n");
	fclose(in);
	fclose(out);

	fprintf(stdout, "%s: treated %d inputs\n", argv[0], count);

	return 0;
}
