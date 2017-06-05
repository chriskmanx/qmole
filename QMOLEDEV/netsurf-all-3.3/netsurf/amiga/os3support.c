/*
 * Copyright 2014 Chris Young <chris@unsatisfactorysoftware.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/** \file
 * Minimal compatibility header for AmigaOS 3
 */

#ifndef __amigaos4__
#include "os3support.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>

#include <proto/bullet.h>
#include <proto/exec.h>
#include <proto/intuition.h>
#include <proto/dos.h>
#include <proto/utility.h>

#include <diskfont/diskfonttag.h>
#include <intuition/gadgetclass.h>

#include "utils/log.h"

#define SUCCESS (TRUE)
#define FAILURE (FALSE)
#define NO      !

/* Diskfont */
struct OutlineFont *OpenOutlineFont(STRPTR fileName, struct List *list, ULONG flags)
{
	BPTR fh = 0;
	int64 size = 0;
	struct TagItem *ti;
	UBYTE *buffer;
	STRPTR fname, otagpath;
	struct BulletBase *BulletBase;
	struct OutlineFont *of = NULL;
	struct GlyphEngine *gengine;
	char *p = 0;

	if(p = strrchr(fileName, '.'))
		*p = '\0';

	otagpath = (STRPTR)ASPrintf("FONTS:%s.otag", fileName);
	fh = Open(otagpath, MODE_OLDFILE);

	if(p) *p = '.';

	if(fh == 0) {
		/*\todo we should be opening the .font file too and checking
		 * for the magic bytes to indicate this is an outline font.
		 */
		LOG(("Unable to open %s", otagpath));
		FreeVec(otagpath);
		return NULL;
	}
	
	size = GetFileSize(fh);
	buffer = (struct TagItem *)AllocVec(size, MEMF_ANY);
	if(buffer == NULL) {
		LOG(("Unable to allocate memory"));
		Close(fh);
		FreeVec(otagpath);
		return NULL;
	}
	
	Read(fh, buffer, size);
	Close(fh);

	/* The first tag is supposed to be OT_FileIdent and should equal 'size' */
	struct TagItem *tag = (struct TagItem *)buffer;
	if((tag->ti_Tag != OT_FileIdent) || (tag->ti_Data != (ULONG)size)) {
		LOG(("Invalid OTAG file"));
		FreeVec(buffer);
		FreeVec(otagpath);
		return NULL;
	}

	/* Relocate all the OT_Indirect tags */
	while (ti = NextTagItem(&tag)) {
		if(ti->ti_Tag & OT_Indirect) {
			ti->ti_Data += buffer;
		}
	}

	/* Find OT_Engine and open the font engine */
	if(ti = FindTagItem(OT_Engine, buffer)) {
		LOG(("Using font engine %s", ti->ti_Data));
		fname = ASPrintf("%s.library", ti->ti_Data);
	} else {
		LOG(("Cannot find OT_Engine tag"));
		FreeVec(buffer);
		FreeVec(otagpath);
		return NULL;
	}

	BulletBase = OpenLibrary(fname, 0L);

	if(BulletBase == NULL) {
		LOG(("Unable to open %s", fname));
		FreeVec(buffer);
		FreeVec(fname);
		FreeVec(otagpath);
	}

	FreeVec(fname);

	gengine = OpenEngine();
	
	SetInfo(gengine,
		OT_OTagPath, otagpath,
		OT_OTagList, buffer,
		TAG_DONE);
	
	of = AllocVec(sizeof(struct OutlineFont), MEMF_CLEAR);
	if(of == NULL) return NULL;

	of->BulletBase = BulletBase;
	of->GEngine = gengine;
	of->OTagPath = otagpath;
	of->olf_OTagList = buffer;

	return of;
}

void CloseOutlineFont(struct OutlineFont *of, struct List *list)
{
	struct BulletBase *BulletBase = of->BulletBase;
	
	CloseEngine(of->GEngine);
	CloseLibrary(BulletBase);
	
	FreeVec(of->OTagPath);
	FreeVec(of->olf_OTagList);
	FreeVec(of);
}


/* DOS */
int64 GetFileSize(BPTR fh)
{
	int32 size = 0;
	struct FileInfoBlock *fib = AllocVec(sizeof(struct FileInfoBlock), MEMF_ANY);
	if(fib == NULL) return 0;

	ExamineFH(fh, fib);
	size = fib->fib_Size;

	FreeVec(fib);
	return (int64)size;
}

void FreeSysObject(ULONG type, APTR obj)
{
	switch(type) {
		case ASOT_PORT:
			DeleteMsgPort(obj);
		break;
		case ASOT_IOREQUEST:
			DeleteIORequest(obj);
		break;
	}
}


/* Exec */
struct Node *GetHead(struct List *list)
{
	struct Node *res = NULL;

	if ((NULL != list) && (NULL != list->lh_Head->ln_Succ))
	{
		res = list->lh_Head;
	}
	return res;
}

struct Node *GetPred(struct Node *node)
{
	if (node->ln_Pred->ln_Pred == NULL) return NULL;
	return node->ln_Pred;
}

struct Node *GetSucc(struct Node *node)
{
	if (node->ln_Succ->ln_Succ == NULL) return NULL;
	return node->ln_Succ;
}


/* Intuition */
uint32 GetAttrs(Object *obj, Tag tag1, ...)
{
	va_list ap;
	Tag tag = tag1;
	ULONG data = 0;
	int i = 0;

	va_start(ap, tag1);

	while(tag != TAG_DONE) {
		data = va_arg(ap, ULONG);
		i += GetAttr(tag, obj, (void *)data);
		tag = va_arg(ap, Tag);
	}
	va_end(ap);

	return i;
}

ULONG RefreshSetGadgetAttrsA(struct Gadget *g, struct Window *w, struct Requester *r, struct TagItem *tags)
{
	ULONG retval;
	BOOL changedisabled = FALSE;
	BOOL disabled;

	if (w) {
		if (FindTagItem(GA_Disabled,tags)) {
			changedisabled = TRUE;
 			disabled = g->Flags & GFLG_DISABLED;
 		}
 	}
	retval = SetGadgetAttrsA(g,w,r,tags);
	if (w && (retval || (changedisabled && disabled != (g->Flags & GFLG_DISABLED)))) {
		RefreshGList(g,w,r,1);
		retval = 1;
	}
	return retval;
}

ULONG RefreshSetGadgetAttrs(struct Gadget *g, struct Window *w, struct Requester *r, Tag tag1, ...)
{
	return RefreshSetGadgetAttrsA(g,w,r,(struct TagItem *) &tag1);
}


/* Utility */
struct FormatContext
{
	STRPTR	Index;
	LONG	Size;
	BOOL	Overflow;
};

STATIC VOID ASM
StuffChar(
	REG(a3, struct FormatContext *	Context),
	REG(d0, UBYTE Char))
{
	/* Is there still room? */
	if(Context->Size > 0)
	{
		(*Context->Index) = Char;

		Context->Index++;
		Context->Size--;

		/* Is there only a single character left? */
		if(Context->Size == 1)
		{
			/* Provide null-termination. */
			(*Context->Index) = '\0';

			/* Don't store any further characters. */
			Context->Size = 0;
		}
	}
	else
	{
		Context->Overflow = TRUE;
	}
}

BOOL
VSPrintfN(
	LONG			MaxLen,
	STRPTR			Buffer,
	const STRPTR	FormatString,
	const va_list	VarArgs)
{
	BOOL result = FAILURE;

	/* format a text, but place only up to MaxLen
	 * characters in the output buffer (including
	 * the terminating NUL)
	 */

	if (Buffer == NULL || FormatString == NULL) return(result);

	if(MaxLen > 1)
	{
		struct FormatContext Context;

		Context.Index		= Buffer;
		Context.Size		= MaxLen;
		Context.Overflow	= FALSE;

		RawDoFmt(FormatString,(APTR)VarArgs,(VOID (*)())StuffChar,(APTR)&Context);

		if(NO Context.Overflow)
			result = SUCCESS;
	}

	return(result);
}

BOOL
SPrintfN(
	LONG			MaxLen,
	STRPTR			Buffer,
	const STRPTR	FormatString,
					...)
{
	va_list VarArgs;
	BOOL result = FAILURE;

	/* format a text, varargs version */

	if (Buffer == NULL && FormatString == NULL) return result;

	va_start(VarArgs,FormatString);
	result = VSPrintfN(MaxLen,Buffer,FormatString,VarArgs);
	va_end(VarArgs);

	return(result);
}

char *ASPrintf(const char *fmt, ...)
{
  int r;
  va_list ap;
  static char buffer[2048];
  char *rbuf;
  
  va_start(ap, fmt);
  r = VSPrintfN(2048, buffer, (const STRPTR)fmt, ap);
  va_end(ap);

	r = strlen(buffer);
	rbuf = AllocVec(r+1, MEMF_CLEAR);
	if (rbuf != NULL)
	{
		strncpy(rbuf, buffer, r);
	}
	return rbuf;
}

/* C */
char *strlwr(char *str)
{
  size_t i;
  size_t len = strlen(str);

  for(i=0; i<len; i++)
  str[i] = tolower((unsigned char)str[i]);

  return str;
}

char *strsep(char **s1, const char *s2)
{
	char *const p1 = *s1;

	if (p1 != NULL) {
		*s1 = strpbrk(p1, s2);
		if (*s1 != NULL) {
			*(*s1)++ = '\0';
		}
	}
	return p1;
}

int scandir(const char *dir, struct dirent ***namelist,
  int (*filter)(const struct dirent *),
  int (*compar)(const struct dirent **, const struct dirent **))
{
	/*\todo stub function, needs writing, preferably into clib2 */
	return 0;
}

long long int strtoll(const char *nptr, char **endptr, int base)
{
	return (long long int)strtol(nptr, endptr, base);
}

#endif

