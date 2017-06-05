/*
 * Copyright 2008-2010 Chris Young <chris@unsatisfactorysoftware.co.uk>
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

#include <sys/stat.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/utility.h>

#include "utils/corestrings.h"
#include "utils/log.h"
#include "utils/file.h"
#include "utils/messages.h"
#include "utils/url.h"

#include "desktop/gui_window.h"

#include "amiga/gui.h"
#include "amiga/misc.h"
#include "amiga/utf8.h"

void *ami_misc_allocvec_clear(int size, UBYTE value)
{
#ifdef __amigaos4__
	return AllocVecTags(size, AVT_ClearWithValue, value, TAG_DONE);
#else
	void *mem = AllocVec(size, MEMF_CLEAR | MEMF_ANY);
	if (mem && (value != 0)) {
		memset(mem, value, size);
	}
	return mem;
#endif
}

static LONG ami_misc_req(const char *message, uint32 type)
{
	LONG ret = 0;

	LOG(("%s", message));
#ifdef __amigaos4__
	ret = TimedDosRequesterTags(
		TDR_TitleString,  messages_get("NetSurf"),
		TDR_FormatString, message,
		TDR_GadgetString, messages_get("OK"),
		TDR_ImageType, type,
		TDR_Window, cur_gw ? cur_gw->shared->win : NULL,
		TAG_DONE);
#else
	printf("%s\n", message);
#endif
	return ret;
}

void ami_misc_fatal_error(const char *message)
{
	ami_misc_req(message, TDRIMAGE_ERROR);
}

void warn_user(const char *warning, const char *detail)
{
	char *utf8warning = ami_utf8_easy(messages_get(warning));
	STRPTR bodytext = ASPrintf("\33b%s\33n\n%s",
		utf8warning != NULL ? utf8warning : warning, detail);

	ami_misc_req(bodytext, TDRIMAGE_WARNING);

	if(bodytext) FreeVec(bodytext);
	if(utf8warning) free(utf8warning);
}

int32 ami_warn_user_multi(const char *body, const char *opt1, const char *opt2, struct Window *win)
{
	int res = 0;
#ifdef __amigaos4__
	char *utf8text = ami_utf8_easy(body);
	char *utf8gadget1 = ami_utf8_easy(messages_get(opt1));
	char *utf8gadget2 = ami_utf8_easy(messages_get(opt2));
	char *utf8gadgets = ASPrintf("%s|%s", utf8gadget1, utf8gadget2);
	free(utf8gadget1);
	free(utf8gadget2);

	res = TimedDosRequesterTags(TDR_ImageType, TDRIMAGE_WARNING,
		TDR_TitleString, messages_get("NetSurf"),
		TDR_FormatString, utf8text,
		TDR_GadgetString, utf8gadgets,
		TDR_Window, win,
		TAG_DONE);

	if(utf8text) free(utf8text);
	if(utf8gadgets) FreeVec(utf8gadgets);
#else
#warning write this for os3
#endif	
	return res;
}

/**
 * Create a path from a nsurl using amiga file handling.
 *
 * @param[in] url The url to encode.
 * @param[out] path_out A string containing the result path which should
 *                      be freed by the caller.
 * @return NSERROR_OK and the path is written to \a path or error code
 *         on faliure.
 */
static nserror amiga_nsurl_to_path(struct nsurl *url, char **path_out)
{
	lwc_string *urlpath;
	char *path;
	bool match;
	lwc_string *scheme;
	nserror res;
	char *colon;
	char *slash;

	if ((url == NULL) || (path_out == NULL)) {
		return NSERROR_BAD_PARAMETER;
	}

	scheme = nsurl_get_component(url, NSURL_SCHEME);

	if (lwc_string_caseless_isequal(scheme, corestring_lwc_file,
					&match) != lwc_error_ok)
	{
		return NSERROR_BAD_PARAMETER;
	}
	lwc_string_unref(scheme);
	if (match == false) {
		return NSERROR_BAD_PARAMETER;
	}

	urlpath = nsurl_get_component(url, NSURL_PATH);
	if (urlpath == NULL) {
		return NSERROR_BAD_PARAMETER;
	}

	res = url_unescape(lwc_string_data(urlpath) + 1, &path);
	lwc_string_unref(urlpath);
	if (res != NSERROR_OK) {
		return res;
	}

	colon = strchr(path, ':');
	if(colon == NULL)
	{
		slash = strchr(path, '/');
		if(slash)
		{
			*slash = ':';
		}
		else
		{
			int len = strlen(path);
			path[len] = ':';
			path[len + 1] = '\0';
		}
	}

	*path_out = path;

	return NSERROR_OK;
}

/**
 * Create a nsurl from a path using amiga file handling.
 *
 * Perform the necessary operations on a path to generate a nsurl.
 *
 * @param[in] path The path to convert.
 * @param[out] url_out pointer to recive the nsurl, The returned url
 *                     must be unreferenced by the caller.
 * @return NSERROR_OK and the url is placed in \a url or error code on
 *         faliure.
 */
static nserror amiga_path_to_nsurl(const char *path, struct nsurl **url_out)
{
	char *colon = NULL;
	char *r = NULL;
	char newpath[1024 + strlen(path)];
	BPTR lock = 0;
	nserror ret;

	if((lock = Lock(path, SHARED_LOCK))) {
		DevNameFromLock(lock, newpath, sizeof newpath, DN_FULLPATH);
		UnLock(lock);
	}
	else strlcpy(newpath, path, sizeof newpath);

	r = malloc(strlen(newpath) + SLEN("file:///") + 1);
	if (r == NULL) {
		return NSERROR_NOMEM;
	}

	if((colon = strchr(newpath, ':'))) *colon = '/';

	strcpy(r, "file:///");
	strcat(r, newpath);

	ret = nsurl_create(r, url_out);
	free(r);

	return ret;
}

/**
 * returns a string with escape chars translated.
 * (based on remove_underscores from utils.c)
 */

char *translate_escape_chars(const char *s)
{
	size_t i, ii, len;
	char *ret;
	len = strlen(s);
	ret = malloc(len + 1);
	if (ret == NULL)
		return NULL;
	for (i = 0, ii = 0; i < len; i++) {
		if (s[i] != '\\') {
			ret[ii++] = s[i];
		}
		else if (s[i+1] == 'n') {
			ret[ii++] = '\n';
			i++;
		}
	}
	ret[ii] = '\0';
	return ret;
}

/**
 * Generate a posix path from one or more component elemnts.
 *
 * If a string is allocated it must be freed by the caller.
 *
 * @param[in,out] str pointer to string pointer if this is NULL enough
 *                    storage will be allocated for the complete path.
 * @param[in,out] size The size of the space available if \a str not
 *                     NULL on input and if not NULL set to the total
 *                     output length on output.
 * @param[in] nelm The number of elements.
 * @param[in] ap The elements of the path as string pointers.
 * @return NSERROR_OK and the complete path is written to str
 *         or error code on faliure.
 */
static nserror amiga_vmkpath(char **str, size_t *size, size_t nelm, va_list ap)
{
	const char *elm[16];
	size_t elm_len[16];
	size_t elm_idx;
	char *fname;
	size_t fname_len = 0;

	/* check the parameters are all sensible */
	if ((nelm == 0) || (nelm > 16)) {
		return NSERROR_BAD_PARAMETER;
	}
	if ((*str != NULL) && (size == NULL)) {
		/* if the caller is providing the buffer they must say
		 * how much space is available.
		 */
		return NSERROR_BAD_PARAMETER;
	}

	/* calculate how much storage we need for the complete path
	 * with all the elements.
	 */
	for (elm_idx = 0; elm_idx < nelm; elm_idx++) {
		elm[elm_idx] = va_arg(ap, const char *);
		/* check the argument is not NULL */
		if (elm[elm_idx] == NULL) {
			return NSERROR_BAD_PARAMETER;
		}
		elm_len[elm_idx] = strlen(elm[elm_idx]);
		fname_len += elm_len[elm_idx];
	}
	fname_len += nelm; /* allow for separators and terminator */

	/* ensure there is enough space */
	fname = *str;
	if (fname != NULL) {
		if (fname_len > *size) {
			return NSERROR_NOSPACE;
		}
	} else {
		fname = malloc(fname_len);
		if (fname == NULL) {
			return NSERROR_NOMEM;
		}
	}

	/* copy the first element complete */
	memmove(fname, elm[0], elm_len[0]);
	fname[elm_len[0]] = 0;

	/* add the remaining elements */
	for (elm_idx = 1; elm_idx < nelm; elm_idx++) {
		if (!AddPart(fname, elm[elm_idx], fname_len)) {
			break;
		}
	}

	*str = fname;
	if (size != NULL) {
		*size = fname_len;
	}

	return NSERROR_OK;
}

/**
 * Get the basename of a file using posix path handling.
 *
 * This gets the last element of a path and returns it.
 *
 * @param[in] path The path to extract the name from.
 * @param[in,out] str Pointer to string pointer if this is NULL enough
 *                    storage will be allocated for the path element.
 * @param[in,out] size The size of the space available if \a
 *                     str not NULL on input and set to the total
 *                     output length on output.
 * @return NSERROR_OK and the complete path is written to str
 *         or error code on faliure.
 */
static nserror amiga_basename(const char *path, char **str, size_t *size)
{
	const char *leafname;
	char *fname;

	if (path == NULL) {
		return NSERROR_BAD_PARAMETER;
	}

	leafname = FilePart(path);
	if (leafname == NULL) {
		return NSERROR_BAD_PARAMETER;
	}

	fname = strdup(leafname);
	if (fname == NULL) {
		return NSERROR_NOMEM;
	}

	*str = fname;
	if (size != NULL) {
		*size = strlen(fname);
	}
	return NSERROR_OK;
}

/**
 * Ensure that all directory elements needed to store a filename exist.
 *
 * @param fname The filename to ensure the path to exists.
 * @return NSERROR_OK on success or error code on failure.
 */
static nserror amiga_mkdir_all(const char *fname)
{
	char *dname;
	char *sep;
	struct stat sb;

	dname = strdup(fname);

	sep = strrchr(dname, '/');
	if (sep == NULL) {
		/* no directory separator path is just filename so its ok */
		free(dname);
		return NSERROR_OK;
	}

	*sep = 0; /* null terminate directory path */

	if (stat(dname, &sb) == 0) {
		free(dname);
		if (S_ISDIR(sb.st_mode)) {
			/* path to file exists and is a directory */
			return NSERROR_OK;
		}
		return NSERROR_NOT_DIRECTORY;
	}
	*sep = '/'; /* restore separator */

	sep = dname;
	while (*sep == '/') {
		sep++;
	}
	while ((sep = strchr(sep, '/')) != NULL) {
		*sep = 0;
		if (stat(dname, &sb) != 0) {
			if (nsmkdir(dname, S_IRWXU) != 0) {
				/* could not create path element */
				free(dname);
				return NSERROR_NOT_FOUND;
			}
		} else {
			if (! S_ISDIR(sb.st_mode)) {
				/* path element not a directory */
				free(dname);
				return NSERROR_NOT_DIRECTORY;
			}
		}
		*sep = '/'; /* restore separator */
		/* skip directory separators */
		while (*sep == '/') {
			sep++;
		}
	}

	free(dname);
	return NSERROR_OK;
}

/* amiga file handling operations */
static struct gui_file_table file_table = {
	.mkpath = amiga_vmkpath,
	.basename = amiga_basename,
	.nsurl_to_path = amiga_nsurl_to_path,
	.path_to_nsurl = amiga_path_to_nsurl,
	.mkdir_all = amiga_mkdir_all,
};

struct gui_file_table *amiga_file_table = &file_table;
